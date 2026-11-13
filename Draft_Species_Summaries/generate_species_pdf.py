#!/usr/bin/env python3
"""Generate parameterized PDF QMDs from the species 3-tab dashboards.

Python 3.9+, standard library only. Dashboard R code is parsed, never executed.
See README_PDF.md for the supported source structure and rendering examples.
"""
import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
PERIODS = ('2001_2020', '2021_2040', '2041_2060', '2061_2080', '2081_2100')
EDATOPES = ('C4', 'B2', 'D6')
CHUNK = re.compile(r'^```\{r[^\n]*\}\s*\n(.*?)^```\s*$', re.M | re.S)
HEADING = re.compile(r'^(#{1,6})\s+(.+?)\s*$', re.M)
TOKEN = re.compile(r'@@([A-Z_]+)@@')


def scalar(text):
    """Read the single-line scalar subset used by dashboard metadata."""
    text = text.strip()
    if text.startswith('"'):
        try:
            return json.loads(text)
        except ValueError as exc:
            raise ValueError(f'Unsupported quoted scalar: {text}') from exc
    if text.startswith("'") and text.endswith("'"):
        return text[1:-1].replace("''", "'")
    if not text or text[0] in '[{|>':
        raise ValueError(f'Expected a single-line scalar, got: {text}')
    return text


def one(pattern, text, label, flags=0):
    matches = list(re.finditer(pattern, text, flags))
    if len(matches) != 1:
        raise ValueError(f'Expected exactly one {label}; found {len(matches)}')
    return matches[0]


def section(text, title):
    # Mask R chunks so their comments cannot be mistaken for Markdown headings.
    masked = CHUNK.sub(lambda m: ' ' * len(m.group()), text)
    headings = list(HEADING.finditer(masked))
    matches = [h for h in headings if re.sub(r'\s*\{.*\}\s*$', '', h[2]).strip() == title]
    if len(matches) != 1:
        raise ValueError(f'Expected exactly one heading {title!r}; found {len(matches)}')
    h = matches[0]
    end = next((x.start() for x in headings if x.start() > h.start() and len(x[1]) <= len(h[1])), len(text))
    return text[h.end():end].strip()


def clean(text):
    text = re.sub(r'<style\b[^>]*>.*?</style>', '', text, flags=re.S | re.I)
    text = re.sub(r'^\s*:{3,}[^\n]*$', '', text, flags=re.M)
    text = re.sub(r'<br\s*/?>', '\n', text, flags=re.I)
    text = re.sub(r'\n{3,}', '\n\n', text).strip()
    if '```' in text or re.search(r'<(?:div|script|iframe)\b', text, re.I):
        raise ValueError('Unsupported executable/HTML content in a prose section; adapt the extractor before generating.')
    return text


def without_leading_bold(text):
    return re.sub(r'^\*\*[^\n]+\*\*\s*\n', '', clean(text), count=1).strip()


def r_string(value):
    return json.dumps(value, ensure_ascii=False)


def r_vector(values):
    return 'c(' + ',\n  '.join(f'{r_string(k)} = {r_string(v)}' for k, v in values.items()) + ')'


def local_asset(base, name):
    path = (base / name).resolve()
    if not path.is_file():
        raise ValueError(f'Missing required asset: {path}')
    return name


def extract(source):
    text = source.read_text(encoding='utf-8-sig')
    front = one(r'\A---\s*\n(.*?)\n---', text, 'YAML front matter', re.S)
    meta = front[1]
    def field(key):
        return scalar(one(r'^' + re.escape(key) + r':\s*(.+)$', meta, key, re.M)[1])
    bibliography = local_asset(source.parent, field('bibliography'))
    csl = local_asset(source.parent, field('csl'))
    field('title')  # Require a valid title, even though the species heading supplies casing.
    body = text[front.end():]
    figure_dir = scalar(one(r'^fig_path\s*<-\s*((?:"[^"\n]*"|\'[^\'\n]*\'))\s*(?:#.*)?$', body, 'fig_path assignment', re.M)[1])
    directory = source.parent / figure_dir
    if not directory.is_dir():
        raise ValueError(f'Missing figure directory: {directory}')
    # Resolve the case mismatches in older dashboard calls (One_panel vs One_Panel).
    assets = {}
    for path in directory.iterdir():
        if path.is_file():
            if path.name.casefold() in assets:
                raise ValueError(f'Ambiguous case-insensitive figure name: {path.name}')
            assets[path.name.casefold()] = path.name
    figures = {}
    for chunk in CHUNK.finditer(body):
        for match in re.finditer(r'^\s*fig\(\s*(["\'])(.+?)\1\s*\)', chunk[1], re.M):
            name = match[2]
            if name.casefold() not in assets:
                raise ValueError(f'Dashboard figure not found: {directory / name}')
            name = assets[name.casefold()]
            cap = re.search(r'^#\| fig-cap:\s*(.+)$', chunk[1], re.M)
            caption = scalar(cap[1]) if cap else ''
            if name in figures:
                raise ValueError(f'Duplicate dashboard figure: {name}')
            figures[name] = caption
    codes = {m[1] for name in figures if (m := re.fullmatch(r'Historic_Suit_Map\.([A-Za-z0-9]+)\.(?:C4|B2|D6)\.png', name))}
    if len(codes) != 1:
        raise ValueError(f'Expected one species code in historical figures; found {sorted(codes)}')
    code = codes.pop()
    def find_figure(pattern):
        hits = [name for name in figures if re.fullmatch(pattern, name, re.I)]
        if len(hits) != 1:
            raise ValueError(f'Expected one figure matching {pattern!r}; found {len(hits)}')
        return hits[0]
    historic = {e: find_figure(rf'Historic_Suit_Map\.{code}\.{e}\.png') for e in EDATOPES}
    bubble = {e: find_figure(rf'{code}_{e}_bub\.png') for e in EDATOPES}
    area = {e: find_figure(rf'{code}_{e}_alluv\.png') for e in EDATOPES}
    projection = {p: {e: find_figure(rf'One_Panel\.{code}\.{e}\.{p}\.png') for e in EDATOPES} for p in PERIODS}
    pie = find_figure(rf'{code}_histpie\.png')
    def caption(name):
        if not figures[name]:
            raise ValueError(f'Missing fig-cap for {name}')
        return figures[name].replace('respresenting', 'representing')
    def shared_caption(group):
        caps = [caption(name) for name in group]
        if len(set(caps)) != 1:
            raise ValueError('Figure captions differ across edatopes/periods; adapt the template to retain each caption.')
        return caps[0]
    reference = section(body, 'Summary text')
    split = one(r'^\*\*Species environmental suitability during the reference period[^\n]*\*\*\s*$', reference, 'historical suitability introduction', re.M)
    geographic = without_leading_bold(reference[:split.start()])
    historical_intro = clean(reference[split.end():])
    health = section(body, 'Forest health')
    headings = list(re.finditer(r'^## (.+)$', health, re.M))
    if not headings:
        raise ValueError('Forest health must contain level-two subsections')
    health_pages = []
    for i, h in enumerate(headings):
        prose = clean(health[h.end():headings[i+1].start() if i+1 < len(headings) else len(health)])
        bold = re.match(r'^\*\*(.+?)\*\*\s*\n', prose)
        heading = bold[1] if bold else h[1]
        if bold:
            prose = prose[bold.end():].strip()
        health_pages.append(f'# Forest health: {heading}\n\n{prose}')
    name = re.search(r'^# (.+)$', CHUNK.sub('', body), re.M)[1]
    data = {
        'TITLE_YAML': r_string(f'{name} ({code}): species outlook'),
        'BIBLIOGRAPHY_YAML': r_string(bibliography), 'CSL_YAML': r_string(csl),
        'SOURCE_NAME': source.name, 'FIG_PATH_R': r_string(figure_dir),
        'HISTORIC_FILES_R': r_vector(historic), 'BUBBLE_FILES_R': r_vector(bubble),
        'AREA_FILES_R': r_vector(area), 'PIE_FILE_R': r_string(pie),
        'PROJECTION_FILES_R': 'list(' + ',\n'.join(f'  {r_string(p)} = {r_vector(v)}' for p, v in projection.items()) + ')',
        'SUMMARY': clean(section(body, 'Col - Welcome text')),
        'LINKS': without_leading_bold(section(body, 'Other reference materials')),
        'RANGE': geographic, 'HISTORIC_INTRO': historical_intro,
        'HISTORIC_CAPTIONS_R': r_vector({e: f'**{e}:** {caption(f)}' for e, f in historic.items()}),
        'BGC_TEXT': without_leading_bold(section(body, 'BGC text')).replace('to the right', 'below'),
        'BUBBLE_TEXT': clean(section(body, 'L Column - Plot description')),
        'BUBBLE_CAPTION': shared_caption(bubble.values()),
        'MAP_TEXT': clean(section(body, 'L col')),
        'MAP_CAPTION': shared_caption([f for group in projection.values() for f in group.values()]).replace('(left)', '(map)').replace('(right)', '(inset)') + ' The displayed maps show the selected period.',
        'AREA_TEXT': clean(section(body, 'Col - description')),
        'AREA_CAPTION': shared_caption(area.values()).replace('(tabs)', '(shown above)') + ' These overview charts retain all time periods.',
        'FOREST_HEALTH': '\n\n{{< pagebreak >}}\n\n'.join(health_pages),
    }
    if any(not v.strip() for v in data.values()):
        raise ValueError('An extracted section is empty')
    return code, data


def generate(source, template, period='2041_2060', edatope='all'):
    code, data = extract(source)
    data.update(PERIOD_YAML=r_string(period), EDATOPE_YAML=r_string(edatope))
    required = set(TOKEN.findall(template))
    if required != set(data):
        raise ValueError(f'Template fields differ: missing={sorted(set(data)-required)}, unknown={sorted(required-set(data))}')
    result = TOKEN.sub(lambda m: data[m[1]], template)
    if TOKEN.search(result):
        raise ValueError('Unresolved template token in generated document')
    return code, result


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('dashboards', nargs='*', type=Path)
    parser.add_argument('--all', action='store_true', help='Generate all species dashboards beside this script (excluding Template)')
    parser.add_argument('--force', action='store_true', help='Replace existing generated PDF QMD files')
    parser.add_argument('--render', action='store_true', help='Also render with Quarto; requires R and XeLaTeX')
    parser.add_argument('--time-period', choices=PERIODS, default='2041_2060')
    parser.add_argument('--edatope', choices=('all', *EDATOPES), default='all')
    args = parser.parse_args(argv)
    if args.all and args.dashboards:
        parser.error('Use either dashboard paths or --all')
    sources = sorted(HERE.glob('*_Summary_Dash_3tab.qmd')) if args.all else args.dashboards
    sources = [p.resolve() for p in sources if not p.name.startswith('Template_')]
    if not sources:
        parser.error('Specify a dashboard path or --all')
    try:
        template = (HERE / 'Species_Summary_PDF.template.qmd').read_text(encoding='utf-8')
        jobs = []
        # Validate the entire batch before writing any outputs.
        for source in sources:
            code, result = generate(source, template, args.time_period, args.edatope)
            target = source.with_name(f'{code}_Summary_PDF.qmd')
            if target.exists() and not args.force:
                raise ValueError(f'{target} exists; use --force to regenerate from the dashboard')
            if target in [job[0] for job in jobs]:
                raise ValueError(f'Duplicate output target: {target}')
            local_asset(source.parent, 'pdf-header.tex')
            local_asset(source.parent, 'images/BCID_H_RGB_rev.png')
            jobs.append((target, result, code))
        for target, result, code in jobs:
            target.write_text(result, encoding='utf-8')
            print(f'Generated {target}', flush=True)
            if args.render:
                subprocess.run(['quarto', 'render', target.name, '--to', 'pdf', '--output',
                                f'{code}_Summary_{args.time_period}_{args.edatope}.pdf'],
                               cwd=target.parent, check=True)
    except (ValueError, OSError, subprocess.CalledProcessError) as exc:
        print(f'Error: {exc}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
