---
editor_options: 
  markdown: 
    wrap: 72
---

# Species PDF outlooks

`generate_species_pdf.py` fills `Species_Summary_PDF.template.qmd` from
each `<species>_Summary_Dash_3tab.qmd`. Dashboards are the source of
species content; the template controls the PDF layout and parameter
logic. Generated files are named `<species>_Summary_PDF.qmd` and use
existing figures and bibliography files.

## Generate or refresh PDF sources

Requires Python 3.9 or newer, with no third-party Python packages. Run
these commands from the repository root (on Windows, `py` can replace
`python`):

``` sh
# One species; creates Fd_Summary_PDF.qmd beside its dashboard.
python Draft_Species_Summaries/generate_species_pdf.py Draft_Species_Summaries/Fd_Summary_Dash_3tab.qmd

# Refresh all species after editing their dashboards or the shared template.
python Draft_Species_Summaries/generate_species_pdf.py --all --force

# Generate and render one portrait report in the same command.
python Draft_Species_Summaries/generate_species_pdf.py Draft_Species_Summaries/Cw_Summary_Dash_3tab.qmd --force --render --time-period 2041_2060 --edatope C4
```

`--all` discovers species dashboards beside the script and excludes
`Template_Summary_Dash_3tab.qmd`. Multiple explicit dashboard paths are
also accepted. Existing PDF QMDs require `--force`; edit content in the
dashboard and formatting in the PDF template, then regenerate. Manual
edits to generated QMDs are replaced. All inputs are validated before
any QMD in a batch is written.

`--time-period` and `--edatope` set defaults in generated QMDs. They can
still be overridden with Quarto's `-P` options when rendering.
`--render` writes `<species>_Summary_<period>_<edatope>.pdf` beside the
QMD and replaces a previous PDF with that name. Generation itself does
not require R or LaTeX and does not execute dashboard code.

The blue header reads the generated document title automatically; no
species name is hard-coded in `pdf-header.tex`. Long titles are scaled
to fit portrait pages. Copy the shared template, generator, and header
together if moving this workflow to another repository.

## Dashboard structure supported by the generator

The existing Fd, Cw, Pl, and Sx dashboards are supported. New species
should follow `Template_Summary_Dash_3tab.qmd` and retain these
structural headings:

-   `Col - Welcome text`, `Summary text`, and `BGC text`.
-   `L Column - Plot description`, `L col`, and `Col - description`.
-   `Forest health` with level-two subsections; their names can vary by
    species.
-   `Other reference materials`.

Widths, tabsets, cards, HTML line breaks, and repeated captions are
converted for print. The generator retains Markdown citations and the
source's species-specific figure captions. Historical captions follow
the edatope selection. The first level-one heading supplies the species
name. The `Summary text` section must retain its bold geographic-range
and historical-suitability subheadings.

Metadata (`title`, `bibliography`, `csl`) and `fig_path` must be
single-line values. Figure calls must use literal filenames, as in the
current dashboards. Required figure families are
`Historic_Suit_Map.<species>.<edatope>.png`,
`One_Panel.<species>.<edatope>.<period>.png`,
`<species>_<edatope>_bub.png`, `<species>_<edatope>_alluv.png`, and
`<species>_histpie.png`, with all three edatopes and five periods.
Filename case is resolved against the actual local files.

Missing/duplicate sections, assets, or inconsistent shared captions stop
generation with an error. If the dashboard structure changes, update the
extractor rather than silently omitting content. This is a converter for
the species-dashboard convention, not an arbitrary Quarto-to-PDF
converter. The tree silhouette, dashboard controls, and
projection-navigation overview are omitted from print. General species
narrative remains in single-edatope reports. Review pagination after
adding a species or substantially expanding text.

Run generator checks with:

``` sh
python -m unittest discover -s Draft_Species_Summaries -p test_generate_species_pdf.py
```

## Render

Requires Quarto, R with `knitr` and `rmarkdown`, and a working XeLaTeX
installation (for example TinyTeX or MiKTeX). No tidyverse or gt
dependency is needed. The PDF uses Segoe UI (the dashboard's system font
on Windows), which must be installed on the rendering machine. Its
first-page header uses `pdf-header.tex` and the dashboard's existing
`images/BCID_H_RGB_rev.png` logo with `#003366` blue.

From the repository root:

``` sh
quarto render Draft_Species_Summaries/Fd_Summary_PDF.qmd --to pdf -P time_period:2041_2060
```

The default period is `2041_2060`. Accepted values are `2001_2020`,
`2021_2040`, `2041_2060`, `2061_2080`, and `2081_2100`. Invalid values
and missing figure files stop rendering with an explanatory error.
Unquoted command-line values such as `-P time_period:2041_2060` are
supported: the template restores the underscore if YAML interprets the
value as an integer. Run
`Rscript Draft_Species_Summaries/test_pdf_parameters.R` from the
repository root to check this parameter handling (requires the R `yaml`
package).

The `edatope` parameter defaults to `all` (C4, B2, and D6 on landscape
pages). Choose `C4`, `B2`, or `D6` for a portrait report containing only
that edatope's historical maps, projection maps, bubble charts, and
suitable-area charts. Orientation is set automatically during rendering;
no separate format or orientation option is required. The historical map
interpretation is also filtered. General species narrative and the
province-wide pie chart are retained.

For example, from the repository root:

``` sh
quarto render Draft_Species_Summaries/Fd_Summary_PDF.qmd --to pdf -P time_period:2041_2060 -P edatope:C4
```

To save a distinct PDF for another period, run from the document folder:

``` sh
cd Draft_Species_Summaries
quarto render Fd_Summary_PDF.qmd --to pdf -P time_period:2081_2100 --output Fd_Summary_2081_2100.pdf
```

These commands write output beside the source document. The same
parameter can be supplied from R, for a future download handler or a
batch export:

``` r
quarto::quarto_render(
  "Draft_Species_Summaries/Fd_Summary_PDF.qmd",
  output_format = "pdf",
  execute_params = list(time_period = "2041_2060", edatope = "all"),
  output_file = "Fd_Summary_2041_2060.pdf"
)
```

The R example additionally requires the `quarto` R package. Concurrent
Shiny downloads should render in separate temporary working directories
containing the QMD, `pdf-header.tex`, `images/BCID_H_RGB_rev.png`,
`Figures/Fd`, `referencesFd.bib`, and `apa.csl`, to avoid colliding
intermediate files. This prototype does not add a dashboard or Shiny
download button.

## Layout and parameter scope

-   With `edatope: all`, landscape letter pages keep C4, B2, and D6 side
    by side, in dashboard order. A single edatope uses portrait letter
    pages.
-   `time_period` selects the `One_Panel.Fd.<edatope>.<period>.png`
    maps.
-   Historical maps and the distribution pie chart remain at the
    reference period.
-   Bubble and alluvial charts are existing multi-period PNGs. They
    retain their full time series; bubble charts always highlight
    2041-2060. Selecting a period does not regenerate or crop those
    charts.
-   The narrative describes broad trends across the century. The
    selected period is printed explicitly in the summary and
    suitability-change section.
-   The map caption comes from the dashboard, with left/right references
    adapted to the map and inset layout.

If Quarto cannot locate an existing R user library on Windows, set
`R_LIBS_USER` to that library for the rendering session before running
the command.
