# Welcome to CCISS!

[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](<Redirect-URL>)

This is the main github repo for the CCISS web tool.

## What is CCISS?

Climate Change Informed Species Selection (CCISS – pronounced ‘kiss’) is a Biogeoclimatic Ecosystem Classification-based analysis framework built to anticipate the change climate implications to tree species environmental suitability at a site specific level. The CCISS tool is a web-based application that makes this analysis accessible to practitioners to help guide climate change adaptation in reforestation decisions.

Understanding climate- and site-level species suitability is one of the foundational pieces of information that practitioners require for the creation of silvicultural prescriptions that will lead to successful reforestation over a rotation period. Climate change will affect this goal by progressively altering environmental conditions and therefore the suitability of tree species established on a site over time.

To address this challenge, the CCISS tool projects changes to species environmental suitability at a site series level for any user selected location in the province and estimates the future suitability of a tree species to this changing climate. To account for future climate uncertainty the tool looks at a wide range of global climate change models and emissions scenarios to capture the range of plausible climate futures for any location in BC in 20-year periods out to 2100.

To assist users, the tool compares the current species selection guidance in the Chief Foresters Reference Guide with the future forecast from the CCISS analysis. Reports from the tool highlight where currently acceptable species are stable/improving or declining/unsuitable and where new species have become suitable and could be considered as candidates for assisted migration.

## Usage

### Installation

```r
remotes::install_github("bcgov/CCISS_ShinyApp")
```

### Run locally

 - Clone repository
 - Setup environment variables
 - Open project
 - Run `shiny::runApp("./app")`

### .Renviron

The app requires the following environment variables.

```bash
BCGOV_USR={postgres user}
BCGOV_PWD={postgres password}
BCGOV_DB={postgres database}
BCGOV_HOST={postgres host}
BCGOV_TILESERVER={tileserver gl x, y, z url template}
BCGOV_TILELAYER={tileserver layer}
BCGOV_MAPBOX_TOKEN={mapbox api key}
BCGOV_MAPBOX_LABELS_STYLE={mapbox labels style ref user/styleref}
BCGOV_MAPBOX_HILLSHADE_STYLE={mapbox hillshade style ref user/styleref}
```

## Repository structure

#### R packages folders

##### app

CCISS Shiny app

##### data
Standard R package folder for the
[Data in packages](https://cran.r-project.org/doc/manuals/R-exts.html#Data-in-packages).
Data is generated from `create_package_data` script in `data-raw/scripts`. The script is run manually
to update data as needed. When a package includes data, they are available like other objects
after you load the library.

##### data-raw
[Commonly used](https://r-pkgs.org/data.html?q=data-raw#data-sysdata) to store scripts to create
package assets. This can include model training, package data creation or, like this package, tiles
creation. Usually code in this folder is ran manually and is not part of the package but kept with
the package repository.

##### inst
Standard R package folder. The contents of the `inst` subdirectory will be copied recursively
to the installation directory. This is described in
[Data in packages](https://cran.r-project.org/doc/manuals/R-exts.html#Data-in-packages)

##### man
Standard R package folder for documentation. Where `roxygen2` saves generated documentation.
This package use [`roxygen2`](https://roxygen2.r-lib.org/) to make documenting functions and
objects easier.

##### R
Also standard, this package R functions, methods and objects are there. The code there
will be included with the package when it is built.

##### src
[Also standard](https://cran.r-project.org/doc/manuals/R-exts.html#Non_002dR-scripts-in-packages).
Code which needs to be compiled.

#### Special

##### .gitignore
Tells git which files to ignore.
See [git documentation](https://git-scm.com/docs/gitignore).

##### .Rbuildignore
Tells R which files to ignore when it builds the package.
See [Writing R extension](https://cran.r-project.org/doc/manuals/R-exts.html#index-_002eRbuildignore-file).

##### .github
Github actions workflows.
 - Package checks (R-CMD-check)
 - Online documentation (pkgdown),
 - Tests coverages report (test-coverage)
 - Style code / run roxygen2 (pr-commands)
You can read more about actions from [r-lib/actions](https://github.com/r-lib/actions)

Workflows are setup or updated with the following instruction.

```r
usethis::use_tidy_github_actions()
```
Custom modifications made.  
 - Reduce the number of target for R CMD check.

To disable a workflow,
go the github actions tab, select a workflow, click on the ... dots on the right
and select disable workflow.

------------------------------------------------------------------------


Copyright 2021 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

