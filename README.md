<!-- badges: start -->
[![R-CMD-check](https://github.com/meztez/CCISS_ShinyApp/workflows/R-CMD-check/badge.svg)](https://github.com/meztez/CCISS_ShinyApp/actions)
[![Codecov test coverage](https://codecov.io/gh/meztez/CCISS_ShinyApp/branch/main/graph/badge.svg)](https://codecov.io/gh/meztez/CCISS_ShinyApp?branch=main)
[![R-CMD-check](https://github.com/bcgov/CCISS_ShinyApp/workflows/R-CMD-check/badge.svg)](https://github.com/bcgov/CCISS_ShinyApp/actions)
<!-- badges: end -->

# CCISS_ShinyApp
CCISS RMD knitr report graphics and Shiny App
This repository is a fork of a development CCISS R script for devexchange code-with-us development.
There are two development tasks.
1. Add in several graphical summaries for an existing CCISS RMD for a formatted knitr report of the RMD. The graphical formats parallel existing web reporting 
2. Build a shiny app version of the CCISS rmd.
 A scaleable BEC map interface for retrieving lat/long for submission to the CCISS script. 
 Graphical output of BGC futures, tree species feasibility ratings by timeperiod, and display of existing stocking standard recommendations

## Usage

```r
library(CCISS)
launch_app()
```

## Installation

```r
remotes::install_github("bcgov/CCISS_ShinyApp")
```

## Repository structure

#### R packages folders

##### data
Standard R package folder for the
[Data in packages](https://cran.r-project.org/doc/manuals/R-exts.html#Data-in-packages).
Data is generated from `create_package_data` script in `data-raw`. The script is run manually
to update data as needed. When a package includes data, they are available like other objects
after you load the library.

##### data-raw
[Commonly used](https://r-pkgs.org/data.html?q=data-raw#data-sysdata) to store scripts to create
package assets. This can include model training, package data creation or, like this package, tiles
creation. Usually code in this folder is ran manually and is not part of the package but kept with
the package repository.

##### inst
Standard R package folder. The contents of the `inst` subdirectory will be copied recursively
to the installation directory. This would be where you put your `Shiny` app or `plumber` api with
there required assets. Again, this is described in
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

##### tests
The fun part. [Tests are a great way](https://testthat.r-lib.org/) to make sure changes do not
break package behavior. Writing tests is pretty straightforward and should be done as you
build functions. You are doing it anyway, might as well make it a test.

#### Building packages
R is a great community. There is a lot of great resources on how to build packages. I highly
recommend [R packages by Hadley Wickham and Jenny Bryan](https://r-pkgs.org/).

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

