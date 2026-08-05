# Welcome to CCISS!!

[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](<Redirect-URL>)

This is the main github repo for the [CCISS Web Tool](https://bcgov-ffec.ca/cciss/).

## What is CCISS?

Climate Change Informed Species Selection (CCISS – pronounced ‘kiss’) is a Biogeoclimatic Ecosystem Classification-based analysis framework built to anticipate the change climate implications to tree species environmental suitability at a site specific level. The CCISS tool is a web-based application that makes this analysis accessible to practitioners to help guide climate change adaptation in reforestation decisions.

Understanding climate- and site-level species suitability is one of the foundational pieces of information that practitioners require for the creation of silvicultural prescriptions that will lead to successful reforestation over a rotation period. Climate change will affect this goal by progressively altering environmental conditions and therefore the suitability of tree species established on a site over time.

To address this challenge, the CCISS tool projects changes to species environmental suitability at a site series level for any user selected location in the province and estimates the future suitability of a tree species to this changing climate. To account for future climate uncertainty the tool looks at a wide range of global climate change models and emissions scenarios to capture the range of plausible climate futures for any location in BC in 20-year periods out to 2100.

To assist users, the tool compares the current species selection guidance in the Chief Foresters Reference Guide with the future forecast from the CCISS analysis. Reports from the tool highlight where currently acceptable species are stable/improving or declining/unsuitable and where new species have become suitable and could be considered as candidates for assisted migration.

The CCISS tool is written in R, using the Shiny package. The main codebase in contained here in `./app`, which relies on functions from the corresponding R package, `ccissr`. Note that CCISS relies heavily on a remote Postgres server as well as a tileserver, and so will in general not be possible to run locally. 

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

