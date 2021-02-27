## Github Actions Workflows

Workflows are setup or updated with the following instruction.

```r
usethis::use_tidy_github_actions()
```

Custom modification are made to remove non targeted system in R-CMD-Check.  
I kept windows/ubuntu with R release version.
Switched Ubuntu version to 20.04 (focal) to benefits from latest GDAL, GEOS, PROJ lib versions.