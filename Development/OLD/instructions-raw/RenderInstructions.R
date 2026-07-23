## render Rmarkdown instructions to PDF
system("rm -r ./app/instructions")
system("mkdir ./app/instructions")

# library(bslib)
# bcgov_theme <- bslib::bs_theme(
#   preset = "bcgov",
#   "navbar-brand-padding-y" = "0rem",
#   "navbar-brand-margin-end" = "4rem"
# )

fnames <- list.files("instructions-raw/")
rmdNames <- fnames[grep(".Rmd",fnames)]

for(fn in rmdNames){
  rmarkdown::render(input = paste0("./instructions-raw/",fn),
                    output_dir = "./app/instructions/",
                    clean = T)
}

