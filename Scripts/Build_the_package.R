# creates description and namespace files
usethis::use_description()
usethis::use_namespace()

# Create R directory
# Go to the R package main directory
base::dir.create("R")

# creates Package-level documentation so you can run ?nameofpackage
usethis::use_package_doc()

# created README.Rmd for Github landing page
# an .Rbuildignore file gets created
usethis::use_readme_rmd()

# Creating an R package
#https://sahirbhatnagar.com/rpkg/

# creates license file
usethis::use_mit_license("Sahir Bhatnagar")

# creates news file
usethis::use_news_md()

# setup continuous integration via travis-ci
#usethis::use_travis()#Deprecated
usethis::use_github_actions()

# sets up testing infrastructure
usethis::use_testthat()

# this will setup the folders needed for the data and raw-data
usethis::use_data_raw()
#usethis::use_data(data, overwrite = TRUE)# This line should be included in an DATASET.R, which has to be executed and kept in data-raw: source("./data-raw/DATASET.R")
# A second script (data.R) has to be created in R/data.R

# This will generate a scaffold for the functions file and documentation
pacman::p_load(sinew)# This can be skipped; ModulonSat_functions.R can be manually created and placed in R/ModulonSat_functions.R
sinew::makeOxyFile("R/ModulonSat_functions.R")

# See available functions
pacman::p_functions("ModulonSat")

devtools::check()# WARNING! Be aware that using check from RStudio build panel is going to wipe out the doc folder; vignettes may stop working properly

usethis::use_vignette(name = "README")

tools::buildVignettes(dir = ".", tangle=TRUE)
dir.create("inst")
dir.create("inst/doc")
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)

# Commit and re-build

browseVignettes('ModulonCore')
vignette('Readme','ModulonCore')

# Dependencies NOT WORKING!
usethis::use_package("igraph", type = "Imports")
usethis::use_package("SANTA", type = "Imports")


# Check the package
devtools::check()


#Update Readme.Rmd
devtools::build_readme()