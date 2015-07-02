# Introduction #
dc.js and crossfilter are quite useful for visualization and exploratory analysis. Although many javascript library already have R packages to use them with Shiny, there is no R package to knowledge to use dc.js in Shiny. Our motivation is to develop an R package that allows us to use dc.js and crossfilter with Shiny, so that we can interactively and quickly build dc.js charts. This project is still in very early stage and just made proof-of-concept to demonstrate the ability to incorporate dc.js and crossfilter in Shiny.

# Installation #
* Prerequisite
We need two R packages:
install.packages("devtools")
install.packgaes("jsonlite")
* Download this repository and open dcr.Rproj
* Install the package
library(devtools)
load_all()
install()