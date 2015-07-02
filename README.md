# Introduction #
dc.js and crossfilter are quite useful for visualization and exploratory analysis. Although many javascript library already have R packages to use them with Shiny, there is no R package to knowledge to use dc.js in Shiny. Our motivation is to develop an R package that allows us to use dc.js and crossfilter with Shiny, so that we can interactively and quickly build dc.js charts. This project is still in very early stage and just made proof-of-concept to demonstrate the ability to incorporate dc.js and crossfilter in Shiny.

# Installation #
###Prerequisite: install necessary packages###
install.packages("devtools")

install.packages("shiny")

install.packgaes("jsonlite")

###Download this repository and open dcr.Rproj###
### Install this dcr package###
library(devtools)

load_all()

install()

# Draw simple dc.js charts#
We will use the mtcars data to make some simple charts

library(dcr)

###Create chart object###
mydcr <- dcr(mtcars)

###Add a pie chart###
chart1 <- dcrchart("pieChart", "chart1", "cyl", reduceCount(), width = 200, height = 200)

###Add a bar chart###
chart2 <- dcrchart("barChart", "chart2", "gear", reduceSum("wt"), 
                   width = 300, height = 200, x = x_ordinal(), xUnits = dc_code("dc.units.ordinal"),
                   xAxisLabel = "Gear", yAxisLabel = "Total Weight")

###Add a row chart###
chart3 <- dcrchart("rowChart", "chart3", "carb", reduceCount(), width = 300, height = 200)

###Finally we can see the charts###
mydcr + chart1 + chart2 + chart3

#Work with Shiny#
After we tested the charts, we can build into Shiny app. The only extra step is to layout the charts.