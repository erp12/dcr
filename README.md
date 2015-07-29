# Introduction #
Dc.js and crossfilter are useful for constructing interactive  visualizations and exploratory analysis. This project integrates their features and capabilities into the Shiny. This package is still in the early stages of development, so use with caution.

# Installation #
```
if (!require(devtools)) install.packages("devtools")
devtools:::install_github("massmutual/dcr")
```

# Usage #
We will use the mtcars data to make some simple charts
```
library(dcr)

#create chart object
mydcr <- dcr(mtcars)

#pie chart
chart1 <- dcrchart("pieChart", "chart1", "cyl", reduceCount(), width = 250, height = 250)

#line chart
chart2 <- dcrchart("lineChart", "chart2", "mpg", reduceMean("wt"), width = 400, height = 250,
                   xAxisLabel = "Gear", yAxisLabel = "Average Weight")


#row chart
chart3 <- dcrchart("rowChart", "chart3", "carb", reduceCount(), width = 300, height = 250)

#view the charts
mydcr + chart1 + chart2 + chart 3
...

#Function Calls#
## Create empty chart object with data##
###dcr(data)###

##Function function definition to create a chart to add to chart object##
###dcrchart(type, id, dimension, reduce, width, height, ...) ###
* __type__: chart type such as pieChart, barChart
* __id__: div id to display in the web page (layout divs in ui.R in shiny for each chart to display in)
* __dimension__: dimension (which variable to group with)
* __reduce__: how records are reduced for each group of dimension (reduceCount() to count number of obs, reduceSum(y) to sum up values of y)
* __width__: chart width
* __height__: chart height
* __...__: additional chart arguments
