# Introduction #
dc.js and crossfilter are quite useful for visualization and exploratory analysis. Although many javascript library already have R packages to use them with Shiny, there is no R package to knowledge to use dc.js in Shiny. Our motivation is to develop an R package that allows us to use dc.js and crossfilter with Shiny, so that we can interactively and quickly build dc.js charts. This project is still in very early stage and just made proof-of-concept to demonstrate the ability to incorporate dc.js and crossfilter in Shiny.

# Installation #
```
#!r
if (!require(devtools)) install.packages("devtools")
devtools:::install_bitbucket("massmutual/dc_charts_r_shiny")
```

# Draw simple dc.js charts#
We will use the mtcars data to make some simple charts
```
#!r
library(dcr)
```


###Create chart object###
```
#!r
mydcr <- dcr(mtcars)
```

###Add a pie chart###
```
#!r
chart1 <- dcrchart("pieChart", "chart1", "cyl", reduceCount(), width = 250, height = 250)
```

###Add a line chart###
```
#!r
chart2 <- dcrchart("lineChart", "chart2", "mpg", reduceMean("wt"), width = 400, height = 250,
                   xAxisLabel = "Gear", yAxisLabel = "Average Weight")
```

###Add a row chart###
```
#!r
chart3 <- dcrchart("rowChart", "chart3", "carb", reduceCount(), width = 300, height = 250)
```

###Finally we can see the charts###
```
#!r
mydcr + chart1 + chart2 + chart3
```

#Work with Shiny#
After we tested the charts, we can build into Shiny app. The only extra step is to layout the charts.

#Function Calls#
## Function definition to create empty chart object with data##
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