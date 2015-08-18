##R PACKAGE `dcr` DOCUMENTATION

###Introduction
Dc.js and crossfilter are useful for constructing interactive visualizations and exploratory analysis. <http://dc-js.github.io/dc.js/> provides documentations and a lot of examples about this javascript library.

This project integrates their features and capabilities into the Shiny. This package is still in the early stages of development, so use with caution.

###Getting Started

####Installation
```r
if (!require(devtools)) install.packages("devtools")
devtools:::install_github("massmutual/dcr")
```

####Example

###Main Function Calls
```r
mydcr <- dcr(data)
```
