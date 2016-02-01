library(shiny)
library(dcr)

shinyUI(fluidPage(
  chartOutput("example"),
  fluidRow(column(6, div(id = "chart1")),
           column(6, div(id = "chart3"))),
  fluidRow(column(6, div(id = "chart2")))
))
