library(shiny)
library(dcr)

shinyUI(fluidPage(
  chartOutput("iris_charts"),
  fluidRow(column(4,
                  div(id = "species_chart")),
           column(8,
                  div(id = "petal_width_chart"))),
  fluidRow(column(6,
                  div(id = "avg_sepal_length_chart")),
           column(6,
                  div(id = "")))
))

