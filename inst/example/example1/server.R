library(shiny)
library(dcr)

shinyServer(function(input, output) {
  
  output$example <- renderChart({
    mydcr <- dcr(mtcars)
    chart_cyl <- dcrchart(type = "pieChart", id = "chart1", dimension = "cyl",
                          reduce = reduceCount(), width = 250, height = 250)
    chart_gear <- dcrchart(type = "lineChart", id = "chart2", dimension = "gear",
                           reduce = reduceMean("hp"), width = 400, height = 250)
    chart_carb <- dcrchart(type = "rowChart", id = "chart3", dimension = "carb",
                           reduce = reduceCount(), width = 300, height = 250)
    mydcr + chart_cyl + chart_gear + chart_carb
  })
  
})
