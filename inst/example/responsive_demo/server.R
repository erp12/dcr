library(shiny)
library(dcr)
library(dplyr)

iris_df <- iris
colnames(iris_df) <- sapply(colnames(iris_df),
                            function(c_name) {
                              gsub("\\.", "_", c_name)
                            })
iris_df <- iris_df %>%
  mutate(Petal_Width_Factor = factor(Petal_Width, levels = seq(min(.$Petal_Width),
                                                               max(.$Petal_Width),
                                                               0.1)))

shinyServer(function(input, output) {

  output$iris_charts <- renderChart({
    dcr_obj <- dcr({iris_df})

    species_chart <- dcrchart(type = "pieChart",
                              id = "species_chart",
                              dimension = "Species",
                              reduce = reduceCount(),
                              width = "30%", height = "45%")

    petal_width_chart <- dcrchart(type = "barChart",
                                  id = "petal_width_chart",
                                  dimension = "Petal_Width_Factor",
                                  reduce = reduceCount(),
                                  width = "65%", height = "45%")

    avg_sepal_length_chart <- dcrchart(type = "rowChart",
                                       id = "avg_sepal_length_chart",
                                       dimension = "Species",
                                       reduceMean("Sepal_Length"),
                                       width = "80%", height = "45%")

    dcr_obj + species_chart + petal_width_chart + avg_sepal_length_chart
  })

})
