require(dcr)
shinyServer(function(input, output) {
  output$show <- dcr:::renderChart({
    return(.chart_object)
  }, divs = TRUE)
})
