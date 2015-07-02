renderChart <- function(expr, divs = FALSE, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  function() {
    chart <- func()
    html(chart, divs)
  }
}