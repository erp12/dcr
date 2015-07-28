########################################################################################################
### Functions For Working With Shiny
########################################################################################################

##' Render dc chart in server.R
##' @param expr expression to evaluate, it should return a dcr chart object
##' @param divs whether to generate division automatically in ui
##' @param env environment to evaluate
##' @param quoted quoted expression or not
##' @export
##'
renderChart <- function(expr, divs = FALSE, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  function() {
    chart <- func()
    html(chart, divs)
  }
}

##' Chart output for shiny in ui.R
##' @param outputId outputId rendered from server.R
##' @export
##'
chartOutput <- function(outputId) {
  suppressMessages(singleton(addResourcePath("js", system.file('js', package='dcr'))))
  suppressMessages(singleton(addResourcePath("css", system.file('css', package='dcr'))))
  suppressMessages(singleton(addResourcePath("geojson", system.file('geojson', package='dcr'))))
  div(class = "dc-chart",
      # Add javascript sources to header
      tagList(
        singleton(tags$head(tags$script(src = "js/d3.js", charset="utf-8", type='text/javascript'))),
        singleton(tags$head(tags$script(src = "js/crossfilter.js", type='text/javascript'))),
        singleton(tags$head(tags$script(src = "js/dc.js", type='text/javascript'))),
        singleton(tags$head(tags$link(rel = "stylesheet", type='text/css', href = 'css/dc.css')))
      ),
      # Add chart html
      htmlOutput(outputId)
  )
}


