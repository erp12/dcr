########################################################################################################
### Functions For Working With Shiny
########################################################################################################

##' Render dc chart in server.R
##' @param expr expression to evaluate, it should return a dcr chart object
##' @param divs whether to generate division automatically in ui
##' @param input_binding whether or not to allow server.R to access each chart's filters, if it is
##' TRUE, then we can obtain the current filter values of chart by its id, input$chart_id. If the
##' chart has no filter, then the value is NULL.
##' @param env environment to evaluate
##' @param quoted quoted expression or not
##' @export
##'
renderChart <- function(expr, divs = FALSE, input_binding = FALSE, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  function() {
    chart <- func()
    html(chart, divs, input_binding = input_binding)
  }
}

##' Render dc chart in server.R
##' @param session shiny session
##' @param expr expression to evaluate, it should return a dcr chart object
##' @param divs whether to generate division automatically in ui
##' @param input_binding whether or not to allow server.R to access each chart's filters, if it is
##' TRUE, then we can obtain the current filter values of chart by its id, input$chart_id. If the
##' chart has no filter, then the value is NULL.
##' @param env environment to evaluate
##' @param quoted quoted expression or not
##' @export
##'
renderChart_csv <- function(session, expr, divs = FALSE, input_binding = FALSE, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  function() {
    chart <- func()
    ## create temporary directory to store temp csv file
#     if (!dir.exists("dcr_temp")) dir.create("dcr_temp")
#     singleton(addResourcePath("dcr_temp", "dcr_temp"))
#     filename <- tempfile(tmpdir = "dcr_temp", fileext = ".csv")
    tdir <- tempdir()
    singleton(addResourcePath("dcr_temp", tdir))
    filename <- tempfile(tmpdir = "", fileext = ".csv")
    filename_abs <- paste0(tdir, filename)
    filename_rel <- paste0("dcr_temp/", filename)
    ## remove temp csv file when session ended
    session$onSessionEnded(function() unlink(filename_abs))
    write.csv(chart@data, file = filename_abs, row.names = FALSE)
    html(chart, divs, csv = TRUE, filename = filename_rel, input_binding = input_binding)
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
        singleton(tags$head(tags$script(src = "js/responsive_chart_manager.js", charset="utf-8", type='text/javascript'))),
        singleton(tags$head(tags$link(rel = "stylesheet", type='text/css', href = 'css/dc.css')))
        ),
      # Add chart html
      htmlOutput(outputId)
  )
}


