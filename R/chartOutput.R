chartOutput <- function(outputId) {
  suppressMessages(singleton(addResourcePath("js", system.file('js', package='dcr'))))
  suppressMessages(singleton(addResourcePath("css", system.file('css', package='dcr'))))
  div(class = "dc-chart",
      # Add javascript sources to header
      tagList(
        singleton(tags$head(tags$script(src = "http://d3js.org/d3.v3.min.js", charset = "utf-8", type='text/javascript'))),
        singleton(tags$head(tags$script(src = "js/crossfilter.js", type='text/javascript'))),
        singleton(tags$head(tags$script(src = "js/dc.js", type='text/javascript'))),
        singleton(tags$head(tags$link(rel = "stylesheet", type='text/css', href = 'css/dc.css')))
      ),
      # Add chart html
      htmlOutput(outputId)
  )
}
