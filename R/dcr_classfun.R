#####################################################################################################
## Functions For Working With dcr and dcrchart Classes
#####################################################################################################

##' Create dc visulization object
##' @param data the data used to visulize
##' @param charts a list of dc charts in the visulization
##' @export
##'
dcr <- function(data, charts = list()) {
  new("dcr", data = data, charts = charts)
}

##' Function to create a dc chart
##' @param type chart type such as pieChart, barChart etc
##' @param id chart div id
##' @param dimension chart dimension
##' @param reduce chart reduce function
##' @param width chart width, e.g. 400
##' @param height chart height, e.g. 600
##' @param ... additional chart options
##' @param group_name group name for this chart, used for name appeared in legend
##' @export
##'
dcrchart <- function(type, id, dimension, reduce, width, height, ..., group_name = "") {

  parsed_width <- parse_chart_width(width = width)
  responsive_width <- ifelse(parsed_width[[2]], parsed_width[[3]], NaN)

  parsed_height <- parse_chart_height(height = height)
  responsive_height <- ifelse(parsed_height[[2]], parsed_height[[3]], NaN)

  opts <- list(...)
  opts <- c(list(width = parsed_width[[1]], height = parsed_height[[1]]), opts)
  new("dcrchart", type = type, id = id, dimension = dimension,
      reduce = reduce, opts = opts, group_name = group_name,
      is_width_responsive = parsed_width[[2]], responsive_width = responsive_width,
      is_height_responsive = parsed_height[[2]], responsive_height = responsive_height)
}

##' Convert dcr object to html code
##' @param obj dcr chart object
##' @param divs whether or not to generate divisions
##' @param csv whether or not export data in csv format, otherwise it will export as json format
##' @param csv file filename
##' @param input_biding whether or not binding filter values to a input so server.R can
##' access the filters of each chart through input$chart
##' @export
##'
html <- function(object, divs = FALSE, csv = FALSE, filename = NULL, input_binding = FALSE) {
  mhtml <- list()
  ## pre-processing
  nochain <- c()
  for (id in names(object@charts)) {
    object@charts[[id]] <- auto_opts(object@charts[[id]], object)
    xaxis <- object@charts[[id]]@opts[["xAxis"]]
    yaxis <- object@charts[[id]]@opts[["yAxis"]]
    if (!is.null(xaxis)) {
      nochain <- c(nochain, sprintf("chart%s.%s;", id, xaxis))
      object@charts[[id]]@opts[["xAxis"]] <- NULL
    }
    if (!is.null(yaxis)) {
      nochain <- c(nochain, sprintf("chart%s.%s;", id, yaxis))
      object@charts[[id]]@opts[["yAxis"]] <- NULL
    }
    # Add input binding (2/2/2016)
    if (input_binding) {
      object@charts[[id]]@opts[["on"]] <- filter_binding(id)
    }
    #
  }
  nochain <- paste0(nochain, collapse = "\n")
  ## head part
  mhtml$head <- "<script type=\"text/javascript\">"
  ## code part for data
  mhtml$code_data <- sprintf("%s;\n%s\nvar ndx = crossfilter(data);",
                             ifelse(csv,
                                    sprintf("d3.csv('%s', function (data) {", filename),
                                    sprintf("var data = %s", jsonlite:::toJSON(object@data))),
                             map_fmt(object@data, csv))
  ## chart definitions
  mhtml$chart_code <- paste(sapply(object@charts, chart_def, global = csv), collapse = "\n")
  mhtml$dim_code <- paste(sapply(object@charts, dimension_def), collapse = "\n")
  mhtml$red_code <- paste(sapply(object@charts, reduce_def, object = object), collapse = "\n")
  for (id in names(object@charts)) object@charts[[id]] <- stack_opts(object@charts[[id]])

  ## codes for each chart
  mhtml$codes <- paste(sapply(object@charts, chartcodes, object = object), collapse = "\n")
  mhtml$nochain <- nochain
  mhtml$responsive_width_charts_js_var <- responsive_width_charts_js_var(object@charts)
  mhtml$responsive_height_charts_js_var <- responsive_height_charts_js_var(object@charts)
  mhtml$responive_charts_init <- "dcr_setResponsiveSizes();"
  mhtml$tail <- html_tail(csv)
  mhtml <-  paste(unlist(mhtml), collapse = "\n")
  if (divs) {
    code_div <- sapply(object@charts, function(x) sprintf("<div id=\"%s\"></div>", x@id))
    code_div <- paste(code_div, collapse = "\n")
    mhtml <- paste(code_div, mhtml, sep = "\n")
  }

  #fileConn<-file("~/mm_projects/dcr/foo.html")
  #writeLines(mhtml[1], fileConn)
  #close(fileConn)

  mhtml
}

## Helper functions-----------------------------------------------------------------------------------
##' Wrap a character string of javascript code or function definition
##' @param x the character string
##' @export
##'
dc_code <- function(x) {attr(x, "code") <- TRUE; x}

##' Convert R object to javascript object
##' @param x R object, if x is a string wrapped by \code{\link{dc_code}} then it is converted to code
##'  such as javascript function definition
##' @export
##'
js <- function(x) {
  if (!is.null(attr(x, "code"))) return(x)
  jsonlite:::toJSON(x, auto_unbox = TRUE)
}

##' Use other chart's dimension as the chart's dimension
##' @param the id of the chart whose dimension will be used
##' @export
##'
use_dimension <- function(id) {
  attr(id, "dimension") <- TRUE
  id
}

## Functions not to export-------------------------------------------------------------------------------
## generate code part for chart definitions
chart_def <- function(e, global = FALSE) {
  g <- ifelse(global, "", "var ")
  sprintf("%schart%s = dc.%s(\"#%s\");", g, e@id, e@type, e@id)
}

## generate code part for dimension definitions
dimension_def <- function(e) {
  if (!is.null(attr(e@dimension, "dimension", exact = TRUE))) return("")
  sprintf("var %sDim = ndx.dimension(function(d) {return d.%s;});", e@id, e@dimension)
}

## generate code part for reduce function definitions
## specially handling stack charts
reduce_def <- function(e, object) {
  dimid <- get_dimid(e@id, object)
  reduces <- get_reduce(e)
  reduces <- ifelse(reduces == "", reduces, paste0(".", reduces))
  reduceids <- ""
  if (length(reduces) > 1) reduceids <- c("", paste0("stack", seq_along(reduces[-1])))
  s <- sprintf("var %sGroup%s = %sDim.group()%s;", e@id, reduceids, dimid, reduces)
  paste0(s, collapse = "\n")
}

## generate chart codes part
chartcodes <- function(e, object) {
  dimid <- get_dimid(e@id, object)
  ## special handling for geoChoroplethChart
  s1 <- ""
  if (e@type == "geoChoroplethChart") {
    ovlay <- e@opts$overlayGeoJson
    s1 <- ovlay$jsondata
    e@opts$overlayGeoJson <- ovlay$jsonopts
  }
  ## initial part of the code
  group_name <- ifelse(e@group_name == "", "", paste0(", ", js(e@group_name)))
  s1 <- paste0(s1, sprintf("chart%s.dimension(%sDim).group(%sGroup%s)", e@id, dimid, e@id, group_name))
  ## add options
  s2 <- sapply(e@opts, js)
  s2 <- paste0(names(s2), "(", s2, ")", collapse = ".")
  ## wrap geoChoroplethChart
  if (e@type == "geoChoroplethChart") s2 <- paste0(s2, ";dc.renderAll();});")
  paste(s1, s2, sep = ".")
}

## function to get dimension name for the chart, in case dimension specified using use_dimension function
get_dimid <- function(id, object) {
  if (is.null(attr(object@charts[[id]]@dimension, "dimension", exact = TRUE))) return(id)
  nextchart <- object@charts[[object@charts[[id]]@dimension]]
  if (is.null(nextchart)) stop(sprintf("Trying to use demension from chart id %s, chart not found", id))
  get_dimid(nextchart@id, object)
}

## generate code for rowchart ordering
rowchart_order <- function(x) {
  codes <- sprintf('if (d.key == "%s") return %s;\n', levels(x), seq_along(levels(x)))
  codes <- paste0(codes, collapse = "")
  dc_code(sprintf("function(d) {%s}", codes))
}

## function to determine type of dimension to automatically add x option
auto_opts <- function(e, object) {
  dimid <- get_dimid(e@id, object)
  value <- object@data[[object@charts[[dimid]]@dimension]]
  vtype <- ifelse(is.numeric(value), "numeric", class(value))
  ## if one options is function, run the function on e
  for (name in names(e@opts)) {
    f <- e@opts[[name]]
    if (class(f) == "function") e@opts[[name]] <- f(e)
  }
  ## Automatically attach x axis definition for line chart and bar chart
  if (e@type %in% c("barChart", "lineChart")) {
    if (!("x" %in% names(e@opts))) {
      if (vtype == "numeric") {
        e@opts[["x"]] <- x_linear(range(value))
      } else if (vtype == "character") {
        e@opts[["x"]] <- x_ordinal(sort(unique(value)))
        if (!("xUnits" %in% names(e@opts))) e@opts[["xUnits"]] <- dc_code("dc.units.ordinal")
      } else if (vtype == "factor") {
        e@opts[["x"]] <- x_ordinal(levels(value))
        if (!("xUnits" %in% names(e@opts))) e@opts[["xUnits"]] <- dc_code("dc.units.ordinal")
      } else if (vtype == "Date") {
        e@opts[["x"]] <- x_time(range(value))
      }
    }
  }
  ## Automatically ording for rowChart if dimension variable is a factor
  if (e@type == "rowChart" && !("ordering" %in% names(e@opts)) && vtype == "factor") {
    e@opts[["ordering"]] <- rowchart_order(value)
  }
  ## if reduce function is reduce mean, then attach valueAccessor
  rmean <- attr(e@reduce, "reduceMean", exact = TRUE)
  if (!(is.null(rmean) || "valueAccessor" %in% names(e@opts))) {
    e@opts[["valueAccessor"]] <- simple_fun("d.value.avg")
  }
  e
}

# Generate javascript code input bindings for filters
# It basically creates a listener to listen to filter event using chart option "on"
filter_binding <- function(id) {
  dc_code(sprintf('"filtered", function(chart) {Shiny.onInputChange("%s", chart.filters());}', id))
}

## generate stack opts
stack_opts <- function(e) {
  stacks <- which(names(e@opts) == "stack")
  if (length(stacks) == 0) return(e)
  for (i in seq_along(stacks)) {
    j <- stacks[i]
    o <- e@opts[[j]]
    o[[1]] <- dc_code(sprintf("%sGroupstack%s", e@id, i))
    o <- o[sapply(o, function(x) !is.null(x))]
    e@opts[[j]] <- dc_code(paste0(sapply(o, js), collapse = ", "))
  }
  e
}

## extract all reduce functions when stack chart present
get_reduce <- function(e) {
  reduce <- e@reduce
  stacks <- which(names(e@opts) == "stack")
  if (length(stacks) == 0) return(reduce)
  c(reduce, sapply(e@opts[stacks], function(x) x[[1]]))
}

## jsonlite maps Date variable in R data to characters in json data, this function finds all Date
## variable in R data, and generate javascript code to convert the converted date strings to date
## format variables in mapped json data
# datefmt <- function(data) {
#   date_vars <- sapply(names(data), function(x) class(data[[x]]))
#   date_vars <- names(date_vars)[date_vars=="Date"]
#   if (length(date_vars) == 0) return("")
#   rep_str <- sprintf("\td.%s = d3.time.format('%%Y-%%m-%%d').parse(d.%s);", date_vars, date_vars)
#   rep_str <- paste0(rep_str, collapse="\n")
#   sprintf("data.forEach(function (d) {
#           %s
# })", rep_str)
# }

## Automatically perform format mapping between R data and json data
## This will generate a javascript code block to do the conversion
## factor/character: no transformation
## integer/numeric: if csv is TRUE then convert to numeric
## date: convert to date
## other format: not implemented
map_fmt <- function(data, csv = FALSE) {
  v_class <- sapply(data, class)
  ## date format
  date_vars <- names(v_class)[v_class == "Date"]
  init_code <- "var dcrx_dateFormat = d3.time.format('%Y-%m-%d');\n"
  date_code <- sprintf("\td.%s = dcrx_dateFormat.parse(d.%s);", date_vars, date_vars)
  ## numeric format
  num_code <- character()
  if (csv) {
    num_vars <- names(v_class)[v_class %in% c("numeric", "integer")]
    num_code <- sprintf("\td.%s = +d.%s;", num_vars, num_vars)
  }
  ## combine all codes and output
  all_code <- c(date_code, num_code)
  if (length(all_code) == 0) return("")
  all_code <- paste0(all_code, collapse="\n")
  sprintf("%sdata.forEach(function (d) {
          %s
});", init_code, all_code)
}

html_tail <- function(csv = FALSE) {
  csv_char <- ifelse(csv, "\n});", "")
  sprintf("dc.renderAll();\ndc.redrawAll();%s\n</script>", csv_char)
}


#########################
# Responsive chart sizes

##' Determine if chart width is given by num of px or % of window width
##' @param width The user specified width
##'
parse_chart_width <- function(width) {
  if (is.numeric(width)) {
    return(list(width, F))
  } else if (is.character(width)) {
    if (substr(width, nchar(width), nchar(width)) == "%") {
      width_pct <- as.numeric(substr(width, 1, nchar(width)-1)) / 100
      return(list(300, T, width_pct))
    } else {
      return(list(as.numeric(width), F))
    }
  }
}

##' Determine if chart height is given by num of px or % of window height
##' @param height The user specified height
##'
parse_chart_height <- function(height) {
  if (is.numeric(height)) {
    return(list(height, F))
  } else if (is.character(height)) {
    if (substr(height, nchar(height), nchar(height)) == "%") {
      height_pct <- as.numeric(substr(height, 1, nchar(height)-1)) / 100
      return(list(300, T, height_pct))
    } else {
      return(list(as.numeric(height), F))
    }
  }
}

##' Adds the charts with responsive widths to JS object
##' so that responsive_chart_manager.js will update them.
##' @param charts List of all charts.
##'
responsive_width_charts_js_var <- function(charts) {
  responsive_charts <- charts[lapply(charts, function(chrt) chrt@is_width_responsive) == T]

  js_var_string <- "var dcr_responsive_chart_widths = {"
  chrt_strings <- lapply(responsive_charts,
                         function(chrt) {
                           paste0('"', chrt@id, '":', chrt@responsive_width, ',')
                          })
  chrt_strings <- paste(chrt_strings, collapse = "\n")
  js_var_string <- paste(js_var_string, chrt_strings, '};', sep = '\n')
  js_var_string
}

##' Adds the charts with responsive heights to JS object
##' so that responsive_chart_manager.js will update them.
##' @param charts List of all charts.
##'
responsive_height_charts_js_var <- function(charts) {
  responsive_charts <- charts[lapply(charts, function(chrt) chrt@is_height_responsive) == T]

  js_var_string <- "var dcr_responsive_chart_heights = {"
  chrt_strings <- lapply(responsive_charts,
                         function(chrt) {
                           paste0('"', chrt@id, '":', chrt@responsive_height, ',')
                         })
  chrt_strings <- paste(chrt_strings, collapse = "\n")
  js_var_string <- paste(js_var_string, chrt_strings, '};', sep = '\n')
  js_var_string
}
