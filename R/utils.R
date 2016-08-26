###################################################################################################
## Utility Functions to Generate Small Pieces of Scripts
###################################################################################################

##' Generate linear scale used with x option
##' @param xlim x axis range, should be a numeric vector of length 2
##' @export
##'
x_linear <- function(xlim) {
  dc_code(sprintf("d3.scale.linear().domain(%s)", js(xlim)))
}

##' Generate ordinal scale used with x option
##' @param levels order of levels, should be a vector of specified ordered levels
##' @export
##'
x_ordinal <- function(levels = NULL) {
  if (is.null(levels)) return(dc_code("d3.scale.ordinal()"))
  dc_code(sprintf("d3.scale.ordinal().domain(%s)", js(levels)))
}

##' Generate time scale used with x option
##' @param daterange date range to display, should be a Date vector of length 1
##' @export
##'
x_time <- function(daterange) {
  drange <- sprintf('new Date("%s")', format(daterange, "%m/%d/%Y"))
  dc_code(sprintf("d3.time.scale().domain([%s, %s])", drange[1], drange[2]))
}

##' Create legend options
##' @param ... legend options, e.g. x, y, itemHeigh, gap
##' @export
##'
dc_legend <- function(...) {
  l <- list(...)
  s1 <- paste0(names(l), "(", sapply(l, js), ")", collapse = ".")
  dc_code(sprintf("dc.legend().%s", s1))
}

##' Add a stacked chart
##' @param reduce reduce function
##' @param name name appearing in the label
##' @param accessor accssor function
##' @export
##'
dc_stack <- function(reduce, name = NULL, accessor = NULL) {
  if (!is.null(attr(reduce, "reduceMean", exact = TRUE)) && is.null(accessor)) {
    accessor <- simple_fun("d.value.avg")
  }
  list(reduce, name, accessor)
}


##' Add link to reset filter
##' @param id chart id the reset link used to reset
##' @param name what the link appears as, default is "reset", used in ui.R
##' @export
##'
dc_reset <- function(id, name = "reset") {
  list(shiny::a(name,
                class = "reset",
                href = sprintf('javascript:chart%s.filterAll();dc.redrawAll();', id),
                style = "display: none;"),
       shiny::div(class = "clearfix")
  )
}

##' Add link to reset all filters
##' @param name what the link appears as, default is "reset", used in ui.R
##' @export
##'
dc_resetAll <- function(name = "Reset All") {
  shiny::div(shiny::a(name, href = "javascript:dc.filterAll(); dc.renderAll();"))
}

##' Display current filters
##'
##' @param text text put before the displayed filters
##' @export
##' @examples
##' \dontrun{
##'
##' # For example, we put following in the ui.R
##' div(dc_filters(), dc_reset("mychart"), id = "mychart")
##' }
##'

dc_filters <- function(text = "Current filter:") {
  shiny::span(text, span(class = "filter"))
}

##' Create a division in ui.R showing one dc chart
##'
##' This function combines frequently used UI such as current filters,
##' reset link, and chart names
##'
##' @param id the chart id
##' @param name the text for the chart showing above the chart
##' @param reset whether or not show the reset link
##' @param show_filter whether or not show the current filters
##' @param reset_text the text for reset link
##' @param filter_text the text preceding displayed filters
##'
##' @examples
##' \dontrun{
##'
##' # We can put following in the ui.R
##' dc_ui("chart1", "The first chart", show_filter = TRUE, filter_text = "Selected:")
##' }
##'
##' @export

dc_ui <- function(id, text = "", reset = TRUE, show_filter = FALSE,
                  reset_text = "reset", filter_text = "Current Filter:") {
  name_ui <- strong(text)
  if (reset) {
    reset_ui <- shiny::a(reset_text,
                         class = "reset",
                         href = sprintf('javascript:chart%s.filterAll();dc.redrawAll();', id),
                         style = "display: none;")
  } else {
    reset_ui <- NULL
  }
  if (show_filter) {
    filter_ui <- shiny::span(class="reset",
                             style = "display: none;",
                             filter_text,
                             shiny::span(class = "filter"))
  } else {
    filter_ui <- NULL
  }
  clearfix_ui <- shiny::div(class = "clearfix")
  div(list(name_ui, filter_ui, reset_ui, clearfix_ui), id = id)
}

##' Define overlayGeoJson for geoChoropleth chart
##' @param jsonfile path for geojson file
##' @param name name of the layer
##' @param keyAccessor accessor function used to extract "key" from GeoJson data
##' @export
##'
geojson <- function(jsonfile, name, keyAccessor) {
  jsondata <- sprintf('d3.json("%s", function (overjson) {', jsonfile)
  jsonopts <- dc_code(sprintf('overjson.features, "%s", %s', name, keyAccessor))
  list(jsondata = jsondata, jsonopts = jsonopts)
}

##' Simple javascript function wrapper
##' @description generate simple one argument javascript function definition on d as
##' function(d) {return x;}, where x is some expression on d for function return
##' @param x expression to return
##' @export
##'
simple_fun <- function(x) {
  dc_code(sprintf("function(d) {return %s;}", x))
}

##' Customizing x Axis
##' @param ... list of options
##' @export
##'
x_axis <- function(...) {
  l <- list(...)
  s1 <- paste0(names(l), "(", sapply(l, js), ")", collapse = ".")
  sprintf("xAxis().%s", s1)
}

##' Customizing y Axis
##' @param ... list of options
##' @export
##'
y_axis <- function(...) {
  l <- list(...)
  s1 <- paste0(names(l), "(", sapply(l, js), ")", collapse = ".")
  sprintf("yAxis().%s", s1)
}

##' Get chart name from chart id
##' @param id chart id
##' @export
chartname <- function(id) dc_code(sprintf("chart%s", id))

##' Get a variable in the key
##' @param var variable
##' @export
##'
pluck_key <- function(var) simple_fun(sprintf("d.key.%s", var))

##' Get a variable in the value
##' @param var variable
##' @export
##'
pluck_value <- function(var) simple_fun(sprintf("d.value.%s", var))

##' Label axis ticks using Killo Mega Billion
##' @export
##'
tick_KMB <- function() dc_code("function (d) {
  if (d >=1000000000) return d/1000000000 + 'B';
  if (d >= 1000000) return d/1000000 + 'M';
  if (d >= 1000) return d/1000 + 'K';
  return d;}")

##' Label function to add label to element of the chart
##'
##' It typically works with label or title argument in charts like pieChart,
##' rowChart, bubbleChart. The label is created as string1 + key + string2 +
##' value + string3. key and value can be mapped in a way specified in keymap
##' and valuemap. By default, the label is shown as key(value percent), for example
##' if a slice with key 'name' has 40\% of total value, then it shows as name(40\%).
##'
##'@param id chart id
##'@param string1 first string
##'@param string2 second string
##'@param string3 theird string
##'@param keymap how to map key to label. If key is NULL (default), then original
##'value is used. If key is NA, then empty string is used. If key is a string vectror,
##'then we assume key is integer 1:n and vector[key] is used for label. This is useful
##'when we convert a factor to numeric to reduce data traffic. We can first convert
##'factor variabel to numeric, 1 to number of levels, and its level can be used as keymap.
##'@param valuemap how to map value to label. If value is NULL, then original value is used.
##'If value is NA, then empty string is used. If value is "percent" (default), then percentage
##' over sum of all values is used.
##'@export
##'
label_keyvalue <- function(string1 = "", string2 = "(", string3 = ")", keymap = NULL, valuemap = "percent") {
  function(e) dc_code(sprintf(" function(d) {\n%s\n%s
  return %s + key + %s + value + %s;
}", key_code(keymap), value_code(e, valuemap), js(string1), js(string2), js(string3)))
}

key_code <- function(keymap) {
  if (is.null(keymap)) return("  var key = d.key;")
  if (length(keymap) == 1 && is.na(keymap)) return("var key = \"\";")
  sprintf("  var keydic = %s;
  key = keydic[d.key-1];
  ", js(keymap))
}

value_code <- function(e, valuemap) {
  if (is.null(valuemap)) return("  var value = d.value;")
  if (length(valuemap) == 1 && is.na(valuemap)) return ("  var value = \"\";")
  if (valuemap == "percent") {
    return(sprintf("  var x = ndx.groupAll().%s.value();
  var value;
  if (chart%s.hasFilter() && !chart%s.hasFilter(d.key)) {
    value = \"0%%\";
  } else {
    value = Math.round(d.value / x * 100) + \"%%\";
  }", e@reduce, e@id, e@id, e@id))
  }
  stop("Invalid valuemap: must be NULL, NA, or percent")
}

