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
