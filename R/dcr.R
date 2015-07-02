## dcr class definition------------------------------------------------------------------------------
dcr <- setClass("dcr",
                slots = list(data = "data.frame",
                             charts = "list"))

## initilize dcr class
setMethod("initialize", "dcr", function(.Object, data) {
  .Object@data <- data
  .Object
})

## show method for dcr class
setMethod("show", "dcr",
          function(object) {
            assign(".chart_object", object, envir = .GlobalEnv)
            shiny::runApp(file.path(system.file(package = "dcr"), "shiny"))
          })

## function convert dcr object to html code
html <- function(object, divs = FALSE) {
  mhtml <- list()
  mhtml$head <- "<script type=\"text/javascript\">"
  ## code part for data
  mhtml$code_data <- sprintf("var data = %s;\nvar ndx = crossfilter(data);", jsonlite:::toJSON(object@data))
  ## chart definitions
  mhtml$chart_code <- paste(sapply(object@charts, chart_def), collapse = "\n")
  mhtml$dim_code <- paste(sapply(object@charts, dimension_def), collapse = "\n")
  mhtml$red_code <- paste(sapply(object@charts, reduce_def), collapse = "\n")
  ## codes for each chart
  mhtml$codes <- paste(sapply(object@charts, chartcodes), collapse = "\n")
  mhtml$tail <- "dc.renderAll();\n</script>"
  mhtml <-  paste(unlist(mhtml), collapse = "\n")
  if (divs) {
    code_div <- sapply(object@charts, function(x) sprintf("<div id=\"%s\"></div>", x@id))
    code_div <- paste(code_div, collapse = "\n")
    mhtml <- paste(code_div, mhtml, sep = "\n")
  }
  mhtml
}

## dcrchart class definition-----------------------------------------------------------------------------
dcrchart <- setClass("dcrchart",
                     slots = list(type = "character",
                                  id = "character",
                                  dimension = "character",
                                  reduce = "character",
                                  opts = "list"))

setMethod("initialize", "dcrchart", function(.Object, type, id, dimension, reduce, ...) {
  .Object@type <- type
  .Object@id <- id
  .Object@dimension <- dimension
  .Object@reduce <- reduce
  .Object@opts <- list(...)
  .Object
})

## function to convert dcrchart to html code blocks
chart_def <- function(e) sprintf("var chart%s = dc.%s(\"#%s\");", e@id, e@type, e@id)
dimension_def <- function(e) sprintf("var %sDim = ndx.dimension(function(d) {return d.%s;});", e@id, e@dimension)
reduce_def <- function(e) {
  if (e@reduce == "") return(sprintf("var %sGroup = %sDim.group();", e@id, e@id))
  return(sprintf("var %sGroup = %sDim.group().%s;", e@id, e@id, e@reduce))
}
dc_code <- function(x) {attr(x, "code") <- TRUE; x}
js <- function(x, ...) {
  if (!is.null(attr(x, "code"))) return(x)
  jsonlite:::toJSON(x, auto_unbox = TRUE, ...)
}
chartcodes <- function(e) {
  s1 <- sprintf("chart%s.dimension(%sDim).group(%sGroup)", e@id, e@id, e@id)
  s2 <- sapply(e@opts, js)
  s2 <- paste0(names(s2), "(", s2, ")", collapse = ".")
  paste(s1, s2, sep = ".")
}

## define "+" method-----------------------------------------------------------------------------------
setMethod("+", signature("dcr", "dcrchart"), function(e1, e2) {
  e1@charts[[e2@id]] <- e2
  e1
})

## function to generate x scale------------------------------------------------------------------------
x_linear <- function(xlim = NULL) {
  dc_code(sprintf("d3.scale.linear().domain(%s)", js(xlim)))
}
x_ordinal <- function(levels = NULL) {
  if (is.null(levels)) return(dc_code("d3.scale.ordinal()"))
  dc_code(sprintf("d3.scale.ordinal().domain(%s)", js(levels)))
}

## wrapper function for reduce functions---------------------------------------------------------------
reduceSum <- function(var) dc_code(sprintf("reduceSum(function(d) {return d.%s;})", var))
reduceCount <- function() dc_code("reduceCount()")
fsum <- function(x) sprintf("+v.%s", x)
fratio <- function(x, y) sprintf("p.%s / p.%s", x, y)
reducefun <- function(...) {
  args <- list(...)
  code <- list(add = list(), remove = list(), init = list(count = 0))
  for (name in names(args)) {
    arg <- args[[name]]
    ops <- if (grepl("^[+]", arg))  c("+", "-") else c("", "")
    arg <- gsub("^[+]", "", arg)
    code$add[[name]] <- paste0("p.", name, ops[1], "=", arg, ";")
    code$remove[[name]] <- paste0("p.", name, ops[2], "=", arg, ";")
    code$init[[name]] <- 0
  }
  code_add <- paste(code$add, collapse = "\n")
  code_add <- sprintf("function(p, v){\n%s}", paste("++p.count;", code_add, "return p;", sep = "\n"))
  code_remove <- paste(code$remove, collapse = "\n")
  code_remove <- sprintf("function(p, v){\n%s}", paste("--p.count;", code_remove, "return p;", sep = "\n"))
  code_init <- js(code$init)
  code_init <- sprintf("function(){return %s;}", code_init)
  dc_code(paste(code_add, code_remove, code_init, sep = ",\n"))
}

pluck_key <- function(x) dc_code(sprintf("function(d) {return d.key.%s;}", x))
pluck_value <- function(x) dc_code(sprintf("function(d) {return d.value.%s;}", x))



