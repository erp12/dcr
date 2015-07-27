#####################################################################################################
## Define dcr and dcrchart classes and their methods
#####################################################################################################

## dcr class definition------------------------------------------------------------------------------
dcr <- setClass("dcr",
                slots = list(data = "data.frame",
                             charts = "list"))

setMethod("show", "dcr",
          function(object) {
            assign(".chart_object", object, envir = .GlobalEnv)
            shiny::runApp(file.path(system.file(package = "dcr"), "shiny"))
          })

## dcrchart class definition---------------------------------------------------------------------------
dcrchart <- setClass("dcrchart",
                     slots = list(type = "character",
                                  id = "character",
                                  dimension = "character",
                                  reduce = "character",
                                  opts = "list",
                                  group_name = "character"))


## define "+" method-----------------------------------------------------------------------------------
setMethod("+", signature("dcr", "dcrchart"), function(e1, e2) {
  e1@charts[[e2@id]] <- e2
  e1
})
