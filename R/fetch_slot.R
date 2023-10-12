# Method to get an sbgridCapsules from combinedSbgridCapsules
methods::setGeneric(
  "getLog",
  function(object, month, year) {
    standardGeneric("getLog")
  }
)

methods::setMethod(
  "getLog",
  signature(object = "combinedSbgridCapsules"),
  function(object, month, year) {
    id_value <- paste0(as.character(year), "-", month)
    index_value <- logs@log_index$index[logs@log_index$id == id_value]
    log <- logs@logs[index_value]
    return(log)
  }
)


methods::setGeneric("getSummary",
           function(object, data_type) standardGeneric("getSummary"))
methods::setMethod("getSummary",
          signature = c(object="combinedSbgridCapsules", data_type = "missing"),
          function(object, data_type){
            return(methods::slotNames(object))
          })
methods::setMethod("getSummary",
          signature = c(object="combinedSbgridCapsules", data_type = "character"),
          function(object, data_type){
            current_data <- methods::slot(object, data_type)
            return(current_data)
          })
