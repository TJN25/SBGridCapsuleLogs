# Method to get an sbgridCapsules from combinedSbgridCapsules
setGeneric(
  "getLog",
  function(object, month, year) {
    standardGeneric("getLog")
  }
)

setMethod(
  "getLog",
  signature(object = "combinedSbgridCapsules"),
  function(object, month, year) {
    id_value <- paste0(as.character(year), "-", month)
    index_value <- logs@log_index$index[logs@log_index$id == id_value]
    log <- logs@logs[index_value]
    return(log)
  }
)


setGeneric("getSummary",
           function(object, data_type) standardGeneric("getSummary"))
setMethod("getSummary",
          signature = c(object="combinedSbgridCapsules", data_type = "missing"),
          function(object, data_type){
            return(slotNames(object))
          })
setMethod("getSummary",
          signature = c(object="combinedSbgridCapsules", data_type = "character"),
          function(object, data_type){
            current_data <- slot(object, data_type)
            return(current_data)
          })
