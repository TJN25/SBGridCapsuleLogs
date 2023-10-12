
sbgridCapsules <- function (month, year, data, file_path) {
  if (missing("data") & missing("file_path")) {
    stop("Please provide data or a file path to data.")
  }
  if (missing("data")) {
    if (file.exists(paste0(file_path, "/", month, "-formatted.csv"))) {
      data <- utils::read.csv(paste0(file_path, "/", month, "-formatted.csv"))
    } else if (file.exists(paste0(file_path, "/", year, "/", month, "-formatted.csv"))) {
      data <- utils::read.csv(paste0(file_path, "/", year, "/", month, "-formatted.csv"))
    } else {
      stop(paste0(file_path, " does not contain the file ", month, "-formatted.csv"))
    }
  }
  if (is.numeric(year)) {
    year <- as.integer(year)
  }

  if("time" %in% colnames(data)){
    data <- data %>% select(-time)
  }

  return(new("sbgridCapsules", month = month, year = year, data = data))

}

combinedSbgridCapsules <- function(logs) {
  new("combinedSbgridCapsules", logs = logs)
}
