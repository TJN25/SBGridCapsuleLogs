#Class for each individual month of data
setClass(
  "sbgridCapsules",
  representation(
    month = "character",
    year = "integer",
    data = "data.frame"
  )
)

capsulesValidObject <-function(object){
  if (!is.character(object@month) | !is.integer(object@year) | !is.data.frame(object@data)) {
    stop("Invalid input. Month should be character, year should be integer, and data should be a data frame.")
  }
  correct_colnames <- c("user", "grid", "os", "arch", "site", "exitcode", "runtime", "program", "title", "version")
  if(!identical(colnames(object@data), correct_colnames) ){
    stop('Invalid input. Data frame colnames should be: c("user", "grid", "os", "arch", "site", "exitcode", "runtime", "program", "title", "version").')
  }
  current_year <- as.integer(format(as.POSIXct(Sys.Date()), format = "%Y"))
  if (object@year < 2003 | object@year > current_year) {
    stop("Please include a valid year for SBGrid")
  }
  return(TRUE)
}

setMethod("initialize", "sbgridCapsules",
          function(.Object, month, year, data) {
            .Object@month <- month
            .Object@year <- year
            .Object@data <- data
            capsulesValidObject(.Object)
            .Object@data <- .Object@data %>%
              mutate(exitcode = as.numeric(exitcode),
                     runtime = as.numeric(runtime)) %>%
              filter(!is.na(exitcode)) %>%
              group_by(user, grid, arch, os, site, exitcode,
                       title, version, program) %>%
              summarise(counts = n(), runtime = sum(runtime),
                        successes = sum(exitcode == 0)) %>%
              mutate(month = .Object@month,
                     year = .Object@year)
            return(.Object)
          }
)

#Class for holding all the individual months and the summaries
setClass(
  "combinedSbgridCapsules",
  representation(
    logs = "list",
    perTitle="data.frame",
    perUser="data.frame",
    perLab="data.frame",
    perOS="data.frame",
    capsulesData="data.frame",
    log_index = "data.frame"
  )
)

# Define an initialization method for combinedSbgridCapsules
setMethod("initialize", "combinedSbgridCapsules",
          function(.Object, logs) {
            .Object@log_index <- data.frame(index = NULL, time.id = NULL)
            for(log in logs){
              dat <- data.frame(index = nrow(.Object@log_index) + 1,
                                id = paste0(as.character(log@year), "-", log@month))
              .Object@log_index <-  .Object@log_index %>% rbind(dat)

              .Object <- addLog(.Object, log)
            }
            return(.Object)
          }
)
