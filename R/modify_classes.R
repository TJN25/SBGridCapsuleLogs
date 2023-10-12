# Method to add a sbgridCapsules to combinedSbgridCapsules
methods::setGeneric(
  "addLog",
  function(object, log) {
    standardGeneric("addLog")
  }
)

methods::setMethod(
  "addLog",
  signature(object = "combinedSbgridCapsules", log = "sbgridCapsules"),
  function(object, log) {
    object@logs <- append(object@logs, list(log))
    object@capsulesData <- object@capsulesData %>% rbind(log@data) %>%
      dplyr::filter(!is.na(runtime))
    return(object)
  }
)

# Method to add a sbgridCapsules to combinedSbgridCapsules
methods::setGeneric(
  "sortLogs",
  function(object) {
    standardGeneric("sortLogs")
  }
)


# Method to sort combinedSbgridCapsules by month and year
methods::setMethod(
  "sortLogs",
  signature(object = "combinedSbgridCapsules"),
  function(object) {
    object@logs <- object@logs[order(sapply(object@logs, function(log) paste0(log@year, sprintf("%02d", as.integer(log@month)))))]
    return(object)
  }
)

methods::setGeneric("updateSummary",
           function(object,os_lookup) standardGeneric("updateSummary"))

methods::setMethod("updateSummary",
          signature = c(object="combinedSbgridCapsules", os_lookup = "data.frame"),
          function(object, os_lookup){
            capsulesData <- object@capsulesData
            capsulesData <- capsulesData %>% dplyr::left_join(os_lookup, by = "os")

            object@perTitle <- capsulesData %>% dplyr::group_by(title, month, year) %>%
              dplyr::summarise(count.called = sum(counts),
                        number.of.programs.called = dplyr::n_distinct(program),
                        number.of.versions = dplyr::n_distinct(paste(version, title)),
                        time.used = sum(runtime),
                        number.of.users = dplyr::n_distinct(user),
                        number.of.os = dplyr::n_distinct(os),
                        number.of.os.major.release = dplyr::n_distinct(os.main),
                        number.of.labs = dplyr::n_distinct(site),
                        number.of.successfull.calls = sum(successes),
                        percent.of.successfull.calls = round(sum(successes)/sum(counts), 2))
            object@perUser <- capsulesData %>% dplyr::group_by(user, month, year) %>%
              dplyr::summarise(count.called = sum(counts),
                        number.of.programs.called = dplyr::n_distinct(program),
                        average.number.of.versions.per.title = round(dplyr::n_distinct(
                          paste(version, title))/dplyr::n_distinct(title), 2),
                        time.used = sum(runtime),
                        number.of.titles = dplyr::n_distinct(title),
                        number.of.os.major.release = dplyr::n_distinct(os.main),
                        number.of.successfull.calls = sum(successes),
                        percent.of.successfull.calls = round(sum(successes)/sum(counts), 2))
            object@perLab <- capsulesData %>% dplyr::group_by(site, month, year) %>%
              dplyr::summarise(count.called = sum(counts),
                        number.of.programs.called = dplyr::n_distinct(program),
                        average.number.of.versions.per.title = round(dplyr::n_distinct(
                          paste(version, title))/dplyr::n_distinct(title), 2),
                        time.used = sum(runtime),
                        number.of.users = dplyr::n_distinct(user),
                        number.of.titles = dplyr::n_distinct(title),
                        number.of.os.major.release = dplyr::n_distinct(os.main),
                        number.of.successfull.calls = sum(successes),
                        percent.of.successfull.calls = round(sum(successes)/sum(counts), 2))
            object@perOS <- capsulesData %>% dplyr::group_by(os.main, month, year) %>%
              dplyr::summarise(count.called = sum(counts),
                        number.of.programs.called = dplyr::n_distinct(program),
                        average.number.of.versions.per.title = round(dplyr::n_distinct(
                          paste(version, title))/dplyr::n_distinct(title), 2),
                        time.used = sum(runtime),
                        number.of.os.point.release = dplyr::n_distinct(os),
                        number.of.users = dplyr::n_distinct(user),
                        number.of.titles = dplyr::n_distinct(title),
                        number.of.labs = dplyr::n_distinct(site),
                        number.of.successfull.calls = sum(successes),
                        percent.of.successfull.calls = round(sum(successes)/sum(counts), 2))


            return(object)
          })

methods::setMethod("updateSummary",
          signature = c(object="combinedSbgridCapsules", os_lookup = "missing"),
          function(object, os_lookup){
            stop("TODO: Need to include the os_lookup data.frame")
          })
