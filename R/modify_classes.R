# Method to add a sbgridCapsules to combinedSbgridCapsules
setGeneric(
  "addLog",
  function(object, log) {
    standardGeneric("addLog")
  }
)

setMethod(
  "addLog",
  signature(object = "combinedSbgridCapsules", log = "sbgridCapsules"),
  function(object, log) {
    object@logs <- append(object@logs, list(log))
    object@capsulesData <- object@capsulesData %>% rbind(log@data) %>%
      filter(!is.na(runtime))
    return(object)
  }
)

# Method to add a sbgridCapsules to combinedSbgridCapsules
setGeneric(
  "sortLogs",
  function(object) {
    standardGeneric("sortLogs")
  }
)


# Method to sort combinedSbgridCapsules by month and year
setMethod(
  "sortLogs",
  signature(object = "combinedSbgridCapsules"),
  function(object) {
    object@logs <- object@logs[order(sapply(object@logs, function(log) paste0(log@year, sprintf("%02d", as.integer(log@month)))))]
    return(object)
  }
)

setGeneric("updateSummary",
           function(object,os_lookup) standardGeneric("updateSummary"))

setMethod("updateSummary",
          signature = c(object="combinedSbgridCapsules", os_lookup = "data.frame"),
          function(object, os_lookup){
            capsulesData <- object@capsulesData
            capsulesData <- capsulesData %>% left_join(os_lookup, by = "os")

            object@perTitle <- capsulesData %>% group_by(title, month, year) %>%
              summarise(count.called = sum(counts),
                        number.of.programs.called = n_distinct(program),
                        number.of.versions = n_distinct(paste(version, title)),
                        time.used = sum(runtime),
                        number.of.users = n_distinct(user),
                        number.of.os = n_distinct(os),
                        number.of.os.major.release = n_distinct(os.main),
                        number.of.labs = n_distinct(site),
                        number.of.successfull.calls = sum(successes),
                        percent.of.successfull.calls = round(sum(successes)/sum(counts), 2))
            object@perUser <- capsulesData %>% group_by(user, month, year) %>%
              summarise(count.called = sum(counts),
                        number.of.programs.called = n_distinct(program),
                        average.number.of.versions.per.title = round(n_distinct(
                          paste(version, title))/n_distinct(title), 2),
                        time.used = sum(runtime),
                        number.of.titles = n_distinct(title),
                        number.of.os.major.release = n_distinct(os.main),
                        number.of.successfull.calls = sum(successes),
                        percent.of.successfull.calls = round(sum(successes)/sum(counts), 2))
            object@perLab <- capsulesData %>% group_by(site, month, year) %>%
              summarise(count.called = sum(counts),
                        number.of.programs.called = n_distinct(program),
                        average.number.of.versions.per.title = round(n_distinct(
                          paste(version, title))/n_distinct(title), 2),
                        time.used = sum(runtime),
                        number.of.users = n_distinct(user),
                        number.of.titles = n_distinct(title),
                        number.of.os.major.release = n_distinct(os.main),
                        number.of.successfull.calls = sum(successes),
                        percent.of.successfull.calls = round(sum(successes)/sum(counts), 2))
            object@perOS <- capsulesData %>% group_by(os.main, month, year) %>%
              summarise(count.called = sum(counts),
                        number.of.programs.called = n_distinct(program),
                        average.number.of.versions.per.title = round(n_distinct(
                          paste(version, title))/n_distinct(title), 2),
                        time.used = sum(runtime),
                        number.of.os.point.release = n_distinct(os),
                        number.of.users = n_distinct(user),
                        number.of.titles = n_distinct(title),
                        number.of.labs = n_distinct(site),
                        number.of.successfull.calls = sum(successes),
                        percent.of.successfull.calls = round(sum(successes)/sum(counts), 2))


            return(object)
          })

setMethod("updateSummary",
          signature = c(object="combinedSbgridCapsules", os_lookup = "missing"),
          function(object, os_lookup){
            stop("TODO: Need to include the os_lookup data.frame")
          })
