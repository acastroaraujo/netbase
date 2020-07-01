
.auth <- NULL

#' User authentication
#'
#' @param user user name
#' @param password password
#'
#' @return an object created by httr::authenticate() which gets stored invisibly in the global environment as `.auth`
#' @export
#'
nb_setup <- function(user, password) {

  .auth <<- httr::authenticate(user, password)

}


# Helper function

col_selector <- function(x) {
  if (is.atomic(x)) {
    !all(is.na(x)) & !all(is.null(x))
  } else if (is.list(x)) {
    !all(purrr::map_lgl(x, ~ purrr::is_empty(.)))
  } else {
    TRUE
  }
}



#' Hello World
#'
#'The helloWorld method returns the string 'Hello, World!' in the language
#'specified by the language parameter. If the server does not recognize the
#'specified langauge, it returns the string 'I don't speak that language' in
#'Traditional Chinese.
#'
#' @param lang language (e.g. "English", "Spanish", "French", etc)
#'
#' @return A list with two values: `language` and `message`
#' @export
#'
nb_hello_world <- function(lang = "English") {

  if (!exists(".auth", where = globalenv())) stop("Use nb_setup() to authenticate yourself!", call. = FALSE)

  result <- httr::GET(
    url = "https://api.netbase.com/cb/insight-api/2/helloWorld?",
    query = list(language = lang),
    config = .auth
  )

  stopifnot(httr::status_code(result) == 200)
  httr::content(result)
}



#'Topic Information
#'
#'This function returns a `tibble` describing the topics that the currently
#'logged-in user can access
#'
#' @param scope "USER", "ORG", "GLOBAL", or "ALL"
#'
#' @return a `tibble`
#' @export
#'
nb_list_topics <- function(scope = c("USER", "ORG", "GLOBAL", "ALL")) {

  if (!exists(".auth", where = globalenv())) stop("Use nb_setup() to authenticate yourself!", call. = FALSE)

  type <- match.arg(scope)

  result <- httr::GET(
    url = "https://api.netbase.com/cb/insight-api/2/topics?",
    query = list(scope = type, datetimeISO = TRUE),
    config = .auth
  )

  stopifnot(httr::status_code(result) == 200)

  httr::content(result, type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    tibble::as_tibble() %>%
    dplyr::select(.data$name, .data$topicId, .data$contentType, .data$createDate, .data$editDate, .data$timeInterval, .data$fromDate, .data$toDate, .data$sharing, .data$status, .data$owner) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("Date")), lubridate::ymd_hms)
}



#' Themes Information
#'
#'This function returns a `tibble` describing the themes that the currently
#'logged-in user can access
#'
#' @param scope "USER", "ORG", "GLOBAL", or "ALL"
#'
#' @return a `tibble`
#' @export
#'
nb_list_themes <- function(scope = c("USER", "ORG", "GLOBAL", "ALL")) {

  if (!exists(".auth", where = globalenv())) stop("Use nb_setup() to authenticate yourself!", call. = FALSE)

  type <- match.arg(scope)

  result <- httr::GET(
    url = "https://api.netbase.com/cb/insight-api/2/themes?",
    query = list(scope = type, datetimeISO = TRUE),
    config = .auth
  )

  stopifnot(httr::status_code(result) == 200)

  httr::content(result, type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    tibble::as_tibble() %>%
    dplyr::select(.data$name, .data$themeId, .data$tags, .data$createdDate, .data$editedDate, .data$sharing, .data$owner) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("Date")), lubridate::ymd_hms)

}



#' Topic definitions
#'
#' This function provides information on the topics the currently logged-in user
#' can access.
#'
#' If you exceed your rate limit, it will wait for 30 seconds before
#' trying again, if this fails it will wait for 30 seconds before trying again,
#' if this fails it will wait for 30 seconds before trying again, if this fails
#' it will wait for 30 seconds before trying again, if this fails it will wait
#' for 30 seconds before trying again, if this fails it will wait for 30 seconds
#' before trying again, if this fails it will wait for 30 seconds before trying
#' again, if this fails it will wait for 30 seconds before trying again, if this
#' fails it will wait for 30 seconds before trying again, if this fails it will
#' wait for 30 seconds before trying again, if this fails it will wait for 30
#' seconds before trying again...
#'
#' @param topic_id a vector of topic ids (max 100 at a time)
#'
#' @return a `tibble` describing how the topics were created
#' @export
#'
nb_topic_definitions <- function(topic_id) {

  if (!exists(".auth", where = globalenv())) stop("Use nb_setup() to authenticate yourself!", call. = FALSE)
  stopifnot(length(topic_id) <= 100)

  url <- "https://api.netbase.com/cb/insight-api/2/topicDefinitions?"
  search_ids <- paste0("ids=", topic_id) %>% paste0(collapse = "&")

  result <- httr::GET(
    url = paste0(url, search_ids, collapse = ""),
    query = list(datetimeISO = TRUE),
    config = .auth
  )

  if (httr::status_code(result) == 403) {

    message("You have exceeded your query rate limit.\nWaiting for 30 seconds...")
    Sys.sleep(30)
    nb_topic_definitions(topic_id)

  } else {

    stopifnot(httr::status_code(result) == 200)

    output <- httr::content(result, type = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      tibble::as_tibble() %>%
      dplyr::select(.data$name, .data$topicId,  .data$lastRunDate, .data$brands, dplyr::everything()) %>%
      dplyr::select_if(col_selector) %>% ## remove empty or "all NA" cols
      dplyr::mutate_at(dplyr::vars(dplyr::ends_with("Date")), lubridate::ymd_hms)

    return(output)

  }
}



#' Theme definitions
#'
#' This function provides information on the themes the currently logged-in user
#' can access.
#'
#' If you exceed your rate limit, it will wait for 30 seconds before
#' trying again, if this fails it will wait for 30 seconds before trying again,
#' if this fails it will wait for 30 seconds before trying again, if this fails
#' it will wait for 30 seconds before trying again, if this fails it will wait
#' for 30 seconds before trying again, if this fails it will wait for 30 seconds
#' before trying again, if this fails it will wait for 30 seconds before trying
#' again, if this fails it will wait for 30 seconds before trying again, if this
#' fails it will wait for 30 seconds before trying again, if this fails it will
#' wait for 30 seconds before trying again, if this fails it will wait for 30
#' seconds before trying again...

#'
#' @param theme_id a vector of theme ids (max 100 at a time)
#'
#' @return a `tibble` describing how the themes were created
#' @export
#'
nb_theme_definitions <- function(theme_id) {

  if (!exists(".auth", where = globalenv())) stop("Use nb_setup() to authenticate yourself!", call. = FALSE)
  stopifnot(length(theme_id) <= 100)

  url <- "https://api.netbase.com/cb/insight-api/2/themeDefinitions?"
  search_ids <- paste0("ids=", theme_id) %>% paste0(collapse = "&")

  result <- httr::GET(
    url = paste0(url, search_ids, collapse = ""),
    query = list(datetimeISO = TRUE),
    config = .auth
  )

  if (httr::status_code(result) == 403) {

    message("You have exceeded your query rate limit.\nWaiting for 30 seconds...")
    Sys.sleep(30)
    nb_theme_definitions(theme_id)

  } else {

    stopifnot(httr::status_code(result) == 200)

    output <- httr::content(result, type = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      tibble::as_tibble() %>%
      dplyr::select(.data$name, .data$themeId, .data$editedDate, dplyr::everything()) %>%
      dplyr::select_if(col_selector) %>%  ## remove empty or all NA cols
      dplyr::mutate_at(dplyr::vars(dplyr::ends_with("Date")), lubridate::ymd_hms)

    return(output)

  }
}


#' Metric Values
#'
#' This function returns one or more metric values for sound bites matching the specified topic, depending on the `time` argument.
#'
#' If you exceed your rate limit, it will wait for 80 seconds before
#' trying again, if this fails it will wait for 80 seconds before trying again,
#' if this fails it will wait for 80 seconds before trying again, if this fails
#' it will wait for 80 seconds before trying again, if this fails it will wait
#' for 80 seconds before trying again, if this fails it will wait for 80 seconds
#' before trying again, if this fails it will wait for 80 seconds before trying
#' again, if this fails it will wait for 80 seconds before trying again, if this
#' fails it will wait for 80 seconds before trying again...
#'
#' @param topic_id the topic id
#' @param metricSeries one of "TotalBuzz", "TotalBuzzPost", "TotalReplies",
#'   "TotalReposts", "OriginalPosts", "Impressions", "PositiveSentiment",
#'   "NegativeSentiment", "NeutralSentiment", "NetSentiment", "Passion",
#'   "UniqueAuthor", "StrongEmotion", "WeakEmotion", "EngagementDislikes",
#'   "EngagementLikes", "EngagementComments", "EngagementShares",
#'   "EngagementTotal", "EngagementRatio", and "EngagementViews"
#' @param time one of "All", "Month", "Week", "Day", "Hour", "QuarterHour", "Minute"
#' @param ... additional arguments. See here: https://nb360.netbase.com/Enterprise/Insight_API/methods/04_metricvalues_method
#'
#' @return a `tibble`
#' @export
#'
#' @examples
#' \dontrun{
#' output <- nb_metric_values(topic_id = 1417805, time = "Month", metricSeries = "Impressions")
#' output <- nb_metric_values(topic_id = 1417805, time = "Day", metricSeries = "TotalBuzz", genders = "Female")
#' output <- nb_metric_values(topic_id = 1423897, time = "Week", metricSeries = c("TotalBuzz", "TotalBuzzPost", "TotalReplies"))
#' }

nb_metric_values <- function(
  topic_id,
  metricSeries = c("TotalBuzz", "TotalBuzzPost", "PositiveSentiment", "NegativeSentiment"),
  time = c("All", "Month", "Week", "Day", "Hour", "QuarterHour", "Minute"),
  ...
) {

  if (!exists(".auth", where = globalenv())) stop("Use nb_setup() to authenticate yourself!", call. = FALSE)
  stopifnot(length(topic_id) == 1)
  # https://nb360.netbase.com/Enterprise/Insight_API/input_parameters/metricSeries
  metric_choices <- c("TotalBuzz", "TotalBuzzPost", "TotalReplies", "TotalReposts", "OriginalPosts", "Impressions", "PositiveSentiment", "NegativeSentiment", "NeutralSentiment", "NetSentiment", "Passion", "UniqueAuthor", "StrongEmotion", "WeakEmotion", "EngagementDislikes", "EngagementLikes", "EngagementComments", "EngagementShares", "EngagementTotal", "EngagementRatio", "EngagementViews")
  metric_type <- match.arg(metricSeries, metric_choices, several.ok = TRUE)
  time_unit <- match.arg(time)

  url <- "https://api.netbase.com/cb/insight-api/2/metricValues?"
  metric_string <- paste0("metricSeries=", metric_type) %>% paste0(collapse = "&")

  result <- httr::GET(
    url = paste0(url, metric_string, collapse = ""),
    query = list(topicIds = topic_id, datetimeISO = TRUE, timeUnits = time_unit, ...),
    config = .auth
  )

  if (httr::status_code(result) == 403) {

    message("You have exceeded your query rate limit.\nWaiting for 80 seconds...")
    Sys.sleep(80)
    nb_metric_values(topic_id, metricSeries, time, ...)

  } else {

    stopifnot(httr::status_code(result) == 200)

    output <- httr::content(result, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()

    date_df <- tibble::tibble(date = output$metrics$columns %>% unlist() %>% lubridate::ymd_hms())

    metrics_df <- output$metrics$dataset %>%
      purrr::pluck(1) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = "seriesName", values_from = "set") %>%
      tidyr::unnest(dplyr::everything())

    return(dplyr::bind_cols(date_df, metrics_df))

  }
}

#' Insight Count
#'
#' This function returns the number of sound bites that match the specified
#' topic, the insight type specified with the categories parameter, and any
#' other specified parameters
#'
#' If you exceed your rate limit, it will wait for 80 seconds before
#' trying again, if this fails it will wait for 80 seconds before trying again,
#' if this fails it will wait for 80 seconds before trying again, if this fails
#' it will wait for 80 seconds before trying again, if this fails it will wait
#' for 80 seconds before trying again, if this fails it will wait for 80 seconds
#' before trying again, if this fails it will wait for 80 seconds before trying
#' again, if this fails it will wait for 80 seconds before trying again, if this
#' fails it will wait for 80 seconds before trying again, if this fails it will
#' wait for 80 seconds before trying again...
#'
#' @param topic_id the topic id
#' @param categories one or more from "Likes", "Dislikes", "PositiveEmotions", "NegativeEmotions", "PositiveBehaviors", "NegativeBehaviors", "Authors", "Domains", "Sources", "Geolocation", "Languages", "Sentiment", "Phrases", "Hashtags", "OrgProducts", "People", and "Things".
#' @param size the number of insight values to return (default is 20)
#' @param ... additional parameters. See here: https://nb360.netbase.com/Enterprise/Insight_API/methods/03_insightcount_method
#'
#' @return a list of `tibble`s, one for each category.
#' @export
#'
#' @examples
#' \dontrun{
#' output <- nb_insights_count(topic_id = 1417805, categories = c("Likes", "Dislikes", "Phrases"), size = 20)
#' output <- nb_insights_count(topic_id = 1417805, size = 20, categories = c("Likes", "Dislikes",  "Sentiment", "Phrases", "Hashtags", "OrgProducts", "People", "Things"))
#' }
#'
nb_insights_count <- function(
  topic_id,
  categories = c("Likes", "Phrases"),
  size = 20,
  ...
) {

  if (!exists(".auth", where = globalenv())) stop("Use nb_setup() to authenticate yourself!", call. = FALSE)
  stopifnot(length(topic_id) == 1)
  #https://nb360.netbase.com/Enterprise/Insight_API/input_parameters/categories_(insightCount_Method)
  category_choices <- c("Likes", "Dislikes", "PositiveEmotions", "NegativeEmotions", "PositiveBehaviors", "NegativeBehaviors", "Authors", "Domains", "Sources", "Geolocation", "Languages", "Sentiment", "Phrases", "Hashtags", "OrgProducts", "People", "Things")
  category_type <- match.arg(categories, category_choices, several.ok = TRUE)

  url <- "https://api.netbase.com/cb/insight-api/2/insightCount?"
  category_string <- paste0("categories=", category_type) %>% paste0(collapse = "&")

  result <- httr::GET(
    url = paste0(url, category_string, collapse = ""),
    query = list(topicIds = topic_id, datetimeISO = TRUE, sizeNeeded = size, ...),
    config = .auth
  )

  if (httr::status_code(result) == 403) {

    message("You have exceeded your query rate limit.\nWaiting for 80 seconds...")
    Sys.sleep(80)
    nb_insights_count(topic_id, categories, size, ...)

  } else {

    stopifnot(httr::status_code(result) == 200)

    obj <- httr::content(result, as = "text") %>%
      jsonlite::fromJSON() %>%
      purrr::pluck(1) %>%
      purrr::pluck("dataset")

    output <- purrr::map(obj, ~ .x$set) %>%
      purrr::flatten() %>%
      purrr::map(~ dplyr::rename(.x, insight = .data$name, count = .data$value))

    names(output) <- purrr::map(obj, ~ .x$insightType) %>%
      purrr::flatten_chr()

    return(output)

  }
}

