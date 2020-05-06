
library(tidyverse)
library(httr)
library(lubridate)
library(jsonlite)


nb_setup <- function(user, password) {
  .auth <<- httr::authenticate(user, password)
}

# 1. Users can change topic names but not topic IDs. Referencing topics by ID
# means topic name changes do not break your code.
#
# 2. https://nb360.netbase.com/Enterprise/Insight_API/getting_started_with_the_insight_api/Frequently_Asked_Questions
#
# 3. https://api.netbase.com/explorer/api/netbase
#
# 4. https://nb360.netbase.com/Enterprise/Insight_API/running_queries/7Rate_Limiting
#
# 5. https://nb360.netbase.com/Enterprise/Insight_API/getting_started_with_the_insight_api/05_metric_calculation_formulas
#

# helper functions --------------------------------------------------------

col_selector <- function(x) {
  if (is.atomic(x)) {
    !all(is.na(x)) & !all(is.null(x))
  } else if (is.list(x)) {
    !all(map_lgl(x, ~ is_empty(.)))
  } else {
    TRUE
  }
}


# api functions -----------------------------------------------------------

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

# nb_hello_world("English")
# nb_hello_world("Spanish")

nb_list_topics <- function(scope = c("USER", "ORG", "GLOBAL", "ALL")) {
  
  if (!exists(".auth", where = globalenv())) stop("Use nb_setup() to authenticate yourself!", call. = FALSE)
  
  type <- match.arg(scope)
  
  result <- httr::GET(
    url = "https://api.netbase.com/cb/insight-api/2/topics?",
    query = list(scope = type, datetimeISO = TRUE),
    config = .auth
  )
  
  stopifnot(httr::status_code(result) == 200)
  
  content(result, type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>% 
    tibble::as_tibble() %>% 
    dplyr::select(name, topicId, contentType, createDate, editDate, timeInterval, fromDate, toDate, sharing, status, owner) %>% 
    dplyr::mutate_at(dplyr::vars(ends_with("Date")), lubridate::ymd_hms)
}

# output <- nb_list_topics("USER")

nb_list_themes <- function(scope = c("USER", "ORG", "GLOBAL", "ALL")) {
  
  if (!exists(".auth", where = globalenv())) stop("Use nb_setup() to authenticate yourself!", call. = FALSE)
  
  type <- match.arg(scope)
  
  result <- httr::GET(
    url = "https://api.netbase.com/cb/insight-api/2/themes?",
    query = list(scope = type, datetimeISO = TRUE),
    config = .auth
  )
  
  stopifnot(httr::status_code(result) == 200)
  
  content(result, type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>% 
    tibble::as_tibble() %>% 
    dplyr::select(name, themeId, tags, createdDate, editedDate, sharing, owner) %>% 
    dplyr::mutate_at(dplyr::vars(ends_with("Date")), lubridate::ymd_hms) 
  
}

# output <- nb_list_themes("ALL")

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
  
  if (status_code(result) == 403) {
    message("You have exceeded your query rate limit.\nWaiting for 30 seconds...")
    Sys.sleep(30)
    nb_topic_definitions(topic_id)
  }
  
  stopifnot(httr::status_code(result) == 200)
  
  message("Success!")
  
  output <- content(result, type = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON() %>% 
    tibble::as_tibble() %>% 
    dplyr::select(name, topicId,  lastRunDate, brands, everything()) %>% 
    dplyr::select_if(col_selector) %>% ## remove empty or "all NA" cols
    dplyr::mutate_at(dplyr::vars(ends_with("Date")), lubridate::ymd_hms) 
  
}

# output <- nb_topic_definitions(1417805)

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
  
  if (status_code(result) == 403) {
    message("You have exceeded your query rate limit.\nWaiting for 30 seconds...")
    Sys.sleep(30)
    nb_theme_definitions(theme_id)
  }
  
  stopifnot(httr::status_code(result) == 200)
  
  content(result, type = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON() %>% 
    tibble::as_tibble() %>% 
    dplyr::select(name, themeId,  editedDate, everything()) %>% 
    dplyr::select_if(col_selector) %>%  ## remove empty or all NA cols
    dplyr::mutate_at(dplyr::vars(ends_with("Date")), lubridate::ymd_hms) 
  
}

# output <- nb_theme_definitions(c(797207, 814627, 807511))
# 
# output %>% 
#   mutate(terms = map(includedTerms, ~ unlist(.$terms))) %>% 
#   select(name, themeId, terms) %>% 
#   unnest(terms)

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
  
  if (status_code(result) == 403) {
    message("You have exceeded your query rate limit.\nWaiting for 30 seconds...")
    Sys.sleep(30)
    nb_metric_values(topic_id, metricSeries, time, ...)
  }
  
  stopifnot(httr::status_code(result) == 200)
  
  message("Success!")
  
  ## data wrangling 
  
  output <- content(result, as = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON() 
  
  date_df <- tibble(date = output$metrics$columns %>% unlist() %>% lubridate::ymd_hms())
  
  metrics_df <- output$metrics$dataset %>% 
    purrr::pluck(1) %>% 
    tibble::as_tibble() %>% 
    tidyr::pivot_wider(names_from = "seriesName", values_from = "set") %>% 
    tidyr::unnest(everything())
  
  return(bind_cols(date_df, metrics_df))
}

# output <- nb_metric_values(topic_id = 1417805, time = "Month", metricSeries = "Impressions")
# output <- nb_metric_values(topic_id = 1417805, time = "Day", metricSeries = "TotalBuzz", genders = "Female")
# output <- nb_metric_values(topic_id = 1423897, time = "Week", metricSeries = c("TotalBuzz", "TotalBuzzPost", "TotalReplies", "TotalReposts", "OriginalPosts", "Impressions", "PositiveSentiment", "NegativeSentiment", "NeutralSentiment", "NetSentiment", "Passion", "UniqueAuthor", "StrongEmotion", "WeakEmotion", "EngagementDislikes", "EngagementLikes", "EngagementComments", "EngagementShares", "EngagementTotal", "EngagementRatio", "EngagementViews"))

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
  
  if (status_code(result) == 403) {
    message("You have exceeded your query rate limit.\nWaiting for 30 seconds...")
    Sys.sleep(30)
    nb_insights_count(topic_id, categories, size, ...)
  }
  
  stopifnot(httr::status_code(result) == 200)
  
  obj <- content(result, as = "text") %>% 
    jsonlite::fromJSON() %>% 
    purrr::pluck(1) %>% 
    purrr::pluck("dataset") 
  
  output <- purrr::map(obj, ~ .x$set) %>% 
    purrr::flatten()
  
  names(output) <- purrr::map(obj, ~ .x$insightType) %>% 
    purrr::flatten_chr()

  return(output)
  
}

# output <- nb_insights_count(topic_id = 1417805, categories = c("Likes", "Dislikes", "Phrases"), size = 20)
# output <- nb_insights_count(topic_id = 1417805, size = 20, categories = c("Likes", "Dislikes", "PositiveEmotions", "NegativeEmotions", "PositiveBehaviors", "NegativeBehaviors", "Authors", "Domains", "Sources", "Geolocation", "Languages", "Sentiment", "Phrases", "Hashtags", "OrgProducts", "People", "Things"))

