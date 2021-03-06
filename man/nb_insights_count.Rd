% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nb.R
\name{nb_insights_count}
\alias{nb_insights_count}
\title{Insight Count}
\usage{
nb_insights_count(topic_id, categories = c("Likes", "Phrases"), size = 20, ...)
}
\arguments{
\item{topic_id}{the topic id}

\item{categories}{one or more from "Likes", "Dislikes", "PositiveEmotions", "NegativeEmotions", "PositiveBehaviors", "NegativeBehaviors", "Authors", "Domains", "Sources", "Geolocation", "Languages", "Sentiment", "Phrases", "Hashtags", "OrgProducts", "People", and "Things".}

\item{size}{the number of insight values to return (default is 20)}

\item{...}{additional parameters. See here: https://nb360.netbase.com/Enterprise/Insight_API/methods/03_insightcount_method}
}
\value{
a list of \code{tibble}s, one for each category.
}
\description{
This function returns the number of sound bites that match the specified
topic, the insight type specified with the categories parameter, and any
other specified parameters
}
\details{
If you exceed your rate limit, it will wait for 80 seconds before
trying again, if this fails it will wait for 80 seconds before trying again,
if this fails it will wait for 80 seconds before trying again, if this fails
it will wait for 80 seconds before trying again, if this fails it will wait
for 80 seconds before trying again, if this fails it will wait for 80 seconds
before trying again, if this fails it will wait for 80 seconds before trying
again, if this fails it will wait for 80 seconds before trying again, if this
fails it will wait for 80 seconds before trying again, if this fails it will
wait for 80 seconds before trying again...
}
\examples{
\dontrun{
output <- nb_insights_count(topic_id = 1417805, categories = c("Likes", "Dislikes", "Phrases"), size = 20)
output <- nb_insights_count(topic_id = 1417805, size = 20, categories = c("Likes", "Dislikes",  "Sentiment", "Phrases", "Hashtags", "OrgProducts", "People", "Things"))
}

}
