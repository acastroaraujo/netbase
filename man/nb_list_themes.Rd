% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nb.R
\name{nb_list_themes}
\alias{nb_list_themes}
\title{Themes Information}
\usage{
nb_list_themes(scope = c("USER", "ORG", "GLOBAL", "ALL"))
}
\arguments{
\item{scope}{"USER", "ORG", "GLOBAL", or "ALL"}
}
\value{
a \code{tibble}
}
\description{
This function returns a \code{tibble} describing the themes that the currently
logged-in user can access
}
