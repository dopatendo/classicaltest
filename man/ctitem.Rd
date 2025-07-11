% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctitem.R
\name{ctitem}
\alias{ctitem}
\title{Classical test theory item statistics}
\usage{
ctitem(
  x,
  key,
  navalue = NA,
  wt = NULL,
  itemcategories = NULL,
  administered = NULL,
  itemscores = NULL
)
}
\arguments{
\item{x}{a data frame or matrix.}

\item{key}{a vector or a list indicating the keys to score the data.
If answers can only be right or wrong use a vector. If partial credits are allowed,
use a list where the first element contains the keys for full credits, and next
elements contain progressively the partial credit information.
If a test has a combination of full credit and partial credit items, leave
the full credit items as NAs in the keys for partial credit.}

\item{navalue}{a single value indicating the score of NAs.}

\item{wt}{a vector with total weights. Default is \code{NULL}.}

\item{itemcategories}{a vector indicating all the possible categories for all
items. If NULL, possible categories will be derived from data.}

\item{administered}{a logical matrix indicating which items were administered.
The dimensions should be the same as \code{x}. If \code{NULL} all items
are considered administered.}

\item{itemscores}{a numeric vector indicating all the possible scores for all items.
If \code{NULL}, possible scores will be derived from data.}
}
\value{
A list.
}
\description{
Calculates several item statistics, including: item means,
frequencies, proportions, and correlations between item responses and the total score.
}
\examples{

# Data preparation
## Random weights creation
set.seed(1919)
wt <- sample(x = 1:4, size = nrow(dichodata), replace = TRUE)

# Item analysis for multiple choice items
ctitem(x = dichodata, key = dichokey, itemcategories = c('A','B','C','D'), wt = wt)


}
