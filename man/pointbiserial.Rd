% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pointbiserial.R
\name{pointbiserial}
\alias{pointbiserial}
\title{Point-biserial correlation}
\usage{
pointbiserial(x, wt = NULL, exclude = FALSE, listwise = FALSE, recScore = TRUE)
}
\arguments{
\item{x}{a numeric data frame or matrix containing only two categories.}

\item{wt}{a numeric vector of total weights. Default is \code{NULL}.}

\item{exclude}{a logical value indicating if the point-biserial correlation
should be calculated excluding the item from the total score.
Defaulft is \code{FALSE}.}

\item{listwise}{only consider complete data (remove rows with NAs).
Defaulft is \code{FALSE}.}

\item{recScore}{a logical value indicating if the total score for
should be calculated based only on valid values. Thus,
if \code{TRUE}: \code{rowMeans(x,na.rm = TRUE)*apply(x,1L,function(k) sum(!is.na(k)))};
if \code{FALSE}: \code{rowMeans(x,na.rm = TRUE)*ncol(x)}. Defaulft is \code{TRUE}.
If \code{listwise}, this argument is meaningless.}
}
\value{
A numeric vector with the point-biserial correlation coefficients
by item.
}
\description{
Estimates the point-biserial correlation coefficient between dichotomous
items and the person's score.
}
\examples{
data(dichodata)
data(dichokey)

# Data preparation
## Corrected data
corr <- correct(x = dichodata, key = dichokey, navalue = NA)
## Random weights creation
set.seed(1919)
wt <- sample(x = 1:4, size = nrow(corr), replace = TRUE)


# Correlations without weights
pointbiserial(x = corr, wt = NULL)

# Correlations with weights
pointbiserial(x = corr, wt = wt)

# Correlations if item is excluded
pointbiserial(x = corr, exclude = TRUE)

# Correlations if NAs are considered 0s (recScore)
pointbiserial(x = corr, recScore = FALSE)

# Correlations with listwise
pointbiserial(x = corr, listwise = TRUE)

}
