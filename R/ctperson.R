#' Classical test theory person statistics
#'
#'
#' Calculates persons statistics given a data frame (or matrix) of corrected
#' data. Including sum scores, number of administered items, number of
#' answered items, proportion of correct items, and mean score by item.
#'
#'
#' @param x a data frame or matrix of scored data.
#' @param administered a logical matrix indicating which items were administered.
#' The dimensions should be the same as \code{x}. If \code{NULL} all items
#' are considered administered.
#' @param maxscore a numeric value indicating which is the maximum score possible
#' per item. The minimum score is assumed to be 0. If \code{NULL} the maximum score is assumed to be
#' derived from the maximum value found in \code{x} or 1 if \code{x} only has 0s.
#'
#'
#' @return A data frame with person statistics.
#'
#'
#' @examples
#' # Data preparation
#' ## Scored data
#' corr <- correct(x = dichodata, key = dichokey, navalue = NA)
#' ## Random administered matrix
#' set.seed(1919)
#' admin <- sample(x = 0:1, size = nrow(corr)*ncol(corr), replace = TRUE, prob = c(.05,.95))
#' admin <- matrix(data = as.logical(admin),nrow = nrow(corr))
#' head(admin)
#'
#'
#' # Person statistics with all items administered
#' ex1 <- ctperson(x = corr)
#' head(ex1)
#'
#' # Person statistics with NOT all items administered
#' ex2 <- ctperson(x = corr, administered = admin)
#' head(ex2)
#'
#' @export
#'

ctperson <- function(x, administered = NULL, maxscore = NULL){

  # Checks ----
  returnisNULL(isnumval,maxscore)
  returnisNULL(isnumbet,maxscore,from = 0, to = Inf)

  if(!(is.data.frame(x)|is.matrix(x)))
    stop(c("\nInvalid input for 'x'.",
           "\nIt should be a matrix or a data frame."))

  if(!is.numeric(as.matrix(x)))
    stop(c("\nInvalid input for 'x'.",
           "\nIt should be a numeric matrix or a data frame."))

  if(min(unique(unlist(c(x)))%in%c(NA,0,1))==0){
    ispoly <- TRUE
  }else{
    ispoly <- FALSE
  }

  if(is.null(administered)){
    administered <- matrix(TRUE,nrow = nrow(x),ncol = ncol(x))
  }else{

    if(!(is.logical(x)|is.matrix(administered)))
      stop(c("\nInvalid input for 'administered'.",
             "\nIt should be a logical matrix."))

    if(min(dim(administered)==dim(x))==0)
      stop(c("\nInvalid input for 'administered'.",
             "\nIt should have the same dimensions as 'x'."))

  }



  # Process ----
  if(is.null(maxscore))
    maxscore <- max(x,na.rm = TRUE)

  if(maxscore==0)
    maxscore <- 1


  X <- x
  X[!administered] <- NA

  sum.score = rowSums(X,na.rm = TRUE)
  it.administered = rowSums(administered)
  it.answered = rowSums(!is.na(X))

  scaled.total = sum.score/(ncol(X)*maxscore)
  scaled.admin = sum.score/(it.administered*maxscore)
  scaled.answr = sum.score/(it.answered*maxscore)


  out <- cbind.data.frame(sum.score,
                          it.administered,
                          it.answered,
                          scaled.total,
                          scaled.admin,
                          scaled.answr)




  # Output ----

  return(out)

}
