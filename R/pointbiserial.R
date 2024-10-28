#' Point-biserial correlation
#'
#'
#' Estimates the point-biserial correlation coefficient between dichotomous
#' items and the person's score.
#'
#' @param x a numeric data frame or matrix containing only two categories.
#' @param wt a numeric vector of total weights. Default is \code{NULL}.
#' @param exclude a logical value indicating if the point-biserial correlation
#' should be calculated excluding the item from the total score.
#' Defaulft is \code{FALSE}.
#' @param listwise only consider complete data (remove rows with NAs).
#' Defaulft is \code{FALSE}.
#' @param recScore a logical value indicating if the total score for
#' should be calculated based only on valid values. Thus,
#' if \code{TRUE}: \code{rowMeans(x,na.rm = TRUE)*apply(x,1L,function(k) sum(!is.na(k)))};
#' if \code{FALSE}: \code{rowMeans(x,na.rm = TRUE)*ncol(x)}. Defaulft is \code{TRUE}.
#' If \code{listwise}, this argument is meaningless.
#'
#'
#' @return A numeric vector with the point-biserial correlation coefficients
#' by item.
#'
#'
#' @examples
#' data(dichodata)
#' data(dichokey)
#'
#' # Data preparation
#' ## Corrected data
#' corr <- correct(x = dichodata, key = dichokey, navalue = NA)
#' ## Random weights creation
#' set.seed(1919)
#' wt <- sample(x = 1:4, size = nrow(corr), replace = TRUE)
#'
#'
#' # Correlations without weights
#' pointbiserial(x = corr, wt = NULL)
#'
#' # Correlations with weights
#' pointbiserial(x = corr, wt = wt)
#'
#' # Correlations if item is excluded
#' pointbiserial(x = corr, exclude = TRUE)
#'
#' # Correlations if NAs are considered 0s (recScore)
#' pointbiserial(x = corr, recScore = FALSE)
#'
#' # Correlations with listwise
#' pointbiserial(x = corr, listwise = TRUE)
#'
#' @export
#'


pointbiserial <- function(x, wt = NULL, exclude = FALSE,
                          listwise = FALSE, recScore = TRUE){


  # Checks ----

  ## x
  if(!(is.data.frame(x)|is.matrix(x)))
    stop(c("\nInvalid input for 'x'.",
           "\nIt should be a matrix or a data frame."))

  if(!is.numeric(as.matrix(x)))
    stop(c("\nInvalid input for 'x'.",
           "\nIt should be a numeric matrix or a data frame."))

  if(nrow(x)<2){
    warning('Not enough cases',call. = FALSE)
    return(NA)
  }

  if(ncol(x)<2){
    warning('Not enough items',call. = FALSE)
    return(NA)
  }

  if(length(unique(stats::na.omit(unlist(c(x)))))!=2)
    stop(c("\nInvalid input for 'x'.",
           "\nIt should only have two categories."))


  ## wt
  if(!is.null(wt)){

    if(!is.vector(wt)&is.numeric(wt))
      stop(c("\nInvalid input for 'wt'.",
             "\nIt should be a numeric vector."))

    if(nrow(x)!=length(wt))
      stop(c("\nInvalid input for 'x' or 'wt'.",
             "\nLength of 'wt' should be equal to the number of rows of 'x'."))
  }else{
    wt <- rep(1L,nrow(x))
  }


  ## listwise
  if(!(is.logical(listwise) && length(listwise)==1 && !is.na(listwise)))
    stop(c("\nInvalid input for 'listwise'.",
           "\nIt should be a logical value."))

  ## exclude
  if(!(is.logical(exclude) && length(exclude)==1 && !is.na(exclude)))
    stop(c("\nInvalid input for 'exclude'.",
           "\nIt should be a logical value."))

  ## recScore
  if(!(is.logical(recScore) && length(recScore)==1 && !is.na(recScore)))
    stop(c("\nInvalid input for 'recScore'.",
           "\nIt should be a logical value."))

  ## listwise remove

  if(listwise){
    X <- cbind(wt,x)
    X <- X[apply(X,1L,function(y) sum(is.na(y))==0L),]

    if(nrow(x)!=nrow(X)){
      warning(paste(nrow(x)-nrow(X),'case(s) removed.'),call. = FALSE)
    }


    if(nrow(X)<2){
      warning('Not enough cases',call. = FALSE)
      return(NA)

    }


    x <- X[,-1L,drop = FALSE]
    wt <- X[,1L,drop = TRUE]
  }




  # Process & Output ----

  .rhototal(x = x, wt = wt, exclude = exclude, recScore = recScore)


}

.rhototal <- function(x, wt = NULL, exclude = FALSE, recScore = TRUE){

  if(is.null(wt))
    wt <- rep(1L,nrow(x))


  sapply(1L:ncol(x), function(y){

    if(exclude){X <- x[,-y,drop = FALSE]}else{X <- x}

    score <- rowSums(X,na.rm = TRUE)
    if(recScore){
      score <- score*ncol(X)/rowSums(!is.na(X))
    }

    goes <- !is.na(x[,y,drop = TRUE])&!is.na(score)&!is.na(wt)

    if(sum(goes)<2)
      return(NA)

    dich <- x[,y,drop = TRUE][goes]
    cont <- score[goes]
    wtgo <- wt[goes]
    mat <- cbind(dich,cont)

    if(nrow(unique(mat))==1)
      return(NA)

    stats::cov.wt(x = mat,
                  wt = wtgo,cor = TRUE)$cor[2L,1L]

  })

}
