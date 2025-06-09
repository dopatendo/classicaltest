#' Standard error of measurement (SEM)
#'
#' Estimates the standard error of measurement (SEM) of a scored matrix or
#' data frame using Cronbach's alpha or the split-halves coefficient.
#'
#'
#' @inheritParams alphatest
#'
#' @keywords stats
#' @return a list.
#'
#' @examples
#' # No weights
#' ex <- correct(x = dichodata, key = dichokey, navalue = NA)
#' ctsem.alpha(ex)
#' ctsem.split(ex,tries = 5)
#'
#'
#' # With weights
#' set.seed(1919)
#' wt <- sample(x = 1:4, size = nrow(dichodata), replace = TRUE)
#' ctsem.alpha(ex,wt = wt)
#' ctsem.split(ex,wt = wt,tries = 5)
#'
#' @export
#'


#' @rdname ctsem
#' @export
ctsem.alpha <- function(x,wt = NULL,
                        administered = NULL,
                        maxscore = NULL){

  # Checks ----

  returnisNULL(isnumvec,wt)
  returnis(isnumVMDF,x)

  x <- as.matrix(x)

  ## Combined ----

  if(!is.null(wt)&&(nrow(x)!=length(wt)))
    stop(c("\nInvalid input for 'wt'.",
           "\nIt should be NULL or a numeric vector with the same length or row numbers of 'x'."))

  # Process ----

  per <- ctperson(x,administered = administered,maxscore = maxscore)
  per <- per[,grep("sum|scaled",colnames(per),value = TRUE)]
  per <- diag(stcov(per,wt = wt))
  alf <- .alfa(x,wt = wt)[1]
  out <- sqrt((1-alf)*per)
  out <- list(sem = out, variance = per, alpha = alf)


  structure(out,class = c("classicaltest","ctsem",class(out)))
}

#' @rdname ctsem
#' @export
ctsem.split <- function(x,wt = NULL,
                        tries = 100,
                        seed = NA,
                        administered = NULL,
                        maxscore = NULL){

  # Checks ----

  returnisNULL(isnumvec,wt)
  returnis(isnumVMDF,x)

  x <- as.matrix(x)

  ## Combined ----

  if(!is.null(wt)&&(nrow(x)!=length(wt)))
    stop(c("\nInvalid input for 'wt'.",
           "\nIt should be NULL or a numeric vector with the same length or row numbers of 'x'."))

  # Process ----

  sph <- splithalf(x = x,wt = wt,tries = tries,seed = seed,
            administered = administered,maxscore = maxscore)

  per <- ctperson(x,administered = administered,maxscore = maxscore)
  per <- per[,grep("sum|scaled",colnames(per),value = TRUE)]
  per <- diag(stcov(per,wt = wt))

  out <- sqrt((1-sph)*per)
  out <- list(sem = out, variance = per, splithalves = sph)


  structure(out,class = c("classicaltest","ctsem",class(out)))
}

#' @export
print.ctsem <- function(x, ...){
  cat("Standard error of measurement (SEM):\n")
  if(names(x)[3]=="alpha"){
    cln <- "Alpha"
  }else{
    cln <- "Split-halves"
  }

  x <- round(cbind(x$sem,x$variance,x[[3]]),5)
  rownames(x) <- c("Sum score",
                   "Scaled of all items",
                   "Scaled of administered",
                   "Scaled of answered")

  colnames(x) <- c("SEM","Variance",cln)
  print(x,row.names = TRUE)
}
