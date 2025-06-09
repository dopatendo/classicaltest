#' Reliability measures
#'
#' Estimates the Cronbach's alpha coefficients and the split-halves coefficient
#' for a scored matrix.
#'
#'
#' @inheritParams ctperson
#' @inheritParams stcov
#' @param tries a numeric value indicating the number of samples for the
#' split-halves coefficient. Default is \code{100}.
#' @inheritParams base::set.seed
#'
#' @return a list or a numeric vector.
#'
#' @examples
#' # No weights
#' ex <- correct(x = dichodata, key = dichokey, navalue = NA)
#' alphatest(ex)
#' splithalf(ex, tries = 5)
#'
#'
#' # With weights
#' set.seed(1919)
#' wt <- sample(x = 1:4, size = nrow(dichodata), replace = TRUE)
#' alphatest(ex,wt = wt)
#' splithalf(ex,wt = wt, tries = 5)
#'
#' @export
#'

#' @rdname reliability
#' @export
alphatest <- function(x,wt = NULL){

  # Checks ----
  returnisNULL(isnumvec,wt)
  returnis(isnumMDF,x)

  x <- as.matrix(x)

  ## Combined ----

  if(!is.null(wt)&&(nrow(x)!=length(wt)))
    stop(c("\nInvalid input for 'wt'.",
           "\nIt should be NULL or a numeric vector with the same length or row numbers of 'x'."))

  # Process ----

  lel <- ifelse(ncol(x)>2,(ncol(x)+1),1)
  out <- vector("list", lel)
  for(i in 1:lel){

    if(i==1){
      X <- x
    }else{
      X <- x[,-(i-1)]
    }

    out[[i]] <- .alfa(X,wt = wt)

  }

  if(lel==1){
    ad <- matrix(NA,2,2)
  }else{
    ad <- do.call(rbind,out[-1])

  }

  rownames(ad) <- colnames(x)
  out <- list(alpha = out[[1]],
              alphadrop = ad)

  structure(out,class = c("classicaltest","alphatest",
                          class(out)))

}


.alfa <- function(x,wt = NULL){
  k <- ncol(x)
  COV <- stcov(x = x, wt = wt)
  COR <- stats::cov2cor(COV)


  # Raw alpha ---------------------------------------------------------------

  vb <- mean(diag(COV))
  cb <- mean(COV[upper.tri(COV,diag = F)])
  ra <- (k*cb)/(vb+(k-1)*cb)


  # Standardized alpha ------------------------------------------------------

  # p <- mean(COR[upper.tri(COR)])
  # sa <- (k*p)/(1+p*(k-1))


# Output ------------------------------------------------------------------

  return(cbind(raw.alpha = ra
               # , std.alpha = sa
               ))
}




#' @export
print.alphatest <- function(x, ...){
  cat("Total Alpha:\n")
  o1 <- as.data.frame(round(x$alpha,5))
  colnames(o1) <- c("Raw Alpha"
                    # ,"Std Alpha"
                    )
  print(o1,row.names = FALSE,col.names = FALSE)

  cat("\nAlpha if an element is dropped:\n")
  o2 <- round(x$alphadrop,5)
  colnames(o2) <- c("Raw Alpha"
                    # ,"Std Alpha"
                    )
  print(o2)
}


.splithalf <- function(x,administered,maxscore,ns,fhs,shs,k,wt){
  ss <- sample(ns)

  sfhs <- sort(ss[fhs])
  sshs <- sort(ss[shs])

  fh <- ctperson(x = x[,sfhs],
                 administered = administered[,sfhs],
                 maxscore = maxscore)
  sh <- ctperson(x = x[,sshs],
                 administered = administered[,sshs],
                 maxscore = maxscore)
  rel <- stcov(cbind(fh[,c(1,4L:6L)],
                     sh[,c(1,4L:6L)]),
               wt = wt)
  rel <- stats::cov2cor(rel)
  rel <- (diag(rel[-(1:4),1:4]))
  rel

}

#' @rdname reliability
#' @export
splithalf <- function(x, wt = NULL,tries = 100, seed = NA,
                      administered = NULL,maxscore = NULL){

  if(!is.na(seed)){
    set.seed(seed)
  }

  ns = 1:ncol(x)
  k = ncol(x)/(floor(ncol(x)/2))

  fhs <- 1:(floor(ncol(x)/2))
  shs <- (floor(ncol(x)/2)+1):(max(fhs)*2)


  relt <- vector("list",tries)
  for(i in 1:tries){
    relt[[i]] <- .splithalf(x,ns=ns,fhs=fhs,shs=shs,wt=wt,
                            administered = administered,
                            maxscore = maxscore)
  }
  relt <- do.call(rbind,relt)
  tries <- (k*relt)/(1+(k-1)*relt)
  relia <- colMeans(tries)
  relia

}
