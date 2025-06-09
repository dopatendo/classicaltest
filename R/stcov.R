#' Population and sample (co)variance
#'
#'
#' Estimates the weighted and unweighted population and sample
#' covariance for a vector, a matrix or a data frame. Missing values are omitted.
#'
#'
#' @param x a numeric vector, matrix or data frame.
#' @param wt a vector with total weights. Default is \code{NULL}.
#' @param sample a logical value indicating if the sample variance should be used.
#' Default is \code{TRUE}. If \code{TRUE}, the unbiased estimate (n-1) is used;
#' if \code{FALSE}, the maximum likelihood estimate is used.
#'
#' @return a numeric value or a numeric matrix.
#'
#' @keywords stats
#'
#'
#' @examples
#'
#' # Variance ----
#'
#' ## Sample variance with no weights
#' stcov(x = iris$Sepal.Length, sample = TRUE)
#'
#' ## Population variance with no weights
#' stcov(x = iris$Sepal.Length, sample = TRUE)
#'
#' ## Sample variance with no weights
#' wt = c(100, rep(1,nrow(iris)-1)) # Unbalanced weights
#' stcov(x = iris$Sepal.Length, sample = FALSE, wt = wt)
#'
#' ## Population variance with no weights
#' wt = c(100, rep(1,nrow(iris)-1)) # Unbalanced weights
#' stcov(x = iris$Sepal.Length, sample = FALSE)
#'
#'
#' # Covariance ----
#'
#' ## Sample covariance with no weights
#' stcov(x = iris[,1:4], sample = TRUE)
#'
#' ## Population covariance with no weights
#' stcov(x = iris[,1:4], sample = TRUE)
#'
#' ## Sample covariance with no weights
#' wt = c(100, rep(1,nrow(iris)-1)) # Unbalanced weights
#' stcov(x = iris[,1:4], sample = FALSE, wt = wt)
#'
#' ## Population covariance with no weights
#' wt = c(100, rep(1,nrow(iris)-1)) # Unbalanced weights
#' stcov(x = iris[,1:4], sample = FALSE)
#'
#'
#'
#' @export


stcov <- function(x,wt = NULL,sample = TRUE){




  # Checks ----
  returnis(islova,sample)
  returnisNULL(isnumvec,wt)
  returnis(isnumVMDF,x)

  x <- as.matrix(x)

  ## Combined ----

  if(!is.null(wt)&&(nrow(x)!=length(wt)))
    stop(c("\nInvalid input for 'wt'.",
           "\nIt should be NULL or a numeric vector with the same length or row numbers of 'x'."))



  # Process ----

  ## If sample and no weights
  if(sample&is.null(wt)){

    out <- stats::cov(x,use = 'pairwise.complete.obs')

    if(all(dim(out)==1))
      return(out[1,1])

    return(out)



  }






  # Process ----

  mod <- as.numeric(sample)



  if(ncol(x)>1){

    nc <- ncol(x)

    kom <- c(lapply(1:nc,function(x) rep(x,2)),
             utils::combn(x = nc,m=2,simplify=FALSE))


    if(is.null(wt)){
      num <- .stcovXX(X = x, mod = mod,kom = kom)
    }else{

      num <- .stcovXXW(X = x, mod = mod,kom = kom, wt = wt)

    }


    out <- matrix(NA,nrow = nc,ncol = nc)
    diag(out) <- num[1:nc]

    out[lower.tri(out)] <- num[-c(1:nc)]
    out[upper.tri(out)] <- t(out)[upper.tri(out)]

    if(!is.null(colnames(x))){
      rownames(out) <- colnames(x)
      colnames(out) <- colnames(x)
    }

    out


  }else{
    if(is.null(wt)){
      out <- .stcovX(x = x, mod = mod)
    }else{

      out <- .stcovXW(x = x, mod = mod, wt = wt)

    }
  }





  # Output ----

  out
}

.stcovX <- function(x, mod){
  x <- x[!is.na(x)]
  sum((x-mean(x))**2)/(length(x)-mod)
}

.stcovXW <- function(x,mod,wt){
  nma <- cbind(x,wt)
  nma <- nma[!is.na(nma[,1])&!is.na(nma[,2]),]
  wm <- sum(nma[,1]*nma[,2])/sum(nma[,2])
  sum(nma[,2]*(nma[,1]-wm)**2)/(sum(nma[,2])-mod)
}


.stcovXX <- function(X,mod,kom){


  sapply(kom,function(x){




    XY <- (cbind(X[,x[1]],X[,x[2]]))


    XY <- XY[!is.na(XY[,1])&!is.na(XY[,2]),]

    sum((XY[,1] - mean(XY[,1])) * (XY[,2] - mean(XY[,2])))/(length(XY[,1]) - mod)

  })



}

.stcovXXW <- function(X,mod,kom,wt){


  sapply(kom,function(x){
    XYW <- (cbind(X[,x[1]],X[,x[2]],wt))

    XYW <- XYW[!is.na(XYW[,1])&!is.na(XYW[,2])&!is.na(XYW[,3]),]

    wmX <- sum(XYW[,1]*XYW[,3])/sum(XYW[,3])
    wmY <- sum(XYW[,2]*XYW[,3])/sum(XYW[,3])

    sum(XYW[,3] * (XYW[,1] - wmX)*(XYW[,2] - wmY))/(sum(XYW[,3]) - mod)

  })



}





