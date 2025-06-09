#' Item mean scores
#'
#' Calculates the item mean scores.
#'
#'
#' @inheritParams ctperson
#' @inheritParams stcov
#'
#' @return a data frame.
#'
#' @examples
#' # No weights
#' ex <- correct(x = dichodata, key = dichokey, navalue = NA)
#' itemmean(ex)
#'
#'
#' # With weights
#' set.seed(1919)
#' wt <- sample(x = 1:4, size = nrow(dichodata), replace = TRUE)
#' itemmean(ex,wt = wt)
#'
#' @export
#'


#' @export
itemmean <- function(x, wt = NULL){


  if(is.null(wt)){
    out <- cbind(colMeans(x,na.rm = TRUE))
  }else{


    out <- colSums(x*(wt),na.rm = TRUE)/colSums((!is.na(x))*wt)
    out <- cbind(out)

  }


  # for dicho

  colnames(out) <- c("itemmean")


  out <- structure(out,class = c("classicaltest","itemmean",
                                 class(out)))
  return(out)

}

#' @export
print.itemmean <- function(x,...){
  # cat("Item mean score\n\n")
  x <- (round(x,5))
  colnames(x) <- c("Item mean score")
  print(as.data.frame(x))
}
