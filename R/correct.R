#' Scoring correct/incorrect answers
#'
#'
#' Transforms a data frame or matrix from raw answers to scores given a key.
#'
#' @param x a data frame or matrix.
#' @param key a vector or a list indicating the keys to score the data.
#' If answers can only be right or wrong use a vector. If partial credits are allowed,
#' use a list where the first element contains the keys for full credits, and next
#' elements contain progressively the partial credit information.
#' If a test has a combination of full credit and partial credit items, leave
#' the full credit items as NAs in the keys for partial credit.
#' @param navalue a single value indicating the score of NAs.
#'
#'
#' @return A data frame with scored answers.
#'
#'
#' @examples
#'
#'
#' # Full credit
#' dichokey
#' ## NAs as NAs
#' head(dichodata)
#' ex1 <- correct(x = dichodata, key = dichokey, navalue = NA)
#' head(ex1)
#'
#' ## NAs as 0s
#' head(dichodata)
#' ex2 <- correct(x = dichodata, key = dichokey, navalue = 0)
#' head(ex2)
#'
#'
#' # Partial credit
#' partialkey
#' ## NAs as NAs
#' head(partialdata)
#' ex3 <- correct(x = partialdata, key = partialkey, navalue = NA)
#' head(ex3)
#'
#' ## NAs as 0s
#' head(partialdata)
#' ex4 <- correct(x = partialdata, key = partialkey, navalue = 0)
#' head(ex4)
#'
#' @export
#'
correct <- function(x, key, navalue = NA){

  # Checks ----
  returnis(isdfonly,x)

  if(!(is.vector(key)|is.list(key)))
    stop(c("\nInvalid input for 'key'.",
           "\nIt should be a vector or a list."))

  if(!is.list(key)){
    key <- list(key)
  }

  uks <- unique(sapply(key,length))

  if(length(uks)!=1)
    stop(c("\nInvalid input for 'key'.",
           "\nLength of elements of 'key' should be equal."))


  if(ncol(x)!=uks)
    stop(c("\nInvalid input for 'x' or 'key'.",
           "\nLength of 'key' should be equal to the number of columns of 'x'."))




  # Process ----



  maxscore <- length(key)
  key <- key[maxscore:1]

  out <- vector("list",maxscore)
  for(i in maxscore:1){
    out[[i]] <- do.call(cbind,lapply(1:ncol(x),function(j) x[,j]%in%key[[i]][j]))*i
  }
  out <- Reduce("+",out)
  out[is.na(x)] <- navalue
  out <- as.data.frame(out)
  colnames(out) <- colnames(x)

  return(out)
}

