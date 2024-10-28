#' Scoring correct/incorrect answers
#'
#'
#' Transforms a data frame or matrix with raw answers into
#' a data frame with 1s (correct answers) and 0s (incorrect answers).
#'
#' @param x a data frame or matrix.
#' @param key a vector indicating the keys to score the data.
#' @param navalue a single value indicating the score of NAs.
#'
#'
#' @return A data frame with 1s for correct answers and 0s for incorrect answers.
#'
#'
#' @examples
#' data(dichodata)
#' data(dichokey)
#'
#' # NAs as NAs
#' head(dichodata)
#' ex1 <- correct(x = dichodata, key = dichokey, navalue = NA)
#' head(ex1)
#'
#' # NAs as 0s
#' head(dichodata)
#' ex2 <- correct(x = dichodata, key = dichokey, navalue = 0)
#' head(ex2)
#'
#'
#' @export
#'

correct <- function(x, key, navalue = NA){

  # Checks ----

  if(!(is.data.frame(x)|is.matrix(x)))
    stop(c("\nInvalid input for 'x'.",
           "\nIt should be a matrix or a data frame."))

  if(!is.vector(key))
    stop(c("\nInvalid input for 'key'.",
           "\nIt should be a vector."))

  if(ncol(x)!=length(key))
    stop(c("\nInvalid input for 'x' or 'key'.",
           "\nLength of 'key' should be equal to the number of columns of 'x'."))


  if(!(is.atomic(navalue)&length(navalue)==1))
    stop(c("\nInvalid input for 'navalue'.",
           "\nIt should be a single value."))


  if(!is.na(navalue))
    if(!is.numeric(navalue))
      stop(c("\nInvalid input for 'navalue'.",
             "\nIt should be a numeric value or an NA."))


  # Process ----


  df <- as.data.frame(x)

  out <- as.data.frame(sapply(1L:length(key), function(y){
    as.numeric(df[, y, drop = TRUE] %in% key[y])
  }))

  colnames(out) <- colnames(df)
  out[is.na(df)] <- navalue

  # Output ----

  return(out)
}
