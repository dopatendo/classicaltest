#' Dichotomize data
#'
#'
#' Converts a matrix or data frame into a dichotomized data frame.
#' Where each possible category is assigned a 1 or a 0.
#'
#' @param x a data frame or matrix.
#' @param id a vector of unique values indicating the ids of the cases.
#' If \code{NULL}, an id will be created following
#' \code{id <- 1L:nrow(x)}. Default is \code{NULL}.
#' @param categories a vector indicating all possible categories.
#' If \code{NULL}, this vector will be created with all the non-NA
#' values present in \code{x}. Default is \code{NULL}.
#' @param NAasNA a logical value indicating if NAs should be kept
#' as NAs. If \code{FALSE}, NAs will be 0s. Default is \code{TRUE}.
#' @param sortbyItem a logical value indicating if the resulting data frame
#' should be order by item names, on the contrary, by ids.
#' Default is \code{FALSE}.
#'
#'
#' @return A data frame with columns for the ids, the item names and all
#' the possible item categories, where 1s mean the person answered that category
#' for that item, and 0s mean the contrary.
#'
#'
#' @examples
#' data(dichodata)
#'
#' # Dichotomize with all present categories
#' ex1 <- dichotomize(dichodata, categories = NULL)
#' head(ex1)
#'
#' # Dichotomize with fixed set of categories
#' ex2 <- dichotomize(dichodata, categories = c('A','B','C','D'))
#' head(ex2)
#'
#' # Dichotomize with NAs as 0s
#' ex3 <- dichotomize(dichodata, NAasNA = FALSE)
#' head(ex3)
#'
#'
#' @export
#'



dichotomize <- function(x, id = NULL, categories = NULL,
                        NAasNA = TRUE, sortbyItem = FALSE){

  # Checks ----

  ## x
  if(!(is.data.frame(x)|is.matrix(x)))
    stop(c("\nInvalid input for 'x'.",
           "\nIt should be a matrix or a data frame."))

  x <- as.data.frame(x)

  ## id
  if(is.null(id)){
    id <- 1L:nrow(x)
  }else{

    if(!is.vector(id))
      stop(c("\nInvalid input for 'id'.",
             "\nIt should be a vector."))

    if(nrow(x)!=length(id))
      stop(c("\nInvalid input for 'x' or 'id'.",
             "\nLength of 'id' should be equal to the number of rows of 'x'."))

    if(length(unique(id))!=nrow(x))
      stop(c("\nInvalid input for 'id'.",
             "\nThe length of unique values should be equal to the number of rows of 'x'."))
  }


  ## categories

  if(is.null(categories)){
    categories <- sort(unique(stats::na.omit(unlist(c(x)))))
  }else{

    if(!is.vector(categories))
      stop(c("\nInvalid input for 'categories'.",
             "\nIt should be a vector."))

    if(min(sort(unique(stats::na.omit(unlist(c(x)))))%in%categories)==0)
      stop(c("\nInvalid input for 'categories'.",
             "\nAll categories should be included if it is not NULL."))
  }


  ## NAasNA
  if(!(is.logical(NAasNA) && length(NAasNA)==1 && !is.na(NAasNA)))
    stop(c("\nInvalid input for 'NAasNA'.",
           "\nIt should be a logical value."))


  ## sortbyItem
  if(!(is.logical(sortbyItem) && length(sortbyItem)==1 && !is.na(sortbyItem)))
    stop(c("\nInvalid input for 'sortbyItem'.",
           "\nIt should be a logical value."))



  # Process ----

  out <- do.call(rbind,lapply(1L:ncol(x),function(i){
    mat <- matrix(data = 0L,ncol = length(categories),nrow = nrow(x))

    mat <- do.call(cbind,lapply(categories,function(j){
      as.numeric(x[,i,drop = TRUE]%in%j)
    }))

    if(NAasNA)
      mat[rowSums(mat)==0L,] <- NA

    return(mat)
  }))

  colnames(out) <- paste0('cat_',categories)

  out <- cbind.data.frame(id = rep(id,ncol(x)),
                          item = rep(colnames(x),each = nrow(x)),
                          out)

  if(!sortbyItem){
    out <- out[order(out[,'id']),]
    rownames(out) <- NULL
  }

  # Output ----

  return(out)

}


























