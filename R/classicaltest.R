#' Classical test theory item and person statistics
#'
#'
#' Calculates several item and person statistics following
#' \code{ctitem()}, and \code{ctperson()}.
#'
#' @param x a data frame or matrix. For multiple choice items, columns can be
#' numeric or strings and keys should be provided. For polytomous items,
#' all columns should be numeric.
#' @param key a vector indicating the keys to score the data. If \code{NULL}
#' items are assumed as polytomous.
#' Default is \code{NULL}.
#' @param categories a vector indicating all possible categories.
#' If \code{NULL}, this vector will be created with all the non-NA
#' values present in \code{x}. Default is \code{NULL}.
#' @param wt a numeric vector of total weights. Default is \code{NULL}.
#' @param listwise only consider complete data (remove rows with NAs).
#' Defaulft is \code{FALSE}.
#' @param recScore a logical value indicating if the total score for
#' should be calculated based only on valid values. Thus,
#' if \code{TRUE}: \code{rowMeans(x,na.rm = TRUE)*apply(x,1L,function(k) sum(!is.na(k)))};
#' if \code{FALSE}: \code{rowMeans(x,na.rm = TRUE)*ncol(x)}. Defaulft is \code{TRUE}.
#' If \code{listwise}, this argument is meaningless.
#' @param administered a logical matrix indicating which items where administered.
#' The dimensions should be the same as \code{x}. If \code{NULL} all items
#' are considered administered.
#'
#'
#' @return A list with item and person statistics.
#'
#'
#' @examples
#' data(dichodata)
#' data(polydata)
#' data(dichokey)
#'
#' # Data preparation
#' ## Random weights creation
#' set.seed(1919)
#' wt <- sample(x = 1:4, size = nrow(dichodata), replace = TRUE)
#'
#' # Item and person analysis for multiple choice items
#' classicaltest(x = dichodata, key = dichokey, categories = c('A','B','C','D'), wt = wt)
#'
#' # Item and person analysis for polytomous items
#' classicaltest(x = polydata, key = NULL, wt = wt)
#'
#' @export
#'

classicaltest <- function(x, key = NULL, categories = NULL, wt = NULL,
                          listwise = FALSE, recScore = TRUE,
                          administered = NULL){

  # Tests ----
  # x = polydata
  # key = NULL

  # x = dichodata
  # key = dichokey
  #
  # categories = NULL
  # wt = NULL
  # listwise = FALSE
  # recScore = TRUE
  # rm(x,key,categories,wt,listwise,recScore)

  # Checks ----


  ## x

  if(!(is.data.frame(x)|is.matrix(x)))
    stop(c("\nInvalid input for 'x'.",
           "\nIt should be a matrix or a data frame."))

  x <- as.data.frame(x)

  ## recScore
  if(!(is.logical(recScore) && length(recScore)==1 && !is.na(recScore)))
    stop(c("\nInvalid input for 'recScore'.",
           "\nIt should be a logical value."))

  ## key & correction

  if(is.null(key)){

    if(!is.numeric(as.matrix(x)))
      stop(c("\nInvalid input for 'x'.",
             "\nIt should be a numeric matrix or a data frame."))

    ispoly <- TRUE

    corre <- x
  }else{

    if(!is.vector(key))
      stop(c("\nInvalid input for 'key'.",
             "\nIt should be a vector."))

    if(ncol(x)!=length(key))
      stop(c("\nInvalid input for 'x' or 'key'.",
             "\nLength of 'key' should be equal to the number of columns of 'x'."))

    corre <- correct(x = x, key = key, navalue = ifelse(recScore,NA,0))

    ispoly <- FALSE
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


  ## listwise
  if(!(is.logical(listwise) && length(listwise)==1 && !is.na(listwise)))
    stop(c("\nInvalid input for 'listwise'.",
           "\nIt should be a logical value."))


  ## listwise remove

  if(listwise){
    X <- cbind(wt,x)
    X <- X[apply(X,1L,function(y) sum(is.na(y))==0L),]

    if(nrow(x)!=nrow(X)){
      warning(paste(nrow(x)-nrow(X),'case(s) removed.'),call. = FALSE)
    }


    if(nrow(X)<2L){
      warning('Not enough cases',call. = FALSE)
      return(NA)

    }


    x <- X[,-1L,drop = FALSE]
    wt <- X[,1L,drop = TRUE]
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

  item <- ctitem(x = x, key = key, categories = categories, wt = wt,
         listwise = listwise, recScore = recScore)

  person <- ctperson(x = corre, administered = administered)

  # Output ----

  list(item = item, person = person)

}
