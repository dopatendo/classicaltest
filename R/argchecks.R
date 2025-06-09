#' Functions for testing arguments and return errors
#'
#'
#' @param x an argument
#'
#' @noRd

isformula <- function(x = NULL){
  if(is.null(x))
    return("It should be a formula.")

  inherits(x,"formula")

}


islist<- function(x = NULL){
  if(is.null(x))
    return("It should be a list.")

  inherits(x,"list")

}


isvec <- function(x = NULL){
  if(is.null(x))
    return("It should be a vector.")

  is.vector(x)

}

ischavec <- function(x = NULL){
if(is.null(x))
  return("It should be a character vector.")

  is.vector(x)&&is.character(x)

}

ischaval <- function(x = NULL){
  if(is.null(x))
    return("It should be a string.")

  is.vector(x)&&is.character(x)&&length(x)==1

}

isrep.mean <- function(x = NULL){
  if(is.null(x))
    return("It should be an object produced by repmean().")

  inherits(x,"repmean")

}

isrep.meansingle <- function(x = NULL){
  if(is.null(x))
    return("It should be an object produced by repmean() for one variable.")

  inherits(x,"repmean.single")

}



islova <- function(x = NULL){
  if(is.null(x))
    return("It should be a logical value.")

  is.vector(x)&&is.logical(x)&&length(x)==1&&!is.na(x)
}

isdfonly <- function(x = NULL){
  if(is.null(x))
    return("Class should be only 'data.frame'.")

  is.data.frame(x)&&length(class(x))==1

}

isdf <- function(x = NULL){
  if(is.null(x))
    return("It should be a data frame.")

  is.data.frame(x)

}

isdf.or.mat <- function(x = NULL){
  if(is.null(x))
    return("It should be a matrix or data frame.")

  is.data.frame(x)|is.matrix(x)

}

is.chavec.or.dfonly <- function(x){
  if(is.null(x))
    return("It should be a character vector or a data frame.")

  ischavec(x)|isdfonly(x)

}




isinvec <- function(x = NULL, choices){
  if(is.null(x))
    return(paste0("It should be one of the following values: ",
                  paste0(eval(choices),collapse = ", "),"."))

  tr = try(pmatch(tolower(x),tolower(eval(choices)),
                  nomatch = FALSE),silent = TRUE)
  if(inherits(tr,"try-error")||tr==0)
    return(FALSE)


  eval(choices)[tr]

}


returnis <- function(f, x, ...){

  f <- match.fun(f)
  ev <- f(x = x, ...)

    if(isFALSE(ev)){

    stop(paste0("\nInvalid input for '",
                gsub("\\[1L\\]","",deparse(substitute(x))),
                "'.\n",f(x = NULL, ...)),call. = FALSE)
    }

 ev





}

returnisNULL <- function(f, x, ...){

  f <- match.fun(f)
  ev <- f(x = x, ...)

  if(!is.null(x)&&isFALSE(ev)){

    stop(paste0("\nInvalid input for '",
                deparse(substitute(x)),"'.\n",
                sub("be ","be NULL or ",f(x = NULL, ...))),call. = FALSE)
  }

  ev

}


# Numeric vector ----------------------------------------------------------



isnumvec <- function(x = NULL){
  if(is.null(x))
    return("It should be a numeric vector.")

  is.vector(x)&&is.numeric(x)

}

isnumval <- function(x = NULL){
  if(is.null(x))
    return("It should be a numeric value.")

  isnumvec(x)&&length(x)==1

}

isnumbet <- function(x = NULL, from = 0, to = 1){
  if(is.null(x))
    return(paste0("It should be between: ",from," and ",to,"."))

  if(!all(x<=to&x>=from))
    return(FALSE)

  x

}


# Compare sizes -----------------------------------------------------------

same.nrow.length <- function(x = NULL, y = NULL, showEr = FALSE){
  if(showEr)
    return(paste0("Length of '",gsub("\\[1L\\]","",y),
                   "' should match the number of rows of '",
                   gsub("\\[1L\\]","",x),
                   "'.\n"))




  nrow(x)==length(y)
}



returnis2 <- function(f, x, y, ...){

  f <- match.fun(f)
  ev <- f(x = x, y = y, ...)

  if(isFALSE(ev)){

    stop(paste0("\nInvalid input for '",
                gsub("\\[1L\\]","",deparse(substitute(x))),
                "' or '",
                gsub("\\[1L\\]","",deparse(substitute(y))),
                "'.\n",f(x = deparse(substitute(x)),
                         y = deparse(substitute(y)),
                         showEr = TRUE, ...)),call. = FALSE)
  }

  ev

}


returnis2NULL <- function(f, x, y, ...){

  f <- match.fun(f)
  ev <- f(x = x, y = y, ...)

  if(!is.null(y)&&isFALSE(ev)){

    stop(paste0("\nInvalid input for '",
                deparse(substitute(y)),"'.\n",
                sub("should ","should be NULL or ",f(x = deparse(substitute(x)),
                                          y = deparse(substitute(y)),
                                          showEr = TRUE, ...))),call. = FALSE)
  }

  ev

}


# x is df, y is colname
isindf <- function(x = NULL, y = NULL, showEr = FALSE){
  if(showEr)
    return(paste0("All values of '",gsub("\\[1L\\]","",y),
                  "' should be in ",
                  gsub("\\[1L\\]","",x),
                  "'.\n"))


  all(y%in%colnames(x))

}



# NEW ---------------------------------------------------------------------

# numeric vector, matrix or df
isnumVMDF <- function(x = NULL){
  if(is.null(x))
    return("It should be a numeric vector, matrix or data frame.")

  idf <- isdfonly(x)
  imt <- inherits(x,"matrix")
  ive <- isnumvec(x)

  # check class
  if(!any(idf,imt,ive))
    return(FALSE)


  # check numeric
  if(idf|imt)
    return(all(apply(x,2,is.numeric)))

  is.numeric(x)

}

# numeric vector, matrix or df
isnumMDF <- function(x = NULL){
  if(is.null(x))
    return("It should be a numeric matrix or data frame.")

  idf <- isdfonly(x)
  imt <- inherits(x,"matrix")


  # check class
  if(!any(idf,imt))
    return(FALSE)


  # check numeric
  all(apply(x,2,is.numeric))


}





