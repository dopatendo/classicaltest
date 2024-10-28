#' Classical test theory item statistics
#'
#'
#' Calculates several item statistics, including: item mean,
# Cronbach's alpha if item is dropped,
#' frequencies, proportions,
#' valid proportions, and correlations between item responses and the total score.
#'
#' If keys are provided, items are assume as dichotomous and
#' transformed into 1s and 0s, where 1s are correct
#' answers. Then, point-biserial correlations are estimated between the item
#' and the total score (PBtotal), the item and the score without the item
#' (PBrest), between each response category and the total score.
#'
#' If keys are not provided, data must be numeric,
#' items are assumed as polytomous and data will not be transformed.
#' For polytomous, Pearson's correlations are estimated between the item
#' and the total score (PEtotal), the item and the score without the item
#' (PErest), between each response category and the total score.
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
#'
#'
#' @return A data frame with item statistics.
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
#' # Item analysis for multiple choice items
#' ctitem(x = dichodata, key = dichokey, categories = c('A','B','C','D'), wt = wt)
#'
#' # Item analysis for polytomous items
#' ctitem(x = polydata, key = NULL, wt = wt)
#'
#' @export
#'







ctitem <- function(x, key = NULL, categories = NULL, wt = NULL,
                   listwise = FALSE, recScore = TRUE){

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


  # Process ----


  ## Dichotomization

  dich <- dichotomize(x = x,
                      id = NULL,
                      categories = categories,
                      NAasNA = TRUE,
                      sortbyItem = TRUE)
  dibyitem <- split(dich[,-(1L:2L)],as.factor(dich[,2L]))

  ## Frequencies

  freqs <- do.call(rbind,lapply(dibyitem,function(y){
    c(colSums(y,na.rm = TRUE),sum(is.na(y[,1L])))
  }))
  colnames(freqs) <- c(sub('cat_','freq_',colnames(dich[,-(1L:2L)])),'freq_NA')

  ## Proportions

  props <- freqs/nrow(x)
  colnames(props) <- sub('freq_','prop_',colnames(props))

  propsv <- freqs[,1L:(ncol(dich)-2L)]/(nrow(x)-freqs[,'freq_NA'])
  colnames(propsv) <- sub('freq_','propvalid_',colnames(propsv))

  ## Correlations by category

  meansco <- rowMeans(corre,na.rm = TRUE)

  if(recScore){
    score <- meansco*apply(corre,1L,function(k) sum(!is.na(k)))
  }else{
    score <- meansco*ncol(x)
  }


  rho <- do.call(rbind,lapply(dibyitem,function(y){
    if(is.null(wt))
      return(c(stats::cor(score,y,use = 'pairwise')))

    mat <- stats::na.omit(cbind(wt,score,y))

    return(c(stats::cov.wt(x = mat[,-1L],wt = mat[,1L],cor = TRUE)$cor[-1L,1L]))

  }))
  colnames(rho) <- sub('propvalid_','PB_',colnames(propsv))

  ## Pointbiserial without exclusion
  PBtotal <- .rhototal(corre, wt = wt,
                           exclude = FALSE,
                           # listwise = listwise,
                           recScore = recScore)

  ## Pointbiserial without exclusion
  PBrest <- .rhototal(corre, wt = wt,
                          exclude = TRUE,
                          # listwise = listwise,
                          recScore = recScore)

  ## Alpha if dropped

  # AlphaIfDrop <- ctrelia(x = corre,
                         # wt = wt,
                         # drop = TRUE,listwise = listwise,recScore = recScore)


  # Output ----

  if(ispoly){

    out <- cbind.data.frame(item = colnames(x),
                            # key = key,
                            mean = colMeans(corre,na.rm = TRUE),
                            # AlphaIfDrop,
                            PBtotal, PBrest,
                            rho,freqs,props,propsv)

    colnames(out)[substr(colnames(out),1,2)=='PB'] <- sub('PB','PE',colnames(out)[substr(colnames(out),1,2)=='PB'])
  }else{

    out <- cbind.data.frame(item = colnames(x),
                            key = key,
                            mean = colMeans(corre,na.rm = TRUE),
                            # AlphaIfDrop,
                            PBtotal, PBrest,
                            rho,freqs,props,propsv)
  }


  rownames(out) <- NULL
  return(out)

}


