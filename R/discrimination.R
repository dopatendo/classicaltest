#' Item discrimination
#'
#' Estimates item discrimination of a corrected matrix or
#' data frame.
#'
#'
#' @inheritParams ctperson
#' @inheritParams stcov
## @param partialcredit a logical value indicating if items should be considered
# as partial credit items. Defaul is \code{FALSE}. If more than one category over 0 is
# detected, items will be considered partial credit items regardless of this argument.
#' @param itemscores a numeric vector indicating all the possible scores for all items.
#' If \code{NULL}, possible scores will be derived from data.
#'
#' @keywords stats
#' @return a list.
#'
#' @examples
#' # No weights
#' ex <- correct(x = dichodata, key = dichokey, navalue = NA)
#' discrimination(ex)
#'
#'
#' # With weights
#' set.seed(1919)
#' wt <- sample(x = 1:4, size = nrow(dichodata), replace = TRUE)
#' discrimination(ex,wt = wt)
#'
#' @export
#'


discrimination <- function(x, wt = NULL,
                            itemscores = NULL,
                            administered = NULL){

  # Checks ----

  returnis(isnumMDF,x)
  # returnis(islova,partialcredit)
  returnisNULL(isnumbet,itemscores,from = 0, to = Inf)



  xcats <- sort(unique(unlist(c(x))))
  if(is.null(itemscores)){
    cats <- xcats
  }else{
    if(length(setdiff(xcats,itemscores))>0)
      stop(c("\nInvalid input for 'itemscores'.",
             "\nMore categories found in 'x'."))

    cats <- sort(unique(itemscores))
  }


  if(sum(sort(unique(unlist(c(x))))>0)>1)
    partialcredit <- TRUE


  if(min(unique(unlist(c(x)))%in%c(NA,0,1))==0){
    ispoly <- TRUE
  }else{
    ispoly <- FALSE
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

  ## Process ----


  .discrimination(x = x, xreal=x,
                  wt = wt, administered = administered,
                  cats = cats
                  # ,inverse = FALSE
                  )


}


#' @export
print.discrimination <- function(x, ...){

  if(!x$partialcredit){
    cat("Item discrimination for full credit items:\n")
    cat("Point-biserial correlation between test score (without considering the item) \nand item score.\n\n")

    o <- round(as.data.frame(x[[2]]),5)
    colnames(o) <- c("Sum score",
                     "Scaled of all items",
                     "Scaled of administered",
                     "Scaled of answered")
    print(o,row.names = TRUE)

  }else{
    cat("Item discrimination for partial credit items:\n")
    cat("Point-biserial correlation between test score (without considering the item) \nand an indicator variable for each possible score.\n\n")
    cat("\n Correlations with Sum scores:\n")
    o <- round(as.data.frame(x[[1]]),5)

    print(o,row.names = TRUE)
  }


}



.discrimination <- function(x,
                            xreal,
                            wt = NULL,
                            administered = NULL,
                            cats
                            # ,inverse = FALSE
                            # ,partialcredit = FALSE
                            ){

  # Checks ----





  ## Process ----

  if(is.null(administered)){
    administered <- matrix(TRUE,nrow = nrow(x),ncol = ncol(x))
  }


  # ct person total
  PER <- ctperson(xreal,administered = administered)
  PER <- PER[,grep("sum|scaled",colnames(PER),value = TRUE)]


  # remove not admin
  Ys <- x
  Ys[!administered] <- NA

  # if(inverse){
  #   Ys <- abs(Ys-1)
  # }


  # cats <- cats
  if(!0%in%cats){
    cats <- c(0,cats)
  }

  pbis <- vector("list",length(cats))

  for(j in 1:length(cats)){

    pbisj <- vector("list",ncol(x))
    names(pbisj) <- colnames(x)

    xj <- ((Ys==cats[j])*1)

    for(i in 1:ncol(x)){
      per <- ctperson(xreal[,-i],administered = administered[,-i])
      per <- per[,grep("sum|scaled",colnames(per),value = TRUE)]
      per <- sw(stats::cov2cor(stcov(cbind(xj[,i],per),wt = wt))[-1,1])
      per[is.nan(per)] <- NA
      pbisj[[i]] <- per


    }

    pbisj <- do.call(rbind,pbisj)
    pbis[[j]] <- pbisj

  }

  names(pbis) <- paste0("score_",cats)
  pbis <- c(pbis,partialcredit = length(pbis)>2)


  if(pbis$partialcredit){
    pbis <- c(lapply(1:ncol(pbis[[1]]),function(y){
      do.call(cbind,lapply(pbis[-length(pbis)],function(w) w[,y]))
    }),TRUE)
    names(pbis) <- c(grep("sum|scaled",colnames(PER),value = TRUE),"partialcredit")
  }




  # out <- list(restofitems = do.call(rbind,out),
              # allitems = stats::cov2cor(stcov(cbind(PER,Ys),wt = wt))[-(1:ncol(PER)),1:ncol(PER)])



  # rownames(out[[1]]) <- colnames(x)
  # rownames(out[[2]]) <- colnames(x)

  structure(pbis,class = c("classicaltest","discrimination",class(pbis)))
}









