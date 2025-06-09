#' Distractor statistics
#'
#' Summarize the statistics for each category of each item.
#'
#' @inheritParams correct
#' @inheritParams ctperson
#' @inheritParams discrimination
#' @param itemcategories a vector indicating all the possible categories for all
#' items. If NULL, possible categories will be derived from data.
#'
#' @return a list.
#'
#' @examples
#' # Full credit items
#' distractors(x = dichodata, key = dichokey)
#'
#' # Partial credit items
#' distractors(x = partialdata, key = partialkey)
#'
#' @export
#'




distractors <- function(x, key,
                        wt = NULL,
                        navalue = NA,
                        itemcategories = NULL,
                        administered = NULL){

  # Checks ----

  # Process ----

  sco <- correct(x = x,key = key,navalue = navalue)
  per <- ctperson(sco)

  ## Categories
  catl <- .cats(x) # from data
  if(!is.null(itemcategories)){
    if(length(setdiff(catl,itemcategories))>0)
      stop(c("\nInvalid input for 'itemscores'.",
             "\nMore categories found in 'x'."))

    catl <- sort(unique(itemcategories))
  }


  catn <- 0:(length(catl)-1)
  xn <- x

  for(i in 1:length(catl)){
    if(i==1){
      xn <- (x==catl[i])*catn[i]
    }else{
      xn <- xn+(x==catl[i])*catn[i]
    }

  }


  # Ns

  if(is.null(wt)){wt <- rep(1,nrow(x))}

  ene <- vector("list",ncol(x))

  for(i in 1:length(ene)){
    xfi <- factor(x[,i],levels = catl)
    tab <- table(factor(x[,i],levels = catl))

    wtt <- stats::xtabs(wt ~ xfi)
    wttv <- wtt/sum(wt[!is.na(xfi)],na.rm = TRUE)
    wtt <- prop.table(tab)
    ene[[i]] <- cbind(tab,wtt,wttv)
  }
  ene <- do.call(rbind,ene)
  colnames(ene) <- c("N","Nprop","NpropWT")

  # discrimination

  toy <- as.data.frame(matrix(rep(catl,ncol(x)),nrow = length(catl)))
  toy <- correct(x = toy,key = key)


  dist <- .discrimination(x = xn, xreal = sco,
                          wt = wt,administered = administered,
                          cats = catn)
  dist <- dist[-length(dist)]
  aa = dist[[1]]

  class(dist) <- "list"



  dist <- lapply(dist, function(y){

    y <- cbind.data.frame(item = rep(colnames(x),each = length(catl)),
                          category = rep(catl,ncol(x)),
                          score = unlist(c(toy)),ene,
                          pointbiserial = c(t(dist[[1]])))
    rownames(y) <- NULL


    # colnames(x) <- paste0("cat_",catl)
    # x <- cbind.data.frame(key,x)
    # x <- t(x)
    # x <- cbind.data.frame(category = catl,x)
    # rownames(x) <- NULL
    return(y)
  })





  # out


  structure(dist,class = c("classicaltest","distractors",class(dist)))

}




#' @export
print.distractors <- function(x, ...){
  cat("Distractor statistics using Sum scores\n\n")

  # x <- head(x[[1]])
  x <- x[[1]]
  x <- do.call(cbind.data.frame,
               lapply(x,function(x){
                 if(!is.numeric(x))
                   return(x)
                 round(x,5)

               }))
  print(as.data.frame(x),row.names = FALSE)

}
