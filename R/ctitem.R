#' Classical test theory item statistics
#'
#'
#' Calculates several item statistics, including: item means,
#' frequencies, proportions, and correlations between item responses and the total score.
#'
#' @inheritParams correct
#' @inheritParams distractors
#' @inheritParams discrimination
#'
#'
#' @return A list.
#'
#'
#' @examples
#'
#' # Data preparation
#' ## Random weights creation
#' set.seed(1919)
#' wt <- sample(x = 1:4, size = nrow(dichodata), replace = TRUE)
#'
#' # Item analysis for multiple choice items
#' ctitem(x = dichodata, key = dichokey, itemcategories = c('A','B','C','D'), wt = wt)
#'
#'
#' @export
#'




ctitem <- function(x,key,navalue = NA, wt = NULL,
                   itemcategories = NULL,
                   administered = NULL,
                   itemscores = NULL){

  scor <- correct(x = x, key = key, navalue = navalue)
  dist <- distractors(x = x, key = key, wt = wt,
                      navalue = navalue,
                      itemcategories = itemcategories,
                      administered = administered)
  disc <- discrimination(x = scor,wt = wt,
                         itemscores = itemscores,
                         administered = administered)

  itme <- itemmean(x = scor,wt = wt)


  # all items
  num <- 2
  dn <- dist[[num]]
  ca <- unique(dist[[num]]$category)
  dn <- split(dn,f=dn$item)
  dn <- do.call(rbind,lapply(dn, function(x){
    unlist(c(x[,c("score","pointbiserial","N","Nprop","NpropWT")]))
  }))
  colnames(dn) <- paste0(rep(c("score","pointbiserial","N","Nprop","NpropWT"),
                             each = length(ca)),"_",ca)
  out1 <- cbind.data.frame(item = colnames(x),
                           # key = key,
                           mean = itme[,1],
                           pointbiserial = disc$score_1[,num],
                           dn)
  rownames(out1) <- NULL

  # admin
  num <- 3
  dn <- dist[[num]]
  ca <- unique(dist[[num]]$category)
  dn <- split(dn,f=dn$item)
  dn <- do.call(rbind,lapply(dn, function(x){
    unlist(c(x[,c("score","pointbiserial","N","Nprop","NpropWT")]))
  }))
  colnames(dn) <- paste0(rep(c("score","pointbiserial","N","Nprop","NpropWT"),
                             each = length(ca)),"_",ca)
  out2 <- cbind.data.frame(item = colnames(x),
                           # key = key,
                           mean = itme[,1],
                           pointbiserial = disc$score_1[,num],
                           dn)
  rownames(out2) <- NULL

  # answered
  num <- 4
  dn <- dist[[num]]
  ca <- unique(dist[[num]]$category)
  dn <- split(dn,f=dn$item)
  dn <- do.call(rbind,lapply(dn, function(x){
    unlist(c(x[,c("score","pointbiserial","N","Nprop","NpropWT")]))
  }))
  colnames(dn) <- paste0(rep(c("score","pointbiserial","N","Nprop","NpropWT"),
                             each = length(ca)),"_",ca)
  out3 <- cbind.data.frame(item = colnames(x),
                           # key = key,
                           mean = itme[,1],
                           pointbiserial = disc$score_1[,num],
                           dn)
  rownames(out3) <- NULL


  out <- list(total = out1,admin = out2,answr = out3)

  out <- structure(out,class = c("classicaltest","ctitem",
                                 class(out)))

  out

}


#' @export
print.ctitem <- function(x,...){
  print(x[[1]])
}
