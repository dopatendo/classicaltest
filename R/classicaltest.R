#' Classical test theory item and person statistics
#'
#'
#' Calculates several item and person statistics following
#' \code{ctitem()}, and \code{ctperson()}.
#'
#' @inheritParams ctitem
#' @inheritParams ctperson
#'
#' @return A list with item and person statistics.
#'
#'
#' @examples

#' # Data preparation
#' ## Random weights creation
#' set.seed(1919)
#' wt <- sample(x = 1:4, size = nrow(dichodata), replace = TRUE)
#'
#' # Item and person analysis for multiple choice items
#' classicaltest(x = dichodata, key = dichokey, wt = wt)
#'
#'
#' @export
#'

classicaltest <- function(x,
                          key,
                          navalue = NA, wt = NULL,
                          itemcategories = NULL,
                          administered = NULL,
                          itemscores = NULL,
                          maxscore = NULL){

  scor <- correct(x = x, key = key, navalue = navalue)


  person <- ctperson(x = scor,
                     administered = administered,
                     maxscore = maxscore)

  items <- ctitem(x = x,key = key,
                 navalue = navalue, wt = wt,
           itemcategories = itemcategories,
           administered = administered,
           itemscores = itemscores)




  out <- list(items = items,person=person)

  out <- structure(out,class = c("classicaltest","ctest",
                                 class(out)))

  out


  }


  #' @export
  print.ctest<- function(x,...){
    print(x[[1]][[1]])
  }

