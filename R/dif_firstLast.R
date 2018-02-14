#' @title Calculate differences between last and first value of a vector
#'
#' @description test
#'
#' @param test test
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

dif_firstLast = function(
   data_in
){
   dif_val = last(data_in) - first(data_in)
   return(dif_val)
}

