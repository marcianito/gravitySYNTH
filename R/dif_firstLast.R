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
#' @import dplyr
#' @examples missing

dif_firstLast = function(
   data_in
){
   dif_val = dplyr::last(data_in) - dplyr::first(data_in)
   return(dif_val)
}

