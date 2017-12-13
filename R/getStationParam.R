#' @title Extract parameter corresponding to a station (location)
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

getStationParam = function(
    Site,
    Param,
    input_dir = dir_input
){
    # check out if station_parameters is loaded
    if(!exists("station_parameters")){
    load(file = paste0(input_dir, "Station_parameters.rData"))
    }
    parameter = station_parameters %>%
       dplyr::filter(Name == Site) %>%
       dplyr::select_(Param)
    # return value
    return(parameter)
}

