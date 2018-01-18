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
    input_dir = "./"
){
    # check out if station_parameters is loaded
    if(!exists("config_file")){
    load(file = paste0(input_dir, "config_file.RData"))
    }
    parameter = config_file %>%
       dplyr::filter(Name == Site) %>%
       dplyr::select_(Param)
    # determine and output correct type (NOT data.frame !!)
    if(is.numeric(parameter[1,1])){
        parameter = as.numeric(parameter)
    }else{
        parameter = as.character(parameter)
    }
    # return value
    return(parameter)
}

