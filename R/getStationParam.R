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
    # # check out if station_parameters is loaded
    # if(!exists("config_file")){
    # load(file = paste0(input_dir, "config_file.RData"))
    # }
    # load .csv file
    config_file = read.table(file=paste0(input_dir, "config_file.csv"),
        sep = ";", dec = ",", header = T, stringsAsFactors = F, nrows = 8)
    # choose parameters
    if(Site == "all"){
    parameter = config_file %>%
       dplyr::select_(Param)
    }else{
    parameter = config_file %>%
       dplyr::filter(Name == Site) %>%
       dplyr::select_(Param)
    }
    # # determine and output correct type (NOT data.frame !!)
    parameter = as.vector(parameter[,1])
    # return value
    return(parameter)
}

