#' @title Get all possible combinations of gravity components
#'
#' @description Get a data.frame with all possible combinations of components for gravity corrections,
#' included in the corresponding folder.
#'
#' @param test test
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

getComponentCombinations = function(
    site_name,
    input_dir
){
    ## DEBUGGING
    # site_name = "test"
    # input_dir = dir_input
    ####################
    ## atmosphere
    # determine number of rows to skip
    # this is defined by hitting "[DATA]" in the .tsf file
    data_in = paste0(site_name, "_atmosphere_hourly.tsf")
    data_dir = paste0(input_dir, "Atmosphere/")
    data_lines = readLines(con = paste0(data_dir, data_in), n = 100)
    line_start = which(data_lines %in% "[DATA]")
    # read in .tsf file
    # and concatenate date information
    data_input = read.table(file = paste0(data_dir, data_in), skip=line_start, header=F, sep="", dec=".", na.strings=9999.999)
    components_atmo = length(data_input[1,]) - 6 
    ####################
    ## global hydrology
    # determine number of rows to skip
    # this is defined by hitting "[DATA]" in the .tsf file
    data_in = paste0(site_name, "_globalHydrology_hourly.tsf")
    data_dir = paste0(input_dir, "GlobalHydrology/")
    data_lines = readLines(con = paste0(data_dir, data_in), n = 100)
    line_start = which(data_lines %in% "[DATA]")
    # read in .tsf file
    # and concatenate date information
    data_input = read.table(file = paste0(data_dir, data_in), skip=line_start, header=F, sep="", dec=".", na.strings=9999.999)
    components_globHyd = length(data_input[1,]) - 6 
    ####################
    ## non tidal ocean loading
    # determine number of rows to skip
    # this is defined by hitting "[DATA]" in the .tsf file
    data_in = paste0(site_name, "_nonTidalOceanLoading_hourly.tsf")
    data_dir = paste0(input_dir, "NonTidalOceanLoading/")
    data_lines = readLines(con = paste0(data_dir, data_in), n = 100)
    line_start = which(data_lines %in% "[DATA]")
    # read in .tsf file
    # and concatenate date information
    data_input = read.table(file = paste0(data_dir, data_in), skip=line_start, header=F, sep="", dec=".", na.strings=9999.999)
    components_ntol = length(data_input[1,]) - 6 
    ####################
    ## tides
    # determine number of rows to skip
    # this is defined by hitting "[DATA]" in the .tsf file
    data_in = paste0(site_name, "_tides_hourly.tsf")
    data_dir = paste0(input_dir, "Tides/")
    data_lines = readLines(con = paste0(data_dir, data_in), n = 100)
    line_start = which(data_lines %in% "[DATA]")
    # read in .tsf file
    # and concatenate date information
    data_input = read.table(file = paste0(data_dir, data_in), skip=line_start, header=F, sep="", dec=".", na.strings=9999.999)
    components_tides = length(data_input[1,]) - 6 
    # 
    ## construct data.frame with all possible combinations
    site_component_combinations = expand.grid(
    atmo = round(seq(1, components_atmo), 0),
    # discharge = 1,
    globHyd = round(seq(1, components_globHyd), 0),
    ntol = round(seq(1, components_ntol), 0),
    # precipitation = 1,
    tides = round(seq(1, components_tides), 0)
    )
    #
    ## return complete dataset
    return(site_component_combinations)
}

