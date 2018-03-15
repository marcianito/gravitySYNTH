#' @title Get the names of all gravity components, used in combinations
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
    input_dir,
    component_name = NA,
    output_singleCombinations 
){
    ## DEBUGGING
    # site_name = "test"
    # input_dir = dir_input
    # component_name = "atmo"
    ####################
    ## atmosphere
    # determine number of rows to skip
    # this is defined by hitting "[DATA]" in the .tsf file
    data_in = paste0(site_name, "_ATMO_all_models.tsf")
    data_dir = paste0(input_dir, "Atmosphere/")
    data_lines = readLines(con = paste0(data_dir, data_in), n = 100)
    line_start = which(data_lines %in% "[DATA]")
    # read in .tsf file
    # and concatenate date information
    data_input = read.table(file = paste0(data_dir, data_in), skip=line_start, header=F, sep="", dec=".", na.strings=9999.999)
    components_atmo = length(data_input[1,]) - 6 

    # read out names
    line_start_channel = which(data_lines %in% "[CHANNELS]")
    component_names = read.table(file = paste0(data_dir, data_in), skip=line_start_channel, nrow = components_atmo, header=F, sep=":", dec=".", na.strings=9999.999, stringsAsFactors = F)[,3]
    names_atmo = data.frame(atmo = round(seq(1:components_atmo),0),
                            Atmosphere = component_names)
    ####################
    ## global hydrology
    # determine number of rows to skip
    # this is defined by hitting "[DATA]" in the .tsf file
    data_in = paste0(site_name, "_GHE_all_models.tsf")
    data_dir = paste0(input_dir, "GlobalHydrology/")
    data_lines = readLines(con = paste0(data_dir, data_in), n = 100)
    line_start = which(data_lines %in% "[DATA]")
    # read in .tsf file
    # and concatenate date information
    data_input = read.table(file = paste0(data_dir, data_in), skip=line_start, header=F, sep="", dec=".", na.strings=9999.999)
    components_globHyd = length(data_input[1,]) - 6 

    # read out names
    line_start_channel = which(data_lines %in% "[CHANNELS]")
    component_names = read.table(file = paste0(data_dir, data_in), skip=line_start_channel, nrow = components_globHyd, header=F, sep=":", dec=".", na.strings=9999.999, stringsAsFactors = F)[,3]
    names_globHyd = data.frame(globHyd = round(seq(1:components_globHyd),0),
                            GlobalHydrology = component_names)
    ####################
    ## non tidal ocean loading
    # determine number of rows to skip
    # this is defined by hitting "[DATA]" in the .tsf file
    data_in = paste0(site_name, "_NTOL_all_models.tsf")
    data_dir = paste0(input_dir, "NonTidalOceanLoading/")
    data_lines = readLines(con = paste0(data_dir, data_in), n = 100)
    line_start = which(data_lines %in% "[DATA]")
    # read in .tsf file
    # and concatenate date information
    data_input = read.table(file = paste0(data_dir, data_in), skip=line_start, header=F, sep="", dec=".", na.strings=9999.999)
    components_ntol = length(data_input[1,]) - 6 

    # read out names
    line_start_channel = which(data_lines %in% "[CHANNELS]")
    component_names = read.table(file = paste0(data_dir, data_in), skip=line_start_channel, nrow = components_ntol, header=F, sep=":", dec=".", na.strings=9999.999, stringsAsFactors = F)[,3]
    names_ntol = data.frame(ntol = round(seq(1:components_ntol),0),
                            NTOL = component_names)
    ####################
    ## tides
    # determine number of rows to skip
    # this is defined by hitting "[DATA]" in the .tsf file
    data_in = paste0(site_name, "_TIDE_all_models.tsf")
    # data_dir = paste0(input_dir, "Tides/")
    data_dir = paste0(input_dir, "Grav/tides/series/")
    data_lines = readLines(con = paste0(data_dir, data_in), n = 100)
    line_start = which(data_lines %in% "[DATA]")
    # read in .tsf file
    # and concatenate date information
    data_input = read.table(file = paste0(data_dir, data_in), skip=line_start, header=F, sep="", dec=".", na.strings=9999.999)
    components_tides = length(data_input[1,]) - 6 

    # read out names
    line_start_channel = which(data_lines %in% "[CHANNELS]")
    component_names = read.table(file = paste0(data_dir, data_in), skip=line_start_channel, nrow = components_tides, header=F, sep=":", dec=".", na.strings=9999.999, stringsAsFactors = F)[,3]
    names_tides = data.frame(tides = round(seq(1:components_tides),0),
                            Tides = component_names)
    # 
    if(!output_singleCombinations){
      ## construct data.frame with all possible combinations
      site_component_combinations = expand.grid(
      atmo = round(seq(1, components_atmo), 0),
      # discharge = 1,
      globHyd = round(seq(1, components_globHyd), 0),
      ntol = round(seq(1, components_ntol), 0),
      # precipitation = 1,
      tides = round(seq(1, components_tides), 0)
      )
      # decide what to return
      if(!is.na(component_name)){
        switch(component_name,
             atmo = {return_data = names_atmo},
             tides = {return_data = names_tides},
             ntol = {return_data = names_ntol},
             globHyd = {return_data = names_globHyd}
             )
      }else{
        return_data = site_component_combinations
      }
    }else{
      switch(component_name,
           atmo = {return_data = data.frame(atmo = round(seq(1, components_atmo), 0))},
           tides = {return_data = data.frame(globHyd = round(seq(1, components_globHyd), 0))},
           ntol = {return_data = data.frame(ntol = round(seq(1, components_ntol), 0))},
           globHyd = {return_data = data.frame(tides = round(seq(1, components_tides), 0))}
           )
    }
    #
    ## return dataset
    return(return_data)
}

