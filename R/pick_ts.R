#' @title Construct a time series out of different combinations of components
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

pick_ts = function(
    input_dir,
    site_name,
    variation,
    timePeriod_start,
    timePeriod_end,
    includeET = FALSE,
    gravityUnits = TRUE,
    facTopo,
    ...
){
    ########
    ## debugging
    # input_dir = dir_input
    # site_name = site
    # variation = site_component_combinations[1,]
    #######
    ## load site parameters
    # facTopo = getStationParam(site_name, "facTopo")
    ## load time series
    ####################
    # atmosphere
    atmosphere_hourly = read_data(
        data_in = paste0(site_name, "_atmosphere_hourly.tsf"),
        data_dir = paste0(input_dir, "Atmosphere/"),
        dat_tsf = (variation$atmo + 6))
    # change column name
    colnames(atmosphere_hourly)[2] = "atmo"
    # limit time period to decided period
    atmosphere_hourly = atmosphere_hourly %>%
        dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end)
    ####################
    # # discharge
    # load(file = paste0(input_dir, "Discharge/", site_name, "_discharge_hourly.rData"))
    # # convert from mm to gravity
    # # using topography factor
    # discharge_hourly = discharge_hourly %>%
    #     dplyr::mutate(discharge = value * facTopo) %>%
    #     dplyr::select(datetime, discharge)
    # # limit time period to decided period
    # discharge_hourly = discharge_hourly %>%
        # dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end)
    ####################
    # global hydrology
    globHyd_hourly = read_data(
        data_in = paste0(site_name, "_globalHydrology_hourly.tsf"),
        data_dir = paste0(input_dir, "GlobalHydrology/"),
        dat_tsf = (variation$globHyd + 6))
    # change column name
    colnames(globHyd_hourly)[2] = "globHyd"
    # limit time period to decided period
    globHyd_hourly = globHyd_hourly %>%
        dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end)
    ####################
    # non tidal ocean loading
    ntol_hourly = read_data(
        data_in = paste0(site_name, "_nonTidalOceanLoading_hourly.tsf"),
        data_dir = paste0(input_dir, "NonTidalOceanLoading/"),
        dat_tsf = (variation$ntol + 6))
    # change column name
    colnames(ntol_hourly)[2] = "ntol"
    # limit time period to decided period
    ntol_hourly = ntol_hourly %>%
        dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end)
    ####################
    # precipitation: precip_hourly
    load(file = paste0(input_dir, "Precipitation/", site_name, "_precipitation_hourly.rData"))
    # convert from mm to gravity
    # using topography factor
    precip_hourly = precip_hourly %>%
        dplyr::mutate(precip = value * facTopo) %>%
        dplyr::select(datetime, precip)
    # limit time period to decided period
    precip_hourly = precip_hourly %>%
        dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end)
    ####################
    # tides
    tides_hourly = read_data(
        data_in = paste0(site_name, "_tides_hourly.tsf"),
        data_dir = paste0(input_dir, "Tides/"),
        dat_tsf = (variation$tides + 6))
    # change column name
    colnames(tides_hourly)[2] = "tides"
    # limit time period to decided period
    tides_hourly = tides_hourly %>%
        dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end)
    ####################
     
    ####################
    ## combine time series
    # include evapotranspiration ??
    if(includeET){ # yes
        # daily
        # load(file = paste0(input_dir, "Evapotranspiration/", site_name, "_evapotranspiration_daily.rData"))
        # hourly
        load(file = paste0(input_dir, "Evapotranspiration/", site_name, "_evapotranspiration_hourly.rData"))
        # convert from mm to gravity
        # using topography factor
        ET_hourly = ET_hourly %>%
            dplyr::mutate(ET = -1 * value * facTopo) %>%
            dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end) %>%
            dplyr::select(datetime, ET)
        ## combine time series
        gravity_ts = ET_hourly %>%
            dplyr::inner_join(atmosphere_hourly) %>%
            # dplyr::inner_join(discharge_daily) %>%
            dplyr::inner_join(globHyd_hourly) %>%
            dplyr::inner_join(ntol_hourly) %>%
            dplyr::inner_join(precip_hourly) %>%
            dplyr::inner_join(tides_hourly) %>%
            # make sure there is no NA value !!
            # convert to 0
            # dplyr::mutate(value = ET + atmo + discharge + globHyd + ntol + precip + tides) %>%
            dplyr::mutate(value = ET + atmo + globHyd + ntol + precip + tides) %>%
            dplyr::select(datetime, value)
    }else{ # no
        ## combine time series
        gravity_ts = atmosphere_hourly %>%
            # dplyr::inner_join(discharge_daily) %>%
            dplyr::inner_join(globHyd_hourly) %>%
            dplyr::inner_join(ntol_hourly) %>%
            dplyr::inner_join(precip_hourly) %>%
            dplyr::inner_join(tides_hourly) %>%
            # make sure there is no NA value !!
            # convert to 0
            # dplyr::mutate(value = atmo + discharge + globHyd + ntol + precip + tides) %>%
            dplyr::mutate(value = atmo + globHyd + ntol + precip + tides) %>%
            dplyr::select(datetime, value)
    }
    ## select units of time series
    # if desired, [nm/s²] are converted in [mm], using the topography factor
    if(!gravityUnits){
      gravity_ts$value = gravity_ts$value / facTopo
    }
    ## return time series
    return(gravity_ts)
}

