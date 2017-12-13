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
    includeET = FALSE,
    gravityUnits = TRUE,
    ...
){
    ########
    ## debugging
    # input_dir = dir_input
    # site_name = site
    # variation = site_component_combinations[1,]
    # facTopo = 0.418
    #######
    ## load site parameters
    facTopo = getStationParam(site_name, "facTopo")
    ## load time series
    ####################
    # atmosphere
    atmosphere_daily = read_data(
        data_in = paste0(site_name, "_atmosphere_hourly.tsf"),
        data_dir = paste0(input_dir, "Atmosphere/"),
        dat_tsf = (variation$atmo + 6))
    # change column name
    colnames(atmosphere_daily)[2] = "atmo"
    ####################
    # # discharge
    # load(file = paste0(input_dir, "Discharge/", site_name, "_discharge_daily.rData"))
    # # convert from mm to gravity
    # # using topography factor
    # discharge_daily = discharge_daily %>%
    #     dplyr::mutate(discharge = value * facTopo) %>%
    #     dplyr::select(datetime, discharge)
    ####################
    # global hydrology
    globHyd_daily = read_data(
        data_in = paste0(site_name, "_globalHydrology_hourly.tsf"),
        data_dir = paste0(input_dir, "GlobalHydrology/"),
        dat_tsf = (variation$globHyd + 6))
    # change column name
    colnames(globHyd_daily)[2] = "globHyd"
    ####################
    # non tidal ocean loading
    ntol_daily = read_data(
        data_in = paste0(site_name, "_nonTidalOceanLoading_hourly.tsf"),
        data_dir = paste0(input_dir, "NonTidalOceanLoading/"),
        dat_tsf = (variation$ntol + 6))
    # change column name
    colnames(ntol_daily)[2] = "ntol"
    ####################
    # precipitation: precip_daily
    load(file = paste0(input_dir, "Precipitation/", site_name, "_precipitation_daily.rData"))
    # convert from mm to gravity
    # using topography factor
    precip_daily = precip_daily %>%
        dplyr::mutate(precip = value * facTopo) %>%
        dplyr::select(datetime, precip)
    ####################
    # tides
    tides_daily = read_data(
        data_in = paste0(site_name, "_tides_hourly.tsf"),
        data_dir = paste0(input_dir, "Tides/"),
        dat_tsf = (variation$tides + 6))
    # change column name
    colnames(tides_daily)[2] = "tides"
    ####################
    # 
    ####################
    ## combine time series
    # include evapotranspiration ??
    if(includeET){ # yes
        load(file = paste0(input_dir, "Evapotranspiration/", site_name, "_evapotranspiration_daily.rData"))
        # convert from mm to gravity
        # using topography factor
        ET_daily = ET_daily %>%
            dplyr::mutate(ET = value * facTopo) %>%
            dplyr::select(datetime, ET)
        ## combine time series
        gravity_ts = ET_daily %>%
            dplyr::inner_join(atmosphere_daily) %>%
            # dplyr::inner_join(discharge_daily) %>%
            dplyr::inner_join(globHyd_daily) %>%
            dplyr::inner_join(ntol_daily) %>%
            dplyr::inner_join(precip_daily) %>%
            dplyr::inner_join(tides_daily) %>%
            # make sure there is no NA value !!
            # convert to 0
            dplyr::mutate(value = ET + atmo + discharge + globHyd + ntol + precip + tides) %>%
            dplyr::select(datetime, value)
    }else{ # no
        ## combine time series
        gravity_ts = atmosphere_daily %>%
            # dplyr::inner_join(discharge_daily) %>%
            dplyr::inner_join(globHyd_daily) %>%
            dplyr::inner_join(ntol_daily) %>%
            dplyr::inner_join(precip_daily) %>%
            dplyr::inner_join(tides_daily) %>%
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

