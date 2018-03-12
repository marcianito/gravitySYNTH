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
    cummulative = TRUE,
    facTopo,
    ...
){
    ########
    ## debugging
    # input_dir = dir_input
    # site_name = site
    # variation = site_component_combinations[combination,]
    # facTopo = TopoFactor
    # timePeriod_start = ts_start
    # timePeriod_end = ts_end
    # includeET = FALSE
    # gravityUnits = TRUE
    #######
    ## load site parameters
    # facTopo = getStationParam(site_name, "facTopo")
    ## load time series
    ####################
    # atmosphere
    atmosphere_hourly = read_data(
        data_in = paste0(site_name, "_ATMO_all_models.tsf"),
        data_dir = paste0(input_dir, "Atmosphere/"),
        dat_tsf = (variation$atmo + 6))
    # change column name
    colnames(atmosphere_hourly)[2] = "atmo"
    # limit time period to decided period
    atmosphere_hourly = atmosphere_hourly %>%
        dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end)
    # subtract first value to set cummulative sums equal to zero at beginning of dataset
    atmosphere_hourly$atmo = atmosphere_hourly$atmo - atmosphere_hourly$atmo[1]
    ####################
    # global hydrology
    globHyd_hourly = read_data(
        data_in = paste0(site_name, "_GHE_all_models.tsf"),
        data_dir = paste0(input_dir, "GlobalHydrology/"),
        dat_tsf = (variation$globHyd + 6))
    # change column name
    colnames(globHyd_hourly)[2] = "globHyd"
    # limit time period to decided period
    globHyd_hourly = globHyd_hourly %>%
        dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end)
    # subtract first value to set cummulative sums equal to zero at beginning of dataset
    globHyd_hourly$globHyd = globHyd_hourly$globHyd - globHyd_hourly$globHyd[1]
    ####################
    # non tidal ocean loading
    ntol_hourly = read_data(
        data_in = paste0(site_name, "_NTOL_all_models.tsf"),
        data_dir = paste0(input_dir, "NonTidalOceanLoading/"),
        dat_tsf = (variation$ntol + 6))
    # change column name
    colnames(ntol_hourly)[2] = "ntol"
    # limit time period to decided period
    ntol_hourly = ntol_hourly %>%
        dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end)
    # subtract first value to set cummulative sums equal to zero at beginning of dataset
    ntol_hourly$ntol = ntol_hourly$ntol - ntol_hourly$ntol[1]
    ####################
    # tides
    tides_hourly = read_data(
        data_in = paste0(site_name, "_TIDE_all_models.tsf"),
        data_dir = paste0(input_dir, "Grav/tides/series/"),
        dat_tsf = (variation$tides + 6))
    # change column name
    colnames(tides_hourly)[2] = "tides"
    # limit time period to decided period
    tides_hourly = tides_hourly %>%
        dplyr::filter(datetime >= timePeriod_start & datetime < timePeriod_end)
    # subtract first value to set cummulative sums equal to zero at beginning of dataset
    tides_hourly$tides = tides_hourly$tides - tides_hourly$tides[1]
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
            dplyr::inner_join(globHyd_hourly) %>%
            dplyr::inner_join(ntol_hourly) %>%
            dplyr::inner_join(tides_hourly) %>%
            # make sure there is no NA value !!
            # convert to 0
            # dplyr::mutate(value = ET + atmo + discharge + globHyd + ntol + precip + tides) %>%
            dplyr::mutate(value = ET + atmo + globHyd + ntol + tides) %>%
            dplyr::select(datetime, value)
    }else{ # no
        ## combine time series
        gravity_ts = atmosphere_hourly %>%
            dplyr::inner_join(globHyd_hourly) %>%
            dplyr::inner_join(ntol_hourly) %>%
            dplyr::inner_join(tides_hourly) %>%
            # make sure there is no NA value !!
            # convert to 0
            # dplyr::mutate(value = atmo + discharge + globHyd + ntol + precip + tides) %>%
            dplyr::mutate(value = atmo + globHyd + ntol + tides) %>%
            dplyr::select(datetime, value)
    }
    ## cummulative or single time point values
    if(!cummulative){
      # in this case: construct time series of differences between time steps
      gravity_ts$value =  c(dplyr::lead(gravity_ts$value, 1) - gravity_ts$value)
      # remove last entry, which is NA due to lagged differences
      gravity_ts = gravity_ts[-length(gravity_ts$value),]
    }
    ## select units of time series
    # if desired, [nm/s²] are converted in [mm], using the topography factor
    if(!gravityUnits){
      gravity_ts$value = gravity_ts$value / facTopo
    }
    ## return time series
    return(gravity_ts)
}

