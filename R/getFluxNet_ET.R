#' @title Extract and format evapotranspiration data from FluxNet
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

getFluxNet_ET = function(
    site_name,
    input_dir,
    plotting = F,
    show_LE = F,
    ...
){
    ## DEBUGGING
    # site_name = "test"
    # site_name = "SU"
    # input_dir = dir_input
    ##
    FluxNet_station = getStationParam(Site = site_name, Param = "FluxNet")
    file_name_path = paste0(input_dir, "Evapotranspiration/",
                "FLX_SUBSET_", FluxNet_station, ".csv")
    # read complete dataset
    data_raw = read.table(file = file_name_path,
                        sep = ",", dec = ".",
                        stringsAsFactors = F,
                        header = T)
    # extract LE data
    LE_raw = data_raw %>%
        dplyr::select(TIMESTAMP_START, LE_CORR) %>%
        dplyr::mutate(datetime = as.POSIXct(strptime(TIMESTAMP_START, format = "%Y%m%d%H%M"))) %>%
        dplyr::select(-TIMESTAMP_START)
    # convert LE to ET
    # constant latent head of vaporization for water
    # in [J / Kg]
    LE_water_const = 2257000
    # latent heat in [W*m-2] per 30 minutes (average)
    # !! attention to the units !!
    # simple division leads to units kg / ( m² * s) for 30 min
    # which is equal to mm / s for 30 min
    # multiply by 60 min for 30 min and add all values to get mm / day
    ET_30min = LE_raw %>%
        dplyr::mutate(value = LE_CORR / LE_water_const * 60 * 30) %>%
        dplyr::select(datetime, value)

    # aggregate data to HOURLY ET values (in mm)
    ET_timeseriesMax_site = ET_30min %>%
        # add one hour, so data comes from the PAST hour, not the next one
        dplyr::mutate(datetime = as.POSIXct(trunc(datetime, "hour")) + 60 * 60) %>%
        dplyr::group_by(datetime) %>%
        dplyr::summarize(value = sum(value, na.rm = T))

    # plot for check
    if(plotting){
        if(show_LE){
            print(
              ggplot(LE_raw, aes(x = datetime, y = LE_CORR)) + geom_point()
            )
        }else{
            print(
              ggplot(ET_timeseriesMax_site, aes(x = datetime, y = value)) + geom_point()
            )
        }
    }

    # return formated dataset
    return(ET_timeseriesMax_site)
}

