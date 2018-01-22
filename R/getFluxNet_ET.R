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
    output_cumsum,
    plotting = F,
    show_raw = F,
    EB_correction = T,
    QF_correction = F,
    ...
){
    ## DEBUGGING
    # site_name = "test"
    # site_name = "BG"
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
    if(EB_correction){
    LE_raw = data_raw %>%
        dplyr::select(TIMESTAMP_START, LE_CORR, LE_F_MDS_QC) %>%
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
        dplyr::mutate(QualityFlags = LE_F_MDS_QC) %>%
        # filter out all -9999 values (=not enough info)
        dplyr::mutate(QualityFlags = ifelse(QualityFlags < 0, NA, QualityFlags)) %>%
        dplyr::select(datetime, value, QualityFlags)
    }else{
    LE_raw = data_raw %>%
        dplyr::select(TIMESTAMP_START, LE_F_MDS, LE_F_MDS_QC) %>%
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
        dplyr::mutate(value = LE_F_MDS / LE_water_const * 60 * 30) %>%
        dplyr::mutate(QualityFlags = LE_F_MDS_QC) %>%
        # filter out all -9999 values (=not enough info)
        dplyr::mutate(QualityFlags = ifelse(QualityFlags < 0, NA, QualityFlags)) %>%
        dplyr::select(datetime, value, QualityFlags)
    }
    # correct for quality flags
    # consequentially only using flag 1 data
    if(QF_correction){
        QF_threshold = 1
        ET_30min = ET_30min %>%
            dplyr::filter(QualityFlags <= QF_threshold)
    }

    # aggregate data to HOURLY ET values (in mm)
    ET_timeseriesMax_site = ET_30min %>%
        # add one hour, so data comes from the PAST hour, not the next one
        dplyr::mutate(datetime = as.POSIXct(trunc(datetime, "hour")) + 60 * 60) %>%
        dplyr::group_by(datetime) %>%
        dplyr::summarize(value = sum(value, na.rm = T))
    #
    # minimal data corrections
    ET_timeseriesMax_site = ET_timeseriesMax_site %>%
        # cut all values < 0
        dplyr::mutate(value = ifelse(value < 0, NA, value))


    # output either as actual value or
    # cumulative sum
    if(output_cumsum){
        # check if NAs exist and set them to 0 (no precipitation)
        # otherwise there is a problem with the cumsum-function
        ET_timeseriesMax_site = ET_timeseriesMax_site %>%
            dplyr::mutate(value = ifelse(is.na(value), 0, value))
        # calculate cummulative sum
        ET_timeseriesMax_site$value = cumsum(ET_timeseriesMax_site$value)
    }
    # plot for check
    if(plotting){
        if(show_raw){
            ET_raw = melt(ET_30min, id = "datetime")
            print(
              ggplot(ET_raw, aes(x = datetime, y = value)) + geom_point() + 
                  facet_grid(variable ~ ., scale = "free_y")
            )
        }else{
            print(
              ggplot(ET_timeseriesMax_site, aes(x = datetime, y = value)) + geom_point()
            )
        }
    }
    # explicitely convert to data.frame
    ET_timeseriesMax_site = as.data.frame(ET_timeseriesMax_site)
    # return formated dataset
    return(ET_timeseriesMax_site)
}

