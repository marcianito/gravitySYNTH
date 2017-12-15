#' @title Compare differences of averages, over the complete day, to daily diferences from actual time series 
#'
#' @description observed data input: hourly (cumsums), actual / reference input: daily (difs)
#'
#' @param data_observed observed and corrected gravity time series. Data.frame (datetime, value) in [nm/s²],
#' @param data_reference measured evapotranspiration time series. Data.frame (datetime, value) in [mm],
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import dplyr, ggplot2
#' @examples missing

difs_dayAvg = function(
        data_observed,
        data_reference,
        topoFactor,
        plotting = FALSE,
        ts_start = NA,
        ts_end = NA

){
    # from averaged truth values (within certain time period),
    # trying to predict non-averaged truth values
    data_observed = gravity_dif
    data_reference = ET_hourly
    #
    ## create daily data, averaged in desired time period
    data_dayAvg = data_observed %>%
        dplyr::mutate(datetime = as.POSIXct(format(datetime, "%Y-%m-%d"))) %>%
        # subtract one day
        # this is to match correct time period of ET signal
        dplyr::mutate(datetime = datetime - 3600*24) %>%
        dplyr::group_by(datetime) %>%
        dplyr::summarize(observed = mean(value, na.rm = T))
    #
    ## create difference of each average
    # at a daily basis
    observed_difsDaily = data_dayAvg
    observed_difsDaily$observed = c(0, c(dplyr::lead(observed_difsDaily$observed, 1) - observed_difsDaily$observed)[-length(observed_difsDaily$observed)])
    ## convert gravity signal time series from nm/s² to mm
    observed_difsDaily$observed = -1 * observed_difsDaily$observed / topoFactor
    #
    ## resample actual time series (reference input) 
    # to same time resolution
    ET_actual_sumsDaily = data_reference %>%
        dplyr::mutate(datetime = as.POSIXct(format(datetime, format = "%Y-%m-%d"))) %>%
        dplyr::group_by(datetime) %>%
        dplyr::summarize(actual = sum(value, na.rm = T))
    #
    ## join observed and actual (reference) time series
    both_timeseries = ET_actual_sumsDaily[-length(ET_actual_sumsDaily$datetime),] %>%
        dplyr::left_join(observed_difsDaily[-1,]) %>%
        # adjust time period
        # dplyr::filter(datetime >= ts_start) %>%
        # dplyr::filter(datetime < ts_end) %>%
        dplyr::select(datetime, observed, actual)
    #
    ## plotting, if desired
    if(plotting){
    method_results_plot = both_timeseries %>%
        melt(id = "datetime", variable.name = "Source") %>%
        ggplot(aes(x = datetime, y = value, colour = Source)) + 
        geom_line() + geom_point() + 
        ylab("Gravity [nm/2²]") + xlab("") + 
        theme(legend.position = "bottom")
    print(method_results_plot)
    }
    ## return both time series: simulated and 
    return(both_timeseries)
}

