#' @title Compare differences of value at midnight to daily diferences from actual time series 
#'
#' @description observed data input: hourly (cumsums), actual / reference input: daily (difs)
#'
#' @param test test
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import dplyr, ggplot2
#' @examples missing

extract_ETavgs = function(
        data_observed,
        data_reference,
        topoFactor,
        avg_start,
        avg_end,
        plotting = FALSE,
        ts_start = NA,
        ts_end = NA

){
    ## DEBUGGING
    # data_observed = ET_observed
    # data_reference = ET_actual
    # avg_start = 0
    # avg_end = 0
    # topoFactor = TopoFactor
    ##
    #
    ## create daily data, averaged in desired time period
    # but in original unit: [nm/s²]
    data_valAvg = data_observed %>%
        dplyr::mutate(hour = as.numeric(format(datetime, "%H"))) %>%
        # dplyr::filter(hour == 0) %>%
        dplyr::filter(hour >= avg_start & hour <= avg_end) %>%
        dplyr::mutate(datetime = as.POSIXct(format(datetime, "%Y-%m-%d"))) %>%
        # group and average
        # average function:mean
        # this reasons in cummulative gravity units [nm/s²]
        dplyr::group_by(datetime) %>%
        dplyr::summarize(observed = mean(value, na.rm = T))
    #
    ## create difference of each average
    # at a daily basis
    observed_difsDaily = data_valAvg
    observed_difsDaily$observed = c(dplyr::lead(observed_difsDaily$observed, 1) - observed_difsDaily$observed)
    ## convert gravity signal time series from nm/s² to mm
    observed_difsDaily$observed = -1 * observed_difsDaily$observed / topoFactor
    #
    ## resample actual time series (reference input) 
    # to daily resolution
    # but in original unit: [mm]
    ET_actual_sumsDaily = aggTS(data_reference, "daily", "sum")
    colnames(ET_actual_sumsDaily)[2] = "actual"
    # # to same time resolution
    # ET_actual_sumsDaily = data_reference %>%
    #     dplyr::mutate(datetime = as.POSIXct(format(datetime, format = "%Y-%m-%d"))) %>%
    #     dplyr::group_by(datetime) %>%
    #     dplyr::summarize(actual = sum(value, na.rm = T))
    #
    ## join observed and actual (reference) time series
    both_timeseries = ET_actual_sumsDaily[-1,] %>%
        dplyr::left_join(observed_difsDaily) %>%
        # adjust time period
        # dplyr::filter(datetime >= ts_start) %>%
        # dplyr::filter(datetime < ts_end) %>%
        dplyr::select(datetime, observed, actual)
    # cut last observation,
    # as it is NA due to difference forming (leads - observed); above
    both_timeseries = both_timeseries[-dim(both_timeseries)[1],]
    #
    ## plotting, if desired
    if(plotting){
        # time series preparation
        difs = both_timeseries %>%
            dplyr::mutate(difference = actual - observed) %>%
            dplyr::mutate(Type = "Difference") %>%
            dplyr::select(datetime, difference, Type) %>%
            melt(id = c("datetime", "Type"), variable.name = "Source")
        ts = both_timeseries %>%
            dplyr::mutate(Type = "Time series") %>%
            melt(id = c("datetime", "Type"), variable.name = "Source")
        ts_plot = rbind(ts, difs)
        # plotting
        method_results_plot = ts_plot %>%
        ggplot(aes(x = datetime, y = value, colour = Source)) + 
        geom_line() + geom_point() + 
        facet_grid(Type ~ ., scale = "free_y") +
        ylab("Gravity [nm/2²]") + xlab("") + 
        theme(legend.position = "bottom")
    print(method_results_plot)
    }
    ## return both time series: simulated and 
    return(both_timeseries)
}

