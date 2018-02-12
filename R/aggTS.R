#' @title Aggregate data.frame time series to other time periods
#'
#' @description test
#'
#' @param timeseries_data data.frame, formatted with columns [datetime, values].
#' @param newPeriod charater string, options are "hourly", "daily", "weekly".
#' @param fun character string, indicating aggregation function.
#' @param time_offset integer, indicating the offset to include in the aggregation.
#' Units are with respect to declared new period.
#' 
#' @return test
#' 
#' @details Convention for setting dates corresponding to new period: POSIXct is
#' always used from the LAST hour, day, etc. of the aggregation period.
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import dplyr
#' @examples missing

aggTS = function(
    timeseries_data,
    newPeriod,
    fun = "sum",
    time_offset = 0,
    conserve_columns = NA
){
    ## DEBUGGING
    # timeseries_data = as.data.frame(ET_hourly)
    # timeseries_data = as.data.frame(data_radar_mod)
    # newPeriod = "daily"
    # fun = "sum"
    # time_offset = 0
    # conserve_columns = "cell_index"
    ##
    ## force input to be a data.frame
    timeseries_data = as.data.frame(timeseries_data)
    # construct list of columns to conserve during summarizing
    if(!is.na(conserve_columns)){
    columns = as.list(c("datetime", conserve_columns))
    }
    # format after new period
    # and include offset
    switch(newPeriod,
           daily = {
        ts_newPeriod = timeseries_data %>%
        dplyr::mutate(datetime = as.POSIXct(trunc(datetime, "days"))) %>%
        dplyr::mutate(datetime = datetime - 3600 * 24 * time_offset)
           },
           hourly = {
        ts_newPeriod = timeseries_data %>%
        dplyr::mutate(datetime = as.POSIXct(trunc(datetime, "hours"))) %>%
        dplyr::mutate(datetime = datetime - 3600 * time_offset)
           },
           weekly = {
               ## still not sure how to do this !! ?
               ## now, grouping should be over "datetime_weeks"
        # week needed for correct time setting:
        # convention: end of week
        week = 3600 * 24 * 7
        ts_newPeriod = timeseries_data %>%
        dplyr::mutate(datetime_weeks = paste0(format(datetime, "%Y-%W"), "-0")) %>%
        dplyr::mutate(datetime = as.POSIXct(strptime(datetime_weeks, "%Y-%W-%w")) + week) %>%
        dplyr::mutate(datetime = datetime - 3600 * 24 * 7 * time_offset) %>%
        dplyr::select(-datetime_weeks)
           }
           )
    #
    # format after aggregation function
    switch(fun,
           sum = {
        ts_newPeriod = ts_newPeriod %>%
        # dplyr::group_by(datetime) %>%
        dplyr::group_by_(.dots = columns) %>%
        # dplyr::summarize(observed = sum(value, na.rm = T))
        dplyr::summarize(value = sum(value, na.rm = T))
           },
           mean = {
        ts_newPeriod = ts_newPeriod %>%
        # dplyr::group_by(datetime) %>%
        dplyr::group_by_(.dots = columns) %>%
        # dplyr::summarize(observed = mean(value, na.rm = T))
        dplyr::summarize(value = mean(value, na.rm = T))
           }
           )
    #
    # convert time pack to POSIXct
    ts_newPeriod$datetime = as.POSIXct(ts_newPeriod$datetime)
    # return new time series data
    return(ts_newPeriod)
}

