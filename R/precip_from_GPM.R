#' @title Precipitation data (30 minutes) from GPM (NASA)
#'
#' @description test
#'
#' @param test test
#' 
#' @return test
#' 
#' @details Datasets are provided by: 
#' https://www.esrl.noaa.gov/psd/data/gridded/data.cpc.globalprecip.html
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import ncdf4, ggplot2, tibbletime
#' @examples missing

precip_from_GPM = function(
    input_dir,
    filenames,
    var_name = NA,
    lon,
    lat,
    output_cumsum = F,
    plotting = T
){
    ## DEBUGGING
    # input_dir = dir_input
    # input_dir = paste0(dir_input, "test/")
    # filenames = file_name
    # lon = getStationParam(Site = site, Param = "Longitude")
    # lat = getStationParam(Site = site, Param = "Latitude")
    # lon = -58.2
    # lat = -34.8
    # i = 3
    # run loop over all input files
    # and stich output
    precip_data_siteXY = data.frame()
    for(i in 1:length(filenames)){
    # open connectin to file
    file_nc = ncdf4::nc_open(paste0(input_dir, filenames[i]))
    # read variables (data) of netcdf-file
    # print(file_nc)
    if(is.na(var_name)){
        precip_data = ncdf4::ncvar_get(file_nc, "HQprecipitation")
    }else{
        precip_data = ncdf4::ncvar_get(file_nc, var_name)
    }
    lon_range = ncdf4::ncvar_get(file_nc, "lon")
    lat_range = ncdf4::ncvar_get(file_nc, "lat")
       
    # time conversion
    # get time information from file name
    fileName = filenames[i]
    datetime_1 = unlist(strsplit(fileName, "IMERG."))
    datetime_data = unlist(strsplit(datetime_1, "-E.[0-9]"))[2]
    # # adding 30 minutes in order to get END time of observation as recorded time stamps
    # datetime_i = as.POSIXct(datetime_data, format = "%Y%m%d-S%H%M%S") + 30*60
    datetime_i = as.POSIXct(datetime_data, format = "%Y%m%d-S%H%M%S")
    # close connection to file
    ncdf4::nc_close(file_nc)
    ## get time series from location X, Y
    # library(rgdal)
    # loc_utm = SpatialPoints(randompoints, proj4string=CRS("+proj=utm +zone=32 +datum=WGS84")  
    # loc_degree = spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
    
    # loc_x = 12.884216
    # loc_y = 49.152940
    # loc_x_index = which(lon_range == loc_x)
    # loc_y_index = which(lat_range == loc_y)
    
    # find closest location match
    lon_range_dif = lon_range - lon
    lat_range_dif = lat_range - lat
    
    lon_index = which(abs(lon_range_dif) == min(abs(lon_range_dif)))
    lat_index = which(abs(lat_range_dif) == min(abs(lat_range_dif)))
    
    precip_data_siteXY_temp = data.frame(
                datetime = datetime_i,
                # lon = lon,
                # lat = lat,
                value = precip_data[lon_index, lat_index]
                )
    # combine datasets over different files
    precip_data_siteXY = rbind(precip_data_siteXY, precip_data_siteXY_temp)
    }
    ## aggregate data to get hourly values
    # from 30 minutes to hourly
    # using tibbletime !?
    # precip_data_siteXY$value = c(1,2,2,3,10,1,2)
    precip_data_siteXY =  as_tbl_time(precip_data_siteXY, datetime) %>%
        tibbletime::time_summarise("hour",
                                   value = sum(value, na.rm = T)
                                   # start_date = trunc(precip_data_siteXY$datetime[1], "hour") 
                                   )
    # precip_data_siteXYttt =  precip_data_siteXY %>%
    #     dplyr::mutate(datet= as.POSIXct(format(datetime, "%Y-%m-%d %H"))) %>%
    #     dplyr::group_by(datet) %>%
    #     dplyr::summarize(val = sum(value, na.rm = T))
    # adding 30 minutes in order to get END time of observation as recorded time stamps
    precip_data_siteXY$datetime = precip_data_siteXY$datetime + 30 * 60
    # add geo-coordinates
    precip_data_siteXY$lon = lon
    precip_data_siteXY$lat = lat
    ## output either as actual value or
    # cumulative sum
    if(output_cumsum){
        # check if NAs exist and set them to 0 (no precipitation)
        # otherwise there is a problem with the cumsum-function
        precip_data_siteXY = precip_data_siteXY %>%
            dplyr::mutate(value = ifelse(is.na(value), 0, value))
        # calculate cummulative sum
        precip_data_siteXY$value = cumsum(precip_data_siteXY$value)
    }
    # plot if desired
    if(plotting){
        print(
          ggplot(precip_data_siteXY, aes(x = datetime, y = value)) + geom_line()
          )
    }
    # return datset
    return(precip_data_siteXY)
}

