#' @title Precipitation data (daily) from ESRL (NOAA)
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
#' @import ncdf4, ggplot2
#' @examples missing

precip_from_esrl = function(
    input_dir,
    filenames,
    lon,
    lat,
    plotting = T
){
    ## DEBUGGING
    # input_dir = dir_input
    # filenames = file_name
    # lon = getStationParam(Site = site, Param = "Longitude")
    # lat = getStationParam(Site = site, Param = "Latitude")
    # i = 1
    # run loop over all input files
    # and stich output
    precip_data_siteXY = data.frame()
    for(i in 1:length(filenames)){
    # open connectin to file
    file_nc = ncdf4::nc_open(paste0(input_dir, filenames[i]))
    # read variables (data) of netcdf-file
    # print(file_nc)
    # precip_attr = ncdf4::ncatt_get(file_nc, "precip")
    precip_data = ncdf4::ncvar_get(file_nc, "precip")
    time_data = ncdf4::ncvar_get(file_nc, "time")
    lon_range = ncdf4::ncvar_get(file_nc, "lon")
    lat_range = ncdf4::ncvar_get(file_nc, "lat")
    
    # time conversion
    time_units = ncatt_get(file_nc, "time", attname="units")
    time_resolution = unlist(strsplit(time_units$value, " "))[1]
    time_origin = unlist(strsplit(time_units$value, " "))[3]
    # convert "time_data" to seconds 
    switch(time_resolution,
           "days" = {time_data = time_data * 3600 * 24},
           "hours" = {time_data = time_data * 3600}
           )
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
                datetime = as.POSIXct(time_data, origin = time_origin),
                lon = lon,
                lat = lat,
                value = precip_data[lon_index, lat_index,]
                )
    # combine datasets over different files
    precip_data_siteXY = rbind(precip_data_siteXY, precip_data_siteXY_temp)
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

