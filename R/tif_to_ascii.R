#' @title tif_to_ascii: converts geoTiff fromat raster data into arc ascii format
#'
#' @description test
#'
#' @param test test
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @examples missing

tif_to_ascii = function(
            file_name_in,
            file_name_out,
            input_dir,
            output_dir
){
    #get name of file
    # file_name = 
    # 
    DEM_input = raster::raster(paste0(input_dir, file_name_in, ".tif"))
    # raster::writeRaster(DEM_input, filename = paste0(output_dir, file_name_out, "_DEM.asc"), format = "ascii") #, datatype="INT4S")
    raster::writeRaster(DEM_input, filename = paste0(output_dir, file_name_in, "/", file_name_out, ".asc"), format = "ascii") #, datatype="INT4S")
    return(NULL)
}
