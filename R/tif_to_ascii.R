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
            file_name,
            input_dir,
            output_dir
){
    #get name of file
    # file_name = 
    # 
    DEM_input = raster::raster(paste0(input_dir, file_name, ".tif"))
    raster::writeRaster(DEM_input, filename = paste0(output_dir, file_name, ".asc"), format = "ascii") #, datatype="INT4S")
    return(NULL)
}
