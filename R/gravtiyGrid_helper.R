#' @title removes all DEM points outside of a certain radius
#'
#' @description 
#'
#' @param grid_input grid from where to limit gcomp to a certain radius. Has to be data.frame with columns (x,y,z).
#' @param gloc location of gravity sensor (x,y,z).
#' @param max_rad radius of exclusion in [meters].
#' @details missing
#' @references Marvin Reich (2014), mreich@@gfz-potsdam.de
#' @examples example MISSING
#' @export

remove_outsideRadius = function(grid_input, gloc, max_rad){
	# limit maximum radius of gravity grid
	# if max_rad = NA, no limitation will be realized
	grid_output = dplyr::mutate(grid_input, distance_rad = sqrt((x-gloc$x)^2+(y-gloc$y)^2+(z-gloc$z)^2)) %>%
			dplyr::mutate(excluse = ifelse(distance_rad > max_rad, TRUE, FALSE)) %>%
			# delete all rows with exclude == TRUE
			dplyr::filter(excluse == FALSE) %>%
			dplyr::select(x,y,z,Depth,layer)
# return result
return(grid_output)
}

#' @title removes all DEM points inside of a certain radius
#'
#' @description 
#'
#' @param grid_input grid from where to limit gcomp to a certain radius. Has to be data.frame with columns (x,y,z).
#' @param gloc location of gravity sensor (x,y,z).
#' @param max_rad radius of exclusion in [meters].
#' @details missing
#' @references Marvin Reich (2014), mreich@@gfz-potsdam.de
#' @examples example MISSING
#' @export

remove_insideRadius = function(grid_input, gloc, max_rad){
	# limit maximum radius of gravity grid
	# if max_rad = NA, no limitation will be realized
	grid_output = dplyr::mutate(grid_input, distance_rad = sqrt((x-gloc$x)^2+(y-gloc$y)^2+(z-gloc$z)^2)) %>%
			dplyr::mutate(excluse = ifelse(distance_rad > max_rad, TRUE, FALSE)) %>%
			# delete all rows with exclude == TRUE
			dplyr::filter(excluse == TRUE) %>%
			dplyr::select(x,y,z,Depth,layer)
# return result
return(grid_output)
}


#' @title removes all DEM points inside a certain area
#'
#' @description 
#'
#' @param grid_input grid from where to limit gcomp to a certain radius. Has to be data.frame with columns (x,y,z).
#' @param gloc location of gravity sensor (x,y,z).
#' @param max_rad radius of exclusion in [meters].
#' @details missing
#' @references Marvin Reich (2014), mreich@@gfz-potsdam.de
#' @examples example MISSING
#' @export

remove_insideArea = function(grid_input, area_poly){
	#library(SDMTools)
	##area_poly = data.frame(x=rem_area[1:2],y=rem_area[3:4])
	#points_in=data.frame(x=grid_input$x, y=grid_input$y)
	#grid_pips = pnt.in.poly(points_in, area_poly)
	##grid_pips = pnt.in.poly(grid_input[,1:2], area_poly)
	#grid_output = dplyr::mutate(grid_pips, z=grid_input$z) %>%
		      #dplyr::filter(pip == 0) %>%
		      #dplyr::select(x,y,z)
	
	grid_output = dplyr::mutate(grid_input, pip = ifelse(
					x > area_poly$x[1] &
					x < area_poly$x[2] &
					y > area_poly$y[3] &
					y < area_poly$y[1]  
					,1,0)) %>%
		      dplyr::filter(pip == 0) %>%
		      dplyr::select(x,y,z,Depth,layer)
return(grid_output)
}
