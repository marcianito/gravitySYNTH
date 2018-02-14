#' @title RMSE of simulated and observed vectors
#'
#' @description test
#'
#' @param test test
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

rmse = function(
    ts_sim,
    ts_obs
){
  ts_df = data.frame(ts_sim,ts_obs)
  ts_df$sqr_dif = (ts_df$ts_sim - ts_df$ts_obs)^2
  rmse_val = sqrt(sum(ts_df$sqr_dif) / length(ts_sim))
  return(rmse_val)
}

