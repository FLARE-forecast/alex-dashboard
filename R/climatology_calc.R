
climatology_calc <- function(obs_df, day_of_interest, clim_var){
  
  current_df <- obs_df |> 
    filter(datetime == day_of_interest,
           variable == clim_var) |> 
    mutate(doy = lubridate::yday(as.Date(datetime)))
  
  clim_avg <- obs_df |> 
    mutate(doy = lubridate::yday(as.Date(datetime))) |> 
    filter(datetime < day_of_interest, 
           variable == clim_var, 
           doy == current_df$doy) |> 
    summarize(obs_avg = mean(observation, na.rm = TRUE),
              obs_95 = quantile(observation, 0.95, na.rm = T),
              obs_05 = quantile(observation, 0.05), na.rm = T)
  
  if ((current_df$observation <= clim_avg$obs_95) & (current_df$observation >= clim_avg$obs_05)){
    arrow_icon <- 'arrow-bar-right'
    clim_text <- 'Similar'
  } else if (current_df$observation > clim_avg$obs_95){
    arrow_icon <- 'arrow-bar-up'
    clim_text <- 'Higher'
  } else if (current_df$observation < clim_avg$obs_05){
    arrow_icon <- 'arrow-bar-down'
    clim_text <- 'Lower'
  }

  return(list(round(clim_avg$obs_avg,1), arrow_icon, clim_text))
}
