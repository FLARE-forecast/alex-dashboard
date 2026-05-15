## function code 

dashboard_plotting_tool <- function(data, historic_data, depths = 0.5, tzone = "America/New_York", ylims = c(-5,35), site_name = "", obs_hist, historical_horizon, forecast_horizon_confidence, title_date){
  
  data_var <- unique(data$variable)
  num_depths <- length(unique(data$depth))
  
  if(data_var == 'temperature'){
    var_title = 'Lake Water Temperature'
    var_unit = 'Temperature (°C)'
    label_height_adjust <- 1
  } else if(data_var == 'salt'){
    var_title = 'Lake Salinity'
    var_unit = 'Salinity (ppt)'
    label_height_adjust <- 0.01
  } else if(data_var == 'depth'){
    var_title = 'Lake Level'
    var_unit = 'Lake level (m, above AHD)' 
    label_height_adjust <- 0.01
  } else{
    var_title = 'Water Quality Variable'
    var_unit = 'Variable Unit'
    label_height_adjust <- 1
  }
  
  # Fix dates and rename columns to match plotting code
  curr_tibble <- data |>
    dplyr::filter(depth %in% depths) |>
    dplyr::mutate(datetime = lubridate::with_tz(lubridate::as_datetime(datetime), tzone),
                  reference_datetime = lubridate::with_tz(lubridate::as_datetime(reference_datetime), tzone), 
                  date = as.Date(datetime)) |>#,
    dplyr::filter(datetime >= reference_datetime) |>
    rename(forecast_mean = mean, forecast_sd = sd, forecast_upper_90 = quantile90, forecast_lower_90 = quantile10,
           forecast_start_day = reference_datetime)
  
  historic_tibble <- historic_data |>
    dplyr::filter(depth %in% depths) |>
    dplyr::mutate(datetime = lubridate::with_tz(lubridate::as_datetime(datetime), tzone),
                  reference_datetime = lubridate::with_tz(lubridate::as_datetime(reference_datetime), tzone), 
                  date = as.Date(datetime),
                  sd = NA, 
                  quantile90= NA, 
                  quantile10 = NA) |>#,
    #dplyr::filter(datetime >= reference_datetime) |>
    rename(forecast_mean = mean, forecast_sd = sd, forecast_upper_90 = quantile90, forecast_lower_90 = quantile10,
           forecast_start_day = reference_datetime)
  
  
  combined_tibble <- dplyr::bind_rows(curr_tibble, historic_tibble) |> 
    arrange(datetime, observation) |> 
    distinct(date, .keep_all = TRUE)
  
  priority_date_cutoff <- lubridate::with_tz(as.Date(most_recent),tzone) + lubridate::days(forecast_horizon_confidence) ## how many days into the forecast do we think we are confident? ALEX we had said 10
  
  primary_forecast_dates <- combined_tibble |> 
    #filter(date >= as.Date(most_recent)) |> 
    mutate(date_fill = dplyr::if_else((date <= as.Date(priority_date_cutoff) & date >= as.Date(most_recent)), date, NA)) |> 
    pull(date_fill)
  
  secondary_forecast_dates <- combined_tibble |> 
    mutate(date_fill = dplyr::if_else(date >= as.Date(priority_date_cutoff), date, NA)) |> 
    pull(date_fill)
  
  combined_tibble$primary_dates <- primary_forecast_dates
  combined_tibble$secondary_dates <- secondary_forecast_dates
  
  
  # ## identify climatology values for time period using historical observations
  # interest_days_doy <- combined_tibble |> 
  #   mutate(doy = lubridate::yday(date)) |> 
  #   pull(doy)
  # 
  # obs_climatology <- obs_hist |>
  #   mutate(datetime = lubridate::force_tz(datetime, tzone = "Australia/Adelaide")) |>
  #   mutate(doy = lubridate::yday(datetime)) |>
  #   filter(doy %in% interest_days_doy) |>
  #   # mutate(climatology_average = mean(observation, na.rm = TRUE)) |>
  #   # select(doy, climatology_average)
  #   group_by(doy) |>
  #   summarize(climatology_average = mean(observation, na.rm = TRUE)) |>
  #   ungroup()
  # 
  # 
  # combined_tibble <- combined_tibble |>
  #   mutate(doy = lubridate::yday(date)) |>
  #   right_join(obs_climatology, by = c('doy'))
  
  obs_hist_full <- obs_hist
  
  if (as.Date(most_recent) - as.Date(min(combined_tibble$date)) < 30){
    xlims <- c(as.Date(min(combined_tibble$date)) - 30 , (as.Date(max(combined_tibble$date)) + lubridate::days(5)))
    obs_hist <- obs_hist |> 
      filter(datetime > min(combined_tibble$datetime) - lubridate::days(30)) |> 
      select(-any_of(c('site_id')))
    
  } else {
    xlims <- c(as.Date(min(combined_tibble$date)), (as.Date(max(combined_tibble$date)) + lubridate::days(5)))
    obs_hist <- obs_hist |> 
      filter(datetime > min(combined_tibble$datetime)) |> 
      select(-any_of(c('site_id')))
    
  }
  
  obs_hist <- obs_hist |> 
    rename(observation_targets = observation) #|> 
    #select(datetime, observation_targets)
  
  combined_tibble <- combined_tibble |>
    full_join(obs_hist, by = c('datetime', 'variable', 'depth')) |>
    mutate(date = as.Date(datetime)) |> 
    mutate(observation = ifelse(is.na(observation), observation_targets, observation), # fill in any missing data from scores with targets
          date = as.Date(datetime))
  
  
  ## identify climatology values for time period using historical observations
  interest_days_doy <- combined_tibble |> 
    mutate(doy = lubridate::yday(date)) |> 
    pull(doy)
  
  obs_climatology <- obs_hist_full |>
    filter(datetime < min(combined_tibble$datetime)) |> 
    mutate(datetime = lubridate::force_tz(datetime, tzone = tzone)) |>
    mutate(doy = lubridate::yday(datetime)) |>
    filter(doy %in% interest_days_doy) |>
    # mutate(`historical mean` = mean(observation, na.rm = TRUE)) |>
    # select(doy, `historical mean`)
    group_by(doy) |>
    summarize(`historical mean` = mean(observation, na.rm = TRUE)) |>
    ungroup()
  
  
  combined_tibble <- combined_tibble |>
    mutate(doy = lubridate::yday(date)) |>
    right_join(obs_climatology, by = c('doy')) |> 
    mutate(forecast_mean_historical = ifelse(date <= Sys.Date(), forecast_mean, NA),
           forecast_mean = ifelse(date < Sys.Date(),NA,forecast_mean),
           date = as.Date(datetime))   # split historical and future forecasts into two separate columns
  
  if (num_depths > 1){
    p <- ggplot2::ggplot(combined_tibble, ggplot2::aes(x = as.Date(date))) +
      ggplot2::ylim(ylims) +
      ggplot2::xlim(xlims) +
      ggplot2::geom_line(ggplot2::aes(y = forecast_mean, color = as.factor(depth)), size = 0.5) +
      ggplot2::geom_ribbon(ggplot2::aes(x = primary_dates, ymin = forecast_lower_90, ymax = forecast_upper_90,
                                        fill = as.factor(depth)),
                           alpha = 0.3) +
      ggplot2::geom_ribbon(ggplot2::aes(x = secondary_dates, ymin = forecast_lower_90, ymax = forecast_upper_90,
                                        fill = as.factor(depth)),
                           alpha = 0.1) +      
      ggplot2::geom_line(ggplot2::aes(y = `historical mean`, color = as.factor(depth)), size = 0.5, linetype = 'dashed') +
      ggplot2::geom_point(data = obs_hist, ggplot2::aes(x=as.Date(datetime),y = observation, color = as.factor(depth)), size = 2) +
      ggplot2::geom_vline(aes(xintercept = as.Date(most_recent),
                              linetype = "solid"),
                          alpha = 1) +
      ggplot2::annotate(x = as.Date(most_recent - 96*60*60), y = max(ylims) - label_height_adjust, label = 'Past', geom = 'text') +
      ggplot2::annotate(x = as.Date(most_recent + 96*60*80), y = max(ylims) - label_height_adjust, label = 'Future', geom = 'text') +
      ggplot2::theme_light() +
      ggplot2::scale_fill_manual(name = "Depth (m)",
                                 values = c("#D55E00", '#009E73', '#0072B2'),
                                 labels = as.character(depths)) +
      ggplot2::scale_color_manual(name = "Depth (m)",
                                  values = c("#D55E00", '#009E73', '#0072B2'),
                                  labels = as.character(depths)) +
      ggplot2::scale_linetype_manual(name = "",
                                     values = c('solid'),
                                     labels = c('Forecast Date')) +
      ggplot2::scale_y_continuous(name = var_unit,
                                  limits = ylims) +
      ggplot2::labs(x = "Date",
                    y = var_unit,
                    fill = 'Depth (m)',
                    color = 'Depth',
                    title = paste0(var_title," Forecast, ", lubridate::date(title_date)), '(30-days ahead)') +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                     plot.title = element_text(hjust = 0.5))
    
  } else if (num_depths == 1){
    message('using one depth...')
    
    p <- ggplot2::ggplot(combined_tibble, ggplot2::aes(x = as.Date(date))) +
      ggplot2::ylim(ylims) +
      ggplot2::xlim(xlims) +
      #ggplot2::geom_ribbon(ggplot2::aes(x = primary_dates, ymin = forecast_lower_90, ymax = forecast_upper_90), color = 'lightblue', fill = 'lightblue') +
      ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = forecast_lower_90, ymax = forecast_upper_90), color = 'black', fill = 'grey') +
      ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = down_q10, ymax = down_q90), color = 'indianred', fill = 'indianred', alpha=0.1) +
      ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = up_q10, ymax = up_q90), color = 'palegreen4', fill = 'palegreen4', alpha=0.1) +
      ##ggplot2::geom_line(ggplot2::aes(y = `historical mean`), color = 'darkslategrey', size = 0.5, linetype = 'longdash') +
      ggplot2::geom_line(ggplot2::aes(y = forecast_mean_historical, color = 'Historical One-Day-\nAhead Predictions'), size = 0.5, linetype = 'solid') +
      ggplot2::geom_line(ggplot2::aes(y = `historical mean`, color = 'Historical Average'), size = 0.5, linetype = 'longdash', size = 0.5) +
      ggplot2::geom_point(ggplot2::aes(y = observation), color = 'red') +
      ggplot2::geom_vline(aes(xintercept = as.Date(lubridate::as_datetime(most_recent), tzone)), alpha = 1, linetype = "solid") +
      #ggplot2::geom_line(ggplot2::aes(y = forecast_mean), color = 'black')+
      ggplot2::geom_line(ggplot2::aes(y = down_median, color = 'More Barrage Gates Open'))+
      ggplot2::geom_line(ggplot2::aes(y = up_median, color = 'More Barrage Gates Closed'))+
      ggplot2::geom_line(ggplot2::aes(y = forecast_mean, color = 'Future Predictions With No Change To Barrage Gates'))+
      ggplot2::annotate(x = as.Date(most_recent - 96*60*60), y = max(ylims) - label_height_adjust, label = 'Past', geom = 'text') +
      ggplot2::annotate(x = as.Date(most_recent + 96*60*80), y = max(ylims) - label_height_adjust, label = 'Future', geom = 'text') +
      ggplot2::theme_light() +
      ggplot2::scale_linetype_manual(name = "",
                                     values = c('solid'),
                                     labels = c('Forecast Date')) +
      ggplot2::scale_y_continuous(name = var_unit,
                                  limits = ylims) +
      ggplot2::labs(x = "Date",
                    y = var_unit,
                    title = paste0(var_title," Forecast, ", lubridate::date(title_date), ' (30-days ahead)')) +
      # scale_colour_manual("", 
      #                     values = c("forecast_mean"="black", `historical mean` ="darkslategrey")) +
      scale_color_manual("", values = c("Future Predictions With No Change To Barrage Gates"="black", 
                                        "More Barrage Gates Open"="indianred",
                                        "More Barrage Gates Closed"="palegreen4",
                                        "Historical One-Day-\nAhead Predictions" ="slategrey", 
                                        "Historical Average" = "royalblue4")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                     plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(limits=c('Future Predictions With No Change To Barrage Gates', 
                                 'More Barrage Gates Opened', 
                                 "More Barrage Gates Closed",
                                 "Historical One-Day-\nAhead Predictions",
                                 "Historical Average"))
  }
  
  return(p)
}
