read.stations <- function(){
  tibble(station=c("lamao","nch","sph"),
                latitude=c(14.514086,14.6205,10.7018),
                longitude=c(120.609287,121.0209,122.5669))
}

read.measurements <- function(){

    d.lamao <- readxl::read_xlsx(file.path("data","LAMAO reading from Feb04-Aug19 2020.xlsx"))
    d.nch <- readxl::read_xlsx(file.path("data","NCH reading from Jan25-May29 2020.xlsx"))
    d.sph <- readxl::read_xlsx(file.path("data","SPH Iloilo reading from Jan25-Aug19 2020.xlsx"))

    d.wide <- rbind(d.lamao %>% mutate(station="lamao"),
               d.nch %>% mutate(station="nch"),
               d.sph %>% mutate(station="sph")
    )

    d <- d.wide %>% rename(timezone=Timezone,
                 date=Datetime,
                 aqi.us=`AQI US`,
                 aqi.cn=`AQI CN`,
                 pm25=`PM2.5 (ug/m3)`,
                 pm10=`PM10 (ug/m3)`,
                 co2=`CO2 (ppm)`,
                 temp_c="Temperature (Celsius)",
                 temp_f="Temperature (Fahrenheit)",
                 hum_pct="Humidity (%)",
                 pm25.outdoor="Outdoor PM2.5 (ug/m3)",
                 hcho="HCHO (ppb)",
                 tvoc="TVOC (ppb)"
                 ) %>%
      tidyr::pivot_longer(cols=!c(timezone, date, station), names_to="indicator", values_to="value")
}


build_trajectories <- function(lat, lon, height, duration, date_from, date_to, met_type, station,
                               direction="backward",
                               use_cache=T,
                               save_to_cache=T){

  filepath <- file.path(dir_results, paste0("trajs_raw_", met_type,"_",station,".RDS"))
  dates <- seq(lubridate::date(date_from), lubridate::date(date_to), by="day")

  # Check cache exists and contains dates
  if(use_cache & file.exists(filepath)){
    trajs <- readRDS(filepath) %>% filter(direction==direction)
    enough <-
      (min(lubridate::date(trajs$traj_dt_i), na.rm=T) <= date_from) &
      (max(lubridate::date(trajs$traj_dt_i), na.rm=T) >= date_to) &
      all(trajs$lat_i==lat) &
      all(trajs$lon_i==lon)

    if(enough) return(trajs)
  }

  # Parallelize trajectory calculations
  dates_split <- split(dates, ceiling(seq_along(dates)/30))
  trajs_at_dates <- function(dates){
    tryCatch({
      hysplit_trajectory(
        lon = lon,
        lat = lat,
        height = height,
        duration = duration,
        days = dates,
        daily_hours = c(0, 6, 12, 18),
        direction = direction,
        met_type = met_type,
        extended_met = F,
        met_dir = dir_hysplit_met,
        exec_dir = dir_hysplit_output,
        clean_up = F
      )
    },
    error=function(c){
      return(NA)
    })
  }

  trajs <- do.call('rbind',
                 pbmclapply(dates_split, trajs_at_dates, mc.cores=detectCores()-1))

  # Update fields to be compatible with OpenAIR
  trajs$hour.inc <- trajs$hour_along
  trajs$date <- trajs$traj_dt_i
  trajs$date2 <- trajs$traj_dt
  trajs$year <- lubridate::year(trajs$traj_dt_i)
  trajs$month <- lubridate::month(trajs$traj_dt_i)
  trajs$day <- lubridate::date(trajs$traj_dt_i)
  trajs$direction <- direction

  if(save_to_cache){
    saveRDS(trajs, filepath)
  }

  return(trajs)
}

