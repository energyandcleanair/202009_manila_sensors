read.stations <- function(with_powerplants){
  tibble(station=c("lamao","nch","sph","sph.powerplant","lamao.powerplant"),
         station_name=c("Lamao","National Children's Hospital","St. Paul’s Hospital","sph.powerplant","lamao.powerplant"),
         latitude=c(14.514086,14.6205,10.7018,10.72444,14.5204064),
         longitude=c(120.609287,121.0209,122.5669,122.59582,120.6026809),
         direction=c("backward","backward","backward","forward","forward"),
         type=c("sensor","sensor","sensor","powerplant","powerplant")
  )
}

read.measurements <- function(stations){

    d.lamao <- readxl::read_xlsx("data/LAMAO AQ - 02 04, 2020 - 12 22, 2020.xlsx")
    d.nch <- readxl::read_xlsx("data/NCH AQ 05 31, 2020.xlsx")
    d.sph <- readxl::read_xlsx("data/SPHI AQ - 03 12 - 04 12, 2021.xlsx")

    d.wide <- rbind(d.lamao %>% mutate(station="lamao"),
               d.nch %>% mutate(station="nch"),
               d.sph %>% mutate(station="sph")
    )

    timestamp_to_date <- function(t){
      t <- as.numeric(t)
      # Some are expressed in days since 1900
      days_since_1900 <- t<50000
      days_offset <- as.integer(lubridate::date("1970-01-01") - lubridate::date("1900-01-01"), unit="days")
      t[days_since_1900] <- (t[days_since_1900]-days_offset)*24*3600

      as.POSIXct(t, origin="1970-01-01", tz="Asia/Manila")
    }

    d <- d.wide %>% rename(timezone=Timezone,
                 date_str=Datetime,
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
      tidyr::pivot_longer(cols=!c(timezone, date_str, station), names_to="indicator", values_to="value") %>%
      left_join(stations) %>%
      mutate(date=lubridate::parse_date_time(date_str, c("%d/%m/%Y %H:%M","%d/%m/%Y %H:%M %p"), tz="Asia/Manila"))

    # Some dates were in timestamp(s)
    d[is.na(d$date),"date"] <- timestamp_to_date(d[is.na(d$date),]$date_str)
    d$date_str <- NULL
    return(d)
}

read.transport.weekhours.2019 <- function(){
  require(jsonlite)
  f <- "data/tomtom/page-data.json"
  wh.json <- jsonlite::fromJSON(f)$result$data$citiesJson$stats2019$results$weekHours

  do.call("rbind", lapply(seq_along(wh.json),
                          function(i){
                            tibble(wh.json[[i]], hour=seq(0,23)) %>%
                              mutate(weekday=names(wh.json)[[i]])}
                          )
          )
}

build_trajectories <- function(lat, lon, height, duration, dates, met_type, station,
                               direction="backward",
                               use_cache=T,
                               save_to_cache=T){

  lon <- unique(lon)
  lat <- unique(lat)
  station <- unique(station)
  direction <- unique(direction)

  filepath <- file.path(dir_results, paste0("trajs_raw_", met_type,"_",station,".RDS"))
  # dates <- seq(lubridate::date(date_from), lubridate::date(date_to), by="day")

  # Check cache exists and contains dates
  if(use_cache & file.exists(filepath)){
    trajs <- readRDS(filepath) %>% filter(direction==direction)
    enough <-
      (min(lubridate::date(trajs$traj_dt_i), na.rm=T) <= min(dates)) &
      (max(lubridate::date(trajs$traj_dt_i), na.rm=T) >= max(dates)) &
      all(trajs$lat_i==lat) &
      all(trajs$lon_i==lon)

    if(enough) return(trajs)
  }

  # Parallelize trajectory calculations
  # dates_split <- split(dates, ceiling(seq_along(dates)/30))
  trajs_at_date <- function(date){
    tryCatch({
      hysplit_trajectory(
        lon = lon,
        lat = lat,
        height = height,
        duration = duration,
        days = date,
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
      print(c)
      return(NA)
    })
  }

  trajs <- do.call('rbind',
                   lapply(dates, trajs_at_date))

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

