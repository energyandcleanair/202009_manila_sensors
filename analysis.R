######################################################################
## Project: Manila Sensors Air Pollution Study
## Script purpose: Analyze backward trajectories leading to Manila sensors
## Date: September 2020
## Author: Hubert Thieriot (hubert@energyandcleanair.org)
######################################################################
require(openair)
# devtools::install_github("rich-iannone/SplitR")
require(splitr)
require(here)
require(lubridate)
require(tibble)
require(rcrea)
require(ggmap)
require(RColorBrewer)
require(sf)
require(iotools)
require(pbmcapply)
require(pbapply)
require(grid)
require(dplyr)
require(tidyr)


source('./config.R')
source('./utils.R')
source('./plots.R')


#########################
# Parameters
#########################
duration <- 48
height <- 10
radius_km <- 100
date_from <- "2020-01-02"
date_to <- "2020-08-31"
met_type <- "gfs0.25"
alt_max <- 2e3


#########################
# Build / read data
#########################
stations <- read.stations()
meas <- read.measurements(stations)


limit <- tibble(indicator=c("pm25", "pm10", "aqi.cn", "aqi.us"),
                indicator_name=c("PM2.5", "PM10", "AQI (CN)", "AQI (US)"),
                limit=c(500, 1041, 300, 300),
                unit=c("µg/m3","µg/m3","-","-"))
meas <- meas %>% left_join(limit) %>% filter(value < limit)

# Simple time series ------------------------------------------------------
plot_ts(meas %>% filter(indicator %in% c("pm10", "pm25")), n_days=7)
plot_ts(meas %>% filter(indicator %in% c("pm10", "pm25")), n_days=30)

# Daily and weekly time series ------------------------------------------------------
plot_hourly_variation(meas, station="lamao", indicator="pm25",
                      indicator_name="PM 2.5", indicator_unit = "µg/m3",
                      folder="results/hourly", weekdays_only = T)

plot_daily_variation(meas, station="lamao", indicator="pm25",
                      indicator_name="PM 2.5", indicator_unit = "µg/m3",
                      folder="results/daily")

plot_hourly_variation(meas, station="nch", indicator="pm25",
                      indicator_name="PM 2.5", indicator_unit = "µg/m3",
                      folder="results/hourly", weekdays_only = T)

plot_daily_variation(meas, station="nch", indicator="pm25",
                     indicator_name="PM 2.5", indicator_unit = "µg/m3",
                     folder="results/daily")

plot_hourly_variation(meas, station="sph", indicator="pm25",
                      indicator_name="PM 2.5", indicator_unit = "µg/m3",
                      folder="results/hourly", weekdays_only = T)

plot_daily_variation(meas, station="sph", indicator="pm25",
                     indicator_name="PM 2.5", indicator_unit = "µg/m3",
                     folder="results/daily")



# Exceedance --------------------------------------------------------------
standard <- tibble(indicator=c("pm25", "pm10"), standard=c(25, 50))
meas %>% group_by(station, indicator, date=lubridate::date(date)) %>%
  summarise(value=mean(value)) %>%
  inner_join(standard) %>%
  filter(value > standard) %>%
  group_by(station, indicator) %>%
  summarise(count=n())

meas %>% group_by(station, indicator, date=lubridate::date(date)) %>%
  summarise(value=mean(value)) %>%
  group_by(station, indicator) %>%
  summarise(count=n())

plot_daily_exceedances(meas, indicator=c("pm25","pm10"), standard,
                       station="lamao",
                       folder="results/daily")

plot_daily_exceedances(meas, indicator=c("pm25","pm10"), standard,
                       station="nch",
                       folder="results/daily")

plot_daily_exceedances(meas, indicator=c("pm25","pm10"), standard,
                       station="sph",
                       folder="results/daily")


plot_daily_exceedances(meas, indicator=c("pm25"), standard,
                       station="lamao",
                       folder="results/daily")

plot_daily_exceedances(meas, indicator=c("pm25"), standard,
                       station="nch",
                       folder="results/daily")

plot_daily_exceedances(meas, indicator=c("pm25"), standard,
                       station="sph",
                       folder="results/daily")

plot_monthly_exceedances(meas, indicator=c("pm25"), standard,
                         station="lamao",
                         folder="results/monthly")

plot_monthly_exceedances(meas, indicator=c("pm25"), standard,
                         station="nch",
                         folder="results/monthly")

plot_monthly_exceedances(meas, indicator=c("pm25"), standard,
                       station="sph",
                       folder="results/monthly")

# Transport ---------------------------------------------------------------

# rush hours
t.2019 <- read.transport.weekhours.2019()
plot_hourly_transport(t.2019, folder="results/hourly")


ggplot(t.2019 %>% group_by(hour))
t <- rcrea::transport.tomtom_congestion(cities=tibble(country="ID",city="Jakarta"))

ggplot(t %>% mutate(weekday=lubridate::wday(date, label=T, week_start=1)) %>% group_by(city,weekday) %>%  summarise(value=mean(value))) + geom_line(aes(weekday, value,group=city))

# Trajectories ------------------------------------------------------------
trajs <- stations %>% rowwise() %>%
  mutate(trajs=list(build_trajectories(latitude, longitude, height, duration, date_from, date_to, met_type=met_type, station=station, direction=direction, use_cache = T)))



# Merge with measurements
meas <- read.measurements() %>% filter(indicator=="pm25")

meas.powerplants <- meas %>%
  filter(station %in% c("lamao", "sph")) %>%
  mutate(station=paste0(station,".powerplant"))

meas <- bind_rows(meas, meas.powerplants)


trajs_meas <- trajs %>% select(-c(direction)) %>%
  tidyr::unnest(cols=c(trajs)) %>%
  inner_join(meas), by=c("station"="station", "date"="date")) %>%
  dplyr::distinct(station, traj_dt, traj_dt_i, .keep_all=T) #TODO find why there are "duplicate" yet not duplicate

# We use maximum daily values rather than mean
# Will give beter results for pollution peaks
# meas_hour_max <- measurements(city='jakarta',
#                      date_from=date_from,
#                      source='openaq',
#                      aggregate_level = "station", # Ideally this line shouldn't be required
#                      process_id='raw',
#              poll=rcrea::PM25,
#                      collect=F) %>%
#   filter(value<500) %>%
#   collect()
#
# meas_max <- meas_hour_max %>%
#   dplyr::mutate(date=date_trunc('day',date)) %>%
#   group_by(process_id, date, poll, unit, source, city) %>%
#   summarize(value=max(value))
#
# trajs_meas <- trajs %>%
#   dplyr::left_join(meas, #meas
#                    by=c("day"="date")) %>%
#   dplyr::distinct(traj_dt, traj_dt_i, .keep_all=T) #TODO find why there are "duplicate" yet not duplicate
#
# trajs_meas_mean_2020 <- trajs_2020 %>%
#   dplyr::left_join(meas,
#                    by=c("day"="date")) %>%
#   dplyr::distinct(traj_dt, traj_dt_i, .keep_all=T) #TODO find why there are "duplicate" yet not duplicate
#
# trajs_meas_max_2020 <- trajs_2020 %>%
#   dplyr::left_join(meas_max,
#                    by=c("day"="date")) %>%
#   dplyr::distinct(traj_dt, traj_dt_i, .keep_all=T) #TODO find why there are "duplicate" yet not duplicate

build.basemap <- function(stations, radius_km, zoom){
  st_as_sf(data.frame(stations), coords=c("longitude","latitude"), crs=4326) %>%
    st_transform(crs=3857) %>%
    st_buffer(radius_km*1000) %>%
    st_transform(crs=4326) %>%
    rowwise() %>%
    mutate(geometry=purrr::map(geometry, st_bbox)) %>%
    mutate(basemap=list(get_map(location=unname(geometry),
                                zoom=zoom,
                                source="stamen")))
}

basemaps <- bind_rows(
  build.basemap(stations, 1, 14) %>% mutate(radius_km=1),
  build.basemap(stations, 10, 12) %>% mutate(radius_km=10),
  build.basemap(stations, 50, 11) %>% mutate(radius_km=50)
  )




#---------------------------------
# Get data
#---------------------------------

# Get plotting data from OpenAir
# plt_freq <- plot_frequency_zoom(trajs_meas) #Very slow
# plt_freq_2020 <- plot_frequency_zoom(trajs_meas_mean_2020) #Very slow
# data_freq <- plt_freq$data
# saveRDS(data_freq, "data_freq.RDS")
# data_freq <- readRDS("data_freq.RDS")
# data_freq[lubridate::month(data_freq$date) %in% seq(5,9),'crea_season'] <- 'Dry Season'
# data_freq[lubridate::month(data_freq$date) %in% c(1,2,3,11,12), 'crea_season'] <- 'Wet Season'

# Cluster data
# plt_clusters <- plot_traj_clusters(trajs_meas %>% filter(station=="lamao"), n_clusters = 15)

# plt_clusters_wet <- plot_traj_clusters(trajs_meas %>%
#                                          filter(lubridate::month(traj_dt_i) %in% c(1,2,3,11,12)),
#                                        n_clusters = 5)
#
# plt_clusters_2020 <- plot_traj_clusters(trajs_meas_mean_2020,
#                                        n_clusters = 5)

# data_cluster <- plt_clusters$results
# data_cluster_wet <- plt_clusters_wet$results
# data_cluster_2020 <- plt_clusters_2020$results


# Build maps
# map_industries(basemap_100km, industries)

# map_frequency_dry(basemap, data_freq, industries, save=T)
# map_frequency_wet(basemap, data_freq, industries, save=T)

#
# map_traj_clusters(basemap=basemap_10km,
#                       # industries=industries,
#                       data_cluster=data_cluster,
#                       filename="traj_clusters_10km.png",
#                       save=T)
#
# map_traj_clusters_wet(basemap=basemap_100km,
#                       industries=industries,
#                       data_cluster=data_cluster_wet,
#                       filename="traj_clusters_wet_100km.png",
#                       save=T)
#
# map_traj_clusters(basemap=basemap_100km,
#                   industries=industries,
#                   data_cluster=data_cluster_2020,
#                   filename="traj_clusters_2020_100km.png",
#                   save=T,
#                   season_name=NULL,
#                   season_definition=NULL,
#                   add_plot=labs(title=paste("Sources of air flowing into Jakarta in 2020"),
#                                    subtitle = 'Jan-May',
#                                    caption="CREA based on HYSPLIT model and PROPER. \n Lines show most representative trajectories arriving in Jakarta."))
#

# Peaks

map_station <- function(trajs_meas, station, radius_km, threshold){

  t <- trajs_meas %>% filter(station==!!station)
  b <- (basemaps  %>% filter(station==!!station,
                             radius_km==!!radius_km) %>%
          pull(basemap))[[1]]
  date.min <- min(t$date, na.rm=T) %>% lubridate::date()
  date.max <- max(t$date, na.rm=T) %>% lubridate::date()

  map_peaks(
      basemap = b,
      trajs_meas = t,
      industries = NULL,
      threshold = threshold,
      filename = paste0("traj_peaks_",radius_km,"km_",station,"_threshold",threshold,".jpg"),
      add_plot=labs(
        title=paste("Source of air flowing into", toupper(station),"sensor"),
        subtitle=paste0(date.min, " - ", date.max, ". Days when PM2.5 levels >= ", threshold, " µg/m3"),
        caption="Source: CREA based on HYSPLIT.")
    )
}

for(threshold in c(30,40,50,60)){
  for(radius_km in c(1,10,50)){
    map_station(trajs_meas, "lamao", radius_km, threshold)
    map_station(trajs_meas, "lamao.powerplant", radius_km, threshold)
    map_station(trajs_meas, "nch", radius_km, threshold)
    map_station(trajs_meas, "sph", radius_km, threshold)
    map_station(trajs_meas, "sph.powerplant", radius_km, threshold)
  }
}



# Old ---------------------------------------------------------------------
#
#
# map_peaks(
#   basemap = (basemaps_10km  %>% filter(station=="lamao") %>% pull(basemap))[[1]],
#   trajs_meas = trajs_meas %>% filter(station=="lamao"),
#   industries = NULL,
#   threshold = 20,
#   filename = "traj_peaks_10km_lamao.png"
# )
#
# map_peaks(
#   basemap = (basemaps_100km  %>% filter(station=="lamao") %>% pull(basemap))[[1]],
#   trajs_meas = trajs_meas %>% filter(station=="lamao"),
#   industries = NULL,
#   threshold = 20,
#   filename = "traj_peaks_100km_lamao.png"
# )
#
# map_peaks(
#   basemap = (basemaps_100km  %>% filter(station=="nch") %>% pull(basemap))[[1]],
#   trajs_meas = trajs_meas %>% filter(station=="nch"),
#   industries = NULL,
#   threshold = 20,
#   filename = "traj_peaks_100km_nch.png"
# )
#
# map_peaks(
#   basemap = (basemaps_100km  %>% filter(station=="sph") %>% pull(basemap))[[1]],
#   trajs_meas = trajs_meas %>% filter(station=="sph"),
#   industries = NULL,
#   threshold = 20,
#   filename = "traj_peaks_100km_sph.png"
# )
#
# map_peaks(
#   basemap = basemap_100km,
#   trajs_meas = trajs_meas_2020 %>% dplyr::filter(traj_dt_i>="2020-01-01"),
#   industries = industries,
#   threshold = 60,
#   add_plot=labs(subtitle="January-May 2020"),
#   filename = "traj_peaks_100km_60_2020.png"
# )
#
# map_peaks(
#   basemap = basemap_100km,
#   trajs_meas = trajs_meas_mean_2020 %>% dplyr::filter(traj_dt_i>="2020-01-01"),
#   industries = industries,
#   threshold = 70,
#   add_plot=labs(subtitle="January-May 2020"),
#   filename = "traj_peaks_100km_70_2020.png"
# )
#
# map_peaks(
#   basemap = basemap_100km,
#   trajs_meas = trajs_meas %>% mutate(year=lubridate::year(traj_dt_i),
#                                      month=lubridate::month(traj_dt_i)) %>%
#     group_by(year, month) %>% filter(value==max(value, na.rm=T)) %>%
#     ungroup() %>% dplyr::filter(traj_dt_i>="2020-01-01"),
#   industries = industries,
#   threshold = 0,
#   add_plot=labs(subtitle="January-May 2020",
#                 caption="CREA based on OpenAQ, HYSPLIT model and PROPER. A pollution peak is defined as the day with highest PM2.5 level in any given month"),
#   filename = "traj_peaks_100km_month_2020.png"
# )
#
#
# map_peaks(
#   basemap = basemap_100km,
#   trajs_meas = trajs_meas %>% mutate(period= traj_dt_i %>%
#                                        cut(breaks=as.POSIXct(c("2020-03-14","2020-04-09","2020-06-04","2020-06-30")),
#                                            labels=c("14 March - 9 April (wfh)","10 April - 4 June (mass physical distancing)","5 June - 30 june (transition to \"normal\")"))) %>%
#     filter(!is.na(period)) %>% group_by(period) %>% filter(value==max(value, na.rm=T)) %>%
#     ungroup(),
#   industries = industries,
#   threshold = 0,
#   add_plot=list(
#     facet_wrap(~period),
#     labs(subtitle=NULL,
#          caption="CREA based on OpenAQ, HYSPLIT model and PROPER. A pollution peak is defined as the day with highest PM2.5 level in the different periods.")),
#   filename = "traj_peaks_100km_period_2020.png"
# )
#
#
# map_peaks(
#   basemap = basemap_100km,
#   trajs_meas = trajs_meas %>% mutate(period= traj_dt_i %>%
#                                        cut(breaks=as.POSIXct(c("2020-03-14","2020-04-09","2020-06-04","2020-06-30")),
#                                            labels=c("14 March - 9 April (wfh)","10 April - 4 June (mass physical distancing)","5 June - 30 june (transition to \"normal\")"))) %>%
#     filter(!is.na(period)) %>% group_by(period) %>%
#     filter(value==max(value, na.rm=T)) %>%
#     ungroup(),
#   industries = industries,
#   threshold = 0,
#   add_plot=list(
#     facet_wrap(~period),
#     labs(subtitle=NULL,
#          caption="CREA based on OpenAQ, HYSPLIT model and PROPER. Pollution peaks are defined as the 3-days with highest PM2.5 level in the different periods.")),
#   filename = "traj_peaks_100km_period_2020.png"
# )
#
#
#
#
# # Lauri trajectories ------------------------------------------------------
#
# trajs_lauri <- get_trajs_lauri()
# map_trajs_lauri(basemap_100km, trajs_lauri, industries, filename = "trajs_20200412_lauri.png")

