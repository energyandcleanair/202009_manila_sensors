
# Global parameters
plot.projection <- "mercator"
plot.smooth <- F
plot.type <- "season"
plot.col <- "jet"
plot.lat.inc <- 0.4
plot.lon.inc <- 0.4
plot.grid.col <- "transparent"


# Plots -------------------------------------------------------------------
plot_ts <- function(meas, n_days){

  if(length(unique(meas$unit))){
    unit <- unique(meas$unit)
  }else{
    unit <- NULL
  }

  (plt <- ggplot(meas %>%
           rcrea::utils.rolling_average("day", n_days, "value", min_values = as.integer(n_days/2))) +
    geom_line(aes(date, value, color=station_name)) +
    facet_wrap(~indicator_name, scales="free_y") +
    scale_color_discrete(name=NULL)+
    ylim(0,NA) +
      guides(color=guide_legend(nrow=1,byrow=TRUE)) +
    theme_crea(legend.position="bottom") +
    scale_y_continuous(limits=c(0,NA),expand=expand_scale(mult = c(0, 0.1))) +
    labs(subtitle=paste0(n_days,"-day running average"), x=NULL, y=unit))

  ggsave(file.path("results", paste0("manila_sensors_",n_days,"d.png")), plot=plt, width=10, height=5)
  return(plt)
}


plot_hourly_variation <- function(meas, station, indicator, indicator_name, indicator_unit, folder, weekdays_only=T){

  m <- meas %>% filter(station==!!station,
                       indicator==!!indicator)
  if(weekdays_only){
    m <- m %>% filter(lubridate::wday(date, week_start=1) <6)
  }

  m.hourly <- m %>%
    filter(!is.na(value)) %>%
    mutate(hour=lubridate::hour(date)) %>%
    group_by(station, station_name, indicator, hour) %>%
    summarize(mean=mean(value),
              p5=quantile(value, 0.05),
              p50=quantile(value, 0.5),
              p95=quantile(value, 0.95)
              )


  (plt <- ggplot(m.hourly) +
    geom_ribbon(aes(hour, ymin=p5, ymax=p95), fill="grey90") +
    geom_line(aes(hour, mean)) +
    rcrea::theme_crea() +
    scale_y_continuous(limits=c(0,NA), expand=expand_scale(mult = c(0, 0.1))) +
    scale_x_continuous(minor_breaks = seq(0,24), expand=c(0,0)) +
    theme(panel.grid.minor.x = element_line(colour="grey90"),
          panel.grid.major.x = element_line(colour="grey90"))+
    labs(title=paste0("Hourly ", indicator_name, " levels in ", unique(m.hourly$station_name),
                      ifelse(weekdays_only, " during weekdays","")),
         y=indicator_unit,
        caption="Grey area represents the 5th and 95th percentiles. Solid line represents average value."
    ))

  dir.create(folder, showWarnings = F, recursive = T)

  ggsave(file.path(folder, paste0("hourly_",indicator,"_", station,".png")), plot=plt,
         width=8, height=4)

  return(plt)
}

plot_daily_variation <- function(meas, station, indicator, indicator_name, indicator_unit, folder){

  m.daily <- meas %>%
    filter(!is.na(value)) %>%
    filter(station==!!station, indicator==!!indicator) %>%
    mutate(weekday=lubridate::wday(date, label=T, week_start=1)) %>%
    group_by(station, station_name, indicator, weekday) %>%
    summarize(mean=mean(value),
              p5=quantile(value, 0),
              p50=quantile(value, 0.5),
              p95=quantile(value, 1),
    ) %>% ungroup()

  plt <- ggplot(m.daily ) +
      geom_ribbon(aes(x=weekday, ymin=p5, ymax=p95, group=station), fill="grey90") +
      geom_line(aes(x=weekday,y=mean, group=station)) +
      rcrea::theme_crea() +
      scale_y_continuous(limits=c(0,NA), expand=expand_scale(mult = c(0, 0.1))) +
      # scale_x_discrete( expand=c(0,0)) +
      theme(panel.grid.minor.x = element_line(colour="grey90"),
            panel.grid.major.x = element_line(colour="grey90"))+
      labs(title=paste0("Daily ", indicator_name, " levels in ", m.daily$station_name),
           y=indicator_unit,
           x=NULL,
           caption="Grey area represents the 5th and 95th percentiles. Solid line represents average value."
      )

  if(indicator=="pm25"){
   plt <- plt + geom_hline(yintercept=25, colour="red", linetype="dashed", show.legend = F) +
      geom_text(aes(x='Mon', y=35,label="WHO guideline \n(25 µg/m3)",col="red",hjust=0), show.legend = F)
  }

  dir.create(folder, showWarnings = F, recursive = T)

  ggsave(file.path(folder, paste0("daily_",indicator,"_", station,".png")), plot=plt,
         width=8, height=4)

  return(plt)
}

plot_daily_exceedances <- function(meas, indicator=c("pm10","pm25"), standard, station, folder){


  m <- meas %>%
    filter(indicator %in% !!indicator,
           !is.na(value)) %>%
    group_by(station, station_name,  indicator, indicator_name, date=lubridate::date(date)) %>%
    summarize(value=mean(value)) %>%
    group_by(station, indicator_name, weekday=lubridate::wday(date, label=T, week_start=1)) %>%
    mutate(n_days_total=n()) %>%
    inner_join(standard) %>%
    filter(value>standard) %>%
    group_by(station, station_name, indicator, indicator_name, weekday, n_days_total) %>%
    summarize(n_days_violations=n()) %>%
    mutate(fraction=n_days_violations/n_days_total)

  if(!is.null(station)){
    m <- m %>% filter(station==!!station)
  }


  station_name <- ifelse(is.null(station), "", paste0(" in ",unique(m$station_name)))

  wdays <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  filler <- tidyr::crossing(station=station, indicator=indicator,
                            weekday=wdays)

  m.filled <- m %>% full_join(filler) %>%
    mutate(weekday=factor(weekday, levels=wdays))


  plt <- ggplot(m.filled %>% replace_na(list("fraction"=0))) +
    geom_bar(stat="identity", aes(x=weekday, y=fraction, fill=indicator_name), position="dodge") +
    scale_y_continuous(labels = scales::percent, expand=expansion(mult=c(0,0.1))) +
    rcrea::CREAtheme.scale_fill_crea_d(name=NULL) +
    theme_crea() +
    labs(title=paste("Violations of air quality levels", station_name),
         y="Share of days above regulatory threshold",
         x=NULL)

  if(is.null(station)){
    plt <- plt + facet_wrap(~station_name)
  }

  dir.create(folder, showWarnings = F, recursive = T)

  ggsave(file.path(folder, paste0("daily_violations_",paste0(indicator,collapse="_"),"_",
                                  ifelse(is.null(station),"all",station),".png")), plot=plt,
         width=10, height=4)

  return(plt)
}

plot_monthly_exceedances <- function(meas, indicator=c("pm10","pm25"), standard, station, folder){

  m <- meas %>%
    filter(indicator %in% !!indicator,
           !is.na(value)) %>%
    group_by(station, station_name,  indicator, indicator_name, date=lubridate::date(date)) %>%
    summarize(value=mean(value)) %>%
    group_by(station, indicator_name, month=lubridate::round_date(date, unit="month")) %>%
    mutate(n_days_total=n()) %>%
    inner_join(standard) %>%
    filter(value>standard) %>%
    group_by(station, station_name, indicator, indicator_name, month, n_days_total) %>%
    summarize(n_days_violations=n()) %>%
    mutate(fraction=n_days_violations/n_days_total)

  if(!is.null(station)){
    m <- m %>% filter(station==!!station)
  }

  station_name <- ifelse(is.null(station), "", paste0(" in ",unique(m$station_name)))

  months <- unique(m$month)
  filler <- tidyr::crossing(station=station, indicator=indicator,
                            month=months)

  m.filled <- m %>% full_join(filler)


  (plt <- ggplot(m.filled %>% replace_na(list("fraction"=0))) +
      geom_bar(stat="identity", aes(x=month, y=fraction, fill=indicator_name), position="dodge") +
      scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
      scale_y_continuous(labels = scales::percent, expand=expansion(mult=c(0,0.1))) +
      rcrea::CREAtheme.scale_fill_crea_d(name=NULL) +
      theme_crea() +
      labs(title=paste("Violations of air quality levels", station_name),
           y="Share of days above regulatory threshold",
           x=NULL))

  if(is.null(station)){
    plt <- plt + facet_wrap(~station_name)
  }

  dir.create(folder, showWarnings = F, recursive = T)

  ggsave(file.path(folder, paste0("monthly_violations_",paste0(indicator,collapse="_"),"_",
                                  ifelse(is.null(station),"all",station),".png")), plot=plt,
         width=10, height=4)

  return(plt)
}


plot_hourly_transport <- function(t, folder, weekdays_only=T){


  if(weekdays_only){
    t <- t %>% filter(!weekday%in% c("Sat","Sun"))
  }

  t.hourly <-t %>%
    filter(!is.na(congestion)) %>%
    group_by(hour) %>%
    summarize(mean=mean(congestion),
              p5=quantile(congestion, 0.05),
              p50=quantile(congestion, 0.5),
              p95=quantile(congestion, 0.95)
    )


  (plt <- ggplot(t.hourly) +
      geom_ribbon(aes(hour, ymin=p5, ymax=p95), fill="grey90") +
      geom_line(aes(hour, mean)) +
      rcrea::theme_crea() +
      scale_y_continuous(limits=c(0,NA), expand=expand_scale(mult = c(0, 0.1))) +
      scale_x_continuous(minor_breaks = seq(0,24), expand=c(0,0)) +
      theme(panel.grid.minor.x = element_line(colour="grey90"),
            panel.grid.major.x = element_line(colour="grey90"))+
      labs(title=paste0("Contestion levels in Manila",
                        ifelse(weekdays_only, " during weekdays",""),
                        " in 2019"),
           y=NULL,
           caption="Grey area represents the 5th and 95th percentiles. Solid line represents average value. Source: TomTom"
      ))

  dir.create(folder, showWarnings = F, recursive = T)

  ggsave(file.path(folder, paste0("hourly_congestion.png")), plot=plt,
         width=8, height=4)

  return(plt)
}


plot_trajectories_oldschool <- function(trajs_meas, year, month){
  # Plot trajectories: CAREFUL. Too time consuming. Will crash on Mac.
  plt <- trajPlot(trajs_meas %>% dplyr::filter(year==year, month=month),
            pollutant = "value", type=c('year','month'), col = "jet", lwd =1, plot.type='l',
            projection="mercator",
            parameters=NULL)

  return(plt)
}


plot_frequency <- function(trajs_meas, type="season", statistics="frequency", save=T, filename="traj_frequency.png"){

  plt <- trajLevel(trajs_meas,
                          type=type,
                          lat.inc=plot.lat.inc,
                          lon.inc=plot.lon.inc,
                          statistic=statistics,
                          projection=plot.projection,
                          smooth=plot.smooth,
                          col=plot.col,
                          grid.col=plot.grid.col,
                          parameters=NULL)

  if(save){
    png(filename=file.path("output", filename),
        type="cairo",
        units="in",
        width=8,
        height=6,
        pointsize=14,
        res=120)
    plt
    dev.off()
  }

  return(plt)
}


plot_frequency_zoom <- function(trajs_meas, type="season", statistics="frequency", save=T, filename="traj_frequency_zoom.png"){

  plt <- trajLevel(trajs_meas,
                   type=type,
                   lat.inc=plot.lat.inc/4,
                   lon.inc=plot.lon.inc/4,
                   statistic=statistics,
                   projection=plot.projection,
                   smooth=plot.smooth,
                   col=plot.col,
                   grid.col=plot.grid.col,
                   parameters=NULL)

  if(save){
    png(filename=file.path("output", filename),
        type="cairo",
        units="in",
        width=8,
        height=6,
        pointsize=14,
        res=120)
    plt
    dev.off()
  }

  return(plt)

}

plot_traj_clusters <- function(trajs_meas, n_clusters=5, statistics="frequency", save=T, filename="traj_frequency_zoom.png"){

  plt <- trajCluster(
    trajs_meas,
    method = "Euclid",
    n.cluster = n_clusters,
    plot = T,
    type = "default",
    cols = "Set1",
    split.after = FALSE,
    map.fill = TRUE,
    map.cols = "grey40",
    map.alpha = 0.4,
    projection = plot.projection,
    parameters = NULL,
    orientation = c(90, 0, 0),
    by.type = FALSE,
    origin = TRUE
  )

  if(save){
    png(filename=file.path("output", filename),
        type="cairo",
        units="in",
        width=8,
        height=6,
        pointsize=14,
        res=120)
    plt
    dev.off()
  }

  return(plt)

}


# Maps --------------------------------------------------------------------
map_trajs_lauri <- function(basemap, trajs_sf, industries, title=NULL, subtitle=NULL, filename="trajs.png", add_plot=NULL){

  (m <- ggmap(basemap) +
     coord_cartesian() +
     geom_point(data=industries, inherit.aes = F, aes(x=lng,y=lat, shape=sector, color=sector), fill="transparent",
                stroke=2.5, alpha=0.8, position="jitter") +
     # Trajectories
     geom_sf(data=trajs_sf, inherit.aes = F, color="darkred") +
     # geom_line(data = trajs_meas %>% dplyr::filter(value>=threshold) , aes(x = lon, y = lat, group=traj_dt_i), alpha=0.6, color='darkred')+

     theme_crea() +
     theme(panel.background = element_rect(fill='lightgray'),
           panel.border = element_rect(color='black', fill=NA),
           panel.grid = element_line(color=NA),
           plot.caption = element_text(lineheight = 0.9),
           legend.position="right",
           legend.key = element_rect(fill='white')) +
     scale_shape_manual(name="Sector", values=c(0,1,2,3,4,5))+
     scale_color_brewer(name="Sector", palette="Dark2")+
     labs(title=title,
          subtitle = subtitle,
          x='', y='',
          caption=paste("CREA based on OpenAQ, HYSPLIT model and PROPER")))

  if(!is.null(add_plot)){
    m <- m + add_plot
  }

  ggsave(plot=m, filename=file.path("output", filename),
         width=12,
         height=8)

  return(m)

}

map_industries <- function(basemap, industries, filename="industries.png", save=T){

  (map <- ggmap(basemap) +
     # Industries
     geom_point(data=industries, inherit.aes = F,
                aes(x=lng,y=lat, shape=sector, color=sector), fill="transparent",
                stroke=2.5, alpha=0.8, position="jitter") +

     # Style
     coord_cartesian() +
     # scale_fill_gradientn(name='Count', colours=rev(brewer.pal(n=10,'Spectral'))) +
     scale_fill_distiller(palette='Spectral', direction=-1,
                          values=seq(0,1,length.out = 5),
                          labels=c("0","50","100","150","200+"),
                          name='Count') +
     scale_shape_manual(name="Sector", values=c(0,1,2,3,4,5))+
     scale_color_brewer(name="Sector", palette="Dark2")+
     theme_crea() +
     theme(panel.background = element_rect(fill='lightgray'),
           panel.border = element_rect(color='black', fill=NA),
           panel.grid = element_line(color=NA),
           plot.caption = element_text(lineheight = 0.9),
           legend.position="right",
           legend.key = element_rect(fill='white')) +
     # Text
     labs(title=NULL,
          subtitle=NULL,
          x='', y='',
          caption="Source: CREA based on PROPER."))

  if(save){
    ggsave(plot=map, filename=file.path("output", filename),
           width=12,
           height=8)
  }

  return(map)
}

map_frequency_season <- function(basemap, data_freq, industries, filename, season_filter, season_definition, save=T){

  (map <- ggmap(basemap) +
     # Industries
     geom_point(data=industries, inherit.aes = F,
                aes(x=lng,y=lat, shape=sector, color=sector), fill="transparent",
                stroke=2.5, alpha=0.8, position="jitter") +
     # Frequency
     geom_raster(data = data_freq %>%
                   dplyr::filter(grepl(season_filter, crea_season)) %>%
                   dplyr::mutate(count=min(count,200)), aes(x = xgrid, y = ygrid, fill = count), alpha=0.7) +
     # Jakarta contours

     # Style
     coord_cartesian() +
     # scale_fill_gradientn(name='Count', colours=rev(brewer.pal(n=10,'Spectral'))) +
     scale_fill_distiller(palette='Spectral', direction=-1,
                          values=seq(0,1,length.out = 5),
                          labels=c("0","50","100","150","200+"),
                          name='Count') +
     scale_shape_manual(name="Sector", values=c(0,1,2,3,4,5))+
     scale_color_brewer(name="Sector", palette="Dark2")+
     theme_crea() +
     theme(panel.background = element_rect(fill='lightgray'),
           panel.border = element_rect(color='black', fill=NA),
           panel.grid = element_line(color=NA),
           plot.caption = element_text(lineheight = 0.9),
           legend.position="right",
           legend.key = element_rect(fill='white')) +
     # Text
     labs(title=paste("Sources or air flowing into Jakarta during the",tolower(season_filter), "season"),
          subtitle = '2017-2020',
          x='', y='',
          caption=paste("CREA based on HYSPLIT model and PROPER. \n\"Count\" represents number of air trajectories passing by the location before arriving in Jakarta.\n", season_definition)))

  if(save){
    ggsave(plot=map, filename=file.path("output", filename),
           width=12,
           height=8)
  }

  return(map)
}

map_frequency_dry <- function(basemap, data_freq, industries, filename="map_frequency_dry.png",
                              save=T){

  return(map_frequency_season(basemap=basemap,
                              data_freq=data_freq,
                              industries=industries,
                              filename=filename,
                              season_filter="Dry",
                              season_definition="Dry season refers to May-September.",
                              save=save))
}

map_frequency_wet <- function(basemap, data_freq, industries, filename="map_frequency_wet.png",
                              save=T){

  return(map_frequency_season(basemap=basemap,
                              data_freq=data_freq,
                              industries=industries,
                              filename=filename,
                              season_filter="Wet",
                              season_definition="Wet season refers to November-March",
                              save=save))
}

map_traj_clusters <- function(basemap, data_clusters, industries, filename="traj_clusters.png", season_name, season_definition, add_plot=NULL, save=T){

  map <- ggmap(basemap) +
     # # Industries
     # geom_point(data=industries, inherit.aes = F,
     #            aes(x=lng,y=lat, shape=sector, color=sector), fill="transparent",
     #            stroke=2.5, alpha=0.8, position="jitter") +
     # Cluster trajectories
     geom_path(data = data_clusters %>%
                 mutate(subcluster=paste(cluster, hour.inc %/% 8)),
               arrow = arrow(angle=18, length=unit(0.1,"inches")),
               aes(x = lon, y = lat, group=subcluster), color="darkred", alpha=0.7) +

     geom_path(data = data_clusters,
               aes(x = lon, y = lat, group=cluster), color="darkred", alpha=0.7) +

     # Style
     coord_cartesian() +
     # scale_fill_gradientn(name='Count', colours=rev(brewer.pal(n=10,'Spectral'))) +
     scale_fill_distiller(palette='Spectral', direction=-1,
                          values=seq(0,1,length.out = 5),
                          labels=c("0","50","100","150","200+"),
                          name='Count') +
     scale_shape_manual(name="Sector", values=c(0,1,2,3,4,5))+
     scale_color_brewer(name="Sector", palette="Dark2")+
     theme_crea() +
     theme(panel.background = element_rect(fill='lightgray'),
           panel.border = element_rect(color='black', fill=NA),
           panel.grid = element_line(color=NA),
           plot.caption = element_text(lineheight = 0.9),
           legend.position="right",
           legend.key = element_rect(fill='white')) +
     # Text
     labs(title=paste("Sources of air flowing into Jakarta during the", season_name, "season"),
          subtitle = '2017-2020',
          x='', y='',
          caption=paste("CREA based on HYSPLIT model and PROPER. \n Lines show most representative trajectories arriving in Jakarta.", season_definition))

  if(!is.null(add_plot)){
    map <- map + add_plot
  }

  (map)
  if(save){
    ggsave(plot=map, filename=file.path("output", filename),
           width=12,
           height=8)
  }

  return(map)
}

map_traj_clusters_dry <- function(basemap, data_clusters, industries, filename="traj_clusters_dry.png", save=T){
  return(map_traj_clusters(
    basemap=basemap,
    data_clusters=data_clusters,
    industries=industries,
    season_name = "dry",
    season_definition = "Dry season refers to May-September.",
    save=save,
    filename=filename
  ))
}

map_traj_clusters_wet <- function(basemap, data_clusters, industries, filename="traj_clusters_wet.png", save=T){
  return(map_traj_clusters(
    basemap=basemap,
    data_clusters=data_clusters,
    industries=industries,
    season_name = "wet",
    season_definition = "Wet season refers to November-March.",
    save=save,
    filename=filename
  ))
}

map_peaks <- function(basemap, trajs_meas, industries, threshold=80, filename="traj_peaks.png", add_plot=NULL){

  t <- trajs_meas %>%
    ungroup() %>%
    mutate(rainy=lubridate::yday(date) >= lubridate::yday("0000-06-01") &
                     lubridate::yday(date) <= lubridate::yday("0000-10-15"),
           season=ifelse(rainy,"Rainy season","Dry season"))

  (m <- ggmap(basemap, extent = "normal", maprange=FALSE) +

     coord_cartesian() +
     # geom_point(data=wri_power %>% dplyr::filter(country=="IDN"), inherit.aes = F, aes(x=longitude,y=latitude),
     #            shape=2, stroke=1.5, color='darkred') +
     # geom_point(data=ct %>% dplyr::filter(country=="Indonesia"), inherit.aes = F, aes(x=lng,y=lat),
     #            shape=2, stroke=1.5, color='darkred') +
     # geom_point(data=industries, inherit.aes = F, aes(x=lng,y=lat, shape=sector, color=sector), fill="transparent",
                # stroke=2.5, alpha=0.8, position="jitter") +
     # Cluster trajectories
     geom_path(data = t %>%
                 dplyr::filter(value>=threshold) %>%
                 dplyr::arrange(hour_along) %>%
                 mutate(subcluster=paste(traj_dt_i, hour_along %/% 8)),
               arrow = arrow(angle=18, length=unit(0.1,"inches")),
               aes(x = lon, y = lat, group=subcluster), color="darkred", alpha=0.6) +

     geom_path(data = t %>%
                 dplyr::filter(value>=threshold) ,
               aes(x = lon, y = lat, group=traj_dt_i), color="darkred", alpha=0.6) +

     coord_quickmap(
               expand=F,
               xlim=c(attr(basemap, "bb")$ll.lon, attr(basemap, "bb")$ur.lon),
               ylim=c(attr(basemap, "bb")$ll.lat, attr(basemap, "bb")$ur.lat)) +

     # geom_line(data = t %>% dplyr::filter(value>=threshold) , aes(x = lon, y = lat, group=traj_dt_i), alpha=0.6, color='darkred')+

     theme_crea() +
     theme(panel.background = element_rect(fill='lightgray'),
           panel.border = element_rect(color='black', fill=NA),
           panel.grid = element_line(color=NA),
           plot.caption = element_text(lineheight = 0.9),
           legend.position="right",
           legend.key = element_rect(fill='white')) +
     scale_shape_manual(name="Sector", values=c(0,1,2,3,4,5))+
     scale_color_brewer(name="Sector", palette="Dark2")+
     labs(title=paste("Sources or air flowing into Jakarta during 'pollution peaks'"),
          subtitle = '2017-2020',
          x='', y='',
          caption=paste("CREA based on OpenAQ, HYSPLIT model and PROPER. A pollution peak is defined as daily PM2.5 >=", threshold, "µg/m3")))

  if(!is.null(add_plot)){
    m <- m + add_plot
  }

  m.season <- m + facet_wrap(~season)

  ggsave(plot=m, filename=file.path("results", "maps", filename),
         width=12,
         height=8)

  ggsave(plot=m.season, filename=file.path("results", "maps", gsub("\\.jpg","_seasons\\.jpg",filename)),
         width=12,
         height=8)

  return(m)

}

