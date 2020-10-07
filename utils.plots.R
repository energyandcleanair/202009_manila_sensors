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


plot.ts <- function(meas, n_days){
  ggplot(meas %>% rcrea::utils.rolling_average("day", n_days, "value", min_values = as.integer(n_days/3))
  ) +
    geom_line(aes(date, value, color=toupper(station))) +
    facet_wrap(~indicator, scales="free_y") +
    scale_color_discrete(name="Station")+
    ylim(0,NA) +
    theme_crea() +
    labs(subtitle=paste0(n_days,"-day running average"), x=NULL, y=NULL)

  ggsave(file.path("results", paste0("manila_sensors_",n_days,"d.png")), width=10, height=8)
}
