

ggmap::register_google("AIzaSyAM2hj3VbXCSjAXIjLjLH_DfPPSiV8Bhg0")


dir_hysplit_output <- here::here("hysplit_output")
dir.create(dir_hysplit_output, showWarnings = F, recursive = T)

dir_hysplit_met <- Sys.getenv("DIR_HYSPLIT_MET", here::here("hysplit_met"))
dir.create(dir_hysplit_met, showWarnings = F, recursive = T)

dir_results <- here::here("results")
dir.create(dir_results, showWarnings = F, recursive = T)
