# setup -------------------------------------------------------------------

library(dataRetrieval)
library(rnoaa)
library(tidyverse)
library(lubridate)
library(tbeptools)
library(WRTDStidal)

chldat <- epcdata

# key for noaa data
mykey <- Sys.getenv("NOAA_KEY")

# get hydroload estimates -------------------------------------------------

hydest <- tibble(
  yr = 1974:2019
  ) %>% 
  group_by(yr) %>% 
  nest %>% 
  mutate(
    data = purrr::pmap(list(yr), function(yr){
      
      cat(yr, '\t')
      
      start <- paste0(yr, "-01-01")
      end <- paste0(yr, "-12-31")
      
      # get rainfall data at station
      tia_rainfall <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00012842",
                           datatypeid = "PRCP", startdate = start, enddate = end,
                           limit = 500, add_units = TRUE, token = mykey)
      
      # convert rain data to inches
      tia_rain <- tia_rainfall$data %>%
        mutate(daily_in = (value/254),
               Date = as.Date(date))
      
      # get hydrological data, 0060 is discharge as cfs, converted to million m3/day
      # converting ft3/s * 86400 s/d / 0.0283168 m3/ft3 * 1,000,000
      bkr<- readNWISdv("02307359", "00060", start, end) %>%
        renameNWISColumns() %>%
        mutate(bkr_flow = (Flow*3.05119225))
      
      out <- left_join(tia_rain, bkr, by=c("Date")) %>%
        select(Date, daily_in, bkr_flow) %>%
        mutate(hyd_est = (154.22+(8.12*bkr_flow)+(6.73*daily_in))/365) %>% 
        drop_na(hyd_est)
      
      return(out)
      
    })
  ) %>% 
  unnest('data')

save(hydest, file = 'data/hydest.RData', compress = 'xz')

# wrtds models ------------------------------------------------------------

data(hydest)

# water quality
wqdat <- epcdata %>% 
  filter(yr > 1973 & yr < 2020) %>% 
  rowwise() %>% 
  mutate(
    date = as.Date(SampleTime), 
    date = floor_date(date, unit = 'month')
  ) %>% 
  ungroup() %>% 
  select(-matches('\\_q$|^sd|^Temp|\\_ppth$|\\_Depth\\_m$|^Lat|^Lon|^yr$|^mo$|^SampleTime$')) %>% 
  group_by(bay_segment, date) %>% 
  summarise(
    tn = median(tn, na.rm = T), 
    chla = median(chla, na.rm = T), 
    .groups = 'drop'
  )

# hydrologic estimates, floored to month, summed in month
hyddat <- hydest %>% 
  ungroup %>% 
  select(date = Date, hyd_est) %>% 
  mutate(
    date = floor_date(date, unit = 'months')
  ) %>% 
  group_by(date) %>% 
  summarise(hyd_est = sum(hyd_est, na.rm = T), .groups = 'drop')

# models
wrtdsmods <- wqdat %>% 
  left_join(hyddat, by = 'date') %>% 
  mutate(
    res = log(chla), 
    res = ifelse(is.infinite(res), NA, res),
    lim = 0, 
  ) %>% 
  select(bay_segment, date, res, flo = hyd_est, lim) %>% 
  na.omit() %>% 
  group_by(bay_segment) %>% 
  nest() %>% 
  mutate(
    mod = purrr::map(data, function(x){
      
      out <- as.data.frame(x) %>% tidalmean %>%  
        modfit(flo_div = 50, fill_empty = T, wins = as.list(c(0.323088491810723, 14.9398949283111, 0.894609819824893)))
      
      return(out)
      
    })
  )

save(wrtdsmods, file = 'data/wrtdsmods.RData', compress = 'xz')
# 
# tmp <- wrtdsmods %>% 
#   filter(bay_segment %in% 'OTB') %>% 
#   pull(data) %>% 
#   .[[1]] %>% 
#   as.data.frame %>% 
#   tidalmean
# 
# library(doParallel)
# ncores <- detectCores() - 1  
# registerDoParallel(cores = ncores)
# res <- winsrch_optim(tmp)

