# setup -------------------------------------------------------------------

library(dataRetrieval)
library(rnoaa)
library(tidyverse)
library(lubridate)
library(tbeptools)
library(WRTDStidal)
library(haven)
library(readxl)

chldat <- epcdata

# key for noaa data
mykey <- Sys.getenv("NOAA_KEY")

# # get hydroload estimates -------------------------------------------------
# 
# hydest <- tibble(
#   yr = 1974:2019
#   ) %>%
#   group_by(yr) %>%
#   nest %>%
#   mutate(
#     data = purrr::pmap(list(yr), function(yr){
# 
#       cat(yr, '\t')
# 
#       start <- paste0(yr, "-01-01")
#       end <- paste0(yr, "-12-31")
# 
#       # get rainfall data at station
#       tia_rainfall <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00012842",
#                            datatypeid = "PRCP", startdate = start, enddate = end,
#                            limit = 500, add_units = TRUE, token = mykey)
# 
#       # convert rain data to inches
#       tia_rain <- tia_rainfall$data %>%
#         mutate(daily_in = (value/254),
#                Date = as.Date(date))
# 
#       # get hydrological data, 0060 is discharge as cfs, converted to million m3/day
#       # converting ft3/s * 86400 s/d / 0.0283168 m3/ft3 * 1,000,000
#       bkr<- readNWISdv("02307359", "00060", start, end) %>%
#         renameNWISColumns() %>%
#         mutate(bkr_flow = (Flow*3.05119225))
# 
#       out <- left_join(tia_rain, bkr, by=c("Date")) %>%
#         select(Date, daily_in, bkr_flow) %>%
#         mutate(hyd_est = (154.22+(8.12*bkr_flow)+(6.73*daily_in))/365) %>%
#         drop_na(hyd_est)
# 
#       return(out)
# 
#     })
#   ) %>%
#   unnest('data')
# 
# save(hydest, file = 'data/hydest.RData', compress = 'xz')

# get hydroload estimates from Janicki ------------------------------------

# this file replaces the original hydest in the commented code above
# it is direct from Janicki models and provides a better estimate of hydro load

# import, format

bayseg <- read_excel('data/Tampa Bay Loadings 1985-2016.xlsx', sheet = 'Segment ID') %>%
  rename(
    bayid = `Bay Segments`,
    bay_segment = `...2`
  ) %>%
  mutate(
    bay_segment = case_when(
      bayid %in% '4' ~ 'Lower Tampa Bay', 
      T ~ bay_segment
    ),
    bay_segment = factor(bay_segment,
      levels = c("Old Tampa Bay", "Hillsborough Bay", "Middle Tampa Baty", "Lower Tampa Bay", "Boca Cieaga Bay", "Terra Ceia Bay", "Manatee River"),
      labels = c('OTB', 'HB', 'MTB', 'LTB', 'BCB', 'TCB', 'MR')
    )
  )

# 1985-2016
hydest <- read_excel('data/Tampa Bay Loadings 1985-2016.xlsx', sheet = 'Monthly H2O Loads') %>%
  mutate(
    bayid = Month,
    dy = 1
  ) %>%
  left_join(bayseg, by = 'bayid') %>%
  unite('date', YEAR, MONTH, dy, sep = '-') %>%
  select(date, hyd_est = `H2O Load (million m3/month)`, bay_segment) %>%
  mutate(date = ymd(date)) %>%
  filter(!bay_segment %in% c('BCB', 'TCB', 'MR'))

# 2017-2019
hydestnew <- read_excel('data/H2OMonthlySeg1719.xlsx') %>% 
  mutate(
    dy = 1
  ) %>%
  unite('date', Year, Month, dy, sep = '-') %>%
  mutate(
    date = ymd(date), 
    Segment = factor(Segment, levels = c('OTB', 'HB', 'MTB', 'LTB'))
  ) %>% 
  select(date, bay_segment = Segment, hyd_est = 'H2O Load (10e6 m3/yr)')

hydest <- bind_rows(hydest, hydestnew) %>% 
  mutate(bay_segment = as.character(bay_segment))

save(hydest, file = 'data/hydest.RData', compress = 'xz')

# wrtds models ------------------------------------------------------------

data(hydest)

# water quality
wqdat <- epcdata %>% 
  filter(yr > 1974 & yr < 2020) %>% 
  rowwise() %>% 
  mutate(
    date = as.Date(SampleTime), 
    date = floor_date(date, unit = 'month')
  ) %>% 
  ungroup() %>% 
  select(-matches('\\_q$|^sd|^Temp|\\_Depth\\_m$|^Lat|^Lon|^yr$|^mo$|^SampleTime$')) %>% 
  mutate(sal = (Sal_Top_ppth + Sal_Mid_ppth + Sal_Bottom_ppth) / 3) %>% 
  group_by(bay_segment, date) %>% 
  summarise(
    tn = median(tn, na.rm = T), 
    chla = median(chla, na.rm = T), 
    sal = median (sal, na.rm = T),
    .groups = 'drop'
  )

# models
wrtdsmods <- wqdat %>% 
  mutate(
    res = log(chla), 
    res = ifelse(is.infinite(res), NA, res),
    lim = 0, 
  ) %>% 
  select(bay_segment, date, res, flo = sal, lim) %>% 
  na.omit() %>% 
  group_by(bay_segment) %>% 
  nest() %>% 
  mutate(
    mod = purrr::map(data, function(x){
      
      out <- as.data.frame(x) %>% tidal %>%  
        modfit(flo_div = 40, fill_empty = T, tau = 0.5)
      
      return(out)
      
    })
  )

save(wrtdsmods, file = 'data/wrtdsmods.RData', compress = 'xz')

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

# loading model results from janicki --------------------------------------

# the original TB Loads folder was on the T drive in NMC committee folder
fls <- list.files(path = '~/Desktop/TBEP/TB_LOADS', recursive = T, full.names = T)
fls <- grep('totn.*month', fls, value = T)
dat <- tibble(
    fl = fls
  ) %>% 
  group_by(fl) %>% 
  nest %>% 
  mutate(
    data = purrr:::pmap(list(fl), function(fl){
      
      out <- read_sas(fl)
      names(out) <- tolower(names(out))
      # out <- out %>% 
      #   select(bay_seg, year, month, tnload, h2oload, source)
      # 
      return(out)
      
    })
  ) %>% 
  unnest(data) %>%
  ungroup %>% 
  select(-fl) %>% 
  filter(bay_seg == 1) %>% 
  mutate(dy = 1) %>% 
  unite('date', year, month, dy, sep = '-') %>% 
  mutate(date = ymd(date)) %>% 
  group_by(date) %>% 
  summarise(
    tnload = sum(tnload, na.rm = T), 
    h2oload = sum(h2oload, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    tnload = case_when(
      date <= as.numeric(as.Date('2011-12-01')) ~ tnload * 0.00110231, 
      T ~ tnload
    )
  )

loadmoddat <- dat

save(loadmoddat, file = 'data/loadmoddat.RData', compress = 'xz')

