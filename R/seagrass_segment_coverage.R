library(tidyverse)
library(sf)
library(here)

fls <- list.files(path = 'T:/04_STAFF/MARCUS/03_GIT/hmpu-workflow/data', pattern = 'sgdat', full.names = T)

for(fl in fls){
  cat(fl, '\n')
  load(file = fl)
}

st_layers('T:/05_GIS/SWFWMD/Seagrass/2022_Seagrass/provisional/DraftMaps2022_1130.gdb/DraftMaps2022_1130.gdb')

sgdat2022 <- st_read('T:/05_GIS/SWFWMD/Seagrass/2022_Seagrass/provisional/DraftMaps2022_1130.gdb/DraftMaps2022_1130.gdb', 
                     layer = 'Seagrass_in_2022_Suncoast') %>% 
  select(FLUCCSCODE = FLUCCS_Code) %>% 
  filter(FLUCCSCODE %in% c(9113, 9116)) %>% 
  st_cast('MULTIPOLYGON')

levs <- c('oldTampaBay', 'hillsboroughBay', 'middleTampaBay', 'lowerTampaBay', 'bocaCiegaBay', 'terraCieaBay', 'manateeRiver')
labs <- c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Boca Ciega Bay', 'Terra Ceia Bay', 'Manatee River')

segswfwmd <- st_read('T:/05_GIS/SWFWMD/Seagrass/2022_Seagrass/provisional/DraftMaps2022_1130.gdb/DraftMaps2022_1130.gdb', 
                     layer = 'suncoastSeagrassSegments') %>% 
  filter(waterbodyName %in% levs) %>% 
  mutate(
    waterbodyName = factor(waterbodyName, levels = levs, labels = labs)
  ) %>% 
  select(segment = waterbodyName)


sgyrs <- fls %>% 
  basename %>% 
  gsub('\\.RData$', '', .) %>% 
  c(., 'sgdat2022')

sgsegest <- NULL
for(sgyr in sgyrs){
  
  cat(sgyr, '\n')
  
  yr <- gsub('sgdat', '', sgyr)
  
  dat <- get(sgyr) %>% 
    filter(FLUCCSCODE %in% c(9113, 9116)) %>% 
    st_union() %>% 
    st_transform(crs = st_crs(segswfwmd)) %>% 
    st_intersection(segswfwmd, .) %>% 
    mutate(
      acres = st_area(.), 
      acres = units::set_units(acres, 'acres'), 
      acres = as.numeric(acres)
    ) %>% 
    st_set_geometry(NULL) %>% 
    mutate(
      year = as.numeric(yr)
    )
  
  sgsegest <- rbind(sgsegest, dat)
  
}

save(sgsegest, file = here('data/sgsegest.RData'))
