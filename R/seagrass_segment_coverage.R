library(tidyverse)
library(sf)
library(here)
library(tbeptools)
library(magick)
library(gridExtra)

# copy sgsegest from seagrass-analysis --------------------------------------------------------

file.copy(from = 'T:/04_STAFF/MARCUS/03_GIT/seagrass-analysis/sgsegest.RData', 
          to = here('data/sgsegest.RData'))

# coverage animation --------------------------------------------------------------------------

ls <- list.files('T:/04_STAFF/MARCUS/03_GIT/hmpu-workflow/data/', pattern = '^sgdat', full.names = T) %>% 
  enframe %>% 
  mutate(
    fl = value,
    yr = gsub('^sgdat|\\.RData$', '', basename(fl))
  ) %>% 
  select(yr, fl)

sgdat <- ls %>% 
  group_by(yr, fl) %>% 
  nest() %>% 
  mutate(
    data = purrr::map(fl, function(fl){
      
      cat(fl, '\n')
      
      load(fl)
      
      dat <- get(gsub('\\.RData$', '', basename(fl))) %>% 
        filter(FLUCCSCODE %in% c(9113, 9116)) %>% 
        st_geometry() %>% 
        st_union() %>% 
        st_simplify(dTolerance = 50) %>% 
        st_union() %>% 
        st_make_valid()
      
      return(dat)
      
    })
  )

sgdat <- sgdat %>% 
  ungroup() %>% 
  select(-fl) %>% 
  unnest(data) %>% 
  st_as_sf()

save(sgdat, file = here::here('data/sgdat.RData'), compress = 'xz')

load(file = here('data/sgdat.RData'))

toplo <- tibble::tibble(
  Year = seq(1988, 2024)
) %>%
  dplyr::left_join(seagrass, by = 'Year', ) %>%
  dplyr::mutate(
    Acres = Acres / 1000,
    ind = 1:nrow(.)
  )

# axis labels
lbs <- c(toplo$Year)
brks <- c(toplo$ind)
lbs2 <- lbs
lbs2[as.numeric(lbs) %% 2 != 0] <- ''

frm <- st_bbox(sgdat) %>% 
  st_as_sfc()

for(yr in sgdat$yr){
  
  cat(yr, '\n')
  
  toplocov <- sgdat %>% 
    filter(yr == !!yr)
  
  p1 <- ggplot() + 
    geom_sf(data = frm, fill = NA, color = NA) +
    geom_sf(data = toplocov, fill = 'darkgreen', color = 'darkgreen') + 
    theme_void() +  
    theme(
      plot.margin = margin(0,0,0,0, 'cm')
    ) + 
    labs(title = yr)
  
  maxyr <- yr
  
  # label for last bar
  lastlab <- seagrass %>%
    filter(Year == maxyr) %>%
    pull(Acres) %>%
    round(0) %>%
    format(big.mark = ',') %>%
    paste(., 'acres')
  
  # y loc for last bar label
  lasty <- seagrass %>%
    filter(Year == maxyr) %>%
    pull(Acres) %>%
    `/`(1000) %>%
    `-`(1)
  
  toplo2 <- toplo %>% 
    filter(Year <= maxyr)
  
  p2 <- ggplot2::ggplot(na.omit(toplo2), ggplot2::aes(x = ind, y = Acres)) +
    ggplot2::geom_col(fill = '#00806E', colour = 'black', width = 1.4) +
    ggplot2::geom_segment(x = 0, xend = 32, y = 38, yend = 38, col = 'red', size = 2) +
    ggplot2::geom_segment(x = 32, xend = nrow(toplo) + 1, y = 40, yend = 40, col = 'red', size = 2) +
    # ggplot2::annotate("text", label = "Seagrass Coverage Goal", x = 4, y = 40.5, color = 'red', size = 5, hjust = 0) +
    ggplot2::annotate('text', x = brks[which(lbs == maxyr)], y = lasty, label = lastlab, angle = 90, hjust = 1, vjust = 0.3, size = 3, color = 'white') +
    ggplot2::scale_x_continuous(breaks = brks, labels = lbs2, limits = c(0, max(brks) + 1), expand = c(0.01, 0.01)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1 * max(toplo$Acres, na.rm = T))) +
    ggplot2::theme_grey() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(),
      panel.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.title.x = ggplot2::element_blank(),
      legend.position = 'none',
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    ) +
    labs(
      y = 'Seagrass Coverage (x1,000 acres)'
    )  
  
  flnm <- paste0('figure/sgdat', yr, '.png')
  
  png(flnm, height = 3.5, width = 8, units = 'in', res = 200)
  grid.arrange(p1, p2, ncol = 2, widths = c(1, 1))
  dev.off()
  
}

list.files(path = here('figure'), pattern = '^sgdat', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write(here('figure/sgcovani.gif')) 

# coverage animation Old Tampa Bay ------------------------------------------------------------

load(file = here('data/sgsegest.RData'))
load(file = here('data/sgdat.RData'))

otbsegshed <- tbsegshed %>% 
  filter(bay_segment == 'OTB') %>% 
  st_transform(crs = st_crs(sgdat))

otbsgdat <- sgdat %>% 
  st_intersection(otbsegshed)

otbsgsegest <- sgsegest %>%
  filter(segment == 'Old Tampa Bay') %>% 
  select(
    Year = year, 
    Acres = acres
    )

toplo <- tibble::tibble(
  Year = seq(1988, 2024)
) %>%
  dplyr::left_join(otbsgsegest, by = 'Year') %>%
  dplyr::mutate(
    Acres = Acres / 1000,
    ind = 1:nrow(.)
  )

# axis labels
lbs <- c(toplo$Year)
brks <- c(toplo$ind)
lbs2 <- lbs
lbs2[as.numeric(lbs) %% 2 != 0] <- ''

frm <- st_bbox(otbsgdat) %>% 
  st_as_sfc()

for(yr in otbsgdat$yr){
  
  cat(yr, '\n')
  
  toplocov <- otbsgdat %>% 
    filter(yr == !!yr)
  
  p1 <- ggplot() + 
    geom_sf(data = frm, fill = NA, color = NA) +
    geom_sf(data = toplocov, fill = 'darkgreen', color = 'darkgreen') + 
    theme_void() +  
    theme(
      plot.margin = margin(0,0,0,0, 'cm')
    ) + 
    labs(title = yr)
  
  maxyr <- yr
  
  # label for last bar
  lastlab <- otbsgsegest %>%
    filter(Year == maxyr) %>%
    pull(Acres) %>%
    round(0) %>%
    format(big.mark = ',') %>%
    paste(., 'acres')
  
  # y loc for last bar label
  lasty <- otbsgsegest %>%
    filter(Year == maxyr) %>%
    pull(Acres) %>%
    `/`(1000) %>%
    `-`(0.1)
  
  toplo2 <- toplo %>% 
    filter(Year <= maxyr)
  
  p2 <- ggplot2::ggplot(na.omit(toplo2), ggplot2::aes(x = ind, y = Acres)) +
    ggplot2::geom_col(fill = '#00806E', colour = 'black', width = 1.4) +
    # ggplot2::geom_segment(x = 0, xend = nrow(toplo) + 1, y = 11.1, yend = 11.1, col = 'red', size = 2) +
    # ggplot2::annotate("text", label = "Seagrass Coverage Goal", x = 4, y = 40.5, color = 'red', size = 5, hjust = 0) +
    ggplot2::annotate('text', x = brks[which(lbs == maxyr)], y = lasty, label = lastlab, angle = 90, hjust = 1, vjust = 0.3, size = 3, color = 'white') +
    ggplot2::scale_x_continuous(breaks = brks, labels = lbs2, limits = c(0, max(brks) + 1), expand = c(0.01, 0.01)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1 * max(toplo$Acres, na.rm = T))) +
    ggplot2::theme_grey() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(),
      panel.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.title.x = ggplot2::element_blank(),
      legend.position = 'none',
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    ) +
    labs(
      y = 'Seagrass Coverage (x1,000 acres)'
    )  
  
  flnm <- paste0('figure/otbsgdat', yr, '.png')
  
  png(flnm, height = 3.5, width = 8, units = 'in', res = 200)
  grid.arrange(p1, p2, ncol = 2, widths = c(1, 1))
  dev.off()
  
}

list.files(path = here('figure'), pattern = '^otbsgdat', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write(here('figure/otbsgcovani.gif')) 
