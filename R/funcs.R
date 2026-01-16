# plotly function for threshold plots
thrplotly <- function(epcdata, bay_segment, maxyr, family, themein){
  
  p1 <- show_thrplot(epcdata, bay_segment = bay_segment, thr = "chla", yrrng = c(1975, maxyr), family = family, txtlab = F, labelexp = F) + 
    ggtitle(NULL) +
    themein +
    scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(1975, maxyr))
  p2 <- show_thrplot(epcdata, bay_segment = bay_segment, thr = "la", yrrng = c(1975, maxyr), family = family, txtlab = F, labelexp = F) + 
    ggtitle(NULL) +
    themein + 
    scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(1975, maxyr))
  
  p3 <- show_segmatrix(epcdata, bay_segment = bay_segment, yrrng = c(1975, maxyr), txtsz = NULL) + 
    scale_y_continuous(expand = c(0,0), breaks = c(1975:maxyr)) +
    coord_flip() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text = element_text(size = 9), 
      text = element_text(family = family)
    ) 
  
  p3 <- ggplotly(p3, tooltip = 'Result') 
  
  p1 <- ggplotly(p1)
  p2 <- ggplotly(p2)
  
  for(i in 1:length(p1$x$data)) p1$x$data[[i]]$showlegend <- FALSE   
  for(i in 1:length(p2$x$data)) p2$x$data[[i]]$showlegend <- FALSE   
  for(i in 1:length(p3$x$data)) p3$x$data[[i]]$showlegend <- FALSE   
  
  # m <- list(
  #   l = 10,
  #   r = 10,
  #   b = 10,
  #   t = 0,
  #   pad = 0
  # )
  # 
  out <- subplot(p1, p3, p2, nrows = 3, heights = c(0.4, 0.2, 0.4), shareX = T, titleY = TRUE) %>% 
    layout(autosize = F, height = 610, width=750
           # yaxis = list(automargin = T), 
           # xaxis = list(automargin = T), 
           # margin = m
    )
  
  return(out)
  
}

# get datasets from repo
# try simple load, download if fail
rdataload <- function(dataurl = NULL){
  
  x <- gsub('\\.RData', '', basename(dataurl))
  
  # try simple load
  ld <- try(load(url(dataurl)), silent = T)
  
  # return x if load worked
  if(!inherits(ld, 'try-error')){
    out <- get(x)
  }

  # download x if load failed
  if(inherits(ld, 'try-error')){
    
    fl <- paste(tempdir(), basename(dataurl), sep = '/')
    download.file(flurl, destfile = fl, quiet = T)
    load(file = fl)
    out <- get(x)
    suppressMessages(file.remove(fl))
    
  }
  
  return(out)
    
}

# plot annual rainfall using SWFWMD data
rainplo_fun <- function(){

  # rain data for relevant TB areas
  raindat <- readxl::excel_sheets(here('data/swfwmdrainfallto2026.xlsx')) %>% 
    grep('jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec', ., value = TRUE) %>% 
    tibble(
      mo = .
    ) %>% 
    nest(.by = mo) %>% 
    mutate(
      data = purrr::map(mo, function(mo){
        
        read_excel(here('data/swfwmdrainfallto2026.xlsx'), sheet = mo, skip = 1) %>% 
          filter(Year %in% 1975:maxyr) %>% 
          select(
            yr = Year, 
            tampacoastal_in = `Tampa Bay/Coastal Areas`, # this just a fringe area around the bay, not the watershed
            hillsborough_in = `Hillsborough River`,
            alafia_in = `Alafia River`,
            littlemanatee_in = `Little Manatee River`,
            manatee_in = `Manatee River`
          ) %>% 
          mutate_all(as.numeric)
        
      })
    ) %>% 
    unnest('data') %>% 
    # mutate(
    #   precip_in = rowSums(select(., -mo, -yr), na.rm = TRUE)
    # ) %>%
    select(mo, yr, precip_in = tampacoastal_in) %>% 
    mutate(
      mo = gsub('\\-usgsbsn$', '', mo),
      mo = as.numeric(factor(mo,
                            levels = c('jan', 'feb', 'mar', 'apr', 'may', 'jun',
                                        'jul', 'aug', 'sep', 'oct', 'nov', 'dec'),
                            labels = 1:12)
      ),
      date = as.Date(paste0(yr, '-', mo, '-01')), 
      precip_mm = precip_in * 25.4
    )

  toplo <- raindat %>% 
    filter(yr > 2009) %>%
    summarise(
      precip_in = sum(precip_in, na.rm = TRUE), 
      .by = yr
    ) |> 
    mutate(
      ave = mean(precip_in), 
      avediff = precip_in - ave
    )


  ave <- mean(toplo$precip_in)

  p1 <- ggplot(toplo, aes(x = yr, y = precip_in)) + 
    geom_col(fill = '#004F7E', color = 'black', alpha = 0.8) +
    geom_hline(yintercept = ave, color = '#5C4A42', linewidth = 1) +
    theme_minimal(base_size = 14) + 
    scale_x_continuous(breaks = unique(toplo$yr)) +
    scale_y_continuous(expand= c(0, 0)) +
    theme(
      axis.text.x =  element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(), 
    ) +
    labs(
      x = NULL,
      y = 'Annual rainfall (inches)',
      # caption = 'Data source: SWFWMD'
    )

  mxdiff <- max(toplo$avediff)
  p2 <- ggplot(toplo, aes(x = yr, y = avediff, fill = avediff)) + 
    geom_col(color = 'black', alpha = 0.8) +
    geom_hline(yintercept = 0, color = '#5C4A42', linewidth = 1) +
    scale_fill_gradient2(midpoint = 0, low = 'tomato1', mid = 'white', high = 'dodgerblue2') +
    theme_minimal(base_size = 14) + 
    scale_x_continuous(breaks = unique(toplo$yr)) +
    scale_y_continuous(expand= c(0, 0)) +
    theme(
      axis.text.x = element_text(angle = 45, size = 10, hjust = 1),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.y = element_blank(),
      legend.position = 'none'
    ) +
    labs(
      x = NULL,
      y = 'Deviation (inches)',
      caption = 'Data source: SWFWMD'
    )

  p1 + p2 + plot_layout(ncol = 1, heights = c(1, 0.5))

}

# plot pyrodinium by time/lat, data from FWRI
pyroplo_fun <- function(yrmin){

  bridges <- tibble(
      brdg = c('Gandy', 'HF', 'CC'),
      Latitude = c(27.880486, 27.922358652608654, 27.965458)
    ) |> 
    crossing(
      yr = seq(yrmin, maxyr)
    )

  # https://f50006a.eos-intl.net/F50006A/OPAC/Details/Record.aspx?BibCode=5635517
  datall <- read.csv('https://f50006a.eos-intl.net/ELIBSQL12_F50006A_Documents/OTBMP_Pyrodinium_Chl_2011-2020_v101922.csv') %>%
    select(
      yr = Year,
      date = Sample_Date,
      Latitude,
      Longitude,
      pyro = P..bahamense.Abundance..cells.L.
    ) %>%
    mutate(date = mdy(date))

  # 2021 only
  dat2021 <- read.csv(here('data/Pyrodinium_Chl_2021_OTBMP_mbeck.csv')) %>%
    select(
      date = Sample_Date,
      Latitude,
      Longitude,
      pyro = Pbahamense..cells.L.
    ) %>%
    mutate(
      date = case_when(
        grepl('[a-z]', date) ~ dmy(date),
        T ~ mdy(date)
      )
    )

  # 2022 only
  dat2022 <- read.csv(here('data/Pyrodinium_Chla_OTBMP_2022.csv')) %>%
    select(
      date = Date,
      Latitude,
      Longitude,
      pyro = Pyrodinium..Cells.L.
    ) %>%
    mutate(date = mdy(date))

  # 2023 only
  dat2023 <- read_excel(here('data/2023 OTB Pyrodinium bahamense abundance data.xlsx')) %>% 
    select(
      date = `Sample Date`,
      Latitude,
      Longitude,
      pyro = `Pyrodinium bahamense abundance (cells/L)`
    ) %>%
    mutate(date = ymd(date))

  # 2024, 2025
  dat2425 <- read_excel(here('data/TB Pyro 2024-2025.12.03.xlsx')) |> 
    select(
      date = `Sample Date`,
      Latitude,
      Longitude,
      pyro = `Pyrodinium bahamesnse abundance (cells/L*)`
    ) %>%
    mutate(date = ymd(date))

  brks <- c(-Inf, 1e4, 1e5, 1e6, Inf)
  labs <- c('No bloom', 'Low', 'Medium', 'High')

  dat <- bind_rows(datall, dat2021, dat2022, dat2023, dat2425) %>%
    mutate(
      yr = year(date),
      doy = yday(date),
      pyro = ifelse(pyro == 0, NA, pyro),
      pyrocat = cut(pyro, breaks = brks, labels = labs),
      pyro = pmin(3e6, pyro)
    )

  toplo <- dat %>% 
    filter(yr >= yrmin)

  # doy x-axis breaks and labels
  dts <- seq.Date(as.Date('2023-01-01'), as.Date('2023-10-01'), by = "3 month")
  xbrks <- yday(dts)                       
  xlabs <- format(dts, "%b")

  p <- ggplot(subset(toplo, !is.na(pyro)), aes(x = doy, y = Latitude)) +
    geom_hline(data = bridges, aes(yintercept = Latitude), linetype = 'solid') +
    geom_point(data = subset(toplo, is.na(pyro)), aes(shape = "No cells"),
              size = 1, color = "lightgrey") +
    geom_point(aes(fill = pyrocat, size = pyro), shape = 21, color = 'black') +
    scale_x_continuous(limits = c(0, 365), breaks = xbrks, labels = xlabs) +
    scale_fill_viridis_d(guide = "legend", option = 'A', direction = -1, na.value = 'lightgrey') +
    scale_size_continuous(range = c(1, 5), breaks = c(1e6, 2e6, 3e6), labels = c('1e6', '2e6', '> 3e6')) +
    facet_wrap(~yr, ncol = 4) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      panel.grid = element_blank(),
      legend.spacing.y = unit(-0.2, "cm")
    ) +
    guides(fill = guide_legend(override.aes = list(size = 3), order = 2),
          size = guide_legend(order = 3),
          shape = guide_legend(order = 1)) +
    labs(
      x = 'Day of Year',
      shape = NULL,
      fill = 'Bloom intensity\n',
      size = 'Cells/L\n',
      subtitle = expression(paste(italic('P. bahamense'), ' cell counts by location and date in Old Tampa Bay')),
      caption = 'Data from FWC-FWRI HAB Monitoring Database, solid lines are bridge locations'
    )
  
  return(p)

}

# nekton annual map, pulled from the nekton dashboard
tbnimap_fun <- function(yr, fimdata){

  tbniscr <- tbeptools::anlz_tbniscr(fimdata)
  avedat <- tbeptools::anlz_tbniave(tbniscr) 
  data('fimstations', package = 'tbeptools')
  data('tbseg', package = 'tbeptools')
  perc <- c(32, 46)
  cols <- c('#CC3231', '#E9C318', '#2DC938')

  tbniyrs <- tbniscr %>%
    dplyr::select(Reference, Year, TBNI_Score, dplyr::matches('Score')) %>% 
    tidyr::gather('var', 'val', -Reference, -Year) %>% 
    dplyr::group_by(Reference, Year, var) %>% 
    dplyr::summarise(val = mean(val, na.rm = T)) %>% 
    tidyr::spread(var, val) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      Action = findInterval(TBNI_Score, perc),
      outcome = factor(Action, levels = c('0', '1', '2'), labels = c('red', 'yellow', 'green')),
      outcome = as.character(outcome),
      Action = factor(Action, levels = c('0', '1', '2'), labels = c('On Alert', 'Caution', 'Stay the Course'))
    )

  # fimstations, clip by segment extents
  tbnistations <- fimstations %>% 
    dplyr::left_join(tbniscr, by = 'Reference') %>% 
    dplyr::select(Reference, Year, Month, TBNI_Score, NumTaxa, BenthicTaxa, TaxaSelect, NumGuilds, Shannon) %>% 
    dplyr::mutate(
      Action = findInterval(TBNI_Score, perc),
      outcome = factor(Action, levels = c('0', '1', '2'), labels = c('red', 'yellow', 'green')),
      outcome = as.character(outcome),
      Action = factor(Action, levels = c('0', '1', '2'), labels = c('On Alert', 'Caution', 'Stay the Course'))
    )

  toplo <- tbniyrs %>% 
      dplyr::filter(Year %in% yr) %>% 
      dplyr::select(Reference, TBNI_Score, Action, outcome) %>% 
      dplyr::right_join(fimstations, ., by = 'Reference')  %>% 
      dplyr::mutate(
        outcome = dplyr::case_when(
          outcome == 'green' ~ '#2DC938', 
          outcome == 'yellow' ~ '#E9C318', 
          outcome == 'red' ~ '#CC3231'
        )
      )

  # segment averages
  attyr <- avedat %>% 
    dplyr::filter(Year %in% yr) %>% 
    dplyr::left_join(tbseg, ., by = 'bay_segment') %>% 
    dplyr::mutate(
      outcome = dplyr::case_when(
        outcome == 'green' ~ '#2DC938', 
        outcome == 'yellow' ~ '#E9C318', 
        outcome == 'red' ~ '#CC3231'
      )
    )

  # map with custom legends
  m <- tbeptools::util_map(attyr, minimap = NULL) %>%
    leaflet::clearMarkers() %>% 
    leafem::removeMouseCoordinates() %>%
    leaflet::addPolygons(
      data = attyr, 
      stroke = T, 
      color = 'grey', 
      weight = 1, 
      layerId = ~long_name, 
      fillColor = ~outcome, 
      fillOpacity = 0.3,
      label = ~paste0(bay_segment, ': ', long_name, '\nTBNI: ', Segment_TBNI, '\nAction: ', Action), 
      labelOptions = leaflet::labelOptions(style = list("white-space" = "pre"))
    ) %>% 
    leaflet::addCircleMarkers(
      data = toplo, 
      layerId = ~Reference,
      stroke = TRUE,
      color = 'black',
      fill = TRUE,
      fillColor = ~outcome,
      weight = 1,
      fillOpacity = 1,
      radius= 4,#~scales::rescale(val, from = scls, to = c(5, 20)),
      label = ~paste0('Site: ', Reference, '\nTBNI: ', round(TBNI_Score, 1), '\nAction: ', Action),
      labelOptions = leaflet::labelOptions(style = list("white-space" = "pre"))
    ) %>% 
    leaflet::addLegend("topright", labels = c("Stay the Course", "Caution", "On Alert"), colors = rev(cols), title = "Bay segment/site <br>matrix outcomes", opacity = 1)

  return(m)

}

# annual tbbi map
tbbimap_fun <- function(yr){

  benmed <- rdataload('https://github.com/tbep-tech/benthic-dash/raw/refs/heads/main/data/benmed.RData')
  benpts <- rdataload('https://github.com/tbep-tech/benthic-dash/raw/refs/heads/main/data/benpts.RData')
  segs <- rdataload('https://github.com/tbep-tech/benthic-dash/raw/refs/heads/main/data/segs.RData')

  cols <- c('#CC3231', '#E9C318', '#2DC938')

  benmap <- tbeptools::util_map(segs, minimap = NULL) |> 
    leaflet::addLegend("topright", labels = c("Good", "Fair", "Poor"), colors = rev(cols), title = "TBBI Category", opacity = 1)
 
  # polygons
  benpol <- benmed %>% 
    dplyr::filter(yr == !!yr) %>% 
    dplyr::inner_join(segs, ., by = 'bay_segment')
  
  # points
  benpts <- benpts %>% 
    dplyr::filter(yr == !!yr) %>% 
    dplyr::filter(FundingProject == 'TBEP')
  
  # map with custom legends
  m <- benmap %>% 
    leaflet::clearMarkers() %>% 
    leaflet::clearShapes() %>% 
    leaflet::addPolygons(
      data = benpol, 
      stroke = T, 
      color = 'grey', 
      weight = 1, 
      layerId = ~bay_segment, 
      fillColor = ~outcome, 
      fillOpacity = 0.3,
      label = ~paste0(bay_segment, ': ', long_name, ', TBBI: ', TBBICat)
    ) %>% 
    leaflet::addCircleMarkers(
      data = benpts, 
      layerId = ~StationID,
      stroke = TRUE,
      color = 'black',
      fill = TRUE,
      fillColor = ~outcome,
      weight = 1,
      fillOpacity = 1,
      radius= 4,
      label = ~paste0('StationNumber ', StationNumber, ', TBBI: ', round(TBBI, 1), ', Category: ', TBBICat)
    )
  
  return(m)

}

# seagrass fo change between years
# uses avg placement length for each transect to account for differing lengths between years
# this differs from tbeptools standard approach
sgfomap_fun <- function(transect, trnpts){

  # get complete data, fill all species not found with zero
  datcmp <- transect %>%
    dplyr::filter(var %in% 'Abundance') %>%
    dplyr::mutate(
      Savspecies = ifelse(grepl('Caulerpa', Savspecies), 'Caulerpa',
        ifelse(grepl('^AA', Savspecies), 'AA',
          ifelse(grepl('^DA', Savspecies), 'DA',
            ifelse(grepl('Dapis', Savspecies), 'Dapis',
              ifelse(grepl('Chaetomorpha', Savspecies), 'Chaetomorpha',
                Savspecies
              )
            )
          )
        )
      )
    ) %>%
    dplyr::select(Date, Transect, Site, Savspecies, bb = aveval) %>%
    dplyr::group_by(Date, Transect, Site, Savspecies) %>%
    dplyr::summarise(bb = mean(bb, na.rm = T), .groups = 'drop') %>%
    dplyr::ungroup() %>%
    tidyr::complete(Savspecies, tidyr::nesting(Date, Transect, Site), fill = list(bb = 0))

  # make no cover a five for bb if nothing else found
  datcmp <- datcmp %>%
    dplyr::group_by(Transect, Date, Site) %>%
    dplyr::mutate(
      bb = ifelse(Savspecies == 'No Cover', 0, bb),
      bb = ifelse(sum(bb[!Savspecies %in% 'No Cover']) == 0 & Savspecies == 'No Cover', 5, bb)
    )

  # get avg placements across dates by transect
  plcmnt <- datcmp %>%
    dplyr::group_by(Date, Transect) %>%
    dplyr::summarize(cnts = length(unique(Site)), .groups = 'drop') %>%
    dplyr::group_by(Transect) %>%
    dplyr::summarize(plcmnt = mean(cnts, na.rm = T), .groups = 'drop')
  
  tots <- datcmp %>%
    dplyr::filter(Savspecies %in% c('Halodule', 'Syringodium', 'Thalassia', 'Ruppia', 'Halophila')) %>%
    dplyr::group_by(Date, Transect, Site) %>%
    dplyr::summarise(bb = mean(bb), .groups = 'drop') %>%
    dplyr::mutate(
      Savspecies = 'total'
    )

  transectoccave <- datcmp %>%
    dplyr::bind_rows(tots) %>%
    left_join(plcmnt, by = 'Transect') %>%
    dplyr::group_by(Date, Transect, Savspecies) %>%
    dplyr::summarize(
      foest = sum(bb > 0, na.rm = T) / unique(plcmnt),
      bbest = sum(bb, na.rm = T) / unique(plcmnt ),
      .groups = 'drop'
    )

  toplo <- transectoccave |> 
    ungroup() |> 
    mutate(
      yr = lubridate::year(Date)
    ) |> 
    filter(Savspecies %in% 'total') |> 
    filter(yr >= 2024) |> 
    select(Transect, yr, foest) |> 
    summarise(
      foest = mean(foest, na.rm = T), 
      .by = c(Transect, yr)
    ) |> 
    mutate(
      cnt = n(), 
      .by = Transect
    ) |> 
    filter(cnt == 2) |> 
    select(-cnt) |> 
    pivot_wider(
      names_from = yr, 
      values_from = foest
    ) |> 
    mutate(
      chg = abs(`2025` - `2024`),
      sgn = sign(`2025` - `2024`)
    )
  tomap <- inner_join(trnpts, toplo, by = c('TRAN_ID' = 'Transect')) |> 
    select(TRAN_ID, chg, sgn, '2024', '2025')

  # make leaflet map with points sized by change and colored by sign
  # add bay segment lines from tbseglines
  m <- leaflet(tomap) |> 
    addProviderTiles(providers$CartoDB.Positron) |> 
    addPolylines(data = tbseglines, color = 'blue', weight = 2, opacity = 0.5) |>
    addCircleMarkers(
      radius = ~ scales::rescale(chg, to = c(5, 15)),
      color = ~ ifelse(sgn > 0, '#2DC938', '#CC3231'),
      stroke = TRUE,
      weight = 1,
      fillOpacity = 0.8,
      label = ~ lapply(paste0('Transect: ', TRAN_ID, '<br>',
                      '2024 F.O.: ', scales::percent(`2024`, accuracy = 0.1), '<br>',
                      '2025 F.O.: ', scales::percent(`2025`, accuracy = 0.1), '<br>',
                      'Change: ', ifelse(sgn > 0, '+', ''), scales::percent(`2025` - `2024`, accuracy = 0.1)), htmltools::HTML)
    ) |> 
    addLegend(
      position = 'bottomright',
      colors = c('#2DC938', '#CC3231'),
      labels = c('Increase', 'Decrease'),
      title = 'Frequency of Occurrence Change (2024 - 2025)'
    )

  return(m)

}