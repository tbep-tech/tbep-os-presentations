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