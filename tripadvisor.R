

trip_lowerleft <- c(52.54447048576368, 13.397322756984629)
trip_upperright <- c(52.54934416088687, 13.423007589404982)

bbox <- sf::st_bbox(shp)

lat_diff <- trip_upperright[1] - trip_lowerleft[1]
lon_diff <- trip_upperright[2] - trip_lowerleft[2]

lat_seq <- seq(bbox[2],bbox[4],by=lat_diff)
lon_seq <- seq(bbox[1],bbox[3],by=lon_diff)


i <- 1

ta_list <- list()


while( i <= length(lat_seq)){
  j <- 1
  while(j <= length(lon_seq)){
    
    next_x <- lon_seq[j]
    next_y <- lat_seq[i]
    
    nextURL <- paste0("https://www.tripadvisor.com.tr/GMapsLocationController?Action=update&from=Restaurants&g=187323&geo=187323&mapProviderFeature=ta-maps-gmaps3&validDates=false&mc=",next_y,",",next_x,"&mz=17&mw=2360&mh=841&pinSel=v2&origLocId=187323&finalRequest=false&includeMeta=false&trackPageView=false")
    
    ta_json <- fromJSON(nextURL)
    
    ta_list <- append(ta_list, list(ta_json))
    
    message(paste0((i-1)*length(lon_seq) + j,"of",length(lon_seq)*length(lat_seq)))
    
    j <- j + 1
    
    
  }
  
  i <- i + 1
  
  
}



  
 write_json(ta_list,"ta_json.json")
 
 
 #analysis
 
 ta_list <- jsonlite::read_json("ta_json.json")
 
 hotels <- lapply(ta_list, function(x) x$hotels)
 rest <- lapply(ta_list, function(x) x$restaurants)
 attr <- lapply(ta_list, function(x) x$attractions)

 rest <- rest[sapply(rest, is.data.frame)]
 rest_df <- rest %>% bind_rows()
 rest_df <- rest_df %>% distinct(url,.keep_all = T)
 rest_df
  