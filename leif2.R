require(jsonlite)
require(tidyverse)
require(sf)
require(igraph)
require(ggraph)
require(ggsci)

 brln <- fromJSON("df_rest3415.json")
 br <- data.frame(matrix(nrow=3413,ncol=7))
 
 br$X1 <- brln$rest_name %>% as.character()
 br$X2 <- brln$lat %>% as.numeric()
 br$X3 <- brln$lon %>% as.numeric() 
 br$X4 <- brln$bew %>% as.character() %>%  parse_number() 
 br$X5 <- brln$rating %>% str_replace(",",".") %>% as.numeric()
 br$X6 <- brln$meal
 br$X7 <- brln$prices
 
 colnames(br) <- names(brln)
 
 
 ggplot(br) + geom_point(aes(x=lon,y=lat))
 
 #berlin <- berlin %>% st_as_sf(coords=c("X3","X2")) %>% st_set_crs(4326)
 
 shp <- read_sf("plz.shp/plz.shp")
 
 df_sub  <- read_csv("C:/Users/Efe/Desktop/Sommer 2021/Econometrics II/lieferando.csv")
 df_sub <- df_sub %>% distinct(url,.keep_all = TRUE)
 df_sub <- df_sub[df_sub$url != "{{RestaurantUrl}}",]

 df_sub$check <- df_sub$name %in% br$rest_name 
 br$check <- br$rest_name %in% df_sub$name 

 df_sub <- df_sub[df_sub$check,]
 

 br$check2 <- FALSE 

 
 for(i in 1:length(br$check2)) {
    br$check2[i] <- br$rest_name[i] == df_sub$name[i]
 }



 br <- br[-c(887,888),] 
 

 
 br$url <- df_sub$url
 
 
 lieferando <- read.csv("df_all.csv",encoding = "UTF-8")
 
 lieferando <- lieferando[!lieferando$name %>% str_detect("[{{]"),]
 
 lieferando <- left_join(lieferando,br,by="url")
 
 lieferando$check3 <- is.na(lieferando$lat)
 
 
 lieferando <- lieferando[!lieferando$check3,]
 
 
 lieferando <- lieferando %>% filter(lat > 52 & lon > 13 & lon < 13.8)
 br <- br %>% filter(lat > 52 & lon > 13 & lon < 13.8)
 


 

 
 
  

 df_graph <- lieferando %>% select(url,PLZ,lat,lon,rest_name)
 ig_graph <- graph_from_data_frame(df_graph,directed = TRUE,)
 
 lyt <- create_layout(ig_graph,layout = "auto")
 
 lyt_which <- lyt$name %in% br$url
 br_which <- br$url %in% lyt$name 
 
 br$check3 <- br_which
 
 br_coord <- br %>% filter(check3) %>% select(rest_name,url,lon,lat)
 
 
 shp <- shp %>% st_transform(4326)
 shp_center <- shp %>% st_centroid() %>% sf::st_transform(4326)
 shp_center$check <- FALSE
 shp_center$check <- shp_center$plz %in% lyt$name
 
 sc_coord <- shp_center %>% st_coordinates()
 plz <- lyt[lyt$y == 1,]
 plz$check <- plz$name %in% shp_center$plz
 
 plz <- left_join(plz,shp_center, by=c("name"="plz"))
 
 shp_center$check <- shp_center$plz %in% plz$name 
 shp_center <- shp_center %>% distinct(plz)
 
 
 
 
 
 
 shp_center <- shp_center %>% filter(plz %in% lyt$name)
 
 
 shp_coords <- shp_center %>% st_coordinates() 
 br_coords <- as.matrix(br_coord %>% select(lon,lat))
 
 shp_coords <- cbind(shp_center$plz,shp_coords) 
 br_coords <- cbind(br_coord$url,br_coords)
 
 
 colnames(shp_coords) <- colnames(br_coords) 
 
 tot_coords <- rbind(br_coords,shp_coords)
 
 tot_coords <- as.data.frame(tot_coords) 
 
 lyt2 <- lyt
 lyt2 <- left_join(lyt2,tot_coords,by=c("name"="V1")) 
 
 lyt$x <- lyt2$lon %>% as.numeric()
 lyt$y <- lyt2$lat %>% as.numeric()
 
 
 
 asd <-lieferando %>% distinct(url,.keep_all = T) %>%  select(url,kitchen)
 br2 <- left_join(br,asd,by="url")
 
 
 

 
 

 
 
 bez <- read_sf("bezirksgrenzen.shp/bezirksgrenzen.shp")
 br2 <- left_join(br,asd,by="url")
 
 
 br2 <- br2 %>% filter(kitchen %>% str_detect("Türk")) 

 br2 <- br2 %>% st_as_sf(coords=c("lon","lat")) %>% st_set_crs(4326)
ggplot(bez) + geom_sf(fill="black") + geom_sf(data = br2,color="yellow",aes(size=log(1+bew)),alpha=0.5,size=0)

ggraph(graph = ig_graph,layout = lyt) +geom_sf(data=shp) + geom_edge_link(alpha=0.1) + geom_node_point(size=0.2,color=lyt$node_color)

#dish_specific
dish = "Döner|Dürüm"

dish_keys <- lapply(lieferando$meal,function(x) which(str_detect(x,dish)))
dish_check <- sapply(dish_keys,function(x) length(x) > 0)
dish_values <- mapply(function(x,y) x[y], x=lieferando$prices,y=dish_keys)
dish_values <- lapply(dish_values, function(x) x %>% str_remove("€") %>% str_replace("[,]",".") %>% str_squish() %>% as.numeric())
dish_values <- sapply(dish_values,mean)

lieferando$dish_values <- dish_values
lief_dish <- lieferando[dish_check,]

plz_dish <- lief_dish %>% group_by(PLZ) %>% summarise(av_value=mean(dish_values))
shp2 <- left_join(shp,plz_dish %>% mutate(PLZ=as.character(PLZ)),by=c("plz"="PLZ"))

quant_10 <- shp2$av_value %>% quantile(0.1,na.rm = T)
quant_90 <- shp2$av_value %>% quantile(0.9,na.rm = T)

ggplot(shp2) + 
  geom_sf(aes(fill=av_value),color="transparent") + 
  geom_sf(data=bez,color="white",size=0.2,fill="transparent") + 
  scale_fill_material("pink",oob=scales::squish,limits=c(quant_10,quant_90)) 



####network viz
sub_plz <- lieferando %>% filter(kitchen %>% str_detect("Türk"))
sub_br <- br2 <- br2 %>% filter(kitchen %>% str_detect("Türk")) 
df_graph <- sub_plz %>% select(url,PLZ,lat,lon,rest_name)
ig_graph <- graph_from_data_frame(df_graph,directed = TRUE)


lyt <- create_layout(ig_graph,layout = "auto")

lyt_which <- lyt$name %in% sub_br$url
br_which <- sub_br$url %in% lyt$name 

sub_br$check3 <- br_which

br_coord <- sub_br %>% filter(check3) %>% select(rest_name,url,lon,lat)


shp <- shp %>% st_transform(4326)
shp_center <- shp %>% st_centroid() %>% sf::st_transform(4326)
shp_center$check <- FALSE
shp_center$check <- shp_center$plz %in% lyt$name

sc_coord <- shp_center %>% st_coordinates()
plz <- lyt[lyt$y == 1,]
plz$check <- plz$name %in% shp_center$plz

plz <- left_join(plz,shp_center, by=c("name"="plz"))

shp_center$check <- shp_center$plz %in% plz$name 
shp_center <- shp_center %>% distinct(plz)






shp_center <- shp_center %>% filter(plz %in% lyt$name)


shp_coords <- shp_center %>% st_coordinates() 
br_coords <- as.matrix(br_coord %>% select(lon,lat))

shp_coords <- cbind(shp_center$plz,shp_coords) 
br_coords <- cbind(br_coord$url,br_coords)


colnames(shp_coords) <- colnames(br_coords) 

tot_coords <- rbind(br_coords,shp_coords)

tot_coords <- as.data.frame(tot_coords) 

lyt2 <- lyt
lyt2 <- left_join(lyt2,tot_coords,by=c("name"="V1")) 

lyt$x <- lyt2$lon %>% as.numeric()
lyt$y <- lyt2$lat %>% as.numeric()
lyt$type <- lyt2$y %>% as.numeric() %>% as.factor()
lyt$node_color <- ifelse(lyt$type == "1","transparent","red")
  
