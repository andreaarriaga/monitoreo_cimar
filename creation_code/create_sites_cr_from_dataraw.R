
## ```{r sites_Costa Rica, echo = FALSE, message = FALSE, warning = FALSE}

# cargando sitios de uvc
fishes_costa_rica_correct_size_abundance <- readRDS("data_intermediate/fish/fishes_costa_rica_correct_size_abundance.rds")

## seleccionando variables de interés
sites_uvc_cr <- fishes_costa_rica_correct_size_abundance[, "site_id", drop = FALSE] %>% unique

rm(fishes_costa_rica_correct_size_abundance)

# 1. cargando los datos de sitios, corrigiendo nombres y verificando coincidencias con su respectiva base de uvc

###########################################---------MURCIELAGO--------###################################################
sites_murcielago <- read_excel("data_raw/sites/COORDENADAS.xlsx")

## seleccionando columnas de interés
# separando las coordenadas en latitud y longitud
sites_murcielago %<>%
  separate(Coordenadas,
           into = c("lat", "long"),
           sep    = "-")

sites_murcielago %<>%
  dplyr::select(sites = Sitio,
                lat,
                long,
                locality = Localidad)


## corrigiendo nombres de sitios y localidades
## cambiando los nombres a minuscula
sites_murcielago$sites %<>% str_to_lower()
sites_murcielago$locality %<>% str_to_lower()

## cambiando caracteres diacriticos
unwanted_array = list('á'='a', 'é'='e', 'í'='i', 'ó'='o', 'ú'='u', 'ñ'='n')

# sites
for(i in seq_along(unwanted_array)){
  sites_murcielago$sites <- gsub(names(unwanted_array)[i],unwanted_array[i], sites_murcielago$sites)
}
# localities
for(i in seq_along(unwanted_array)){
  sites_murcielago$locality <- gsub(names(unwanted_array)[i],unwanted_array[i], sites_murcielago$locality)
}

## agregando guion bajo
sites_murcielago$sites <- gsub(pattern = " ", replacement = "_", sites_murcielago$sites)
sites_murcielago$locality <- gsub(pattern = " ", replacement = "_", sites_murcielago$locality)

## corrigiendo formato de las coordenadas

## definiendo la funcion
dg2dec <- function(varb, Dg=NA, Min=NA, Sec=NA, SW.Hemisphere="S|W") {
  
  DMS <- sapply(strsplit(varb, paste0('[', Dg, Min, Sec, ']')), as.numeric)
  
  decdg <- abs(DMS[1, ]) + DMS[2, ]/60 + ifelse(dim(DMS)[1] > 2  & !is.na(Sec), DMS[3, ]/3600, 0)
  
  SW <- grepl(pattern = SW.Hemisphere, x = varb, ignore.case = TRUE)
  return(ifelse(SW, -1, 1) * decdg)
}

## arreglando latitud
lat <- c()
for (i in sites_murcielago$lat){
  lat[i] <- dg2dec(i, Dg='°', Min="'", Sec='\\\\" N')
} 

lat %<>% as.data.frame()
names(lat)[1] <- "lat"

## arreglando longitud
long <- c()
for (i in sites_murcielago$long){
  long[i] <- dg2dec(i, Dg='°', Min="'", Sec='\\\\"W')
} 

long %<>% as.data.frame()
names(long)[1] <- "long"

## uniendo las nuevas coordenadas en una base de datos
sites_murcielago1 <- cbind(lat, long, sites_murcielago$sites) ## agregar localidad
names(sites_murcielago1)[grep("sites", names(sites_murcielago1))] <- "sites"
sites_murcielago1$locality <- sites_murcielago$locality

sites_murcielago <- sites_murcielago1
rm(sites_murcielago1)
sites_murcielago$region <- "costa_rica"

# creando identificador unico para cada sitio
sites_murcielago$site_id <-  paste(sites_murcielago$region, sites_murcielago$sites, sep = "-")
sites_murcielago$region_site <-  paste(sites_murcielago$region, sites_murcielago$sites, sep = "-")

# ## guardando el documento. Falta definirlo como un pobejo espacial para poder visualizarlo!!
# write.table(sites_murcielago, "data_intermediate/sites/costa_rica/sites_murcielago.csv",  sep = ";", dec=",")
# write.xlsx(sites_uvc_murcielago, "data_intermediate/sites/costa_rica/sites_uvc_murcielago.xlsx")
# rgdal::writeOGR(sites_uvc_murcielago, ".", "data_intermediate/sites/costa_rica/sites_uvc_murcielago.xlsx", driver="ESRI Shapefile")

###########################################---------JUANCA--------#####################################################
## cargando la base de datos de sites del laboratorio (solo datos de CR)
sites_juanca <- read_excel("data_raw/sites/2019-10-10_ArrecifesPacifico.xlsx")

### cambiando los nombres para que coincidan
sites_juanca %<>% dplyr::select("region"   = Region,
                                "locality" = Localidad,
                                "sites"    = Sitio,
                                "lat"      = Latitud,
                                "long"     = Longitud)

### cambiando los nombres de sitio, localidad y region a minuscula
sites_juanca$sites %<>% str_to_lower()
sites_juanca$locality %<>% str_to_lower()
sites_juanca$region <- "costa_rica"

## quitando sitios sin coordenadas
sites_juanca <- sites_juanca[-c(which(is.na(sites_juanca$lat) == TRUE)),]

### cambiando caracteres diacriticos
unwanted_array = list('á'='a', 'é'='e', 'í'='i', 'ó'='o', 'ú'='u', 'ñ'='n')

for(i in seq_along(unwanted_array)){
  sites_juanca$sites <- gsub(names(unwanted_array)[i],unwanted_array[i], sites_juanca$sites)
}

## agregando guion bajo
sites_juanca$sites <- gsub(pattern = " ", replacement = "_", sites_juanca$sites)
sites_juanca$locality <- gsub(pattern = " ", replacement = "_", sites_juanca$locality)

# cambiando nombre de localidad para que calce con base de censos
sites_juanca$locality <- gsub(pattern = "punta_gorda", replacement = "bahia_culebra", sites_juanca$locality)

sites_juanca$locality <- gsub(pattern = "peninsula_de_osa", replacement = "osa", sites_juanca$locality)
sites_juanca$locality[sites_juanca$lat == 8.719593] <- "golfo_dulce"

# creando identificador unico para cada sitio
sites_juanca$site_id <-  paste(sites_juanca$region, sites_juanca$sites, sep = "-")
sites_juanca$region_site <-  paste(sites_juanca$region,  sites_juanca$sites, sep = "-")

# objeto con region_site dplicadas 
dup1 <- sites_juanca[duplicated(sites_juanca$region_site),]

sites_juanca_fix <- sites_juanca[sites_juanca$region_site %in% dup1$region_site,]

# arreglando el site_id
sites_juanca_fix$site_id <- paste(sites_juanca_fix$region, sites_juanca_fix$locality, sites_juanca_fix$sites, sep = "-")

sites_juanca <- sites_juanca[!sites_juanca$region_site %in% sites_juanca_fix$region_site, ] %>% 
  rbind(sites_juanca_fix)

###########################################---------CULEBRA--------####################################################
## cargando la base de datos de sites de Culebra
sites_culebra <- read_excel("data_raw/sites/sites_coordenates.costa_rica.xlsx")

### cambiando los nombres para que coincidan
sites_culebra %<>% 
  dplyr::select("locality" = Locality,
                "sites"    = Site,
                "lat"      = Lat,
                "long")

sites_culebra$region <- "costa_rica"

### cambiando los nombres de sitio, localidad y region a minuscula
sites_culebra$sites %<>% str_to_lower()
sites_culebra$locality %<>% str_to_lower()
# sites_culebra$long %<>% as.character() # provisional por mientras arreglo coordenadas
# sites_culebra$lat %<>% as.character() # provisional por mientras arreglo coordenadas

# arreglando north y west a las coordenadas
sites_culebra$lat[c(1:9)] <- paste0(sites_culebra$lat[c(1:9)], "0\"N")
sites_culebra$lat[12] <- paste0(sites_culebra$lat[12], "\"N")
sites_culebra$lat <- gsub(pattern = " ", replacement = "", sites_culebra$lat)

sites_culebra$long[c(1:9)] <- paste0(sites_culebra$long[c(1:9)], "0\"W")
sites_culebra$long[12] <- paste0(sites_culebra$long[12], "\"W")
sites_culebra$long <- gsub(pattern = "O", replacement = "W", sites_culebra$long)
sites_culebra$long[c(10, 11, 13, 14, 15, 16)] <- paste0(sites_culebra$long[c(10, 11, 13, 14, 15, 16)], "W")

## arreglando latitud
lat <- c()
for (i in sites_culebra$lat){
  lat[i] <- dg2dec(i, Dg='°', Min="'", Sec='\\\\" N')
} 

lat %<>% as.data.frame()
names(lat)[1] <- "lat"

## arreglando longitud
long <- c()
for (i in sites_culebra$long){
  long[i] <- dg2dec(i, Dg='°', Min="'", Sec='\\\\"W')
} 

long %<>% as.data.frame()
names(long)[1] <- "long"

## uniendo las nuevas coordenadas en una base de datos
sites_culebra1 <- cbind(lat, long, sites_culebra$sites) ## agregar localidad
names(sites_culebra1)[grep("sites", names(sites_culebra1))] <- "sites"
sites_culebra1$locality <- sites_culebra$locality

sites_culebra <- sites_culebra1
rm(sites_culebra1)

### cambiando caracteres diacriticos
unwanted_array = list('á'='a', 'é'='e', 'í'='i', 'ó'='o', 'ú'='u', 'ñ'='n')

for(i in seq_along(unwanted_array)){
  sites_culebra$sites <- gsub(names(unwanted_array)[i],unwanted_array[i], sites_culebra$sites)
}

## agregando guion bajo
sites_culebra$sites <- gsub(pattern = " ", replacement = "_", sites_culebra$sites)
sites_culebra$locality <- gsub(pattern = " ", replacement = "_", sites_culebra$locality)

sites_culebra$locality <- gsub(pattern = "punta_gorda", replacement = "bahia_culebra", sites_culebra$locality)

sites_culebra$sites <- gsub(pattern = "guiri-guiri", replacement = "guiri_guiri", sites_culebra$sites)
sites_culebra$sites <- gsub(pattern = "	viradores_", replacement = "guiri_guiri", sites_culebra$sites)
sites_culebra$sites <- gsub(pattern = "	viradores", replacement = "guiri_guiri", sites_culebra$sites)

# creando identificador unico para cada sitio
sites_culebra$site_id <-  paste(sites_culebra$region, sites_culebra$sites, sep = "-")
sites_culebra$region_site <-  paste(sites_culebra$region,  sites_culebra$sites, sep = "-")


###########################################-----Unificacion-----#####################################################
#  2. se unen todas las coordenadas en una misma base de datos

sites_uvc_murcielago <- sites_uvc_cr %>%
  left_join(sites_murcielago)

### se separan los  sitios con coordenadas de aquellos que no encontraron coincidencias
sites_uvc_coord_murcielago <- sites_uvc_murcielago[-c(which(is.na(sites_uvc_murcielago$long))), ]
sites_uvc_nocoord <- sites_uvc_murcielago[which(is.na(sites_uvc_murcielago$long)), c("site_id"), drop = FALSE]

#
sites_uvc_juanca <- sites_uvc_nocoord %>%
  left_join(sites_juanca)

### se separan los  sitios con coordenadas de aquellos que no encontraron coincidencias
sites_uvc_coord_juanca <- sites_uvc_juanca[-c(which(is.na(sites_uvc_juanca$long))), ]
sites_uvc_nocoord <- sites_uvc_juanca[which(is.na(sites_uvc_juanca$long)), c("site_id"), drop = FALSE]

#
sites_uvc_culebra <- sites_uvc_nocoord %>%
  left_join(sites_culebra)

### se separan los  sitios con coordenadas de aquellos que no encontraron coincidencias
sites_uvc_coord_culebra <- sites_uvc_culebra[-c(which(is.na(sites_uvc_culebra$long))), ]
sites_uvc_nocoord <- sites_uvc_culebra[which(is.na(sites_uvc_culebra$long)), c("site_id"), drop = FALSE]

# 4.  uniendo todas las coordenadas en una misma base de datos
sites_crlab <- sites_uvc_coord_murcielago %>%
  bind_rows(sites_uvc_coord_juanca)  

sites_crlab <-  sites_crlab %>%
  bind_rows(sites_uvc_coord_culebra) 

# sites_crlab_dup <- sites_crlab[duplicated(sites_crlab$site_id), ]
# sites_crlab_dup <- sites_crlab[sites_crlab$site_id %in% sites_crlab_dup$site_id, ]

## agregando identificador unico para cada sitio
sites_crlab$region_site <- paste(sites_crlab$region, sites_crlab$sites, sep = "-")

# definiendo dataset_sites
sites_crlab$dataset_sites <- "lab"

## seleccionando columnas de interes
sites_crlab <- sites_crlab[, c("lat", "long", "locality", "region_site", "site_id", "dataset_sites")]

# 4. guardando el archivo en formato .rda
## guardando el archivo
saveRDS(sites_crlab,
        file = "data_intermediate/sites/sites_crlab.rds")

# clean up intermediate objects
rm(unwanted_array,
   sites_juanca,
   sites_juanca_fix,
   sites_culebra,
   sites_uvc_cr,
   sites_murcielago,
   sites_uvc_murcielago,
   sites_uvc_murcielago_nocoord,
   dg2dec,
   i,
   lat,
   long)

## ```

## ```{r sites_f y sites_metadata, echo = FALSE, message = FALSE, warning = FALSE}
# 2. cargando todas las bases de datos de sitios y corrigiendo los nombres (i.e. minusculas, sin signos diacriticos y sin nombres mal escritos)

#############################################--------sites_FRANZ---------##################################################
## cargando la base de sites con las coordenadas (provista por Franz)
load("data_raw/sites/fishes_sites.andrea.rda")

### cambiando los nombres para que coincidan
sites_f <- fishes_sites.andrea[, -1]
rm(fishes_sites.andrea)
sites_f %<>% dplyr::select("sites" = Sitio,
                           "long"  = Longitud,
                           "lat"   = Latitud)

### cambiando los nombres de sitio a minuscula
sites_f$sites %<>% str_to_lower()
# sites_f$long %<>% as.character() # provisional por mientras arreglo coordenadas
# sites_f$lat %<>% as.character() # provisional por mientras arreglo coordenadas

### cambiando caracteres diacriticos
unwanted_array = list('á'='a', 'é'='e', 'í'='i', 'ó'='o', 'ú'='u', 'ñ'='n')

for(i in seq_along(unwanted_array))({
  sites_f$sites <- gsub(names(unwanted_array)[i],unwanted_array[i], sites_f$sites)
})

## agregando guion bajo
sites_f$sites <- gsub(pattern = " ", replacement = "_", sites_f$sites)

# agregandoles la region
sites_f$region <- NA

sites_f$region[sites_f$lat >= 15.68722]  <- "mexico" 
sites_f$region[sites_f$lat >= 11.06447 & sites_f$lat <= 12.47578]  <- "nicaragua" 
sites_f$region[sites_f$lat >= 5.436698 & sites_f$lat <=  5.650014 & sites_f$long <= -86.976293]  <- "costa_rica" # coco
sites_f$region[sites_f$lat >= 8.028344 & sites_f$lat <= 11.04865 & sites_f$long <= -82.897413]  <- "costa_rica" 
sites_f$region[sites_f$lat >= 7.103914 & sites_f$lat <= 9.025267 & sites_f$long >= -82.904214]  <- "panama" 
sites_f$region[sites_f$lat >=  1.477157 & sites_f$lat <= 7.226235 & sites_f$long >= -86.976293]  <- "colombia"
sites_f$region[sites_f$lat >= -3.390584 & sites_f$lat <= 1.477157] <- "ecuador" 
sites_f$region[sites_f$lat >=  -1.691239 & sites_f$lat <= 1.680950 & sites_f$long <=-87.226897] <- "galapagos"
sites_f$region[sites_f$lat <= -18.357159] <- "chile"

## agregando identificador unico para cada sitio
sites_f$site_id <- paste(sites_f$region, sites_f$sites, sep = "-")

sites_f$region_site <- paste(sites_f$region, sites_f$sites, sep = "-")

## eliminando sitio con el mismo nombre en distinta region que crean conflictos
# sites_f <- sites_f[!duplicated(sites_f$region_site), ]

# modificando roca_partida para que tenga la localidad correcta
sites_f$site_id <- gsub("mexico-roca_partida", "mexico-revillagigedo-roca_partida", sites_f$site_id)

sites_f$site_id <- gsub("costa_rica-san_pedrillo", "costa_rica-murcielago-san_pedrillo", sites_f$site_id)


## Eliminando sitios en mx que difieren en ubicacion de otros con el mismo nombre en base de mx
# sites_incorrect_mx <-
#   c("los_nidos",
#     "punta_lobos")
# 
# sites_f <- sites_f[!sites_f$sites %in% sites_incorrect_mx, ]

## seleccionando las columnas de interes
sites_f <- sites_f[, c("site_id", "lat", "long", "region_site")]

# seleccionando subconjunto de CR
sites_f <- sites_f[grep("costa_rica", sites_f$region_site),]

# definiendo dataset_sites
sites_f$dataset_sites <- "franz"
sites_f$locality <- NA

#############################################--------sites_METADATA---------###################################################
## cargando la base de Quimbayo MetaData
sites_metadata <- read_excel("data_raw/sites/MetaData.xlsx")

### cambiando los nombres para que coincidan
sites_metadata %<>% dplyr::select("region"   = Country,
                                  "locality" = Locality,
                                  "sites"    = Sites,
                                  "lat"      = Lat,
                                  "long"     = Long)

### cambiando los nombres de sitio a minuscula
sites_metadata$sites %<>% str_to_lower()
sites_metadata$locality %<>% str_to_lower()

### cambiando nombres mal escritos
sites_metadata$locality <- gsub("islas_murielago", "islas_murcielago", sites_metadata$locality)
sites_metadata$locality <- gsub("cabo_pulmon", "cabo_pulmo", sites_metadata$locality)

sites_metadata$locality <- gsub("isla_cerralvo",  "isla_ceralvo", sites_metadata$locality)
sites_metadata$locality <- gsub("revillagigedo_roca_partida", "revillagigedo", sites_metadata$locality)

sites_metadata$site_latlong <- paste0(sites_metadata$sites, "-", sites_metadata$lat, " ", sites_metadata$long)
sites_metadata <- sites_metadata[!duplicated(sites_metadata$site_latlong), ]
# sites_metadata[sites_metadata$sites %in% dup,] %>% View
sites_metadata$site_latlong <- NULL

# sites_metadata$sites <- sites_metadata$sites %>% replace(sites_metadata$sites == "^el_bajo_seco_sur$", "bajo_seco_sur")

## Eliminando sitios que tienen mal las coordenadas y caen en tierra, para usar las de franz
sites_land <-
  {  c(
    # "la_lobera",
    # "los_nidos",
    # "punta_lobos",
    # "bajo_del_tigre",
    # "roca_partida",
    "chicarias_afuera",
    "majagual",
    "nacascolo",
    "paloma_norte",
    "paloma_sur",
    "punta_la_flor",
    "punta_clavo",
    "bajo_del_tigre", 
    "isla_pelada",
    "la_anciana",
    "el_toro",
    "pena_rota",
    "marsella",
    "guacalito",
    "palmitas",
    "guiri_guiri",
    "matapalo",
    "esmeralda",
    "pelonas",
    "jicaral",
    "bajo_los_castillo",
    "punta_el_indio",
    "punta_carrillo",
    "el_muneco",
    "tiburon",
    "el_cirial",
    "el_reloj",
    # "punta_demonio",
    "sueno_del_pescador",
    "montana_rusa",
    "brincanco",
    "uva",
    "canales_de_tierra",
    "mali_mali",
    "iglesias",
    # "machete",
    "machete_punta",
    "wahoo",
    # "buffete",
    "bajo_del_pulpo",
    "faro",
    "don_juan",
    "granito_de_oro",
    "dos_tetas",
    "tintorera",
    "catedral",
    # "islote_santa_cruz",
    "san_marin",
    "cativo",
    "cimarrones",
    "puerto_escondido",
    # "la_botella",
    "champion",
    "la_guina",      # mismas coordenadas que roca_patida y generan conflicto
    "la_piramide",   # mismas coordenadas que roca_patida y generan conflicto
    "punta_chivato", # mismas coordenadas que roca_patida y generan conflicto
    "la_bufadora",   # mismas coordenadas que roca_patida y generan conflicto
    "resumidero")}   # mismas coordenadas que roca_patida y generan conflicto

sites_metadata <- sites_metadata[!sites_metadata$sites %in% sites_land, ]

## quitando sitios sin lat
sites_metadata <- sites_metadata[!sites_metadata$lat %>% is.na, ]

## creando identificador unico para cada sitio
sites_metadata$site_id <-  paste(sites_metadata$region, sites_metadata$sites, sep = "-")

sites_metadata$region_site <-  paste(sites_metadata$region, sites_metadata$sites, sep = "-")

# creando identificador unico para los sitios duplicados en region_site
# eliminando duplicados de coordenadas
sites_metadata1 <- sites_metadata[!duplicated(sites_metadata[, c("lat", "long")]), ]

## buscando duplicados
dup1 <- sites_metadata1[duplicated(sites_metadata1$region_site),]

sites_metadata1_fix <- sites_metadata1[sites_metadata1$region_site %in% dup1$region_site,]

# arreglando el site_id
sites_metadata1_fix$site_id <- paste(sites_metadata1_fix$region, sites_metadata1_fix$locality, sites_metadata1_fix$sites, sep = "-")

sites_metadata <- sites_metadata[!sites_metadata$region_site %in% sites_metadata1_fix$region_site, ] %>% 
  rbind(sites_metadata1_fix)

## seleccionando las columnas de interes
sites_metadata <- sites_metadata[, c("site_id", "lat", "long", "locality", "region_site")]

# eliminando roca partida por site_id
sites_metadata <- sites_metadata[!sites_metadata$site_id == "mexico-revillagigedo-roca_partida",]

# seleccionando subconjunto de CR
sites_metadata <- sites_metadata[grep("costa_rica", sites_metadata$region_site),]

# definiendo dataset_sites
sites_metadata$dataset_sites <- "metadata"

## ```

## ```{r Unificacion, echo = FALSE, message = FALSE, warning = FALSE}
## cargando la base de sites con las coordenadas de costa rica
sites_crlab <- readRDS("data_intermediate/sites/sites_crlab.rds")

sites_cr <- rbind(sites_f, sites_metadata, sites_crlab)

#. haciendo todas las longitudes negativas
sites_cr$long <- sites_cr$long %>% abs
sites_cr$long <- sites_cr$long *-1

## guardando el archivo
saveRDS(sites_cr,
        file = "data_intermediate/sites/sites_cr.rds")

## ```

## ```{r eliminando duplicados site_id,  echo = FALSE, message = FALSE, warning = FALSE}
# evaluando los duplicados de site_id
sites_cr_dup <- sites_cr$site_id[duplicated(sites_cr$site_id)]
sites_cr_dup <- sites_cr[sites_cr$site_id %in% sites_cr_dup, ]

sites_cr_dup$remove <- NA

# quitando los que vienen de la base metadata
sites_cr_dup$remove[grep("metadata", sites_cr_dup$dataset_sites)] <- "remove" 

sites_cr <- sites_cr %>% left_join(sites_cr_dup)
sites_cr <- sites_cr[sites_cr$remove %>% is.na(), ]
sites_cr$remove <- NULL

# quitando los que vienen de la base de Franz
sites_cr_dup <- sites_cr$site_id[duplicated(sites_cr$site_id)]
sites_cr_dup <- sites_cr[sites_cr$site_id %in% sites_cr_dup, ]

sites_cr_dup$remove <- NA
sites_cr_dup$remove[grep("franz", sites_cr_dup$dataset_sites)] <- "remove" 

sites_cr <- sites_cr %>% left_join(sites_cr_dup)
sites_cr <- sites_cr[sites_cr$remove %>% is.na(), ]

sites_cr$remove <- NULL

# arreglando las localidades
# sites_cr$locality <- do.call('rbind', strsplit(sites_cr$site_id, '-', fixed=TRUE))[,2]

# seleccionando en la base de sitios solo los que se encuentran en los censos
abundance_and_func_traits <- readRDS("data_intermediate/fish/fishes_costa_rica_correct_size_abundance.rds")

sites_cr_fromrawdata <- sites_cr[sites_cr$site_id %in% abundance_and_func_traits$site_id, ]

## guardando el archivo
saveRDS(sites_cr_fromrawdata,
        file = "data_intermediate/sites/sites_cr_fromrawdata.rds")

## guardando el archivo
write.table(sites_cr, "data_intermediate/sites/sites_cr_fromrawdata.csv",  sep = ";", dec = ".", row.names = FALSE)

# guardandolo como objeto espacial
coordinates(sites_cr_fromrawdata) <- c("long", "lat")
proj4string(sites_cr_fromrawdata) <- CRS("+init=epsg:4326")

rgdal::writeOGR(sites_cr_fromrawdata,
                "data_intermediate/sites/sites_cr_fromrawdata.kml", layer="sites_cr_fromrawdata", driver="KML", overwrite_layer = TRUE)

# clean up other intermediate objects
rm(sites_cr_fromrawdata)
## ```

## ```{r buscando duplicados coordenadas,  echo = FALSE, message = FALSE, warning = FALSE}
# Estos son los sitios que tienen la misma coordenada y distinto nombre
sites_cr_fromrawdata <- readRDS(file = "data_intermediate/sites/sites_cr_fromrawdata.rds")

# buscando duplicados de coordenadas
sites_cr_fromrawdata$latlong <- paste(sites_cr_fromrawdata$lat, sites_cr_fromrawdata$long, sep = " ")

coord_duplicados <- aggregate(site_id ~ latlong, data = sites_cr_fromrawdata, FUN = function(x) paste(as.character(sort(unique(x))), collapse = "; ")) 

coord_duplicados <- coord_duplicados %>% separate(site_id, c("uno", "dos", "tres"), ";")
coord_duplicados <- coord_duplicados[!coord_duplicados$dos %>% is.na(), ]
# coord_duplicados

## ```

