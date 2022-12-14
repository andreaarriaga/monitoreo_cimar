
#```{r sites_map setup, eval = FALSE, echo = FALSE, message = FALSE, warning = FALSE}
fishes_sites$region <- sapply(fishes_sites$regn_st %>% as.character(), function(x) strsplit(x, "-")[[1]][1], USE.NAMES = FALSE)

fishes_sites %<>% st_as_sf(coords = c("long", "lat"),
                           crs = 4326, remove = FALSE)

# cargando el archivo con los polígonos de los paises 
# world <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# generando un objeto con la cantidad de sitios por region y la geolocalizacion 
sites_per_locality <- aggregate(site_id ~ region + locality, FUN = function(x) length(unique(x)), data = abundance_and_func_traits)
names(sites_per_locality)[grep("site_id", names(sites_per_locality))] <- "site_id_length_loc"

sites_per_locality1 <- fishes_sites %>% left_join(sites_per_locality)
# agregando en las regiones las islas oceanicas
# sites_per_locality1$region[sites_per_locality1$locality == "revillagigedo"] <- "México - Revillagigedo"
sites_per_locality1$region[sites_per_locality1$locality == "isla_del_coco"] <- "Costa Rica - Isla del Coco"
# sites_per_locality1$region[sites_per_locality1$locality == "malpelo"] <- "Colombia - Malpelo"

# agregando en las regiones las islas oceanicas
# sites_per_locality$region[sites_per_locality$locality == "revillagigedo"] <- "México - Revillagigedo"
sites_per_locality$region[sites_per_locality$locality == "isla_del_coco"] <- "Costa Rica - Isla del Coco"
# sites_per_locality$region[sites_per_locality$locality == "malpelo"] <- "Colombia - Malpelo"

# calculando la cantidad de sitios por region con las islas como regiones
sites_per_region <- aggregate(site_id_length_loc ~ region, FUN = sum, data = sites_per_locality)

names(sites_per_region)[grep("site_id", names(sites_per_region))] <- "site_id_length"

# agregandole geometry
sites_per_region <- sites_per_region %>% left_join(sites_per_locality1)

sites_per_region <- sites_per_region[!duplicated(sites_per_region$region), ]  # esta dejando la primera coordenada, se puede mejorar a seleccionar el promedio

# corrigiendo formato de nombre de regiones
# sites_per_region$region[sites_per_region$region == "colombia"]   <- "Colombia"
sites_per_region$region[sites_per_region$region == "costa_rica"] <- "Costa Rica"     
# sites_per_region$region[sites_per_region$region == "ecuador"]    <- "Ecuador"
# sites_per_region$region[sites_per_region$region == "france"]     <- "Clipperton"
# sites_per_region$region[sites_per_region$region == "galapagos"]  <- "Ecuador - Galápagos"
# sites_per_region$region[sites_per_region$region == "mexico"]     <- "México"
# sites_per_region$region[sites_per_region$region == "nicaragua"]  <- "Nicaragua"
# sites_per_region$region[sites_per_region$region == "panama"]     <- "Panamá"

sites_per_region$region %<>% as.factor()

#```
