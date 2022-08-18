library(tidyverse)

file_list <- list.files("Trackers_comunidades/", 
           recursive = TRUE, full.names = TRUE,
           pattern = ".xlsx") %>%  
      as.data.frame() %>% 
      filter(str_detect(., "MARGARITA", negate = T)) %>% 
      pull(.)


metadata <- map_dfr(file_list, function(x) {readxl::read_xlsx(x)})


meta_tocorrect <- metadata %>% 
      filter(str_detect(IDViaje, "export"))


write_csv(meta_tocorrect,
                 file = "metadata_tocorrect_file.csv")

# GPS tracks --------------------------------------------------------------

file_list <- list.files("Trackers_comunidades/", 
                        recursive = TRUE, full.names = TRUE,
                        pattern = ".csv") %>%  
      as.data.frame() %>% 
      filter(str_detect(., "export_")) %>% 
      pull(.)

## Loading shapefile of Mexico
load("data/mx_shape.RData")


# Set the path to metadata file
metadata <- "metadata_tocorrect_file.csv"

# Loading functions
lapply(list.files("R/", pattern = ".R", full.names = T), source)


# Clean file names
invisible(lapply(file_list, change_file_name_csv, metadata))


# Cleaning land points

file_list <- list.files("preprocessed/", recursive = T, full.names = T)

cleaned <- lapply(file_list, clean_land_points, mx_shape)


## Exporting point shapefiles

dir.create("outputs/", showWarnings = F)

## Modelling states 

modelled <- lapply(cleaned, model_vms_artesanal)

lapply(modelled, function(x) {sf::st_write(sf::st_as_sf(x, coords = c("longitud", "latitud"), crs = 4326, remove = F), 
                                       paste0("outputs/", unique(x$nombre_del_archivo_gps_corregido), ".shp"), append = F)})



## Calculating stats from gps tracks
library(sf)
tracks <- lapply(modelled, transform_lines)

tracks_jnd <- do.call(rbind, tracks)

mapview::mapview(tracks_jnd, zcol = "nombre_del_archivo_gps_corregido")


#### Merging metadata

metadata_df <- read_csv(metadata) %>% 
      janitor::clean_names() 



meta_w_stats <- merge(metadata_df %>% rename(nombre_del_archivo_gps = id_viaje), as.data.frame(tracks_jnd) %>% select(-geometry), by = "nombre_del_archivo_gps", all.x = T) %>% 
      relocate(nombre_del_archivo_gps_corregido, .after = nombre_del_archivo_gps)


## Exporting metadata

xlsx::write.xlsx(meta_w_stats, showNA = F, row.names = F, "full_metadata_file.xlsx")



