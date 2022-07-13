## Loading libraries
library(tidyverse)
library(sf)

## Loading shapefile of Mexico
load("data/mx_shape.RData")


# Set the path to metadata file
metadata <- "data/CSL_turismo1.xlsx"

# Loading functions
lapply(list.files("R/", pattern = ".R", full.names = T), source)



# List raw gps files to be cleaned
file_list <- list.files("data/01_points/", recursive = T, full.names = T)


# Clean file names
invisible(lapply(file_list, change_file_name, metadata))


# Cleaning land points

file_list <- list.files("preprocessed/", recursive = T, full.names = T)


cleaned <- lapply(file_list, clean_land_points, mx_shape)


## Exporting point shapefiles

dir.create("outputs/", showWarnings = F)


## Modelling states 

modelled <- lapply(cleaned, model_vms_artesanal)


lapply(modelled, function(x) {st_write(st_as_sf(x, coords = c("longitud", "latitud"), crs = 4326, remove = F), 
                                      paste0("outputs/", unique(x$nombre_del_archivo_gps_corregido), ".shp"), append = F)})




## Calculating stats from gps tracks

tracks <- lapply(modelled, transform_lines)

tracks_jnd <- do.call(rbind, tracks)

mapview::mapview(tracks_jnd)


#### Merging metadata

metadata_df <- readxl::read_xlsx(metadata) %>% 
          janitor::clean_names() 


meta_w_stats <- merge(metadata_df, as.data.frame(tracks_jnd) %>% select(-geometry), by = "nombre_del_archivo_gps", all.x = T) %>% 
   relocate(nombre_del_archivo_gps_corregido, .after = nombre_del_archivo_gps) %>% 
   select(-c("hora_inicio", "hora_final", "duracion", "distancia"))


## Exporting metadata

xlsx::write.xlsx(meta_w_stats, showNA = F, row.names = F,
                 file = paste0(str_replace_all(unique(meta_w_stats$comunidad), " ", "_"), "_metadata_file.xlsx"))

