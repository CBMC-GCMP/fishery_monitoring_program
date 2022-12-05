library(tidyverse)

file_list <- list.files("data/Trackers_comunidades/", 
           recursive = TRUE, full.names = TRUE,
           pattern = ".xlsx") %>%  
      as.data.frame() %>% 
      #filter(str_detect(., "MARGARITA", negate = T)) %>% 
      pull(.)


metadata <- map_dfr(file_list, function(x) {readxl::read_xlsx(x)})


meta_tocorrect <- metadata %>% 
      filter(str_detect(IDViaje, "export"))


write_csv(meta_tocorrect,
                 file = "metadata_tocorrect_file.csv")

csv_all <- metadata %>% 
      select(IDViaje, Numero_tracker_meta = Numero_tracker, Region, Comunidad_meta = Comunidad) %>% 
      unique()


csv_tracks <- list.files("data/Trackers_comunidades/", 
                         recursive = TRUE, full.names = TRUE,
                         pattern = ".csv") %>% 
      as.data.frame() %>% 
      rename(path = ".") %>% 
      mutate(IDViaje = str_remove_all(basename(path), ".csv"), 
             Numero_tracker_file = basename(dirname(path)), 
             Comunidad_file = str_remove_all(basename(dirname(dirname(path))), "USB")) %>% 
      mutate(Comunidad_file = str_remove_all(Comunidad_file, "Trackers "))
      
glimpse(csv_all)
glimpse(csv_tracks)
# 
# merged <- merge(csv_tracks, csv_all, by = c("IDViaje"), all.x = T) %>% 
#       mutate(toclean = ifelse(str_detect(IDViaje, "export"), "YES", "NO")) %>% 
#       mutate(exists_in_meta = ifelse(is.na(Numero_tracker_meta), "NO", "YES"))
# 
# merged %>% 
#       group_by(toclean, exists_in_meta) %>% 
#       count()


# GPS tracks --------------------------------------------------------------

file_list <- list.files("data/Trackers_comunidades/", 
                        recursive = TRUE, full.names = TRUE,
                        pattern = ".csv") %>%  
      as.data.frame() %>% 
      filter(str_detect(., "export_")) %>% 
      pull(.)

## Loading shapefile of Mexico
mx_shape <- sf::read_sf("data/capa_costa_trackers/lcosta_gc_wvs_utm.shp")


# Set the path to metadata file
metadata <- "metadata_tocorrect_file.csv"

# Loading functions
lapply(list.files("R/", pattern = ".R", full.names = T), source)

# 
# library(future.apply)
# plan(multisession, workers = 10) ## Run in parallel on local computer

# Clean file names
#invisible(future_lapply(file_list, change_file_name_csv, metadata, future.seed=TRUE))

lapply(file_list, change_file_name_csv, metadata)
beepr::beep()


# Cleaning land points

file_list <- list.files("cleaned_csv/", recursive = T, full.names = T)
library(future.apply)
plan(multisession, workers = 4) ## Run in parallel on local computer

cleaned <- future_lapply(file_list, clean_land_points, mx_shape, future.seed=TRUE)
#cleaned <- lapply(file_list, clean_land_points, mx_shape)


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



