## Loading libraries
library(tidyverse)
library(sf)
load("data/mx_shape.RData")

# Setting parameters
metadata <- "data/CSL_turismo1.xlsx"

# Loading functions
lapply(list.files("R/", pattern = ".R", full.names = T), source)



file_list <- list.files("data/01_points/", recursive = T, full.names = T)


# Clean file names
invisible(lapply(file_list, change_file_name, metadata))



# Cleaning land points

file_list <- list.files("preprocessed/", recursive = T, full.names = T)

load("data/mx_shape.RData")

cleaned <- lapply(file_list, clean_land_points, mx_shape)



## Exporting point shapefiles

dir.create("outputs/", showWarnings = F)


## MODELLING STATES

modelled <- lapply(cleaned, model_vms_artesanal)


lapply(modelled, function(x) {st_write(st_as_sf(x, coords = c("longitud", "latitud"), crs = 4326, remove = F), 
                                      paste0("outputs/", unique(x$nombre_del_archivo_gps_corregido), ".gpkg"), append = F)})




## Calculating stats from gps tracks

tracks <- lapply(modelled, transform_lines)

tracks_jnd <- do.call(rbind, tracks)

mapview::mapview(tracks_jnd)


#### Merging metadata

metadata_df <- readxl::read_xlsx(metadata) %>% 
          janitor::clean_names() 


meta_w_stats <- merge(metadata_df, as.data.frame(tracks_jnd) %>% select(-geometry), by = "nombre_del_archivo_gps")



metatomerge <- metadata_df %>% 
      group_by(nombre_del_archivo_gps, actividad_turistica) %>%
      summarise(sights = sum(no_ballenas_adultas)) %>% 
      filter(actividad_turistica %in% c("jorobada", "ballena de aleta")) 

tracks_w_meta <- merge(do.call(rbind, modelled),  metatomerge, by = "nombre_del_archivo_gps") %>% 
      st_as_sf(., coords = c("longitud", "latitud"), crs = 4326, remove = F)

library(ggdensity)
tracks_w_meta %>% 
      filter(behaviour == "hauling") %>% 
      ggplot() +
      geom_hdr(aes(x=longitud, y=latitud, fill = after_stat(probs)), 
               alpha = 1) +
      geom_sf(col = "gray50", size = .1) +
      geom_sf(data = mx_shape, 
              fill = "gray90", 
              col = "black") +
      coord_sf(xlim = c(min(tracks_w_meta$longitud, na.rm = T), 
                        max(tracks_w_meta$longitud, na.rm = T)), 
               ylim = c(min(tracks_w_meta$latitud, na.rm = T), 
                        max(tracks_w_meta$latitud, na.rm = T))) +
      theme_void()


tracks_w_meta %>% 
      filter(behaviour == "hauling") %>% 
      ggplot() +
      geom_sf(aes(size = sights), 
              pch = 21, 
              alpha = .4,
              fill = "gray90", 
              col = "black") +
      geom_sf(data = mx_shape, 
              fill = "gray90", 
              col = "black") +
      coord_sf(xlim = c(min(tracks_w_meta$longitud, na.rm = T), 
                        max(tracks_w_meta$longitud, na.rm = T)), 
               ylim = c(min(tracks_w_meta$latitud, na.rm = T), 
                        max(tracks_w_meta$latitud, na.rm = T))) +
      theme_void()

### VIZ

shape <- st_read("outputs/TUR29_20220126_0828.gpkg")

library(dafishr)

shape %>% 
          ggplot() +
          geom_sf(data = mx_shape, 
                  col = NA, 
                  fill = "gray90") +
          geom_sf(aes(col = nombre_del_archivo_gps)) +
          coord_sf(xlim = c(-110.5, -110.1), ylim = c(24.2, 24.8)) +
          facet_wrap(~panga) +
          theme_bw() +
          theme(legend.position = "")
