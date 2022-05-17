## Loading libraries
library(tidyverse)
library(sf)



# Loading functions
lapply(list.files("R/", pattern = ".R", full.names = T), source)

# Setting parameters
community <- "PA"

file_list <- list.files("data/rawest_files/01/", recursive = T, full.names = T)

# Clean file names
lapply(file_list, change_file_name, community)

# Clean files
file_list <- list.files("preprocessed/", recursive = T, full.names = T)

lapply(file_list, clean_file2)

# Load files

file_list <- list.files("data/ILP_20180403/01_points/", recursive = T, full.names = T)

lapply(file_list, clean_file)

## Creating a shapefile 

transform_lines(list.files("preprocessed/", recursive = T, full.names = T))

#### Merging metadata

metadata <- readxl::read_xlsx("data/ILP_20180403/ILP-2018-04-03.xlsx") %>% 
          janitor::clean_names() 

merge_stats(metadata)


## Create summarystats
read_csv("outputs/tables/ILP_001_20180302_0702-to-ILP_003_20180324_0529_METADATA.csv") %>% 
          janitor::clean_names() %>%  
          select(-hora_de_inicio, hora_final) %>%  
          mutate(ganancia = captura_kg*precio) %>% 
          write_csv(., 
          paste0("outputs/tables/", 
                 min(.$nombre_del_archivo_gps), 
                 "-to-", 
                 max(.$nombre_del_archivo_gps), "_SUMMARY.csv"), append = F)


### VIZ

shape <- st_read("outputs/shp/ILP_001_20180302_0702-to-ILP_005_20180323_0535_METADATA.gpkg")

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
