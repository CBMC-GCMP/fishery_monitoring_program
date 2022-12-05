
process_gps <- function(file_path, path_to_metadatos) {
          
          metadatos <- readxl::read_xlsx(path_to_metadatos) %>% 
                    janitor::clean_names()
          
          archive_name <- basename(file_path) %>% 
                    str_remove(., ".csv")
          
          cleaned <- read_csv(file_path, col_names = F, show_col_types = F) %>% 
                    mutate(filename = str_remove(archive_name, ".csv")) %>% 
                    mutate(X8 = X8*-1) %>% 
                    select(-X7, -X9) 
          
          names(cleaned) <- c("ID", "date", "time_zulu", "date2", 
                              "time_local", "latitude", "longitude",
                              "angle", "speed", "nombre_del_archivo_gps")          
          
          dir.create("preprocessed/")
          
          buff <- cleaned %>% 
                    mutate(date = lubridate::ymd_hms(paste(date, time_zulu)), 
                           date2 = lubridate::ymd_hms(paste(date2, time_local))) %>% 
                    filter(date == min(date)) %>% 
                    select(buff = ID, longitude, latitude) %>% 
                    st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
                    st_buffer(., 500)
          
          
          cleaned_sf <- st_as_sf(cleaned, coords = c("longitude", "latitude"), crs = 4326, remove = F)
          
          
          
          cleaned_sf_buff <- st_join(cleaned_sf, buff) %>% 
                    mutate(location = ifelse(buff == 1, "at_port", "at_sea")) %>% 
                    mutate(location = replace_na(location, "at_sea")) %>% 
                    select(-buff) %>% 
                    select(-geometry) %>% 
                    as.data.frame()
          
          write_csv(cleaned_sf_buff, paste0("preprocessed/", archive_name, ".csv"), append = F)
          
          fishing_stats <- cleaned %>% 
                    mutate(date = lubridate::ymd_hms(paste(date, time_zulu)), 
                           date2 = lubridate::ymd_hms(paste(date2, time_local))) %>% 
                    group_by(nombre_del_archivo_gps) %>% 
                    summarise(final_time = max(date2), 
                              start_time = min(date2), 
                              horas_en_mar = as.double(difftime(max(date2), min(date2), units = "hours"))) %>% 
                    select(nombre_del_archivo_gps, 
                           horas_en_mar, 
                           final_time, 
                           start_time)
          
         merge(metadatos, fishing_stats, by = "nombre_del_archivo_gps") %>% 
                   write.csv(., "metadata_stats.csv")
          
}

