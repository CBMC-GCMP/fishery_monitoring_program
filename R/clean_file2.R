
clean_file2 <- function(file_path) {
          
          archive_name <- basename(file_path) %>% 
                    str_remove(., ".csv")
          
          cleaned <- read_csv(file_path, col_names = T, show_col_types = F) %>% 
                    mutate(filename = str_remove(archive_name, ".csv")) %>% 
                    mutate(X8 = X8*-1) %>% 
                    select(-X7, -X9) 
          
          names(cleaned) <- c("ID", "fecha", "hora_zulu", "fecha2", 
                              "hora_local", "latitud", "longitud",
                              "profundidad", "velocidad", "nombre_del_archivo_gps")          
          
          dir.create("preprocessed/", showWarnings = F)
          
          buff <- cleaned %>% 
                    mutate(fecha = lubridate::ymd_hms(paste(fecha, hora_zulu)), 
                           fecha2 = lubridate::ymd_hms(paste(fecha2, hora_local))) %>% 
                    filter(fecha == min(fecha)) %>% 
                    select(buff = ID, longitud, latitud) %>% 
                    sf::st_as_sf(., coords = c("longitud", "latitud"), crs = 4326) %>% 
                    sf::st_buffer(., 200)
          
          cleaned_sf <- sf::st_as_sf(cleaned, coords = c("longitud", "latitud"), crs = 4326, remove = F)
          
          cleaned_sf_buff <- sf::st_join(cleaned_sf, buff) %>% 
                    mutate(ubicacion = ifelse(buff == 1, "at_port", "at_sea")) %>% 
                    mutate(ubicacion = replace_na(ubicacion, "at_sea")) %>% 
                    select(-buff) %>% 
                    as.data.frame() %>% 
                    select(-geometry) %>% 
                    select(-fecha, -hora_zulu) %>% 
                    mutate(fecha_hora = as.POSIXct(paste(fecha2, hora_local), format="%Y-%m-%d %H:%M:%S")) %>% 
                    relocate(fecha_hora, .after = ID) %>%  
                    mutate(ano = lubridate::year(fecha_hora), 
                           mes = lubridate::month(fecha_hora), 
                           dia = lubridate::month(fecha_hora)) %>% 
                    relocate(ano, .after = fecha_hora) %>% 
                    relocate(mes, .after = ano) %>% 
                    relocate(dia, .after = mes) %>% 
                    rename(fecha = fecha2)
          
          
          write_csv(cleaned_sf_buff, paste0("preprocessed/", archive_name, ".csv"), append = F)
          
          
}
