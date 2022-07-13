transform_lines <- function(preprocessed_file) {
          
      if(is.character(preprocessed_file)) {
            
            res <- read_csv(preprocessed_file, show_col_types = F)  %>% 
                  ungroup() %>% 
                  sf::st_as_sf(., coords = c("longitud", "latitud"), crs = 4326, remove = F) %>% 
                  group_by(nombre_del_archivo_gps) %>% 
                  summarize(start_time = min(fecha_hora), 
                            nombre_del_archivo_gps_corregido = unique(nombre_del_archivo_gps_corregido),
                            finish_time = max(fecha_hora), 
                            horas_en_mar = as.double(difftime(max(fecha_hora), 
                                                              min(fecha_hora), 
                                                              units = "hours")), 
                            do_union = FALSE) %>% 
                  st_cast("LINESTRING") %>% 
                  mutate(distancia_km = as.numeric(st_length(.))*0.001) %>% 
                  relocate(distancia_km, .after = horas_en_mar) 
            
      }else {
            
            res <- preprocessed_file  %>% 
                  ungroup() %>% 
                  sf::st_as_sf(., coords = c("longitud", "latitud"), crs = 4326, remove = F) %>% 
                  group_by(nombre_del_archivo_gps) %>% 
                  summarize(start_time = min(fecha_hora), 
                            finish_time = max(fecha_hora),
                            nombre_del_archivo_gps_corregido = unique(nombre_del_archivo_gps_corregido),
                            horas_en_mar = as.double(difftime(max(fecha_hora), 
                                                              min(fecha_hora), 
                                                              units = "hours")), 
                            do_union = FALSE) %>% 
                  st_cast("LINESTRING") %>% 
                  mutate(distancia_km = as.numeric(st_length(.))*0.001) %>% 
                  relocate(distancia_km, .after = horas_en_mar) 
      }
          
          
}
