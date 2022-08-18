transform_lines <- function(preprocessed_file) {
          
      if(is.character(preprocessed_file)) {
            
            res <- read_csv(preprocessed_file, show_col_types = F)  %>% 
                  st_as_sf(., coords = c("longitud", "latitud"), crs = 4326, remove = F) %>% 
                  group_by(nombre_del_archivo_gps) %>% 
                  summarize(start_time = min(fecha_hora), 
                            finish_time = max(fecha_hora), 
                            horas_en_mar = as.double(difftime(max(fecha_hora), 
                                                              min(fecha_hora), 
                                                              units = "hours")), 
                            do_union = FALSE) %>% 
                  st_cast("LINESTRING") %>% 
                  mutate(distancia_m = as.numeric(st_length(.))) %>% 
                  relocate(distancia_m, .after = horas_en_mar) 
            
      }else {
            
            res <- preprocessed_file  %>% 
                  st_as_sf(., coords = c("longitud", "latitud"), crs = 4326, remove = F) %>% 
                  group_by(nombre_del_archivo_gps) %>% 
                  summarize(start_time = min(fecha_hora), 
                            finish_time = max(fecha_hora), 
                            horas_en_mar = as.double(difftime(max(fecha_hora), 
                                                              min(fecha_hora), 
                                                              units = "hours")), 
                            do_union = FALSE) %>% 
                  st_cast("LINESTRING") %>% 
                  mutate(distancia_m = as.numeric(st_length(.))) %>% 
                  relocate(distancia_m, .after = horas_en_mar) 
      }
          
          
}
