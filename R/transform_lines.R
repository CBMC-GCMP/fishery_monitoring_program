transform_lines <- function(preprocessed_file, metadata_files) {
          
          res <- read_csv(preprocessed_file, show_col_types = F)  %>% 
                    st_as_sf(., coords = c("longitud", "latitud"), crs = 4326, remove = F) %>% 
                    filter(ubicacion == "at_sea") %>% 
                    group_by(nombre_del_archivo_gps) %>% 
                    summarize(start_time = min(fecha_hora), 
                              finish_time = max(fecha_hora), 
                              horas_en_mar = as.double(difftime(max(fecha_hora), 
                                                                min(fecha_hora), 
                                                                units = "hours")), 
                              do_union = FALSE) %>% 
                    st_cast("LINESTRING") %>% 
                    mutate(length = st_length(.)) %>% 
                    relocate(length, .after = horas_en_mar) 
          
          dir.create("outputs")
          dir.create("outputs/shp/")
          
          st_write(res, paste0("outputs/shp/", min(res$nombre_del_archivo_gps), "-to-", max(res$nombre_del_archivo_gps), ".gpkg"), append = F)
          
          dir.create("outputs/tables/")
          res %>% 
                    as.data.frame() %>% 
                    select(-geometry) %>% 
                    write_csv(., paste0("outputs/tables/", min(.$nombre_del_archivo_gps), "-to-", max(.$nombre_del_archivo_gps), ".csv"), append = F)
}
