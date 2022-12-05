#' Changes the names of raw GPS files to standard ones
#'
#' It takes the names of the GPS output files and creates new files with the correct names. 
#' 
#' @param file_path a string indicating the folder containing the raw files
#' @param metadata path to metadata file that has the codename of the tracker. 
#'
#' @return saves a .csv file within an autocreated "preprocessed" folder. 
#' @export
#'
#' @examples
#' 

change_file_name_csv <- function(file_path, metadata) {
      if(missing(metadata)) {
            cat("The path to the metadata reference file must be set")
            stop()
      } else {
            
      out <- return(tryCatch({
            ## Getting track names
            track_names <- readr::read_csv(metadata) %>% 
                  janitor::clean_names() %>% 
                  select(nombre_del_archivo_gps = id_viaje, numero_del_tracker = numero_tracker) %>% 
                  mutate(nombre_del_archivo_gps = str_remove_all(nombre_del_archivo_gps, ".csv")) %>% 
                  unique()
            
            track_names$nombre_del_archivo_gps_corregido <- track_names$nombre_del_archivo_gps %>% 
                  str_remove(.,"export_") %>% 
                  str_remove_all(., "-") %>% 
                  str_replace_all(., " ", "_") %>% 
                  if_else(str_detect(., "nGPS"), .,
                          str_c(track_names$numero_del_tracker, ., sep = "_"))
            
            new_file_name <- basename(file_path) %>% 
                  str_remove_all(., ".csv") 
            
            dir.create("cleaned_csv/", showWarnings = F)
            
            cleaned <- read_csv(file_path, col_names = F, show_col_types = F) %>% 
                  mutate(nombre_del_archivo_gps = new_file_name) %>% 
                  merge(., track_names, by = "nombre_del_archivo_gps") %>% 
                  mutate(X8 = X8*-1) %>% 
                  select(-X7, -X9, -numero_del_tracker)
            
            names(cleaned) <- c("nombre_del_archivo_gps", "ID", "fecha", "hora_zulu", "fecha2", 
                                "hora_local", "latitud", "longitud",
                                "profundidad", "velocidad", "nombre_del_archivo_gps_corregido")
            
            cleaned <- cleaned %>% 
                  mutate(fecha_hora = as.POSIXct(paste(as.character(fecha), as.character(hora_local)), format = "%Y-%m-%d %H:%M:%S")) %>% 
                  relocate(fecha_hora, .after = ID) %>%  
                  mutate(ano = lubridate::year(fecha_hora), 
                         mes = lubridate::month(fecha_hora), 
                         dia = lubridate::month(fecha_hora)) %>% 
                  relocate(ano, .after = fecha_hora) %>% 
                  relocate(mes, .after = ano) %>% 
                  relocate(dia, .after = mes) 
            
            write_csv(cleaned, paste0("cleaned_csv/", unique(cleaned$nombre_del_archivo_gps_corregido), ".csv"))
            
            message(paste0("cleaned file: ", 
                           read_csv(file_path, col_names = F, show_col_types = F) %>% 
                                 mutate(nombre_del_archivo_gps = new_file_name) %>% 
                                 merge(., track_names, by = "nombre_del_archivo_gps") %>% 
                                 pull(nombre_del_archivo_gps_corregido) %>% 
                                 unique(),
                           " \n"))
            },
                      error = function(cond) {
                            message(paste("Can't clean file ", new_file_name, "\n"))
                         track_names <- readr::read_csv(metadata) %>% 
                            janitor::clean_names() %>% 
                            select(nombre_del_archivo_gps = id_viaje, numero_del_tracker = numero_tracker) %>% 
                            mutate(nombre_del_archivo_gps = str_remove_all(nombre_del_archivo_gps, ".csv")) %>% 
                            unique()
                         
                         track_names$nombre_del_archivo_gps_corregido <- track_names$nombre_del_archivo_gps %>% 
                            str_remove(.,"export_") %>% 
                            str_remove_all(., "-") %>% 
                            str_replace_all(., " ", "_") %>% 
                            if_else(str_detect(., "nGPS"), .,
                                    str_c(track_names$numero_del_tracker, ., sep = "_"))
                         
                         new_file_name <- basename(file_path) %>% 
                            str_remove_all(., ".csv") 
                         
                         dir.create("cleaned_csv/", showWarnings = F)
                         
                         cleaned <- read_csv(file_path, col_names = F, show_col_types = F) %>% 
                            mutate(nombre_del_archivo_gps = new_file_name) %>% 
                            merge(., track_names, by = "nombre_del_archivo_gps") %>% 
                            mutate(X8 = X8*-1) %>% 
                            select(-X7, -X9, -numero_del_tracker)
                         
                         names(cleaned) <- c("nombre_del_archivo_gps", "ID", "fecha", "hora_zulu", "fecha2", 
                                             "hora_local", "latitud", "longitud",
                                             "profundidad", "velocidad", "nombre_del_archivo_gps_corregido")
                         
                         cleaned <- cleaned %>% 
                            mutate(fecha_hora = as.POSIXct(paste(as.character(fecha), as.character(hora_local)), format = "%Y-%m-%d %H:%M:%S")) %>% 
                            relocate(fecha_hora, .after = ID) %>%  
                            mutate(ano = lubridate::year(fecha_hora), 
                                   mes = lubridate::month(fecha_hora), 
                                   dia = lubridate::month(fecha_hora)) %>% 
                            relocate(ano, .after = fecha_hora) %>% 
                            relocate(mes, .after = ano) %>% 
                            relocate(dia, .after = mes) 
                         
                         trackname <- basename(dirname(file_path)) %>% 
                            str_remove_all(., "-")
                         
                         filename <- unique(cleaned$nombre_del_archivo_gps_corregido) %>% as.data.frame() %>% 
                            filter(str_detect(., trackname)) %>% 
                            pull()
                         cleaned <- cleaned %>% 
                            filter(str_detect(nombre_del_archivo_gps_corregido, trackname))
                         
                         write_csv(cleaned, paste0("cleaned_csv/", filename, ".csv"))
                         
                            return(NULL)
                      }))
            return(out)
            
            
      
      }
}