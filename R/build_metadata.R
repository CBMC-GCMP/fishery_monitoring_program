build_metadata <- function(folder_name, type) {
      
      filenames <- list.files(folder_name, recursive = T)
      
      metadata <- str_split_fixed(filenames, pattern = "/", 2) %>%  
            as.data.frame() %>% 
            rename(numero_del_tracker = V1, nombre_del_archivo_gps = V2) %>%  
            mutate(nombre_del_archivo_gps = str_remove_all(nombre_del_archivo_gps, ".csv"))
      
      xlsx::write.xlsx(metadata, row.names = F, paste0(folder_name, "_", type, ".xlsx"))
      
}