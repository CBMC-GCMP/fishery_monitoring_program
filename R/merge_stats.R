merge_stats <- function(metadata) {
          
          tables <- read_csv(list.files("outputs/tables", full.names = T))
          shps <- st_read(list.files("outputs/shp/", full.names = T, pattern = ".gpkg"))
          
          res <- merge(metadata, tables, by = "nombre_del_archivo_gps")
          write_csv(res, paste0("outputs/tables/", min(res$nombre_del_archivo_gps), "-to-", max(res$nombre_del_archivo_gps), "_METADATA.csv"), append = F)
          
          res_shp <- merge(shps, metadata, by = "nombre_del_archivo_gps", all = T)
          st_write(res_shp, paste0("outputs/shp/", min(res_shp$nombre_del_archivo_gps, na.rm = T), "-to-", max(res_shp$nombre_del_archivo_gps, na.rm = T), "_METADATA.gpkg"), append = F)
          
}