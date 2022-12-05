
clean_land_points <- function(x, land_shape) {
      
      sf::sf_use_s2(FALSE)
      
      land_area <- sf::st_make_valid(sf::st_transform(land_shape, crs = 4326))
      
      x <- sf::st_as_sf(as.data.frame(readr::read_csv(x)) |>  filter(!is.na(longitud)), coords = c("longitud", "latitud"), crs = 4326, remove = F)
      x <- sf::st_difference(x, sf::st_union(land_area))
      x <- x %>%
            as.data.frame() %>%
            dplyr::select(-.data$geometry)
      
      
      gc()
      
      x
}