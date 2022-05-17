
model_vms_artesanal <- function(path) {
          
          df <- read_csv(path) %>%
                    filter(!is.na(speed)) 
          
          at_sea <-  df %>% 
                    dplyr::filter(.data$location == "at_sea") %>%
                    dplyr::select(.data$ID, .data$speed)
          
                    
          out <- return(tryCatch(
                    {
                              EM_all <- mixtools::normalmixEM(at_sea$speed,
                                                              mu = c(10, 20, 30),
                                                              sigma = c(1, 1, 1)
                              )
                              
                              speed_threshold_EM <- EM_all$mu[1] + 1.96 * EM_all$sigma[1]
                              at_sea$behaviour <- ifelse(at_sea$speed < speed_threshold_EM,
                                                         "hauling", "not_hauling"
                              )
                              at_sea <- dplyr::select(at_sea, -.data$speed)
                              
                              res <- merge(df, at_sea, by = "ID", all.x = T) %>%
                                        dplyr::mutate(vessel_state = ifelse(.data$location == "at_port", "at_port", .data$behaviour)) %>%
                                        dplyr::mutate(vessel_state = ifelse(.data$vessel_state == "at_port" & .data$speed > 0, "not_hauling", .data$vessel_state)) %>%
                                        dplyr::mutate(model_type = "three_states")
                              
                              return(res)
                    },
                    error = function(test) {
                              message(paste("Test two state basic parameter \n"))
                              
                              EM_all <- mixtools::normalmixEM(at_sea$speed,
                                                              mu = c(5, 15),
                                                              sigma = c(1, 1)
                              )
                              
                              speed_threshold_EM <- EM_all$mu[1] + 1.96 * EM_all$sigma[1]
                              at_sea$behaviour <- ifelse(at_sea$speed < speed_threshold_EM,
                                                         "hauling", "not_hauling"
                              )
                              at_sea <- dplyr::select(at_sea, -.data$speed)
                              
                              test <- merge(df, at_sea, by = "ID", all.x = T) %>%
                                        dplyr::mutate(vessel_state = ifelse(.data$location == "at_port", "at_port", .data$behaviour)) %>%
                                        dplyr::mutate(vessel_state = ifelse(.data$vessel_state == "at_port" & .data$speed > 0, "not_hauling", .data$vessel_state)) %>%
                                        dplyr::mutate(model_type = "two_states") 
                              return(test)
                    },
                    error = function(cond) {
                              message(paste("Parameters failed, using manual threshold:", "\n"))
                              
                              df$behaviour <- ifelse(df$speed < 2,
                                                     "hauling", "not_hauling")
                              test <- df %>%
                                        dplyr::mutate(behaviour = "not_modelled", vessel_state = "not_modelled", model_type = "manual_threshold") %>%
                                        dplyr::mutate(vessel_state = ifelse(.data$vessel_state != "at_port" & .data$speed < 5, "hauling", "not_hauling"))
                              
                              # Choose a return value in case of error
                              return(test)
                    }
          ))
          return(out)
}
