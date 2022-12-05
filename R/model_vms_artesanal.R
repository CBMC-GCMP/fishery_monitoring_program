
model_vms_artesanal <- function(x) {
          
          df <- x %>%
                    filter(!is.na(velocidad)) 
          
        
          out <- return(tryCatch(
                    {
                              EM_all <- mixtools::normalmixEM(df$velocidad,
                                                              mu = c(10, 20, 30),
                                                              sigma = c(1, 1, 1)
                              )
                              
                              speed_threshold_EM <- EM_all$mu[1] + 1.96 * EM_all$sigma[1]
                              df$behaviour <- ifelse(df$velocidad < speed_threshold_EM,
                                                         "hauling", "not_hauling"
                              )

                              res <- df %>%
                                        dplyr::mutate(model_type = "three_states")
                              
                              return(res)
                    },
                    error = function(test) {
                              message(paste("Test two state basic parameter \n"))
                              
                              EM_all <- mixtools::normalmixEM(df$velocidad,
                                                              mu = c(5, 15),
                                                              sigma = c(1, 1)
                              )
                              
                              speed_threshold_EM <- EM_all$mu[1] + 1.96 * EM_all$sigma[1]
                              df$behaviour <- ifelse(df$velocidad < speed_threshold_EM,
                                                         "hauling", "not_hauling"
                              )

                              test <- df %>% 
                                        dplyr::mutate(model_type = "two_states") 
                              return(test)
                    },
                    error = function(cond) {
                              message(paste("Parameters failed, using manual threshold:", "\n"))
                              
                              df$behaviour <- ifelse(df$velocidad < 2,
                                                     "hauling", "not_hauling")
                              test <- df %>%
                                        dplyr::mutate(behaviour = "not_modelled", model_type = "manual_threshold") 
                              # Choose a return value in case of error
                              return(test)
                    }
          ))
          return(out)
}
