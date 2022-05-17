
#' Changes the names of raw GPS files to standard ones
#'
#' It takes the names of the GPS output files and creates new files with the correct names. 
#' 
#' @param file_path a string indicating the folder containing the raw files
#' @param community the fishery community code, it must be defined before usage. 
#'
#' @return saves a .csv file within an autocreated "preprocessed" folder. 
#' @export
#'
#' @examples
#' 
change_file_name <- function(file_path, community) {
          
          if(missing(community)) {
                    cat("A community code must be set. For example: PA")
                    stop()
          } else {
                    
                    new_file_name <- basename(file_path) %>% 
                              str_remove(.,"export_") %>% 
                              str_remove_all(., "-") %>% 
                              str_replace_all(., " ", "_") %>% 
                              str_c(community, ., sep = "_")
                    
                    dir.create("preprocessed/")
                    
                    read_csv(file_path, col_names = F, show_col_types = F) %>% 
                              write_csv(., paste0("preprocessed/", new_file_name))
                    cat(paste0("cleaned file: ", new_file_name, " \n"))
          }
         
}