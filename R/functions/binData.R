
binData <- function(sample_files,dp){
  
  ## For some reason the drake dynamic branching inputs the vector of file paths within a list. This fixes it.
  sample_files <- sample_files[[1]]
  
  options(binner_dp = dp)
  
  sample_matrix <- sample_files %>% 
    map_chr(~{
      .x %>% 
        fs::path_split() %>%
        .[[1]] %>% 
        .[length(.) - 1]
    })
  
  sample_info <- tibble(
    fileName = basename(sample_files),
    matrix = sample_matrix,
    dp = dp)
  
  binning_parameters <- detectParameters(sample_files)
  cls(binning_parameters) <- 'matrix'
  
  binned_data <-  binneRlyse(
    sample_files,
    info = sample_info,
    binning_parameters) 
  
  options(binner_dp = NULL)
  
  return(binned_data)
}
