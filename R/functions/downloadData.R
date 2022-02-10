
downloadData <- function(data_set_tag,destination = tempdir()){
  
  data_path <- str_c(destination,'/',data_set_tag)
  dir.create(data_path)
  
  pb_download(repo = 'jasenfinch/Spectral_binning_as_an_approach_to_post-acquisition_processing_of_FIE-HRMS_data',
              tag = data_set_tag,
              dest = data_path)
  
  data_path %>%
    list.files(full.names = TRUE,
               pattern = '.mzML.gz')
}
