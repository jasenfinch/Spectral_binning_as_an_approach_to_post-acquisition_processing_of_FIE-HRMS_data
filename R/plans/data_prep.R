
data_prep <- drake_plan(
  
  ## get file paths
  files = list(`Brachypodium leaf` = "BdistachyonTechnical",
               `Human plasma` = "PlasmaTechnical",
               `Human urine` = "UrineTechnical",
               `Horse serum` = "SerumTechnical") %>%
    map(~{
      metaboData::filePaths('FIE-HRMS',
                .x,
                ask = FALSE)
    }),
  
  ## decimal places for spectral binning
  decimal_places = 0:5,
  
  ## spectrally bin data
  binned_data = target(
    binData(
      files,
      decimal_places
    ),
    dynamic = cross(
      files,
      decimal_places
    )
  ),
  
  ## load all data
  all_data = binned_data %>%
    dataPrep(),
  
  ## gather intensity matrices
  binned_intensity_matrices = binned_data %>% 
    map(~{
      sample_info <- sampleInfo(.x) %>% 
        rename(Sample = fileName,
               Matrix = matrix,
               dp = dp)
      
      intensity_matrix <- .x %>% 
        binnedData() %>% 
        map(~{
          .x %>% 
            bind_cols(sample_info) %>% 
            gather(Feature,Intensity,-(Sample:dp))
        }) %>% 
        bind_rows(.id = 'Mode')
    }) %>% 
    bind_rows() %>% 
    mutate(Matrix = Matrix %>% 
             str_replace_all('BdistachyonTechnical',
                             'Brachypodium leaf') %>% 
             str_replace_all('PlasmaTechnical',
                             'Human plasma') %>% 
             str_replace_all('SerumTechnical',
                             'Horse serum') %>%
             str_replace_all('UrineTechnical',
                             'Human urine'),
           amu = 10 ^ -dp,
           `m/z` = str_remove_all(Feature,
                                  '[:alpha:]') %>% 
             as.numeric()) %>% 
    select(-dp),
  
  ## summarised accurate data
  accurate_data = binned_data %>% 
    map(~{
      .x@spectra$fingerprints
    }) %>% 
    bind_rows() %>%
    select(-bin) %>% 
    distinct() %>% 
    rename(Mode = polarity,
           Matrix = matrix,
           Sample = fileName,
           `m/z` = mz,
           Intensity = intensity) %>%
    mutate(Matrix = Matrix %>% 
             str_replace_all('BdistachyonTechnical',
                             'Brachypodium leaf') %>% 
             str_replace_all('PlasmaTechnical',
                             'Human plasma') %>% 
             str_replace_all('SerumTechnical',
                             'Horse serum') %>%
             str_replace_all('UrineTechnical',
                             'Human urine')) %>% 
    group_by(Matrix,Mode,`m/z`) %>%
    summarise(Intensity = mean(Intensity),
              .groups = 'drop'),
  
  ## bin B. distachyon files to 2 decimal places without single scan event filtering
  dp_2_b_dist_no_sse_filter = files$`Brachypodium leaf` %>% 
    {
      is <- detectInfusionScans(.)
      readFiles(.,dp = 2,scans = is[1]:is[2]) %>%
        map(~{
          as_tibble(.) %>%
            rowid_to_column(var = 'Sample') %>%
            gather('m/z','Intensity',-Sample)
        }) %>%
        bind_rows(.id = 'Mode')
    } %>%
    mutate(`m/z` = `m/z` %>% 
             str_replace_all('[:alpha:]','') %>% 
             as.numeric())
)