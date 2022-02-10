
dataPrep <- function(binned_data){
  
  binned_data %>% 
    map(accurateData) %>% 
    bind_rows() %>%
    mutate(dp = bin %>% as.character() %>% 
             str_extract('(?<=[.])[\\w+.-]+') %>% 
             nchar(),
           amu = 10 ^ -dp) %>% 
    replace_na(replace = list(dp = 0,amu = 1)) %>%
    relocate(amu,.after = matrix) %>% 
    rename(Mode = polarity,
           Matrix = matrix,
           Sample = fileName,
           `m/z` = mz,
           Bin = bin,
           Intensity = intensity,
           Purity = purity,
           Centrality = centrality) %>% 
    mutate(Matrix = Matrix %>% 
             str_replace_all('BdistachyonTechnical',
                             'Brachypodium leaf') %>% 
             str_replace_all('PlasmaTechnical',
                             'Human plasma') %>% 
             str_replace_all('SerumTechnical',
                             'Horse serum') %>%
             str_replace_all('UrineTechnical',
                             'Human urine')
           ) %>% 
    select(-dp)
}


