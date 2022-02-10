
standards <- drake_plan(
  ## get standards file paths
  standards_file_paths = downloadData('standards-mix',
                                      destination = 'data'),
  
  ## load standards compound info
  standards_compound_info = read_csv(file_in('data/standards_info.csv')),
  
  ## Detect standards data binning parameters
  standards_binning_parameters = detectParameters(standards_file_paths),
  
  ## bin standards data
  standards_binned_data = map(2:3,~{
    options(binner_dp = .x)
    
    binneRlyse(standards_file_paths,
               tibble(fileName = basename(standards_file_paths),
                      class = 1),
               standards_binning_parameters)
  }) %>% 
    set_names(2:3),
  
  ## prep standards features
  standards_features = names(standards_binned_data) %>%
    map(~{
      decimal_places <- .x
      standards_binned_data[[decimal_places]] %>% 
        binnedData() %>%
        map(~{
          .x %>%
            analysisData(.,standards_binned_data[[decimal_places]] %>%
                           sampleInfo()) %>%
            metabolyseR::occupancyMaximum(occupancy = 1) %>%
            dat() %>%
            rowid_to_column(var = 'Sample') %>%
            gather(Feature,Intensity) %>%
            group_by(Feature) %>%
            summarise(Intensity = mean(Intensity),
                      .groups = 'drop')
        }) %>%
        bind_rows() %>%
        mutate(Mode = str_sub(Feature,1,1),
               mz = str_remove_all(Feature,'[:alpha:]') %>%
                 as.numeric()) %>%
        left_join(standards_binned_data[[decimal_places]] %>% 
                    accurateData() %>% 
                    mutate(Feature = str_c(polarity,mz)) %>% 
                    select(Feature,purity,centrality) %>% 
                    group_by(Feature) %>% 
                    summarise(purity = mean(purity),
                              centrality = mean(centrality)),
                  by = 'Feature')
    }) %>% 
    set_names(2:3) %>% 
    bind_rows(.id = 'dp'),
  
  ## calculate standards standards descriptors
  standards_metabolite_database = standards_compound_info %>%
    rename(InChI = INCHI) %>%
    mutate(ID = 1:nrow(.) %>%
             as.character()) %>% 
    select(ID,everything()) %>%
    rowwise() %>%
    mutate(SMILES = convert(InChI,'inchi','smiles')) %>%
    metaboliteDB(.,descriptors(.$SMILES)),
  
  ## Negative mode adducts to search
  negative_mode_adducts = c("[M-H]1-","[M+Cl]1-","[M+Cl37]1-","[M+K-2H]1-", 
                            "[2M-H]1-","[M-2H]2-"),
  
  ## Positive mode adducts to search
  positive_mode_adducts = c("[M+H]1+","[M+K]1+","[M+K41]1+","[M+Na]1+",
                            "[2M+H]1+","[M+2H]2+"),
  
  ## adducts to search
  search_adducts = c(negative_mode_adducts,
                     positive_mode_adducts),
  
  ## calculate adduct m/z for standards standards
  standards_adduct_mzs =  standards_metabolite_database %>%
    getAccessions() %>%
    .$ID %>%
    map(calcAdducts,db = standards_metabolite_database) %>%
    set_names(standards_metabolite_database %>%
                getAccessions() %>%
                .$ID) %>%
    bind_rows(.id = 'ID') %>%
    left_join(adducts() %>% 
                select(Name,Nelec),
              by = c('Adduct' = 'Name')) %>%
    mutate(Mode = NA) %>%
    {
      .$Mode[.$Nelec > 0] <- 'n'
      .$Mode[.$Nelec < 0] <- 'p'
      .
    } %>%
    select(-Nelec,-MF) %>%
    filter(Possible == TRUE,
           Adduct %in% adducts()$Name[adducts()$Default == 1]) %>%
    left_join(standards_metabolite_database %>%
                getAccessions(),by = 'ID') %>%
    left_join(standards_metabolite_database %>%
                getDescriptors() %>%
                select(ID,MF),by = 'ID') %>%
    select(ID,Name = NAME,InChI,MF,Adduct,Mode,`m/z`) %>%
    filter(Adduct %in% search_adducts),
  
  ## search standards standards binned data for adduct matches
  standards_feature_matches = standards_adduct_mzs %>%
    split(1:nrow(.)) %>%
    map(~{
      add <- .x
      ppm_range <- ppmRange(add$`m/z`,10)
      standards_features %>%
        {
          matches <- filter(.,Mode == add$Mode,mz > ppm_range$lower,
                            mz < ppm_range$upper) %>%
            mutate(`PPM error` = ppmError(mz,add$`m/z`) %>%
                     abs())
          if(nrow(matches) > 0){
            bind_cols(select(add,
                             ID:Adduct,
                             `Theoretical m/z` = `m/z`),
                      matches)
          } else {
            NULL
          }
        }
    }) %>%
    bind_rows() %>%
    ungroup(),
  
  ## retreive predicted adduct m/z that were unmatched but had bins present in the data set
  unmatched_standards_adducts_bins = standards_adduct_mzs %>%
    anti_join(standards_feature_matches,
              by = c('Adduct' = 'Adduct','m/z' = 'Theoretical m/z')) %>% 
    mutate(Bin = round(`m/z`,digits = 2)) %>% 
    left_join(standards_features %>% 
                mutate(Bin = round(mz,digits = 2)),
              by = c('Mode','Bin')) %>% 
    filter(!is.na(Feature)),
  
  ## Filter standards standards base peak adducts
  standards_base_peak_matches = standards_feature_matches %>%
    group_by(ID) %>% 
    filter(Intensity == max(Intensity)) %>%
    ungroup() %>%
    select(Metabolite = Name,`Molecular formula` = MF,Adduct:`Theoretical m/z`,
           `Measured m/z` = mz,Abundance = Intensity,purity:`PPM error`),
  
  ## plot the matched peaks ppm error with abundance an purity
  standards_accuracy_plot = accuracyPlot(standards_feature_matches)
)