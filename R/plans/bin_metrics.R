
bin_metrics <- drake_plan(
  ## calculate bin measures
  bin_measures = calcMetrics(accurate_data),
  
  ## extract top 20 intense 0.01 amu bins with purity < 0.5
  low_purity_bins = bin_measures %>% 
    filter(Matrix %in% c('Brachypodium leaf','Human urine'),amu == 0.01,Purity < 0.5) %>% 
    arrange(desc(intensity)) %>%
    .[1:20,] %>%
    {
      .$Mode[.$Mode == 'Positive'] <- 'p'
      .$Mode[.$Mode == 'Negative'] <- 'n'
      .
    },
  
  ## extract top 20 intense 0.01 amu bins with purity > 0.5
  high_purity_bins = bin_measures %>% 
    filter(Matrix %in% c('Brachypodium leaf','Human urine'),amu == 0.01,Purity > 0.5) %>% 
    arrange(desc(intensity)) %>%
    .[1:20,] %>%
    {
      .$Mode[.$Mode == 'Positive'] <- 'p'
      .$Mode[.$Mode == 'Negative'] <- 'n'
      .
    },
  
  ## plot low purity bins
  low_purity_bin_plots = low_purity_bins %>%
    split(1:nrow(.)) %>%
    map(binExample,accurate_data,
        title = 'a) Low purity',
        type = 'Purity') %>%
    set_names(str_c(low_purity_bins$Matrix,
                    low_purity_bins$Mode,
                    low_purity_bins$Bin,
                    sep = '-')),
  
  ## plot high purity bins
  high_purity_bin_plots = high_purity_bins %>%
    split(1:nrow(.)) %>%
    map(binExample,accurate_data,
        title = 'b) High purity',
        type = 'Purity') %>%
    set_names(str_c(high_purity_bins$Matrix,
                    high_purity_bins$Mode,
                    high_purity_bins$Bin,
                    sep = '-')),
  
  ## example low purity bin
  example_low_purity_bin_plot = low_purity_bin_plots$`Human urine-p-163.12`,
  
  ## example high purity bin
  example_high_purity_bin_plot = high_purity_bin_plots$`Human urine-p-114.07`,
  
  ## extract top 20 intense 0.01 amu bins with centrality < 0.5
  low_centrality_bins = bin_measures %>% 
    filter(Matrix %in% c('Brachypodium leaf','Human urine'),amu == 0.01,Centrality < 0.5) %>% 
    arrange(desc(intensity)) %>%
    .[1:20,] %>%
    {
      .$Mode[.$Mode == 'Positive'] <- 'p'
      .$Mode[.$Mode == 'Negative'] <- 'n'
      .
    },
  
  ## extract top 20 intense 0.01 amu bins with centrality > 0.5
  high_centrality_bins = bin_measures %>% 
    filter(Matrix %in% c('Brachypodium leaf','Human urine'),amu == 0.01,Centrality > 0.5) %>% 
    arrange(desc(intensity)) %>%
    .[1:20,] %>%
    {
      .$Mode[.$Mode == 'Positive'] <- 'p'
      .$Mode[.$Mode == 'Negative'] <- 'n'
      .
    },
  
  ## plot low purity bins
  low_centrality_bin_plots = low_centrality_bins %>%
    split(1:nrow(.)) %>%
    map(binExample,accurate_data,
        'a) Low centrality',
        type = 'Centrality') %>%
    set_names(str_c(low_centrality_bins$Matrix,
                    low_centrality_bins$Mode,
                    low_centrality_bins$Bin,
                    sep = '-')),
  
  ## plot high purity bins
  high_centrality_bin_plots = high_centrality_bins %>%
    split(1:nrow(.)) %>%
    map(binExample,
        accurate_data,
        'b) High centrality',
        
        type = 'Centrality') %>%
    set_names(str_c(high_centrality_bins$Matrix,
                    high_centrality_bins$Mode,
                    high_centrality_bins$Bin,
                    sep = '-')),
  
  ## example low purity bin
  example_low_centrality_bin_plot = low_centrality_bin_plots$`Brachypodium leaf-p-94.05`,
  
  ## example high purity bin
  example_high_centrality_bin_plot = high_centrality_bin_plots$`Human urine-p-61.04`,
  
  ## bin measures summary
  bin_measures_summary = bin_measures %>%
    gather('Measure','Score',-(Matrix:Bin)) %>%
    group_by(Matrix,Mode,amu,Measure) %>%
    summarise(N = n(),
              Mean = mean(Score),
              SD = sd(Score),
              .groups = 'drop'
    ) %>%
    mutate(SE = SD / sqrt(N)) %>%
    arrange(Matrix,Mode,desc(amu)) %>%
    ungroup() %>%
    mutate(amu = as.character(amu)) %>%
    {
      .$amu[.$amu == "1e-04"] <- '0.0001' 
      .
    },
  
  ## plot bin purity
  bin_purity_plot = binPurityPlot(bin_measures_summary),
  
  ## plot bin centrality
  bin_centrality_plot = binCentralityPlot(bin_measures_summary)
)