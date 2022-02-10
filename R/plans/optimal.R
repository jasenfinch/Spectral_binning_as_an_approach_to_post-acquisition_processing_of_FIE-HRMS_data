
optimal <- drake_plan(
  ## matrix summary
  matrix_features_overview = binned_intensity_matrices %>% 
    select(Matrix,amu,Mode,`m/z`) %>%
    distinct() %>% 
    group_by(Matrix,amu,Mode) %>% 
    summarise(N = n(),
              .groups = 'drop'),
  
  ## matrix missing data summary
  matrix_missing_data = binned_intensity_matrices %>%
    filter(Intensity == 0) %>%
    select(Matrix,Mode,amu,Sample,`m/z`) %>%
    distinct() %>%
    group_by(Matrix,Mode,amu) %>%
    summarise(N = n(),
              .groups = 'drop') %>%
    left_join(matrix_features_overview %>%
                rename(Total = N), by = c("Matrix", "Mode", "amu")) %>%
    mutate(Total = Total * 10,Percent = N/Total * 100),
  
  ## Summary of missing data in 0.01 amu binned B. distachyon data without single scan m/z filtering
  dp_2_b_dist_no_sse_filter_missing_data_overview = dp_2_b_dist_no_sse_filter %>% 
    filter(Intensity == 0) %>% 
    select(Mode,Sample,`m/z`) %>%
    distinct() %>%
    group_by(Mode) %>%
    summarise(N = n(),
              .groups = 'drop') %>%
    left_join(dp_2_b_dist_no_sse_filter %>%
                select(Mode,`m/z`) %>%
                distinct() %>% 
                group_by(Mode) %>% 
                summarise(Total = n(),
                          .groups = 'drop'), 
  by = "Mode") %>%
    mutate(Total = Total * 10,Percent = N/Total * 100),
  
  ## extract the amu deviation of malate [M-H]1- from an example Brachypodium sample
  malate_single_sample_deviation = files[[1]][1] %>%
    {
      s <- detectInfusionScans(.)
      
      file <- openMSfile(.)
      
      h <- file %>%
        header() %>%
        as_tibble() %>%
        filter(polarity == 0) %>%
        rowid_to_column(var = 'scan') %>%
        mutate(scan = as.numeric(scan)) %>%
        filter(scan %in% s)
      
      file %>%
        peaks() %>%
        .[h$seqNum] %>%
        map(as_tibble) %>%
        set_names(h$scan) %>%
        bind_rows(.id = 'scan') %>%
        mutate(scan = as.numeric(scan)) %>%
        rename(mz = V1,intensity = V2) %>%
        filter(mz > 133.01,mz < 133.02)
    } %>%
    mutate(deviation = mz - (sum(mz * intensity) / sum(intensity))), 
  
  ## plot single sample deviatio of malate [M-H]1-
  malate_single_sample_deviation_plot = ggplot(malate_single_sample_deviation) +
    geom_hline(yintercept = 0,linetype = 2,colour = 'grey') +
    geom_segment(aes(x = scan,xend = scan,y = 0,yend = deviation),linetype = 2) +
    geom_point(aes(x = scan,y = deviation),size = 3,shape = 21,fill = ptol_pal()(1)) +
    theme_bw() +
    labs(title = 'a) m/z deviation between\nscans in a single sample.',
         x = 'Scan',
         y = 'm/z deviation (amu)') +
    theme(panel.grid = element_blank(),
          axis.title = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold'),
          panel.border = element_blank(),
          axis.line = element_line()) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = seq(5,11,1)),
  
  ## extract amu deviation of malate [M-H]1- across injection of Brachypodium samples
  malate_sample_deviation = files[[1]] %>%
    map(~{
      f <- .x %>%
        openMSfile()
      
      s <- detectInfusionScans(.x)
      
      h <- f %>%
        header() %>%
        as_tibble() %>%
        filter(polarity == 0) %>%
        rowid_to_column(var = 'scan') %>%
        mutate(scan = as.numeric(scan)) %>%
        filter(scan %in% s)
      
      f %>%
        peaks() %>%
        .[h$seqNum] %>%
        map(as_tibble,name.repair = make.names) %>%
        set_names(h$scan) %>%
        bind_rows(.id = 'scan') %>%
        mutate(scan = as.numeric(scan)) %>%
        rename(mz = V1,intensity = V2) %>%
        filter(mz > 133.0135,mz < 133.0145) %>%
        {
          tibble(mz = sum(.$mz * .$intensity) / sum(.$intensity))
        }
    })%>%
    bind_rows() %>%
    rowid_to_column(var = 'injection') %>%
    mutate(deviation = mz - mean(mz)),
  
  ## plot sample deviatio of malate [M-H]1-
  malate_sample_deviation_plot = ggplot(malate_sample_deviation) +
    geom_hline(yintercept = 0,linetype = 2,colour = 'grey') +
    geom_segment(aes(x = injection,xend = injection,y = 0,yend = deviation),linetype = 2) +
    geom_point(aes(x = injection,y = deviation),size = 3,shape = 21,fill = ptol_pal()(1)) +
    theme_bw() +
    labs(title = 'b) m/z deviation between\ninjections.',
         x = 'Injection',
         y = 'm/z deviation (amu)') +
    theme(panel.grid = element_blank(),
          axis.title = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold'),
          panel.border = element_blank(),
          axis.line = element_line()) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = seq(1,10,1)),
  
  ## extract 0.001 amu bins
  dp3_bins = all_data %>%
    filter(Matrix %in% c('Brachypodium leaf','Human urine'),amu == 0.001) %>% 
    select(Matrix,Mode,Bin) %>%
    distinct(),
  
  ## calculate 0.001 amu adjacent bins
  dp3_adjacent = calcAdjacentBins(dp3_bins,
                                  binned_intensity_matrices %>%
                                    filter(Matrix %in% c('Brachypodium leaf','Human urine'))),
  
  ## extract potentially split 0.001 amu bins
  dp3_split_bins = dp3_adjacent %>%
    filter(p < 0.05),
  
  ## plot split bins
  dp3_split_bin_plots = dp3_split_bins %>%
    split(1:nrow(.)) %>%
    map(~{
      accurate_data %>%
        mutate(dp2 = round(`m/z`,2)) %>%
        filter(Matrix == .x$Matrix,Mode == .x$Mode,dp2 == .x$dp2) %>%
        binPlot(.x$dp2,.x$Bin1,.x$Bin2)
    }) %>%
    set_names(dp3_split_bins %>% 
                mutate(name = str_c(Matrix,Mode,dp2,sep = '-')) %>%
                .$name),
  
  ## plot adjacent bin scatter plots
  dp3_adjacent_scatter_plots =  dp3_split_bins %>%
    split(1:nrow(.)) %>%
    map(~{
      binned_intensity_matrices %>%
        filter(amu == 0.001) %>% 
        mutate(Bin = round(`m/z`,3)) %>% 
        filter(Matrix == .x$Matrix,
               Mode == .x$Mode,
               Bin %in% c(.x$Bin1,.x$Bin2)
        ) %>%
        select(Sample,Bin,Intensity) %>%
        adjacentScatterPlot(as.character(.x$Bin1),as.character(.x$Bin2))
    }) %>%
    set_names(dp3_split_bins %>% 
                mutate(name = str_c(Matrix,Mode,dp2,sep = '-')) %>%
                .$name),
  
  ## example bin to use in manuscript
  example_bin = 'Human urine-n-215.09',
  
  ## example split bin plot
  example_dp3_split_bin_plot = dp3_split_bin_plots[[example_bin]],
  
  ## example adjacent bin scatter plot
  example_dp3_adjacent_scatter_plot = dp3_adjacent_scatter_plots[[example_bin]]
)