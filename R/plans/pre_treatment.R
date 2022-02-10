
pre_treatment <- drake_plan(
  ## bin occupancy
  bin_occupancy = all_data %>%
    filter(amu == 0.01,Intensity > 0) %>%
    group_by(Matrix,Mode,`m/z`) %>%
    summarise(N = n(),Occupancy = N/10,
              Percent = Occupancy * 100,
              .groups = 'drop'),
  
  ## bin RSD
  bin_rsd = all_data %>%
    filter(amu == 0.01) %>%
    group_by(Matrix,Mode,`m/z`) %>%
    summarise(MeanIntensity = mean(Intensity),
              LogIntensity = log10(MeanIntensity),
              RSD = sd(Intensity)/MeanIntensity,
              .groups = 'drop') %>%
    left_join(bin_occupancy, by = c('Matrix',"Mode", "m/z")) %>%
    {
      .$Mode[.$Mode == 'n'] <- 'Negative'
      .$Mode[.$Mode == 'p'] <- 'Positive'
      .
    },
  
  ## bin occupancy plot
  bin_occupancy_plot = bin_rsd %>%
    ggplot() +
    geom_boxplot(aes(x = Percent,
                     y = LogIntensity,
                     group = Percent,
                     colour = Matrix)) +
    scale_colour_ptol() +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.title = element_text(face = 'bold'),
          axis.title = element_text(face = 'bold'),
          legend.margin = ggplot2::margin(t = -8,b = -1),
          plot.title = element_text(face = 'bold'),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          strip.background = element_blank(),
          strip.text = element_text(face = 'bold',
                                    size = 11)) +
    labs(x = 'Occupancy (%)',
         y = expression(bold(Log[10]~Abundance)),
         fill = 'Biological matrix') +
    facet_grid(Matrix~Mode) +
    scale_x_continuous(breaks = seq(0,100,10)) +
    guides(colour = 'none'),
  
  ## bin RSD plot
  bin_rsd_plot = bin_rsd %>%
    filter(Occupancy > 2/3) %>%
    {
      ggplot(.,aes(x = LogIntensity,y = RSD,colour = Matrix)) +
        geom_point(size = 0.25, alpha = 0.5) + 
        theme_bw() +
        scale_colour_ptol() +
        labs(x = expression(bold(Log[10]~Abundance))) +
        theme(legend.position = 'bottom',
              legend.title = element_text(face = 'bold'),
              axis.title = element_text(face = 'bold'),
              legend.margin = margin(t = -8,b = -1),
              plot.title = element_text(face = 'bold'),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(),
              strip.background = element_blank(),
              strip.text = element_text(face = 'bold',
                                        size = 11)) +
        facet_grid(Matrix~Mode) +
        guides(colour = 'none') +
        scale_x_continuous(breaks = seq(0,8,1))
    }
)