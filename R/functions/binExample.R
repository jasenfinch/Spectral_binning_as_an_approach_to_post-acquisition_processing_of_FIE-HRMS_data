
binExample <- function(bin,accurate_data,title,type = 'Purity'){
  bin_data <- accurate_data %>%
    mutate(Bin = round(`m/z`,2)) %>%
    inner_join(bin %>%
                 ungroup() %>%
                 select(Matrix,Mode,Bin),
               by = c("Matrix", "Mode", "Bin"))
  
  mzs <-  bin_data %>%
    split(1:nrow(.)) %>%
    map(~{
      rep(.$`m/z`,.$Intensity)
    }) %>%
    unlist() %>%
    enframe(name = NULL,value = 'm/z')
  
  
    
    ydown <- bin_data %>% 
      {max(.$Intensity)} %>% 
      {. + . * 0.05}
    
    density_plot <- mzs %>%
      ggplot(aes(x = `m/z`)) +
      geom_density() +
      theme_bw() +
      labs(title = title,
           subtitle = str_c('Bin = ',
                            bin$Bin,
                            ' m/z; ',
                            type,' = ',
                            bin[[type]] %>% 
                              signif(3)),
           y = 'Density') +
      theme(plot.title = element_text(face = 'bold'),
            axis.title.x = element_blank(),
            axis.title.y = element_text(face = 'bold'),
            axis.line.y = element_line(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            plot.margin = margin(t = 0)) +
      scale_x_continuous(limits = c(bin$Bin - 0.005,
                                    bin$Bin + 0.005),
                         expand = c(0,0),
                         position = 'top') +
      scale_y_continuous(expand = c(0,0))
    
    spectrum_plot <- bin_data %>% 
      ggplot() +
      geom_segment(aes(x = `m/z`,
                       xend = `m/z`,
                       y = 0,
                       yend = Intensity)) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      labs(y = 'Abundance') +
      theme(plot.title = element_text(face = 'bold'),
            axis.title = element_text(face = 'bold'),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(),
            plot.margin = margin(t = 0),
            axis.text.x = element_text(angle = 20,
                                       hjust = 1)) +
      scale_x_continuous(limits = c(bin$Bin - 0.005,
                                    bin$Bin + 0.005),
                         expand = c(0,0)) +
      scale_y_reverse(expand = c(0,0),
                      limits = c(ydown,0))
    
    wrap_plots(density_plot,
               spectrum_plot,
               ncol = 1)
}