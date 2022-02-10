
accuracyPlot <- function(HILIC_feature_matches){
  standards_purity_abundance <- HILIC_feature_matches %>% 
    select(Purity = purity,Abundance = Intensity,`PPM error`)
  
  abundance_ppm_plot <- standards_purity_abundance %>% 
    ggplot(aes(x = Abundance,y = `PPM error`)) +
    geom_point() +
    theme_bw() +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    labs(y = expression(bold(Delta ~ 'ppm')))
  
  purity_ppm_plot <- standards_purity_abundance %>% 
    ggplot(aes(x = Purity,y = `PPM error`)) +
    geom_point() +
    lims(x = c(0,1)) +
    theme_bw() +
    labs(y = expression(bold(Delta ~ 'ppm')))
  
  accuracy_plot <- abundance_ppm_plot + 
    purity_ppm_plot +
    plot_annotation(tag_levels = 'a',
                    tag_suffix = ')',
                    theme = theme(plot.title = element_text(face = 'bold'))) +
    plot_layout(ncol = 1) &
    theme(plot.title = element_text(face = 'bold'),
          plot.subtitle = element_text(face = 'bold'),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = 'bold'))
  
  return(accuracy_plot)
}
