
binPlot <- function(bin,example_bin,dp3_bin1,dp3_bin2){
  mzs <- bin %>%
    split(1:nrow(.)) %>%
    map(~{
      rep(.$`m/z`,.$Intensity)
    }) %>%
    unlist() %>%
    enframe(name = NULL)
  
  d <- mzs$value %>%
    density()
  
  yup <- d$y %>%
    max() %>%
    {. + (. * 0.05)}
  
  ggplot(mzs,aes(x = value)) +
    geom_rect(xmin = dp3_bin1 - 0.00055,xmax = dp3_bin1 + 0.00055,ymin = 0,ymax = yup,fill = wes_palette("Chevalier1")[3]) +
    geom_rect(xmin = dp3_bin2 - 0.00055,xmax = dp3_bin2 + 0.00055,ymin = 0,ymax = yup,fill = wes_palette("Chevalier1")[3]) +
    geom_density() +
    geom_vline(xintercept = seq(example_bin - 0.0055,example_bin + 0.0055,0.001),linetype = 2,colour = wes_palette("Darjeeling1")[1],alpha = 0.5) +
    theme_bw(base_size = 10) +
    labs(x = 'm/z',
         y = 'Density') +
    theme(plot.title = element_text(face = 'bold'),
          axis.text = element_text(angle = 20,hjust = 1),
          axis.title = element_text(face = 'bold'),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line())  +
    scale_y_continuous(limits = c(0,yup),expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0))
}
