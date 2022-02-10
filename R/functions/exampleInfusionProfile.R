
exampleInfusionProfile <- function(example_header,example_infusion_scans) {
  
  yup <- max(example_header$totIonCurrent) + max(example_header$totIonCurrent) * 0.05
  
  example_header %>%
    ggplot(aes(x = scan,y = totIonCurrent)) +
    geom_rect(xmin = min(example_infusion_scans),xmax = max(example_infusion_scans),ymin = 0,ymax = yup,fill = "#F0F0F0") +
    geom_line() +
    # geom_vline(xintercept = c(min(example_infusion_scans),max(example_infusion_scans)),linetype = 2,colour = 'red') +
    geom_hline(yintercept = max(example_header$totIonCurrent)/2,linetype = 2,colour = 'red') +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = 'bold')) +
    scale_y_continuous(limits = c(0,yup),expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = 'Scan',
         y = 'Total ion count')
}
