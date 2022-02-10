
binCentralityPlot <- function(bin_measures){
  
  bin_measures %>%
    mutate(Matrix = str_replace_all(Matrix,'Brachypodium leaf','B. distachyon leaf'),
           Mode = factor(Mode) %>%
             fct_recode(`a) Negative ionisation mode` = 'Negative',
                        `b) Positive ionisation mode` = 'Positive')) %>%
    filter(Measure == 'Centrality') %>%
    {
      ggplot(.,aes(x = amu,y = Mean)) +
        geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),width = 0.3) +
        geom_line(aes(group = Matrix,colour = Matrix),linetype = 2) +
        geom_point(aes(fill = Matrix),shape = 21) +
        theme_bw() +
        theme(plot.title = element_text(face = 'bold'),
              legend.position = 'bottom',
              legend.title = element_text(face = 'bold'),
              axis.title = element_text(face = 'bold'),
              legend.margin = ggplot2::margin(t = -8,b = -1),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(),
              strip.background = element_blank(),
              strip.text = element_text(face = 'bold',
                                        size = 11,
                                        hjust = 0)) +
        scale_colour_ptol() +
        scale_fill_ptol() +
        labs(x = 'Bin width (amu)',
             y = 'Bin centrality',
             colour = 'Sample matrix',
             fill = 'Sample matrix') +
        facet_wrap(~Mode) +
        guides(colour = guide_legend(nrow = 2))
    }
}