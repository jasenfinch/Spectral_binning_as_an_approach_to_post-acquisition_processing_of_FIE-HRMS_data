
performancePlot <- function(performance_summary){
  processing_time_plot <- performance_summary %>% 
    rename(time = `time (seconds)`) %>% 
    mutate(workers = factor(workers),
           time = time/60) %>% 
    ggplot(aes(x = `# samples`,
               y = `time`)) +
    geom_line(aes(group = workers,
                  colour = workers),
              linetype = 2) +
    geom_point(aes(fill = workers),
               shape = 21,
               size = 2) +
    scale_colour_ptol() +
    scale_fill_ptol() +
    scale_x_log10() +
    theme_bw() +
    theme(plot.title = element_text(face = 'bold',
                                    hjust = 0.5),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = 'bold'),
          legend.title = element_text(face = 'bold')) +
    labs(title = 'a) Processing time',
         x = 'Number of data files',
         y = 'Elapsed time (minutes)',
         colour = 'CPU workers',
         fill = 'CPU workers')
  
  memory_plot <- performance_summary %>% 
    rename(`RAM usage` = `RAM usage (MB)`) %>% 
    mutate(workers = factor(workers),
           `RAM usage` = `RAM usage` %>% 
             str_c(.,'MB') %>% 
             fs_bytes()) %>% 
    ggplot(aes(x = `# samples`,
               y = `RAM usage`)) +
    geom_line(aes(group = workers,
                  colour = workers),
              linetype = 2) +
    geom_point(aes(fill = workers),
               shape = 21,
               size = 2) +
    scale_colour_ptol() +
    scale_fill_ptol() +
    scale_y_continuous(labels = label_bytes()) +
    scale_x_log10() +
    theme_bw() +
    theme(plot.title = element_text(face = 'bold',
                                    hjust = 0.5),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = 'bold'),
          legend.title = element_text(face = 'bold')) +
    labs(title = 'b) Memory usage',
         x = 'Number of data files',
         colour = 'CPU workers',
         fill = 'CPU workers')
  
  processing_time_plot + 
    memory_plot +
    plot_layout(ncol = 1,
                guides = 'collect')
}