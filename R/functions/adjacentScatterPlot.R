
adjacentScatterPlot <- function(bin,dp3_bin1,dp3_bin2){
  
  adjacent <- bin %>%
    spread(Bin,Intensity) %>%
    {
      colnames(.)[2:3] <- c('Bin1','Bin2')
      .
    }
  
  xmin <- min(adjacent$Bin1)
  xmax <- max(adjacent$Bin1)
  ymin <- min(adjacent$Bin2)
  ymax <- max(adjacent$Bin2)
  
  cors <- rcorr(adjacent$Bin1,
                adjacent$Bin2) %>%
    {tibble(r = .$r[1,2],
            p = .$P[1,2],
            x = xmin + ((xmax - xmin) * 0.5),
            y = ymin + ((ymax - ymin) * 0.95))} 
  
  if (cors$p < 0.001) {
    cors <- cors %>%
      mutate(label =  str_c('italic(r) == ',r %>% signif(3),'~italic(p) < 0.001'))
  } else {
    cors <- cors %>%
      mutate(label =  str_c('italic(r) == ',r %>% signif(3),'~italic(p) == ',p %>% signif(3)))
  }
  
  ggplot(adjacent,aes(x = Bin1,y = Bin2)) +
    geom_smooth(method = 'lm',se = F,colour = 'grey') +
    geom_point(shape = 21,fill = ptol_pal()(1),size = 3) +
    geom_text(data=cors,aes(x = x,y = y,label = label),parse = T) +
    theme_bw(base_size = 10) +
    labs(x = str_c('Bin ',dp3_bin1,'m/z abundance'),
         y = str_c('Bin ',dp3_bin2,'m/z abundance')) +
    theme(plot.title = element_text(face = 'bold'),
          axis.title = element_text(face = 'bold'),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line())
}
