calcMetrics <- function(accurate_data){
  
  dp <- 0:4
  
  metrics <- dp %>%
    map(~{
      pd <- .
      accurate_data %>%
        ungroup() %>%
        mutate(Bin = round(`m/z`,pd),amu = pd) %>%
        group_by(Matrix,Mode,amu,Bin) %>%
        summarise(
          intensity = sum(Intensity),
          Mean = binneR:::binMean(`m/z`,Intensity),
          MAE = binneR:::binMAE(`m/z`,Intensity),
          Purity = binneR:::binPurity(`m/z`,Intensity,dp = pd),
          Centrality = binneR:::binCentrality(`m/z`,Intensity,dp = pd),
          .groups = 'drop'
        )
    }) %>%
    bind_rows()
  
  metrics$amu[metrics$amu == 4] <- 0.0001
  metrics$amu[metrics$amu == 3] <- 0.001
  metrics$amu[metrics$amu == 2] <- 0.01
  metrics$amu[metrics$amu == 1] <- 0.1
  metrics$amu[metrics$amu == 0] <- 1
  
  metrics$Mode[metrics$Mode == 'n'] <- 'Negative'
  metrics$Mode[metrics$Mode == 'p'] <- 'Positive'
  
  return(metrics)   
}
