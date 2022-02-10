
calcAdjacentBins <- function(dp3_bins,binned_intensity_matrices){
  cl <- makeCluster(detectCores() * 0.75,type = 'FORK')
  
  dp3_bins %>%
    split(.$Matrix) %>%
    map(~{
      .x %>%
        split(.$Mode) %>%
        map(~{
          .x %>%
            select(Bin) %>%
            {expand_grid(Bin1 = .$Bin,Bin2 = .$Bin)} %>%
            filter(Bin2 > Bin1) %>%
            mutate(Difference = abs(Bin1 - Bin2) %>%
                     round(3)) %>%
            filter(Difference == 0.001) %>%
            mutate(dp2_1 = round(Bin1,2),dp2_2 = round(Bin2,2)) %>%
            filter(dp2_1 == dp2_2) %>%
            select(everything(),dp2 = dp2_1,-dp2_2,-Difference)
        }) %>%
        set_names(.x$Mode %>%
                    unique()) %>%
        bind_rows(.id = 'Mode')
    }) %>%
    set_names(dp3_bins$Matrix %>%
                unique()) %>%
    bind_rows(.id = 'Matrix') %>%
    split(1:nrow(.)) %>%
    parLapply(cl,.,function(.x,binned_intensity_matrices){
      d <- binned_intensity_matrices %>%
        filter(amu == 0.001) %>% 
        mutate(Bin = round(`m/z`,3)) %>% 
        filter(Bin %in% c(.x$Bin1,.x$Bin2),
               Matrix == .x$Matrix,
               Mode == .x$Mode) %>%
        select(Sample,Bin,Intensity) %>%
        spread(Bin,Intensity)
      
      if (ncol(d) == 3){
        d <- d %>%
          select(-Sample) %>%
          {rcorr(.[,1] %>%
                   deframe(),
                 .[,2] %>%
                   deframe())
          } %>%
          {tibble(r = .$r[1,2],p = .$P[1,2])} %>%
          {bind_cols(.x,.)}
      } else {
        d <- NULL
      }
      return(d) 
    },
    binned_intensity_matrices = binned_intensity_matrices) %>%
    bind_rows()
}