
binneRmemUsage <- function(f,i,n_files,n_cores){
  temp_dir <- tempdir()
  
  temp_files <- f[1:n_files] %>% 
    tibble(file_path = .) %>%
    rowid_to_column(var = 'file_number') %>% 
    mutate(temp_file = glue('{temp_dir}/{file_number}.mzML.gz'))
  
  temp_files %>% 
    split(1:nrow(.)) %>% 
    walk(~{
      file_copy(path = .$file_path,
                new_path = .$temp_file,
                overwrite = TRUE)
    })
  
  f <- temp_files$temp_file
  i <- i[1:n_files,] %>% 
    mutate(fileOrder = 1:nrow(.),
           injOrder = 1:nrow(.),
           fileName = basename(f),
           name = 1:nrow(.))
  
  if (n_cores > 1) {
    binneR::plan('multicore',workers = n_cores)  
  } else {
    binneR::plan('sequential')
  }
  
  if (n_files > 10 & n_cores == 1) {
    interval <- 0.5  
  } else {
    interval <- 0.01
  }
  
  mem_usage <- profvis({
    bp <- detectParameters(f)
    bd <- binneRlyse(f,i,bp) 
  },
  interval = interval) %>%
    {
      .$x$message$prof 
    } %>%
    as_tibble() %>%
    {
      tibble(min = min(.$memalloc),max = max(.$memalloc)) %>%
        mutate(`RAM usage (MB)` = max - min,
               workers = n_cores,
               `# samples` = n_files) %>%
        select(-min,-max)
    }
  
  return(mem_usage)
}
