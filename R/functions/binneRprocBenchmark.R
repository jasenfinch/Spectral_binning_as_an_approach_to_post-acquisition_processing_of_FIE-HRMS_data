
binneRprocBenchmark <- function(f,i,n_files,n_cores,reps = 10){
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
  
  proc_time <- benchmark(
    {
      bp <- detectParameters(f)
      bd <- binneRlyse(f,i,bp) 
    },
    replications = reps,
    columns = c('replications',
                'elapsed')
  ) %>%
    as_tibble() %>% 
    rename(`time (seconds)` = elapsed) %>% 
    mutate(workers = n_cores,
           `# samples` = n_files)
  
  return(proc_time)
}