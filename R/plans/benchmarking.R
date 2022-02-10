
benchmarking <- drake_plan(
  
  ## number of files to benchmark
  n_files =  c(1,10,100,1000),
  
  ## test files for performance benchmarking
  performance_files = rep(files[[1]],100),
  
  ## sample information for performance benchmarking
  performance_sample_information = 1:100 %>%
    map(~{runinfo('FIE-HRMS',"BdistachyonTechnical",ask = FALSE)}) %>%
    bind_rows(),
  
  ## benchmark binneR processing time
  binneR_proc_time = target(
    binneRprocBenchmark(performance_files,
                      performance_sample_information,
                      nfiles,
                      ncores,
                      reps = 1),
    transform = cross(
      nfiles = c(1,10,100,1000),
      ncores =  c(1,4,16,64)
    )),
  
  ## aggregate binneR benchmarking
  binneR_proc_time_summary = target(
    bind_rows(binneR_proc_time),
    transform = combine(binneR_proc_time)
  ),
  
  ## benchmark binneR memory usage
  binneR_mem_usage = target(
    binneRmemUsage(performance_files,
                      performance_sample_information,
                      nfiles,
                      ncores),
    transform = cross(
      nfiles = c(1,10,100,1000),
      ncores =  c(1,4,16,64)
    )),
  
  ## aggregate binneR memory usage
  binneR_mem_usage_summary = target(
    bind_rows(binneR_mem_usage),
    transform = combine(binneR_mem_usage)
  ),
  
  ## combine binneR processing time and memory usage results
  binneR_performance = left_join(binneR_proc_time_summary,
                                 binneR_mem_usage_summary,
                                 by = c('workers','# samples')),
  
  ## plot performance summary
  performance_plot = performancePlot(binneR_performance),
  
  ## CPU information
  cpu_information = minder::cpuInfo(),
  
  ## Memory information
  memory_information = minder::memoryInfo()
)