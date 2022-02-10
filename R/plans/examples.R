
examples <- drake_plan(
  ## approach graph plot
  approach_graph = approach("exports/figures/approach.pdf"),
  
  ## get example headers for infusion profile
  example_header = files[[3]][1] %>%
    openMSfile() %>%
    header() %>%
    split(.$polarity) %>%
    map(rowid_to_column,var = 'scan') %>%
    bind_rows() %>%
    group_by(scan) %>%
    summarise(totIonCurrent = mean(totIonCurrent),
              .groups = 'drop'),
  
  ## example infusion scans
  example_infusion_scans = binneR::detectInfusionScans(files[[3]][1]),
  
  ## plot example infusion profile
  example_infusion_profile = exampleInfusionProfile(example_header,example_infusion_scans)
)