
manuscript <- drake_plan(
  
  ## compile supplementary methods
  supplementary_methods = render(knitr_in('manuscript/supplementary_methods.Rmd'),
                         output_dir = 'exports/supplementary_materials',
                         quiet = TRUE),
  
  ## export supplementary table S1
  table_S1 = standards_compound_info %>% 
    rename(Compound = NAME,
           InChI = INCHI) %>% 
    write_csv(file = file_out('exports/supplementary_materials/Table_S1.csv')), 
  
  ## compile supplementary tables
  supplementary_tables = render(knitr_in('manuscript/supplementary_tables.Rmd'),
                                 output_dir = 'exports/supplementary_materials',
                                 quiet = TRUE),
  
  ## export supplementary table S4
  table_s4 = standards_feature_matches %>%
    filter(dp == 2) %>% 
    select(Compound = Name,
           `Molecular formula` = MF,Adduct:`Theoretical m/z`,
           `Measured m/z` = mz,
           Abundance = Intensity,
           purity:`PPM error`) %>% 
    write_csv(file = file_out('exports/supplementary_materials/Table_S4.csv')),
  
  ## compile supplementary figures
  supplementary_figures = render(knitr_in('manuscript/supplementary_figures.Rmd'),
                                 output_dir = 'exports/supplementary_materials',
                                 quiet = TRUE),
  
  ## zip supplementary material
  supplementary_zip = {
    invisible(supplementary_methods)
    invisible(supplementary_figures)
    invisible(supplementary_tables)
    invisible(table_s4)
    
    zipfile <- 'exports/supplementary_materials.zip'
    
    if (file.exists(zipfile)) {
      unlink(zipfile) 
    }
    
    zip(file_out('exports/supplementary_materials.zip'),
        list.files('exports/supplementary_materials',full.names = TRUE) %>%
          .[!str_detect(.,'.tex')],
        flags = '-r9Xj')
  },
  
  ## compile manuscript
  manuscript = render(knitr_in('manuscript/manuscript.Rmd'),
                      output_dir = 'exports',
                      quiet = TRUE,
                      output_format = 'all')
)