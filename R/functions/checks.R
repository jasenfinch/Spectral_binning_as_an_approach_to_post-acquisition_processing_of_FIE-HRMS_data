
manuscript_check <- function(file = 'manuscript.Rmd', checks = c('word count','spelling','grammar')){
  
  options(knitr.table.format = 'markdown')
  
  cat('######### Manuscript Checks #########\n')
  
  if ('word count' %in% checks) {
    cat('\nWord count')
    
    suppressMessages({
      wordcountaddin::text_stats(file) %>%
        print()
    }) 
  }
  
  if ('spelling' %in% checks) {
    cat('\nSpelling\n\n')
    
    spelling::spell_check_files(file,lang = 'en_GB') %>%
      print()
  }
  
  if ('grammar' %in% checks) {
    cat('\nGrammar')
    
    gramr::write_good_file(file) %>%
      print()
  }
}