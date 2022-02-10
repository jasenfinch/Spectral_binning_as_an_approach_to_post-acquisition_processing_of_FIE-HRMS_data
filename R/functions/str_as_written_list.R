str_as_written_list <- function(string_vector,sep = ', '){
  seperator <- sep
  
  string_vector[1:(length(string_vector) - 2)] <- str_c(string_vector[1:(length(string_vector) - 2)],seperator)
  string_vector[length(string_vector)] <- str_c(' and ',string_vector[length(string_vector)])
  
  string_vector <- str_c(string_vector,collapse = '')
  
  return(string_vector)
}
