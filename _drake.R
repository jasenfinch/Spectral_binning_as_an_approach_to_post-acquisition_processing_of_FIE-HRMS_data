source('R/packages.R')

'R/functions/' %>%
  list.files(full.names = T) %>%
  walk(source)

'R/plans/' %>%
  list.files(full.names = T) %>%
  walk(source)

source('R/plan.R')

options(parallelly.fork.enable = TRUE)

drake_config(plan,lock_envir = FALSE)