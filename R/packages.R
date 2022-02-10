
pacman::p_load(drake,magrittr,purrr,parallel,
               tidyr,dplyr,stringr,rmarkdown,ggplot2,
               wesanderson,patchwork,Hmisc,ggthemes,
               DiagrammeR,spelling,bookdown,rsvg,
               DiagrammeRsvg,knitr,conflicted,
               mzR,kableExtra,scales,piggyback,readr,
               metabolyseR,mzAnnotation,future,forcats,
               tools,rbenchmark,profvis,glue,fs,
               metaboData,tibble,xfun,
               install = FALSE)

pacman::p_load_gh('aberHRML/binneR',
                  'ropenscilabs/gramr',
                  'benmarwick/wordcountaddin',
                  install = FALSE)

conflict_prefer("filter","dplyr",quiet = TRUE)
conflict_prefer("gather","tidyr",quiet = TRUE)
conflict_prefer("split",'base',quiet = TRUE)
conflict_prefer('filePaths','metaboData',quiet = TRUE)
conflict_prefer('plan','drake',quiet = TRUE)
conflict_prefer('version','base',quiet = TRUE)
conflict_prefer("sampleInfo", "binneR",quiet = TRUE)

## Set up parallel backend
# binneR::plan(multisession,workers = 3)