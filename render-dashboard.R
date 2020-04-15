library(glue)
library(rmarkdown)
library(here)
library(lubridate)

render(input =  "ozcoviz.Rmd",
       output_file = glue("docs/ozcovis-{lubridate::today()}.html"),
       output_dir = here::here("local_docs"),
       clean = TRUE)

# generate the index
render(input =  "ozcoviz.Rmd",
       output_file = "index.html",
       output_dir = here::here("docs"),
       clean = TRUE)
