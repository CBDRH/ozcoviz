covid_dashboard_name <- glue::glue("docs/ozcovis-{lubridate::today()}.html")
covid_dashboard_name

rmarkdown::render(input =  "ozcoviz.Rmd",
                  output_file = covid_dashboard_name,
                  output_dir = here::here("docs"),
                  clean = TRUE)

rmarkdown::render(input =  "ozcoviz.Rmd",
                  output_file = "index.html",
                  output_dir = here::here("docs"),
                  clean = TRUE)
