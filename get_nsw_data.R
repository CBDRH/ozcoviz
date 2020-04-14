# returns NSW incidence tibble
get_nsw_data <- function() {

    # NSW cases by source scraped from chart on 31/3/2020
    # figures pre 9/3/2020 should be stable
    nsw_cases_from_chart <- tribble(~notification_date, ~overseas_pix, ~local_contacts_pix, ~local_no_contacts_pix, ~under_invest_pix,
    "2020-01-22",4,0,0,0,
    "2020-01-23",0,0,0,0,
    "2020-01-24",4,0,0,0,
    "2020-01-25",9,0,0,0,
    "2020-01-26",0,0,0,0,
    "2020-01-27",0,0,0,0,
    "2020-01-28",0,0,0,0,
    "2020-01-29",0,0,0,0,
    "2020-01-30",0,0,0,0,
    "2020-01-31",0,0,0,0,
    "2020-02-01",0,0,0,0,
    "2020-02-02",0,0,0,0,
    "2020-02-03",0,0,0,0,
    "2020-02-04",0,0,0,0,
    "2020-02-05",0,0,0,0,
    "2020-02-06",0,0,0,0,
    "2020-02-07",0,0,0,0,
    "2020-02-08",0,0,0,0,
    "2020-02-09",0,0,0,0,
    "2020-02-10",0,0,0,0,
    "2020-02-11",0,0,0,0,
    "2020-02-12",0,0,0,0,
    "2020-02-13",0,0,0,0,
    "2020-02-14",0,0,0,0,
    "2020-02-15",0,0,0,0,
    "2020-02-16",0,0,0,0,
    "2020-02-17",0,0,0,0,
    "2020-02-18",0,0,0,0,
    "2020-02-19",0,0,0,0,
    "2020-02-20",0,0,0,0,
    "2020-02-21",4,0,0,0,
    "2020-02-22",0,0,0,0,
    "2020-02-23",0,0,0,0,
    "2020-02-24",0,0,0,0,
    "2020-02-25",0,0,0,0,
    "2020-02-26",0,4,0,0,
    "2020-02-27",4,0,0,0,
    "2020-02-28",0,0,0,0,
    "2020-02-29",4,0,0,0,
    "2020-03-01",5,4,0,0,
    "2020-03-02",5,4,0,0,
    "2020-03-03",26,4,0,0,
    "2020-03-04",10,30,0,0,
    "2020-03-05",15,30,0,0,
    "2020-03-06",11,24,0,0,
    "2020-03-07",5,16,14,0,
    "2020-03-08",15,26,4,0,
    "2020-03-09",26,14,0,0,
    "2020-03-10",30,0,0,0,
    "2020-03-11",52,26,24,0,
    "2020-03-12",57,42,19,0,
    "2020-03-13",72,73,15,0,
    "2020-03-14",57,49,25,0,
    "2020-03-15",94,69,15,0,
    "2020-03-16",181,64,6,4,
    "2020-03-17",124,106,41,0,
    "2020-03-18",192,111,15,0,
    "2020-03-19",290,80,25,0,
    "2020-03-20",420,178,68,4,
    "2020-03-21",363,85,68,14,
    "2020-03-22",715,116,74,9,
    "2020-03-23",658,177,100,4,
    "2020-03-24",699,189,89,14,
    "2020-03-25",673,158,116,14,
    "2020-03-26",643,162,142,35,
    "2020-03-27",544,168,225,24,
    "2020-03-28",384,95,84,45,
    "2020-03-29",213,80,89,107,
    "2020-03-30",83,12,39,77)

    pix150 <- 776

    nsw_cases_from_chart <- nsw_cases_from_chart %>%
      mutate(notification_date = ymd(notification_date),
             overseas = round(150*overseas_pix/pix150),
             local_contacts = round(150*local_contacts_pix/pix150),
             local_no_contacts = round(150*local_no_contacts_pix/pix150),
             under_invest = round(150*under_invest_pix/pix150)) %>%
      mutate(total = overseas + local_contacts + local_no_contacts + under_invest) %>%
      mutate(imported = overseas,
             local_plus_uix = local_contacts + local_no_contacts + under_invest,
             local_known_only = local_contacts + local_no_contacts + under_invest)

    nsw_cases_from_chart_compare <- nsw_cases_from_chart %>%
          rename(OS=overseas,
                 LC=local_contacts,
                 LNC=local_no_contacts,
                 UIX=under_invest) %>%
          select(notification_date, OS, LC, LNC, UIX) %>%
          pivot_longer(-notification_date,
                       names_to="source",
                       values_to="n") %>%
          mutate(provenance = "health.nsw.gov.au")

    # fetch latest data
    nsw_incidence_by_source_url <- paste0("https://data.nsw.gov.au/data/dataset",
                                   "/c647a815-5eb7-4df6-8c88-f9c537a4f21e",
                                   "/resource/2f1ba0f3-8c21-4a86-acaf-444be4401a6d",
                                   "/download/covid-19-cases-by-notification-date",
                                   "-and-likely-source-of-infection.csv")

    colspec <- cols(notification_date = col_date(format = "%d-%m-%y"),
                    likely_source_of_infection = col_character())

    nsw_incidence_by_source <- read_csv(nsw_incidence_by_source_url,
                                        col_types = colspec) %>%
        rename(source=likely_source_of_infection) %>%
        mutate(source=case_when(
          source == paste0("Locally acquired - contact of a ",
                           "confirmed case and/or in a known cluster") ~ "LC",
          source == "Locally acquired - contact not identified" ~ "LNC",
          source == "Overseas" ~ "OS",
          source == "Interstate" ~ "IS",
          source == "Under investigation" ~ "UIX",
          TRUE ~ "OTH")) %>%
        group_by(notification_date, source) %>%
        summarise(n=n()) %>%
        ungroup() %>%
        mutate(provenance = "data.nsw.gov.au")

    nsw_incidence_by_source %>%
      bind_rows(nsw_cases_from_chart_compare) %>%
      ggplot(aes(x=notification_date, y=n, fill=provenance)) +
      geom_col(position = "dodge") +
      facet_grid(source~.)

    nsw_incidence <- nsw_incidence_by_source %>%
      bind_rows(nsw_cases_from_chart_compare %>%
                  filter(notification_date < ymd("2020-03-09"),
                         notification_date >= ymd("2020-02-29"))) %>%
      mutate(source = factor(source)) %>%
      pivot_wider(id_cols = c(notification_date, provenance),
                  names_from = source,
                  values_from = n,
                  values_fill = list(n = 0)) %>%
      arrange(notification_date)

    return(nsw_incidence)
}

