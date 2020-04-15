# returns NSW incidence tibble
get_guardian_aust_data <- function() {

    guardian_aust_ssid <- as_sheets_id("https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505")

    test_guardian_aust <- read_sheet(guardian_aust_ssid)

    target_colnames <-  c("State", "Date",
                          "Time", "Cumulative case count",
                          "Cumulative deaths", "Tests conducted (negative)",
                          "Tests conducted (total)", "Hospitalisations (count)",
                          "Intensive care (count)", "Ventilator usage (count)",
                          "Recovered (cumulative)", "Update Source",
                          "Under 60", "Over 60",
                          "Community", "Community - no known source",
                          "Travel-related", "Under investigation",
                          "Notes")
    if (identical(colnames(test_guardian_aust), target_colnames)) {
      guardian_aust <- test_guardian_aust
    } else {
      stop("Guardian Australian spreadsheet column names have chnaged, please review")

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

