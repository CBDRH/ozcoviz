library(tidyverse)
library(ggeasy)

# source: https://www.statista.com/statistics/1107149/covid19-cases-age-distribution-canada/
canada <- tribble(~Country, ~AgeGroup, ~PercentOfCases,
               "Canada", " 0-19", 5.02,
               "Canada", "20-29", 11.96,
               "Canada", "30-39", 13.88,
               "Canada", "40-49", 15.8,
               "Canada", "50-59", 16.54,
               "Canada", "60-69", 12.32,
               "Canada", "70-79", 8.21,
               "Canada", "80+", 16.28)

# source: https://www.health.gov.au/resources/covid-19-cases-by-age-group-and-sex
aust <- tribble(~Country, ~AgeGroup, ~CountOfCases,
               "Australia", " 0-19", 36 + 39 + 98 + 104,
               "Australia", "20-29", 632 + 779,
               "Australia", "30-39", 541 + 511,
               "Australia", "40-49", 492 + 372,
               "Australia", "50-59", 524 + 561,
               "Australia", "60-69", 569 + 556,
               "Australia", "70-79", 392 + 327,
               "Australia", "80+", 101 + 82 + 20 + 26) %>%
        mutate(PercentOfCases = 100 * CountOfCases/sum(CountOfCases))

aust_deaths <- tribble(~Country, ~AgeGroup, ~CountOfDeaths,
               "Australia", " 0-19", 0,
               "Australia", "20-29", 0,
               "Australia", "30-39", 0,
               "Australia", "40-49", 1,
               "Australia", "50-59", 2,
               "Australia", "60-69", 7 + 4,
               "Australia", "70-79", 19 + 12,
               "Australia", "80+", 18 + 14 + 9 + 10) %>%
        mutate(ProportionOfDeaths = CountOfDeaths/sum(CountOfDeaths))

aust %>%
  left_join(aust_deaths) %>%
  mutate(cfr = CountOfDeaths / CountOfCases) %>%
  left_join(canada) %>%
  mutate(exp_percentage_deaths = cfr * PercentOfCases/100) %>%
  summarise(crude_percentage_deaths = sum(exp_percentage_deaths)) %>%
  mutate(exp_deaths = crude_percentage_deaths * 51150)

canada %>%
  bind_rows(aust) %>%
  ggplot(aes(x=AgeGroup, y=PercentOfCases, fill=Country)) +
    geom_col() +
    facet_grid(Country~.) +
  ggeasy::easy_remove_legend() +
  labs(title="COVID-19 confirmed cases as at 4th May 2020")



