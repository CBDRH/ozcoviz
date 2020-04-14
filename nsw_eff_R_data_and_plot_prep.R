# data prep and plot generation for NSW effective R tabs

# get NSW data
nsw_incidence <- get_nsw_data()

nsw_incidence_total <- nsw_incidence %>%
  mutate(dates=notification_date,
         I=OS + IS + LC + LNC + UIX) %>%
  select(dates, I)

nsw_incidence_split_incl_uix <- nsw_incidence %>%
  mutate(dates=notification_date,
         imported=OS + IS,
         local= LC + LNC + UIX) %>%
  select(dates, imported, local)

nsw_incidence_split_excl_uix <- nsw_incidence %>%
  mutate(dates=notification_date,
         imported=OS + IS,
         local= LC + LNC) %>%
  select(dates, imported, local)


# get Nishiura SI samples
fc <- cache_filesystem(".nishiura_si_dist")
memo_get_nishiura_si_sample <- memoise(get_nishiura_si_sample, cache = fc)
nishi_si_sample <- memo_get_nishiura_si_sample()

# configs for eff R estimation
# SI distribution from Nishiura et al.
parametric_si_nishiura_config <- make_config(list(mean_si = 4.7,
                                         std_si = 2.9))

# values from https://www.nejm.org/doi/full/10.1056/NEJMoa2001316
parametric_si_li_config <- make_config(list(mean_si = 7.5,
                                         std_si = 3.4))

# posterior sample based on Nishiura et al SI data
si_from_sample_nishiura_config <-  make_config(list(n1=500, n2 = 50, seed=2))

# estimate eff R
nsw_effr_total_para_si <- estimate_R(nsw_incidence_total,
                                     method="parametric_si",
                                     config = parametric_si_li_config)

nsw_effr_total_si_from_sample <- estimate_R(nsw_incidence_total,
                            method="si_from_sample",
                            si_sample=nishi_si_sample,
                            config = si_from_sample_nishiura_config)

nsw_effr_split_incl_uix_para_si <- estimate_R(nsw_incidence_split_incl_uix,
                                      method="parametric_si",
                                      config = parametric_si_li_config)

nsw_effr_split_incl_uix_si_from_sample <- estimate_R(nsw_incidence_split_incl_uix,
                            method="si_from_sample",
                            si_sample=nishi_si_sample,
                            config = si_from_sample_nishiura_config)

nsw_effr_split_excl_uix_para_si <- estimate_R(nsw_incidence_split_excl_uix,
                                      method="parametric_si",
                                      config = parametric_si_li_config)

nsw_effr_split_excl_uix_si_from_sample <- estimate_R(nsw_incidence_split_excl_uix,
                            method="si_from_sample",
                            si_sample=nishi_si_sample,
                            config = si_from_sample_nishiura_config)

nsw_incidence_split_excl_uix_under_asc <- nsw_incidence_split_excl_uix %>%
    mutate(local = local * 10,
           imported = imported * 1.5)

nsw_effr_split_excl_uix_para_si_under_asc <- estimate_R(nsw_incidence_split_excl_uix_under_asc,
                                      method="parametric_si",
                                      config = parametric_si_li_config)

nsw_effr_split_excl_uix_si_from_sample_under_asc <- estimate_R(nsw_incidence_split_excl_uix_under_asc,
                            method="si_from_sample",
                            si_sample=nishi_si_sample,
                            config = si_from_sample_nishiura_config)

# generate plots
# A
plots_A <- plot_Ri(parametric_si_obj=nsw_effr_split_excl_uix_para_si,
                 si_from_sample_obj=nsw_effr_split_excl_uix_si_from_sample,
                 split=TRUE)

A1a <- plots_A$p_R_parametric_si +
  scale_y_log10(limits=c(0.25, 10)) +
  labs(title=expression("COVID-19 incidence in NSW: 7-day sliding window effective reproduction number R"[t]),
       subtitle=paste0("Local & overseas/interstate sources of infection treated separately\n",
                       "Parametric serial interval distribution"))

A1b <- plots_A$p_R_si_from_sample +
  scale_y_log10(limits=c(0.25, 10)) +
  labs(title=expression("COVID-19 incidence in NSW: 7-day sliding window effective reproduction number R"[t]),
       subtitle=paste0("Local & overseas/interstate sources of infection treated separately\n",
                       "Serial interval distribution estimated from data"))

A2a <- plots_A$p_I_loc +
    # ylim(0, plots_A$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with local sources of infection",
          subtitle="(excluding under investigation)")

A2b <- plots_A$p_I_loc +
    # ylim(0, plots_A$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with local sources of infection",
          subtitle="(excluding under investigation)")

A3a <- plots_A$p_I_imp +
    # ylim(0, plots_A$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with overseas/interstate sources of infection")

A3b <- plots_A$p_I_imp +
    # ylim(0, plots_A$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with overseas/interstate sources of infection")

A4a <- plots_A$p_SI_parametric_si +
  xlim(0,25) +
  labs(title="Explored parametric serial interval distribution",
       subtitle=expression(paste("from parameters given by Li ", italic("et al."))),
       caption=paste("CC BY", # '\U1F16D','\U1F16F',
                     "Tim Churches, UNSW Medicine &\n",
                     "Nick Tierney, Monash University"))

A4b <- plots_A$p_SI_si_from_sample +
  xlim(0,25) +
  labs(title="Explored serial interval posterior estimates",
       subtitle=expression(paste("from data given by Nishiura ", italic("et al."))),
       caption=paste("CC BY", # '\U1F16D','\U1F16F',
                     "Tim Churches, UNSW Medicine &\n",
                     "Nick Tierney, Monash University"))

# A
plots_UA <- plot_Ri(parametric_si_obj=nsw_effr_split_excl_uix_para_si_under_asc,
                 si_from_sample_obj=nsw_effr_split_excl_uix_si_from_sample_under_asc,
                 split=TRUE)

UA1a <- plots_UA$p_R_parametric_si +
  scale_y_log10(limits=c(0.25, 10)) +
  labs(title=expression("COVID-19 incidence in NSW: 7-day sliding window effective reproduction number R"[t]),
       subtitle=paste0("Adjusted for 10-fold local case under-ascertainment\n",
                       "and 50% imported case under-ascertainment.\n",
                       "Local & overseas/interstate sources of infection treated separately\n",
                       "Parametric serial interval distribution"))

UA1b <- plots_UA$p_R_si_from_sample +
  scale_y_log10(limits=c(0.25, 10)) +
  labs(title=expression("COVID-19 incidence in NSW: 7-day sliding window effective reproduction number R"[t]),
       subtitle=paste0("Adjusted for 10-fold local case under-ascertainment\n",
                       "and 50% imported case under-ascertainment.\n",
                       "Local & overseas/interstate sources of infection treated separately\n",
                       "Serial interval distribution estimated from data"))

UA2a <- plots_UA$p_I_loc +
    # ylim(0, plots_A$max_I) +
    labs(title="COVID-19 incidence in NSW: \\: cases with local sources of infection",
          subtitle=paste0("(excluding under investigation)\n",
                          "Adjusted for 10-fold local case under-ascertainment\n",
                          "and 50% imported case under-ascertainment."))

UA2b <- plots_UA$p_I_loc +
    # ylim(0, plots_A$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with local sources of infection",
          subtitle=paste0("(excluding under investigation)\n",
                          "Adjusted for 10-fold local case under-ascertainment\n",
                          "and 50% imported case under-ascertainment."))

UA3a <- plots_UA$p_I_imp +
    # ylim(0, plots_A$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with overseas/interstate sources of infection",
          subtitle=paste0("Adjusted for 10-fold local case under-ascertainment\n",
                          "and 50% imported case under-ascertainment."))

UA3b <- plots_UA$p_I_imp +
    # ylim(0, plots_A$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with overseas/interstate sources of infection",
          subtitle=paste0("Adjusted for 10-fold local case under-ascertainment\n",
                          "and 50% imported case under-ascertainment."))

UA4a <- plots_UA$p_SI_parametric_si +
  xlim(0,25) +
  labs(title="Explored parametric serial interval distribution",
       subtitle=expression(paste("from parameters given by Li ", italic("et al."))),
       caption=paste("CC BY", # '\U1F16D','\U1F16F',
                     "Tim Churches, UNSW Medicine &\n",
                     "Nick Tierney, Monash University"))

UA4b <- plots_UA$p_SI_si_from_sample +
  xlim(0,25) +
  labs(title="Explored serial interval posterior estimates",
       subtitle=expression(paste("from data given by Nishiura ", italic("et al."))),
       caption=paste("CC BY", # '\U1F16D','\U1F16F',
                     "Tim Churches, UNSW Medicine &\n",
                     "Nick Tierney, Monash University"))

# B
plots_B <- plot_Ri(parametric_si_obj=nsw_effr_split_incl_uix_para_si,
                 si_from_sample_obj=nsw_effr_split_incl_uix_si_from_sample,
                 split=TRUE)

B1a <- plots_B$p_R_parametric_si +
  scale_y_log10(limits=c(0.25, 10)) +
  labs(title=expression("COVID-19 incidence in NSW: 7-day sliding window effective reproduction number R"[t]),
       subtitle=paste0("Local & overseas/interstate sources of infection treated separately\n",
                       "Parametric serial interval distribution"))

B1b <- plots_B$p_R_si_from_sample +
  scale_y_log10(limits=c(0.25, 10)) +
  labs(title=expression("COVID-19 incidence in NSW: 7-day sliding window effective reproduction number R"[t]),
       subtitle=paste0("Local & overseas/interstate sources of infection treated separately\n",
                       "Serial interval distribution estimated from data"))

B2a <- plots_B$p_I_loc +
    # ylim(0, plots_B$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with local sources of infection",
         subtitle="(including under investigation)")

B2b <- plots_B$p_I_loc +
    # ylim(0, plots_B$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with local sources of infection",
          subtitle="(including under investigation)")

B3a <- plots_B$p_I_imp +
    # ylim(0, plots_B$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with overseas/interstate sources of infection")

B3b <- plots_B$p_I_imp +
    # ylim(0, plots_B$max_I) +
    labs(title="COVID-19 incidence in NSW: cases with overseas/interstate sources of infection")

B4a <- plots_B$p_SI_parametric_si +
  xlim(0,25) +
  labs(title="Explored parametric serial interval distribution",
       subtitle=expression(paste("from parameters given by Li ", italic("et al."))),
       caption=paste("CC BY", # '\U1F16D','\U1F16F',
                     "Tim Churches, UNSW Medicine &\n",
                     "Nick Tierney, Monash University"))

B4b <- plots_B$p_SI_si_from_sample +
  xlim(0,25) +
  labs(title="Explored serial interval posterior estimates",
       subtitle=expression(paste("from data given by Nishiura ", italic("et al."))),
       caption=paste("CC BY", # '\U1F16D','\U1F16F',
                     "Tim Churches, UNSW Medicine &\n",
                     "Nick Tierney, Monash University"))

# C
plots_C <- plot_Ri(parametric_si_obj=nsw_effr_total_para_si,
                  si_from_sample_obj=nsw_effr_total_si_from_sample)

C1a <- plots_C$p_R_parametric_si +
  labs(title=expression("COVID-19 incidence in NSW: 7-day sliding window effective reproduction number R"[t]),
       subtitle=paste0("All sources of infection treated as local\n",
                       "Parametric serial interval distribution"))

C1b <- plots_C$p_R_si_from_sample +
  labs(title=expression("COVID-19 incidence in NSW: 7-day sliding window effective reproduction number R"[t]),
       subtitle=paste0("All sources of infection treated as local,\n",
                       "Serial interval distribution estimated from data"))

C2a <- plots_C$p_I +
  labs(title="COVID-19 incidence in NSW: all sources of infection",
       subtitle="(local, imported and under investigation)")

C2b <- plots_C$p_I +
  labs(title="COVID-19 incidence in NSW: all sources of infection",
       subtitle="(local, imported and under investigation)")

C3a <- plots_C$p_SI_parametric_si +
  xlim(0,25) +
  labs(title="Explored parametric serial interval distribution",
       subtitle=expression(paste("from parameters given by Li ", italic("et al."))),
       caption=paste("CC BY", # '\U1F16D','\U1F16F',
                     "Tim Churches, UNSW Medicine &\n",
                     "Nick Tierney, Monash University"))

C3b <- plots_C$p_SI_si_from_sample +
  xlim(0,25) +
  labs(title="Explored serial interval posterior estimates",
       subtitle=expression(paste("from data given by Nishiura ", italic("et al."))),
       caption=paste("CC BY", # '\U1F16D','\U1F16F',
                     "Tim Churches, UNSW Medicine &\n",
                     "Nick Tierney, Monash University"))

# icon for flexdashboard
if (FALSE) {
    flex_icon <- plots_A$p_R_parametric_si +
       xlim(ymd("2020-03-09"),NA) +
       labs(title=NULL,
            subtitle=NULL,
            caption=NULL) +
       theme_void()

  ggsave("images/flex_icon.png", plot = flex_icon,
         units="in", dpi="retina",
         height=48/320, width=(48/320)*2)
}

