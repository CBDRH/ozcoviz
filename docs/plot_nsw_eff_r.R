# plots the NSW split effective R plots

extract_R_I_df <- function(estimate_R_obj) {
  R_df <-  estimate_R_obj$R
  R_df$dates <- estimate_R_obj$dates[8:length(estimate_R_obj$dates)]
  I_df <- tibble(dates=estimate_R_obj$dates,
                 I=estimate_R_obj$I,
                 I_local=estimate_R_obj$I_local,
                 I_imported=estimate_R_obj$I_imported)
  R_df <- I_df %>%
            left_join(R_df, by="dates")

  R_df <- R_df %>%
          select(dates, everything()) %>%
          rename(mean_R="Mean(R)",
                 median_R="Median(R)",
                 CI95_L_R="Quantile.0.025(R)",
                 CI95_U_R="Quantile.0.975(R)")

  return(R_df)
}

plot_eff_R <- function(R_df, title=NULL, subtitle=NULL) {
  p_R <- R_df %>%
         ggplot(aes(x=dates, y=median_R,
                    ymin=CI95_L_R,
                    ymax=CI95_U_R)) +
         geom_hline(yintercept=1.0, colour="red", alpha=0.6) +
         geom_line() +
         geom_ribbon(alpha=0.3) +
         scale_y_log10() +
         labs(x="Date", y="Estimated effective R",
              title=title,
              subtitle=subtitle)
  return(p_R)
}

plot_Ri <- function(parametric_si_obj=NULL,
                    si_from_sample_obj=NULL,
                    split=FALSE,
                    title="",
                    subtitle="") {

  R_df_parametric_si <- extract_R_I_df(parametric_si_obj)

  R_df_si_from_sample <- extract_R_I_df(si_from_sample_obj)

  p_R_parametric_si <- plot_eff_R( R_df_parametric_si)

  p_R_si_from_sample <- plot_eff_R(R_df_si_from_sample)

  p_SI_parametric_si <- plot(parametric_si_obj, "SI")

  p_SI_si_from_sample <- plot(si_from_sample_obj, "SI")

  if (split) {
    p_I_imp <- R_df_parametric_si %>%
           ggplot(aes(x=dates, y=I_imported)) +
           geom_col(fill="blue") +
           labs(x="Date", y="Incidence",
                title="Overseas-acquired incidence")

    p_I_loc <- R_df_parametric_si %>%
           ggplot(aes(x=dates, y=I_local)) +
           geom_col(fill="red") +
           labs(x="Date", y="Incidence",
                title="Locally-acquired incidence")

    return(list(p_R_parametric_si=p_R_parametric_si,
                p_R_si_from_sample=p_R_si_from_sample,
                p_I_loc=p_I_loc,
                p_I_imp=p_I_imp,
                p_SI_parametric_si=p_SI_parametric_si,
                p_SI_si_from_sample=p_SI_si_from_sample))

  } else {
    p_I <- R_df_parametric_si %>%
           ggplot(aes(x=dates, y=I)) +
           geom_col(fill="red") +
           labs(x="Date", y="Incidence",
                title="Overseas- and locally-acquired incidence")

    return(list(p_R_parametric_si=p_R_parametric_si,
                p_R_si_from_sample=p_R_si_from_sample,
                p_I=p_I,
                p_SI_parametric_si=p_SI_parametric_si,
                p_SI_si_from_sample=p_SI_si_from_sample))
  }
}
