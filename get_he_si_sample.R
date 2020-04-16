# returns sample from posterior estimates from He et al.
# https://www.nature.com/articles/s41591-020-0869-5.pdf
# SI data

get_he_et_al_si_sample <- function() {


    he_et_al_si_data_url <- "https://github.com/ehylau/COVID-19/raw/master/Fig1c_data.xlsx"

    tf = tempfile(fileext = ".xlsx")
    utils::download.file(he_et_al_si_data_url, tf, mode = "wb")
    he_et_al_si_data <- readxl::read_excel(tf)
    unlink(tf)


    he_et_al_si <- he_et_al_si_data %>%
      mutate(EL = as.integer(difftime(x.lb,x.lb, units="days")) + 1L,
             ER = as.integer(difftime(x.ub,x.lb, units="days")) + 1L,
             SL = as.integer(difftime(y,x.lb, units="days")) + 1L,
             SR = as.integer(difftime(y,x.lb, units="days")) + 1L,
             type=1L) %>%
      select(EL, ER, SL, SR, type) %>%
      filter(SL >= 0 & SR >= 0) %>%
      as.data.frame()


    ## estimate the serial interval from data
#    suppressMessages({
      he_et_al_si_fit <- coarseDataTools::dic.fit(dat = he_et_al_si,
                           dist = "L", n.boots=100)
#    })

    ## use coarse2estim to turn this in the right format for estimate_R
    he_et_al_si_sample <- coarse2estim(he_et_al_si_fit, thin = 100)$si_sample

    return(he_et_al_si_sample)
}

a <- get_he_et_al_si_sample()
