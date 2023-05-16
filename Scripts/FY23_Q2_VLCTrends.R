# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  data pull for VLC trends up to FY23 Q2
# REF ID:   889zd9f3
# LICENSE:  MIT
# DATE:     2023-05-16
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

library(tidyverse)
library(extrafont)
library(gagglr)
library(janitor)
library(gt)
library(scales)

# GLOBAL VARIABLES ------------------------------------------------------------

ref_id <- "889zd9f3"

# IMPORT ----------------------------------------------------------------------

# SI specific paths/functions
load_secrets()
merdata <- file.path(si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
                           pattern = "PSNU_IM_Democratic_Republic_of_the_Congo")

# Grab metadata
get_metadata(file_path)

df <- read_psd(file_path)

# MUNGE -----------------------------------------------------------------------
df_filt_snu <- df %>%
    filter(
        indicator %in% c("TX_CURR", "TX_PVLS"),
        funding_agency == "USAID",
        snu1 %in% c("Haut Katanga", "Lualaba", "Kinshasa"),
        standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    clean_indicator() %>%
    group_by(fiscal_year, snu1, indicator) %>%
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE))) %>%
    reshape_msd(include_type = FALSE) %>%
    pivot_wider(
        names_from = indicator,
        names_glue = "{tolower(indicator)}") %>%
    mutate(
        tx_curr_lag2 = lag(tx_curr, 2, order_by = period),
        vlc_snu = as.numeric(tx_pvls_d / tx_curr_lag2),
        vlc_snu_label = if_else(vlc_snu > 1 | vlc_snu < 0.5,
                                scales::percent(vlc_snu, 2),
                                "")) %>%
    filter(period %in% c("FY22Q1", "FY22Q2", "FY22Q3", "FY22Q4",
                         "FY23Q1", "FY23Q2"))

df_filt_sakania <- df %>%
    filter(
        psnu == "Sakania",
        indicator %in% c("TX_CURR", "TX_PVLS"),
        funding_agency == "USAID",
        standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    clean_indicator() %>%
    group_by(fiscal_year, psnu, indicator) %>%
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE))) %>%
    reshape_msd(include_type = FALSE) %>%
    pivot_wider(
        names_from = indicator,
        names_glue = "{tolower(indicator)}") %>%
    mutate(
        tx_curr_lag2 = lag(tx_curr, 2, order_by = period),
        vlc_psnu = as.numeric(tx_pvls_d / tx_curr_lag2),
        vlc_psnu_label = if_else(vlc_psnu > 1 | vlc_psnu < 0.5,
                                scales::percent(vlc_psnu, 2),
                                "")) %>%
    filter(period %in% c("FY22Q1", "FY22Q2", "FY22Q3", "FY22Q4",
                         "FY23Q1", "FY23Q2"))

# Summarize -------------------------------------------------------------------


df_filt_snu %>%
    filter(!period %in% c("FY22Q1", "FY22Q2")) %>%
    ggplot(aes(x = period, y = vlc_snu,
               group = snu1, color = snu1, fill = snu1,
               alpha = 0.7)) +
    geom_area() +
    facet_wrap(~snu1) +
    geom_text(aes(label = vlc_snu_label),
                  vjust = -1, family = "Source Sans Pro",
                  size = 3, color = usaid_darkgrey) +
   scale_color_manual(values = c(
       "Haut Katanga" = genoa,
       "Kinshasa" = moody_blue,
       "Lualaba" = scooter)) +
    scale_fill_manual(values = c(
        "Haut Katanga" = genoa,
        "Kinshasa" = moody_blue,
        "Lualaba" = scooter)) +
   scale_y_continuous(labels = scales::percent_format(2),
                      limits = c(0, 1.1),
                      breaks = c(0, .25, .5, .75, 1)) +
    si_style_ygrid() +
    theme(legend.position = "none",
          axis.text = element_text(size = 11)) +
    labs(x = NULL, y = NULL,
         title = "VLC Trends by SNU",
         caption = glue::glue(metadata$caption))

df_filt_sakania %>%
    mutate(
        tx_curr = replace_na(as.character(comma(tx_curr)), "-"),
        tx_pvls_d = replace_na(as.character(comma(tx_pvls_d)), "-"),
        tx_curr_lag2 = replace_na(as.character(comma(tx_curr_lag2)), "-"),
        vlc_psnu = replace_na(as.character(scales::percent(vlc_psnu, 2)), "-"),
        tx_curr_lag2 = if_else(tx_curr_lag2 == "0", "-", tx_curr_lag2),
        vlc_psnu = if_else(vlc_psnu == "Inf", "-", vlc_psnu)) %>%
    ungroup() %>%
    select(period, tx_curr, tx_pvls_d, tx_curr_lag2, vlc_psnu) %>%
    gt() %>%
    # add custom title
    tab_header(
        title = "Sakania: VLC Trends",
        subtitle = "Please note that FY23Q2 data are pre-clean") %>%
    tab_footnote(glue::glue("{metadata$caption},
                             VLC = TX_PVLS (Denominator)/TX_CURR Lag (2)")) %>%
    # relabel columns nicely
    cols_label(
         period = "Period",
         tx_curr = "TX_CURR",
         tx_pvls_d = "TX_PVLS (Denominator)",
         tx_curr_lag2 = "TX_CURR Lag (2)",
         vlc_psnu = "VLC (%)")
