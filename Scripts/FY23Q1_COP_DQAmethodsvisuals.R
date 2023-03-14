# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  DQA methodology visuals
# REF ID:   f441718a
# LICENSE:  MIT
# DATE:     2023-03-14
# UPDATED:

# DEPENDENCIES -----------------------------------------------------------------

    library(tidyverse)
    library(extrafont)
    library(gagglr)
    library(janitor)
    library(ggtext)
    library(ggplot2)
    library(googlesheets4)
    library(scales)
    library(glue)

# GLOBAL VARIABLES -------------------------------------------------------------

    ref_id <- "f441718a"
    load_secrets()

# IMPORT -----------------------------------------------------------------------

    #Copy of Copy of DRC....
    path <- "1DdrGWSeDnS0RX-Byim194yNj_9scyD-XQmQ18LqFrXY"

    df <- read_sheet(path, "Targets") %>%
        clean_names()

# MUNGE ------------------------------------------------------------------------

    df_viz <- df %>%
        # select only the columns needed for viz
        select(psnu, reported_fy23_q1_tx_curr, starts_with("fy23_"),
               difference_between_reported_and_proxy, percent_difference) %>%
        drop_na(psnu) %>%
        # keep only PSNUs with projected FY23Q4 TX_CURR > 1000
        filter(fy23_q4 > 1000) %>%
        pivot_longer(cols = c("reported_fy23_q1_tx_curr", "fy23_q2",
                              "fy23_q3","fy23_q4"),
            names_to = "fiscal_year",
            values_to = "tx_curr") %>%
        mutate(
            psnu = if_else(psnu == "_Military Democratic Republic of the Congo", "Military", psnu),
            fiscal_year = if_else(fiscal_year == "reported_fy23_q1_tx_curr",
                                  "fy23_q1",
                                  fiscal_year),
            type = if_else(fiscal_year == "fy23_q1",
                             "reported",
                           "projected"),
            period = case_when(
                fiscal_year == "fy23_q1" ~ "Q1",
                fiscal_year == "fy23_q2" ~ "Q2",
                fiscal_year == "fy23_q3" ~ "Q3",
                fiscal_year == "fy23_q4" ~ "Q4"),
            diff_pct = percent(percent_difference, accuracy = 1),
            diff_num = scales::comma(difference_between_reported_and_proxy, accuracy = 1),
            diff_label = glue::glue("{diff_num} ({diff_pct})"),
            type_color = if_else(type == "reported", scooter, scooter_light)) %>%
        group_by(psnu, fiscal_year, period, type)

# VIZ --------------------------------------------------------------------------

# What is the projected TX_CURR?

    # total
    df_viz %>%
        filter(psnu == "Grand Total") %>%
        ggplot(aes(x = period, y = tx_curr, fill = type)) +
        geom_col() +
        facet_wrap(~psnu) +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_fill_manual(values = c(scooter_light, scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "Reported and Projected TX_CURR",
            subtitle = glue("Difference Between Reported TX_CURR and TX_CURR Proxy: {df_viz$diff_label[1]}"),
            caption = glue("Source: DRC TX_CURR By PSNU and COP23 targets | DRC PEPFAR SI Technical Working Group")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 8))

    # by PSNU
    df_viz %>%
        filter(psnu != "Grand Total") %>%
        mutate(
            psnu = glue("{psnu} | {diff_label}")) %>%
        ggplot(aes(x = period, y = tx_curr, fill = type)) +
        geom_col() +
        facet_wrap(~forcats::fct_reorder(psnu, -tx_curr)) +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_fill_manual(values = c(scooter_light, scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "Reported and Projected TX_CURR",
            subtitle = glue("Labelled with Quarterly Difference Between Reported TX_CURR and TX_CURR Proxy by PSNU"),
            caption = glue("Source: DRC TX_CURR By PSNU and COP23 targets | DRC PEPFAR SI Technical Working Group")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 8))


