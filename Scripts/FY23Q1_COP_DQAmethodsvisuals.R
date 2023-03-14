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

    psnu_snu <- read_sheet(path, "TX_CURR Proxy By PSNU & Age Sex") %>%
        clean_names() %>%
        select(snu, psnu) %>%
        distinct()

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
            diff_pct_min = percent(percent_difference - .01, accuracy = 1),
            diff_pct_max = percent(percent_difference + .01, accuracy = 1),
            diff_num_min = comma(difference_between_reported_and_proxy - (percent_difference / .01),  accuracy = 1),
            diff_num_max = comma(difference_between_reported_and_proxy + (percent_difference / .01),  accuracy = 1),
            # diff_pct = percent(percent_difference, accuracy = 1),
            # diff_num = scales::comma(difference_between_reported_and_proxy, accuracy = 1),
            diff_label = glue::glue("{diff_num_min} - {diff_num_max} ({diff_pct_min} - {diff_pct_max})"),
            type_color = if_else(type == "reported", scooter, scooter_light))

    # SNU level
    df_viz_snu <-  df_viz %>%
        left_join(., psnu_snu, by = c("psnu")) %>%
        group_by(snu, psnu, fiscal_year,
                 period, type, difference_between_reported_and_proxy,
                 percent_difference) %>%
        mutate(
            snu = case_when(
                (is.na(snu) == TRUE & psnu == "Military") ~ "Military",
                (is.na(snu) == TRUE & psnu != "Military") ~ "National",
                TRUE ~ snu),
            snu = if_else(psnu == "Grand Total",
                          "National", snu),
            psnu = if_else(psnu == "Grand Total",
                           "National", psnu)) %>%
        filter(psnu != "National") %>%
        ungroup() %>%
        group_by(snu, fiscal_year, period, type) %>%
        summarize(across(c(tx_curr, difference_between_reported_and_proxy, percent_difference), \(x) sum(x, na.rm = TRUE))) %>%
        mutate(
            diff_pct_min = percent(percent_difference - .01,  accuracy = 1),
            diff_pct_max = percent(percent_difference + .01, accuracy = 1),
            diff_num_min = comma(difference_between_reported_and_proxy - (percent_difference / .01),  accuracy = 1),
            diff_num_max = comma(difference_between_reported_and_proxy + (percent_difference / .01),  accuracy = 1),
            # diff_pct = percent(percent_difference, accuracy = 1),
            # diff_num = scales::comma(difference_between_reported_and_proxy, accuracy = 1),
            diff_label = glue::glue("{diff_num_min} - {diff_num_max} ({diff_pct_min} - {diff_pct_max})"),)

# VIZ --------------------------------------------------------------------------

# What is the projected TX_CURR?
    # Q what units is the difference #/% in? people or bottles?

    # total
    df_viz %>%
        filter(psnu == "National") %>%
        ggplot(aes(x = period, y = tx_curr, fill = type)) +
        geom_col() +
        facet_wrap(~psnu) +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_fill_manual(values = c(scooter_light, scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "Reported and Projected TX_CURR",
            subtitle = glue("Difference Between Reported TX_CURR and TX_CURR Proxy: {df_viz$diff_label[1]}"),
            caption = glue("Source: DRC TX_CURR By PSNU and COP23 targets | DRC PEPFAR SI Technical Working Group
                           ref id: {ref_id}")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 8))

    # SNU
   df_viz_snu %>%
       mutate(
           snu = glue("{snu} | {diff_label}")) %>%
        ggplot(aes(x = period, y = tx_curr, fill = type)) +
        geom_col() +
        facet_wrap(~snu) +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_fill_manual(values = c(scooter_light, scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "Reported and Projected TX_CURR",
            subtitle = glue("Labelled with Quarterly Difference Between Reported TX_CURR and TX_CURR Proxy by SNU"),
            caption = glue("Source: DRC TX_CURR By SNU and COP23 targets | DRC PEPFAR SI Technical Working Group
                           ref id: {ref_id}")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 8))

    # by PSNU
    df_viz %>%
        filter(psnu != "National") %>%
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
            caption = glue("Source: DRC TX_CURR By PSNU and COP23 targets | DRC PEPFAR SI Technical Working Group
                           ref id: {ref_id}")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 8))


