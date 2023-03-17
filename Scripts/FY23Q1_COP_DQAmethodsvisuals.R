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

    # DRC TX_CURR By PSNU and COP23 targets_20230314 SM
    path <- "1QsUJm5QzSuRaBGdTTQtfDCCYFs4TH68k5BSXTWJ4Y9E"

    df <- read_sheet(path, "Targets", skip = 1) %>%
        clean_names()

    # proposed targets, scenario 2

    df_proposed <- data.frame(
        snu = c("Military", "Haut Katanga", "Kinshasa", "Lualaba", "OU Total",
                "Military", "Haut Katanga", "Kinshasa", "Lualaba", "OU Total",
                "Military", "Haut Katanga", "Kinshasa", "Lualaba", "OU Total"),
        type = c("reported", "reported", "reported", "reported", "reported",
                 "projected", "projected","projected","projected","projected",
                 "targets","targets","targets","targets","targets"),
        fiscal_year = c("2023", "2023", "2023", "2023", "2023",
                        "2023", "2023", "2023", "2023", "2023",
                        "2024", "2024", "2024", "2024", "2024"),
        tx_curr = c(15802, 127254, 100432, 32728, 276216,
                    11442, 91611, 77528, 21864, 202445,
                    12630, 101121, 85577, 24133, 223461))

# MUNGE ------------------------------------------------------------------------

    df_viz <- df %>%
        # select only the columns needed for viz
        select(psnu, reported_fy23_q1_tx_curr, starts_with("fy23_")) %>%
        drop_na(psnu) %>%
        pivot_longer(cols = c("reported_fy23_q1_tx_curr", "fy23_q2",
                              "fy23_q3","fy23_q4"),
            names_to = "fiscal_year",
            values_to = "tx_curr") %>%
        mutate(
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
            type_color = if_else(type == "reported", scooter, scooter_light)) %>%
        group_by(fiscal_year, period, type, type_color) %>%
        summarize(
            tx_curr = sum(tx_curr),
            tx_curr_min = tx_curr - (tx_curr *0.01),
            tx_curr_max = tx_curr + (tx_curr *0.01),
            )

    # SNU level
    df_viz_snu <-  df_viz %>%
        left_join(., psnu_snu, by = c("psnu")) %>%
        group_by(snu, psnu, fiscal_year, period, type, tx_curr) %>%
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
        summarize(across(c(tx_curr), \(x) sum(x, na.rm = TRUE)))

    df_prop_viz <- df_proposed %>%
        mutate(
            tx_curr_min = if_else(
                snu == "OU Total",
                round_half_up(tx_curr - (tx_curr *.01), 0),
                0),
            tx_curr_max = if_else(
                snu == "OU Total",
                round_half_up(tx_curr + (tx_curr *.01), 0),
                0))
# VIZ --------------------------------------------------------------------------

# What is the projected TX_CURR?

    # total w/ absolute values
    df_viz %>%
        ggplot(aes(x = period, y = tx_curr, fill = type)) +
        geom_col() +
        geom_text(aes(label = comma(tx_curr, accuracy = 1), y = tx_curr_max + 10000, color = type),
                  family = "Source Sans Pro", size = 20 / .pt,
                  vjust = .5, na.rm = TRUE) +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_color_manual(values = c(scooter_light, scooter)) +
        scale_fill_manual(values = c(scooter_light, scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "Reported and Projected TX_CURR",
            caption = glue("Source: DRC TX_CURR By PSNU and COP23 targets | DRC PEPFAR SI Technical Working Group
                           ref id: {ref_id}")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20))


    # total w/ ranges
    df_viz %>%
        ggplot(aes(x = period, y = tx_curr, fill = type)) +
        geom_col() +
        geom_text(aes(label = glue("{comma(tx_curr_min, accuracy = 1)} - {comma(tx_curr_max, accuracy = 1)}"),
                                   y = tx_curr_max + 10500, color = type),
                  family = "Source Sans Pro", size = 14 / .pt,
                  vjust = .5, na.rm = TRUE) +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_color_manual(values = c(scooter_light, scooter)) +
        scale_fill_manual(values = c(scooter_light, scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "Reported and Projected TX_CURR",
            caption = glue("Source: DRC TX_CURR By PSNU and COP23 targets | DRC PEPFAR SI Technical Working Group
                           ref id: {ref_id}")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20))

    # from data in slides

    tx_curr_abs <- df_prop_viz %>%
        filter(type %in% c("projected"),
               snu == "OU Total") %>%
        select(tx_curr) %>%
        pull() %>%
        comma()

    # without range
    df_prop_viz %>%
        filter(type %in% c("projected", "reported"),
               snu == "OU Total") %>%
        ggplot(aes(x = forcats::fct_reorder(snu, tx_curr),fill = type)) +
        geom_col(aes(y = tx_curr), alpha = 0.7) +
        coord_flip() +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_color_manual(values = scooter_light) +
        scale_fill_manual(values = c(
            "projected" = scooter_light,
            "reported" = scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "Reported and Projected TX_CURR for FY23",
            subtitle = glue("Projected OU-level growth for FY 23: { tx_curr_abs}"),
            caption = glue("Source: Proposed targets (Scenario 2) | DRC PEPFAR SI Technical Working Group
                           ref id: {ref_id}")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 20))

    ou_max <- df_prop_viz %>%
                filter(type %in% c("projected"),
                       snu == "OU Total") %>%
        select(tx_curr_max) %>%
        pull() %>%
        comma()

    ou_min <- df_prop_viz %>%
        filter(type %in% c("projected"),
               snu == "OU Total") %>%
        select(tx_curr_min) %>%
        pull() %>%
        comma()

    # with range (+/- 1%)
    df_prop_viz %>%
        filter(type %in% c("projected", "reported"),
               snu == "OU Total") %>%
        ggplot(aes(x = forcats::fct_reorder(snu, tx_curr),fill = type)) +
        geom_col(aes(y = tx_curr), alpha = 0.7) +
        geom_pointrange(aes(y = tx_curr, ymin = tx_curr_min,
                            ymax = tx_curr_max,  color = snu)) +
        coord_flip() +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_color_manual(values = trolley_grey_light) +
        scale_fill_manual(values = c(
            "projected" = scooter_light,
            "reported" = scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "Reported and Projected TX_CURR for FY23",
            subtitle = glue("Projected OU-level growth for FY 23: {ou_min} - {ou_max}"),
            caption = glue("Source: Proposed targets (Scenario 2) | DRC PEPFAR SI Technical Working Group
                           ref id: {ref_id}")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 20))



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


