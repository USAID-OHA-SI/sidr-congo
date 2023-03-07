# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:
# REF ID:   1c459613
# LICENSE:  MIT
# DATE:     2023-03-07
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

    library(tidyverse)
    library(extrafont)
    library(gagglr)
    library(janitor)
    library(ggtext)
    library(ggplot2)
    library(googlesheets4)
    library(stringr)
    library(glue)
    library(scales)
    library(forcats)

# GLOBAL VARIABLES -------------------------------------------------------------

    load_secrets()

# IMPORT -----------------------------------------------------------------------

    path <- "PSNU_IM_Democratic_Republic_of_the_Congo"

    df <- si_path() %>%
        return_latest(path) %>%
        read_psd()

    #metadata
    si_path() %>%
        return_latest(path) %>%
        get_metadata()


# MUNGE ------------------------------------------------------------------------

    peds <- c("<01", "01-04", "05-09", "10-14")
    adults <- c(
        "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
        "50-54", "55-59", "60-64", "65+")

    # OU level national growth rate
    df_ou <- df %>%
        filter(
            indicator == "TX_CURR",
            (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
                (standardizeddisaggregate == "Total Numerator") |
                (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% adults)) %>%
        mutate(type = case_when(standardizeddisaggregate == "Total Numerator" ~ "Total",
                                standardizeddisaggregate == "Age/Sex/HIVStatus" &
                                    ageasentered %in% adults ~ "Adults",
                                standardizeddisaggregate == "Age/Sex/HIVStatus" &
                                    ageasentered %in% peds ~ "Pediatric"),
               psnu = "National") %>%
        group_by(fiscal_year, operatingunit, psnu, indicator, type) %>%
        summarise(across(c(targets, starts_with("qtr")), \(x) sum(x, na.rm = TRUE)),
                  .groups = "drop") %>%
        reshape_msd("quarters") %>%
        select(-results_cumulative) %>%
        arrange(type, operatingunit, psnu, period)


    # national growth rate
    df_growth_ou <- df_ou %>%
        mutate(growth_rate_req = case_when(period == metadata$curr_pd ~
                                               ((targets/results)^(1/(4-metadata$curr_qtr))) -1)) %>%
        group_by(type) %>%
        fill(growth_rate_req, .direction = "updown") %>%
        mutate(growth_rate = (results/lag(results, order_by = period)) - 1,
               growth_rate = na_if(growth_rate, Inf)) %>%
        ungroup() %>%
        mutate(
            gr_lab = case_when(fiscal_year == metadata$curr_fy ~
                                   glue("{percent(growth_rate, 1)}")),
            gr_label_position = 0,
            results_lab = case_when(fiscal_year == metadata$curr_fy ~
                                        glue("{gr_lab} ({comma(results)})")))

    # PSNU
    df_psnu <- df %>%
        filter(
            indicator == "TX_CURR",
            (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
                (standardizeddisaggregate == "Total Numerator") |
                (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% adults)) %>%
        mutate(type = case_when(standardizeddisaggregate == "Total Numerator" ~ "Total",
                                standardizeddisaggregate == "Age/Sex/HIVStatus" &
                                    ageasentered %in% adults ~ "Adults",
                                standardizeddisaggregate == "Age/Sex/HIVStatus" &
                                    ageasentered %in% peds ~ "Pediatric"),
               psnu = str_replace(psnu, "_Military Democratic Republic of the Congo", "Military"),
               operatingunit = psnu) %>%
        group_by(fiscal_year, psnu, indicator, type) %>%
        summarise(across(c(targets, starts_with("qtr")), \(x) sum(x, na.rm = TRUE)),
                  .groups = "drop") %>%
        reshape_msd("quarters") %>%
        select(-results_cumulative) %>%
        arrange(type, psnu, period)


     # PSNU growth rate
     df_growth_psnu <- df_psnu %>%
         mutate(growth_rate_req = case_when(period == metadata$curr_pd ~
                                                ((targets/results)^(1/(4-metadata$curr_qtr))) -1)) %>%
         group_by(type, psnu) %>%
         fill(growth_rate_req, .direction = "updown") %>%
         mutate(growth_rate = (results/lag(results, order_by = period)) - 1,
                growth_rate = na_if(growth_rate, Inf)) %>%
        ungroup() %>%
        mutate(
            gr_lab = case_when(fiscal_year == metadata$curr_fy ~
                                   glue("{percent(growth_rate, 1)}")),
            gr_label_position = 0,
            results_lab = case_when(fiscal_year == metadata$curr_fy ~
                                        glue("{gr_lab} ({comma(results)})")))

     # add national results to PSNU results
     df_growth_total <- df_growth_psnu %>%
         bind_rows(df_growth_ou) %>%
         filter(type == "Total",
                period == "FY22Q4") %>%
         # National and 20 PSNUs by highest TX_CURR
         slice_max(n = 21,
                   order_by = tibble(psnu, results))

     df_growth_adults <- df_growth_psnu %>%
         bind_rows(df_growth_ou) %>%
         filter(type == "Adults",
                period == "FY22Q4") %>%
         # National and 20 PSNUs by highest TX_CURR
         slice_max(n = 21,
                   order_by = tibble(psnu, results))

     df_growth_peds <- df_growth_psnu %>%
         bind_rows(df_growth_ou) %>%
         filter(type == "Pediatric",
                period == "FY22Q4") %>%
         # National and 20 PSNUs by highest TX_CURR
         slice_max(n = 21,
                   order_by = tibble(psnu, results))

# VIZ --------------------------------------------------------------------------

    df_growth_total %>%
        ggplot(aes(fct_reorder(psnu, results), results, fill = as.character(period))) +
        geom_col(na.rm = TRUE, alpha = .7, width = 1) +
        geom_text(aes(label = gr_lab, y = results),
                  family = "Source Sans Pro", color = scooter, size = 9 / .pt,
                  vjust = .5, na.rm = TRUE) +
        coord_flip() +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_fill_manual(values = c(
            scooter_light, scooter_light, scooter_light, scooter_light,
            scooter_light, scooter_light, scooter_light, scooter_light,
            scooter, scooter, scooter, scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "PSNUs reporting more people on ART had Q4 growth rates similar to that of
                     to the country overall while PSNUs reporting fewer people on ART had much higher Q4 growth rates",
            subtitle = glue("How did TX_CURR grow in the largest PSNUs in Q4 of FY22? | Age Group: Total"),
            caption = glue("
                     Note: Adults = Ages 15+ and Children= Ages <15,
                     PSNUs limited by highest 20 reported TX_CURR results from FY22Q4
                     {metadata$caption} | US Agency for International Development")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 8))

    # Adults

    df_growth_adults %>%
        ggplot(aes(fct_reorder(psnu, results), results, fill = as.character(period))) +
        geom_col(na.rm = TRUE, alpha = .7, width = 1) +
        geom_text(aes(label = gr_lab, y = results),
                  family = "Source Sans Pro", color = scooter, size = 9 / .pt,
                  vjust = .5, na.rm = TRUE) +
        coord_flip() +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_fill_manual(values = c(
            scooter_light, scooter_light, scooter_light, scooter_light,
            scooter_light, scooter_light, scooter_light, scooter_light,
            scooter, scooter, scooter, scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "PSNUs reporting more adults on ART had Q4 growth rates similar to that of
                     to the country overall while PSNUs reporting fewer adults on ART had much higher Q4 growth rates",
            subtitle = glue("How did TX_CURR grow in the largest PSNUs in Q4 of FY22? | Age Group: Total"),
            caption = glue("
                     Note: Adults = Ages 15+ and Children= Ages <15,
                     PSNUs limited by highest 20 reported TX_CURR results from FY22Q4
                     {metadata$caption} | US Agency for International Development")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 8))

    # Pediatrics
    df_growth_peds %>%
        ggplot(aes(fct_reorder(psnu, results), results, fill = as.character(period))) +
        geom_col(na.rm = TRUE, alpha = .7, width = 1) +
        geom_text(aes(label = gr_lab, y = results),
                  family = "Source Sans Pro", color = scooter, size = 9 / .pt,
                  vjust = .5, na.rm = TRUE) +
        coord_flip() +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_fill_manual(values = c(
            scooter_light, scooter_light, scooter_light, scooter_light,
            scooter_light, scooter_light, scooter_light, scooter_light,
            scooter, scooter, scooter, scooter)) +
        labs(
            x = NULL, y = NULL,
            title = "PSNUs reporting more children on ART had Q4 growth rates similar to that of
                     to the country overall while PSNUs reporting fewer children on ART had much higher Q4 growth rates",
            subtitle = glue("How did TX_CURR grow in the largest PSNUs in Q4 of FY22? | Age Group: Total"),
            caption = glue("
                     Note: Adults = Ages 15+ and Children= Ages <15,
                     PSNUs limited by highest 20 reported TX_CURR results from FY22Q4
                     {metadata$caption} | US Agency for International Development")) +
        si_style_ygrid() +
        theme(
            legend.position = "none",
            panel.spacing = unit(.5, "picas"),
            axis.text.x = element_text(size = 8))
