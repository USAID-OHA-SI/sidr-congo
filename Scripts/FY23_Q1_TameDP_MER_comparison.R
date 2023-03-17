# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to pull targets from PrEP and TB tabs + join with MER
# REF ID:   17b1bfce
# LICENSE:  MIT
# DATE:     2023-03-17
# UPDATED:


# DEPENDENCIES ----------------------------------------------------------------

    library(tidyverse)
    library(tameDP)

    # for checking against MSD
    library(gagglr)

    # export to read/write excel
    library(readxl)
    library(writexl)

# GLOBAL VARIABLES ------------------------------------------------------------

    ref_id <- "17b1bfce"
    load_secrets()

    tst_path <- here::here("Data/TST/Target Setting Tool_Democratic Republic of the Congo_20230214211948.xlsx")

    # PreP trends 21-23 from PrEPQuarterlyDashboard
    prep_path <-  here::here("Data/TST/PrEP Trends_1.xlsx")

    # TB trends 17-23 from TBHIVQuarterlyDashboard
    tb_path <- here::here("Data/TST/Target volume.xlsx")

# IMPORT ----------------------------------------------------------------------

    tst_df <- tame_dp(tst_path)
    #
    # msd_df <- si_path() %>%
    #     return_latest(psnu_path) %>%
    #     read_psd()
    #
    # get_metadata(type = "PSNU_IM")
    # metadata_msd_psnu <- metadata

    prep_df <- readxl::read_xlsx(prep_path) %>%
        janitor::clean_names()

    tb_df <- readxl::read_xlsx(tb_path, skip = 1)%>%
        janitor::clean_names()

# MUNGE -----------------------------------------------------------------------

    # psnu_level TST
    tst_psnu <- tst_df %>%
        clean_indicator() %>%
        ungroup() %>%
        group_by(
            operatingunit, country,
            psnu,
            fiscal_year,
            indicator, standardizeddisaggregate,snuprioritization,
            ageasentered, sex, numeratordenom) %>%
        summarize(
            targets = sum(targets),
            cumulative = sum(cumulative)) %>%
        arrange(psnu, fiscal_year, indicator, ageasentered, sex) %>%
        ungroup() %>%
        select(country,psnu,fiscal_year,
               indicator, snuprioritization,   standardizeddisaggregate, ageasentered, sex,
               cumulative, targets) %>%
        rename(
            cumulative_tst = cumulative,
            targets_tst = targets,
            age = ageasentered) %>%
        filter(stringr::str_detect(indicator, "TB") == TRUE | stringr::str_detect(indicator, "PrEP") == TRUE)

    tb <- tst_psnu %>%
        filter(stringr::str_detect(indicator, "TB") == TRUE)

    write_xlsx(tb, "Dataout/TB_TST_Targets_03172023.xlsx")

    prep <- tst_psnu %>%
        filter(stringr::str_detect(indicator, "PrEP") == TRUE)


    write_xlsx(prep, "Dataout/PreP_TST_Targets_03172023.xlsx")


    # psnu level MSD to match
    # msd_psnu <- msd_df %>%
    #     clean_indicator() %>%
    #     mutate(
    #         # match age disaggs to TST
    #         age = case_when(
    #             ageasentered == "<01" ~ "<01",
    #             (ageasentered == "01-04" |  ageasentered == "05-09") ~ "01-09",
    #             ageasentered == "10-14"  ~ "10-14",
    #             (ageasentered == "15-19" | ageasentered == "20-24") ~ "15-24",
    #             (ageasentered == "25-29" | ageasentered == "30-34") ~ "25-34",
    #             (ageasentered == "35-39" | ageasentered == "40-44" |
    #                  ageasentered == "45-49") ~ "35-49",
    #             (ageasentered == "50-54" | ageasentered == "55-59" |
    #                  ageasentered == "60-64") ~ "50+")) %>%
    #     group_by(
    #         operatingunit, country,
    #         psnu,
    #         fiscal_year,
    #         indicator, age, sex,
    #         standardizeddisaggregate, snuprioritization) %>%
    #     summarise(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
    #     select(country,psnu,fiscal_year,
    #            indicator,  standardizeddisaggregate, snuprioritization, age, sex, cumulative, targets) %>%
    #     filter(psnu %in% tst_psnu$psnu,
    #            indicator %in% tst_psnu$indicator,
    #            snuprioritization %in% tst_psnu$snuprioritization,
    #            standardizeddisaggregate %in% tst_psnu$standardizeddisaggregate,
    #            fiscal_year %in% tst_psnu$fiscal_year) %>%
    #     rename(
    #         cumulative_msd = cumulative,
    #         targets_msd = targets) %>%
    #     filter(stringr::str_detect(indicator, "TB") == TRUE | stringr::str_detect(indicator, "PrEP") == TRUE) %>%
    #     # remove totals
    #     drop_na(age)

    tb_filt <- tb_df %>%
        pivot_wider(names_from = "cumulative",
                    values_from = "x3737")

    # combine + compare

    combined <- full_join(tst_psnu, msd_psnu, by = c("country","psnu","fiscal_year",
                                                     "indicator","snuprioritization",
                                                     "standardizeddisaggregate",
                                                     "age", "sex"))