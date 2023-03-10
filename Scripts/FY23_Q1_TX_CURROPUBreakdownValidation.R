# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to verify the targets for selected indicators in OPU
# REF ID:   802f460c
# LICENSE:  MIT
# DATE:     2023-03-10
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

    library(tidyverse)
    library(extrafont)
    library(gagglr)
    library(janitor)
    library(ggtext)
    library(ggplot2)
    library(googlesheets4)
    library(assertthat)
    library(assertr)

# GLOBAL VARIABLES ------------------------------------------------------------

    ref_id <- "802f460c"
    load_secrets()

# IMPORT ----------------------------------------------------------------------

    path <- "PSNU_IM_Democratic_Republic_of_the_Congo"

    df_mer <- si_path() %>%
        return_latest(path) %>%
        read_psd()

    opu_path <- "18tR_zmQGe_2FNgJ_1YcFr4DQuxWfCYJd2t5J5lXLcos"

    df_opu <- read_sheet(opu_path, skip = 3, col_names = TRUE,
                         sheet = "Target Update") %>%
        clean_names()

# MUNGE -----------------------------------------------------------------------

    df_drc <- df_mer %>%
        filter(
            fiscal_year == "2023",
            funding_agency == "USAID",
            indicator %in% c("TX_CURR", "GEND_GBV", "HTS_INDEX",
                                "HTS_RECENT", "HTS_SELF", "HTS_TST",
                                "HTS_TST_POS", "OVC_HIVSTAT",
                                "OVC_SERV","PMTCT_ART", "PMTCT_EID",
                                "PMTCT_STAT", "PMTCT_STAT_POS",
                                "PrEP_CT", "PrEP_NEW", "TB_ART",
                                "TB_PREV", "TB_STAT","TB_STAT_POS",
                                "TX_CURR","TX_NEW", "TX_PVLS", "TX_TB",
                                 "KP_PREV"),
            mech_code %in% c("84206", "85505", "85506", "18093","84207")) %>%
        resolve_knownissues() %>%
        group_by(fiscal_year, mech_name, mech_code, numeratordenom, indicator) %>%
        # summarize targets by mech and year
        summarize(
            fy23_target = sum(targets, na.rm = TRUE)) %>%
        arrange(mech_name, mech_code) %>%
        # provide names from the OPU sheet for mechs with placeholder names
        mutate(
            mech_name = if_else(
                mech_code == "84206", "CHALPA Kinshasa", mech_name),
            mech_name = if_else(
                mech_code == "85505", "CHALPA Lualaba", mech_name),
            mech_name = if_else(
                mech_code == "85506", "CHEK", mech_name))

    # Munge OPU data
    df_opu_filt <- df_opu %>%
        rename(indicator = indicator_2,
               mech_name = implementing_mechanism,
               difference = difference_13) %>%
        mutate(
            mech_code = as.character(mech_id_1),
            difference = as.numeric(difference)) %>%
        select(mech_code, mech_name, indicator, numerator_denominator,
               x2023_target, new_total, difference)

    # join and compare MER to OPU Breakdown

    full_df <- df_drc %>%
        full_join(df_opu_filt, by = c("mech_code",
                                      "mech_name",
                                      "indicator",
                                      "numeratordenom" = "numerator_denominator")) %>%
        ungroup() %>%
        select(mech_code, indicator, mech_name, numeratordenom, x2023_target, new_total, difference, fy23_target) %>%
        group_by(mech_code, indicator) %>%
        # filter out those indicators with both n and d targets since these are causing
        # formatting issues
        filter(!(numeratordenom == "N" & x2023_target == "NULL")) %>%
        drop_na(indicator) %>%
        # reverse engineer the new_total from the MER 2023 target by adding the
        # difference from the OPU breakdown to the reported fy23_target,
        # expect to get the same value as the new_total column
        mutate(
            calc_target = fy23_target + difference)

    # Are there places where this pattern doesn't hold?
    # review those
    mismatches <- full_df %>%
        filter(new_total != calc_target,
               calc_target != abs(difference))

    full_df %>%
        write_sheet("1tqSWsHbMnsOXTO1T7PVIU2v0fLMGnMtA9hZYRvJv_bA", "Sheet 1")



    # keep only indicators with both null targets and are numerators
    # to fix and check later
    numd_df <- df_drc %>%
        full_join(df_opu_filt, by = c("mech_code",
                                      "mech_name",
                                      "indicator",
                                      "numeratordenom" = "numerator_denominator")) %>%
        ungroup() %>%
        select(mech_code, indicator, mech_name, numeratordenom, x2023_target, difference, fy23_target) %>%
        group_by(mech_code, indicator) %>%
        # Since the targets are the same for N and D where both exist,
        # this will remove extra Nulls
        filter(numeratordenom == "N" & x2023_target == "NULL")



