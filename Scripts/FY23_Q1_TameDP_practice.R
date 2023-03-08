# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  investigate TameDP with initial draft of TaST
# REF ID:   76f4279d
# LICENSE:  MIT
# DATE:     2023-03-08
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

    library(tidyverse)
    library(extrafont)
    library(gagglr)
    library(janitor)
    library(ggtext)
    library(ggplot2)
    library(tameDP)
    library(gt)
    library(scales)
    library(stringr)

# GLOBAL VARIABLES ------------------------------------------------------------

    ref_id <- "76f4279d"

    #required if connecting to DATIM, otherwise not
    load_secrets()

# IMPORT ----------------------------------------------------------------------

    # dp w/o PSNUxIM tab
    dp_path <- "Data/TaST/Target Setting Tool_Democratic Republic of the Congo_20230214211948.xlsx"

    # read in Target Setting Tool & tidy
    df_dp <- tame_dp(path)

    # read in PLHIV and SUB_NAT data from the Target Setting Tool
    df_plhiv <- tame_dp(path, type = "PLHIV")

    # get all the targets
    df_all <- tame_dp(path, type = "ALL")

    # get the PSNUxIM tab without mech names/prime partner data

    dp_psnu_im_path <- ""

    df_dp_nomech <- tame_dp(dp_psnu_im_path, type = "PSNUxIM")

    # get the PSNUxIM tab with mech names/prime partner data from DATIM
    # note this queries DATIM so it needs internet and may be slow
    df_dp_mech <- tame_dp(path, type = "PSNUxIM", map_names = TRUE)

    # aggregate the data up to the PSNU level if working with PSNUxIM data
    df_dp_psnu <- tame_dp(path, type = "PSNUxIM", psnu_lvl = TRUE)

    # to see an example of how to iterate over multiple files see:
    # https://github.com/USAID-OHA-SI/coRps/blob/5ca79809f46bab10d1c6d08da26102e6efe1b19d/2023-01-11/2023-tameDP-review.md

    # to combine with the MSD, use the PSNUxIM data
    # https://usaid-oha-si.github.io/tameDP/articles/combining-with-msd.html

    # check against the MER data
    msd_path <- "PSNU_IM_Democratic_Republic_of_the_Congo"

    psnu_df <- si_path() %>%
        return_latest(msd_path) %>%
        read_psd()

    get_metadata(type = "PSNU_IM")
    metadata_msd_psnu <- metadata
    rm(metadata)

# MUNGE -----------------------------------------------------------------------

    # what districts (PSNUs) have the highest share newly identified positives from the community?

    df_dp %>%
        filter(indicator == "HTS_TST_POS",
               standardizeddisaggregate != "KeyPop") %>%
        mutate(source = ifelse(stringr::str_detect(modality, "Mod"), "Comm", "Fac")) %>%
        count(psnu, source, wt = targets, name = "targets") %>%
        group_by(psnu) %>%
        mutate(share = targets/sum(targets, na.rm = TRUE),
               source = str_replace(source, "Comm", "Community"),
               psnu = str_replace(psnu, "_Military", "Military")) %>%
        ungroup() %>%
        filter(source == "Community") %>%
        arrange(desc(share)) %>%
        print(n = Inf) %>%
        mutate(share = percent(share, 1)) %>%
        gt() %>%
        # add custom title
        tab_header(
            title = "PSNUs with the highest share of newly identified positives from the community") %>%
        tab_footnote("Source: DRC TaST | SI Analytics") %>%
        # relabel columns nicely
        cols_label(
            psnu = "PSNU",
            source = "Source",
            targets = "Targets",
            share = "Share (%)")

    # spot check the Cascade tab in the initial DP
    # this may take a minute or two
    spot_check <- psnu_df %>%
        resolve_knownissues() %>%
        clean_indicator() %>%
        filter(indicator %in% c("HTS_TST_POS", "PMTCT_HEI_POS", "TX_NEW",
                                "TX_CURR", "TX_PVLS_D", "TX_PVLS_D"),
               fiscal_year == "2022",
               str_detect(standardizeddisaggregate, "Age/Sex/") == TRUE,
               snuprioritization != "8- Not PEPFAR Supported") %>%
        group_by(fiscal_year, psnu, indicator, ageasentered, sex) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
        reshape_msd() %>%
        filter(value >0,
               period == "FY22Q4") %>%
        arrange(psnu, indicator, ageasentered, sex)

    #get just the data from the cascade tab, compare against the spot_check
    # cascade_tab <- df_dp






