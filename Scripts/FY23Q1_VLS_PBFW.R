# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  QC for VLS among PBFW
# REF ID:   888ad9f2
# LICENSE:  MIT
# DATE:     2023-03-16
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

    library(tidyverse)
    library(extrafont)
    library(gagglr)
    library(janitor)

# GLOBAL VARIABLES ------------------------------------------------------------

    ref_id <- "888ad9f2"

# IMPORT ----------------------------------------------------------------------

    # SI specific paths/functions
    load_secrets()
    merdata <- file.path(si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
                               pattern = "OU_IM_FY20-23_20221216")

    # Grab metadata
    get_metadata(file_path)

    df <- read_psd(file_path)

# MUNGE -----------------------------------------------------------------------

    df_vl_pmtct_viz <- df %>%
        clean_indicator() %>%
        filter(
            fiscal_year == 2023,
            funding_agency == "USAID",
            operatingunit == "Democratic Republic of the Congo",
            (indicator == "PMTCT_ART" & numeratordenom == "N" &
                 otherdisaggregate == "Life-long ART, Already") |
                (indicator == "TX_PVLS" & numeratordenom == "D" &
                     standardizeddisaggregate == "PregnantOrBreastfeeding/Indication/HIVStatus" &
                     otherdisaggregate %in% c("Pregnant, Routine", "Pregnant, Targeted"))) %>%
        group_by(fiscal_year, indicator) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
        reshape_msd(include_type = FALSE) %>%
        pivot_wider(names_from = indicator,
                    names_glue = "{tolower(indicator)}") %>%
        mutate(pmtct_art_lag4 = lag(pmtct_art, 4, na.rm = TRUE),
               vlc = tx_pvls_d/pmtct_art_lag4)