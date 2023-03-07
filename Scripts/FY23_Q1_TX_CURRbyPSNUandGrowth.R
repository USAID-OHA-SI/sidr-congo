# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:
# REF ID:   7233c573
# LICENSE:  MIT
# DATE:     2023-03-07
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

    library(tidyverse)
    library(extrafont)
    library(gagglr)
    library(janitor)
    library(ggext)
    library(ggplot)
    library(readexcel)
    library(googlesheets4)
    library(stringr)

# GLOBAL VARIABLES ------------------------------------------------------------

    load_secrets()
    ref_id <- "7233c573"

# IMPORT ----------------------------------------------------------------------

    # Cleaned Q4 Commodities Data
    path <- "1-X4MBRAwInOI54o9h46C22_atvBi-9lA7EKLQC82Zfo"

    df_psnu <- read_sheet(path, sheet = "Commodities Data") %>%
        clean_names()

# MUNGE -----------------------------------------------------------------------

    # Apply methodology for PSNU level projected growth

    # need by PSNU:
    # - 1. TLD standardization
    # - 2. Estimated Monthly Consumption based on prev 12 months
    # - 3. Consumption adjustement based on annual growth of TX curr from MER (x%) and TX_CURR at PSNU level from DQA
    # - 4. Projection (commodities in, by month, and by qtr, MoS)

    df_clean <- df_psnu %>%
        # group data by date, year, designation, provinces, health zone
        group_by(date, date_pr, annee, designation,
                 provinces, unite, zone_de_sante) %>%
        mutate(
            across(starts_with("date",
                               ~as.Date(., origin = "1899-12-30"))),
            tld_std_cats = case_when(
                str_detect(designation,
                           "Abacavir (ABC) et  lamivudivine (3TC) 120mg/60mg tab 60") ~ "ABC+3TC 120/60 60",
                        # ask Katie is this is correct
                           ("Dolutegravir/Lamivudine/Tenofovir 50mg/300mg/300mg Tablets, 180 tablets") |
                           ("Dolutégravir/Lamivudine/Tenofovir DF 50/300/300mg,tab,180,vrac") ~ "TLD 180",

                           ("Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg,tablet,90") |
                           ("Dolutegravir/Tenofovir/ Lamividine 50/300/300 mg ,90") |
                           ("Dolutegravir/Lamivudine/Tenofovir 50mg/300mg/300mg Tablets, 90 tablets") ~ "TLD90",

            )
        # reformat the dates
        # create the TLD standard categories from designation







