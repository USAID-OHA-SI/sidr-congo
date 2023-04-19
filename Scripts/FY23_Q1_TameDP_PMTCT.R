# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  update TST data from PMTCT tab to fit updated age bands
# REF ID:   bd3b59c4
# LICENSE:  MIT
# DATE:     2023-03-22
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

    library(tidyverse)
    library(extrafont)
    library(gagglr)
    library(janitor)
    library(tameDP)
    library(googlesheets4)

# GLOBAL VARIABLES ------------------------------------------------------------

    ref_id <- "bd3b59c4"

    # create function to update age bands from COP22 to use in COP23

    update_age <- function(.age_old, ...){

    # match age disaggs to TST
    age_updated <- dplyr::case_when(
      .age_old == "<01" ~ "<01",
      (.age_old == "01-04" | .age_old == "05-09") ~ "01-09",
      .age_old == "10-14" ~ "10-14",
      (.age_old == "15-19" | .age_old == "20-24") ~ "15-24",
      (.age_old == "25-29" | .age_old == "30-34") ~ "25-34",
      (.age_old == "35-39" | .age_old == "40-44" |
        .age_old == "45-49") ~ "35-49",
      (.age_old == "50-54" | .age_old == "55-59" |
        .age_old == "60-64"|  .age_old == "65+") ~ "50+"
    )

    return(age_updated)

    }

# IMPORT ----------------------------------------------------------------------

    # read in old DP tab
    old_path <- here::here("Data/TST/PMTCT tabs_COP22 and 23.xlsx")

    # read in Target Setting Tool & tidy
    df_old <- tame_dp(old_path)

# MUNGE -----------------------------------------------------------------------

    df_old_filt <- df_old %>%
        rename(targets_cop22 = targets) %>%
        mutate(
            age = update_age(ageasentered),
            fiscal_year = if_else(fiscal_year == 2023, 2022, 2023)) %>%
        # keep only targets for cop22
        filter(fiscal_year == 2023) %>%
        # group by all + new age var
        group_by(psnu, psnuuid, fiscal_year, indicator, standardizeddisaggregate,
            numeratordenom, age, modality,statushiv, otherdisaggregate) %>%
        # summarize by new age var
        summarize(
            targets_new_age = sum(targets_cop22))

    write_sheet(df_old_filt, "1OVxazs0BrH_Iw8xp9WB7N4-6unGD75vivpWwMyIColo",
                "PMTCT_COP22")

