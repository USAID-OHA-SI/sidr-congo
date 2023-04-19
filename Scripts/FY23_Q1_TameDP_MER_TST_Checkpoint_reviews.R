# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  exploring the draft TST
# REF ID:   9316532e
# LICENSE:  MIT
# DATE:     2023-03-27
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

library(tidyverse)
library(tameDP)

# for checking against MSD
library(gagglr)
library(readxl)

# export to google sheets
library(googlesheets4)
library(gt)

# GLOBAL VARIABLES ------------------------------------------------------------

ref_id <- "9316532e"
load_secrets()

tst_path <- here::here("Data/TST/")

psnu_path <- "Genie_PSNU_IM_Democratic_Republic_of_the_Congo"

# update age bands

age_xwalk_path <- here::here("Data/TST")

# IMPORT ----------------------------------------------------------------------

tst_df <- tst_path %>%
    return_latest("Target Setting Tool") %>%
    tame_dp()

msd_df <- si_path() %>%
    return_latest(psnu_path) %>%
    read_psd()

get_metadata(type = "PSNU_IM")
metadata_msd_psnu <- metadata

age_map <- age_xwalk_path %>%
    return_latest("age_mapping.xlsx") %>%
    readxl::read_xlsx()

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
        age = ageasentered)

# psnu level MSD to match
msd_psnu <- msd_df %>%
    clean_indicator() %>%
    resolve_knownissues() %>%
    left_join(age_map, by = c("indicator", "ageasentered" = "age_msd")) %>%
    mutate(age = ifelse(is.na(age_dp), ageasentered, age_dp)) %>%
    select(-c(operatingunit, ageasentered, age_dp)) %>%
    group_by(
        country,psnu,fiscal_year,indicator, age, sex,
        standardizeddisaggregate, snuprioritization) %>%
    # group_by(across(-c(cumulative, targets))) %>%
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
    filter(psnu %in% tst_psnu$psnu,
           indicator %in% tst_psnu$indicator,
           snuprioritization %in% tst_psnu$snuprioritization,
           standardizeddisaggregate %in% tst_psnu$standardizeddisaggregate,
           fiscal_year %in% tst_psnu$fiscal_year) %>%
    rename(
        cumulative_msd = cumulative,
        targets_msd = targets)

# combine + compare

combined <- full_join(tst_psnu, msd_psnu, by = c("country","psnu","fiscal_year",
                                                 "indicator","snuprioritization",
                                                 "standardizeddisaggregate",
                                                 "age", "sex")) %>%
    replace_na()

write_sheet(combined, "10CLNhl5kMatgEe6wCAt0fKFwtCPGw5BEOfQ_e_vAoLE",
            "Checkpoint 1")

#matches
comb_matches_tgts <- combined %>%
    filter(
        targets_tst == targets_msd)

write_sheet(comb_matches_tgts, "1zCtWP7vNC4Nc18tLRkKIACqtLJ4o9DG6AuqKaruiNgw",
            "Checkpoint 1")

#matches
comb_matches_cml <- combined %>%
    filter(
        cumulative_msd == cumulative_tst)

write_sheet(comb_matches_cml, "1hwdNNQ-zsVnDpiIZ1won1plIKeJ9OaBvds-Sli--yqc",
            "Checkpoint 1")

#mismatches
comb_mismatches_tgts <- combined %>%
    filter(
        targets_tst != targets_msd)

write_sheet(comb_mismatches_tgts, "1U2Vzfbhun5ZMHz5B48nB336lFpaW5kAhVwk3CrdrMUs",
            "Checkpoint 1")

#mismatches
comb_mismatches_cml <- combined %>%
    filter(
        cumulative_msd != cumulative_tst)

write_sheet(comb_mismatches_cml, "1Gbm5KQ_TaxrrpjXdjvrtUJKe6Oq9SkNWrYjQKJM0yHw",
            "Checkpoint 1")

# summary table of targets by year

# indicator targets by year

all_inds <- tst_df %>%
    group_by(
        fiscal_year,indicator) %>%
    summarize(
        targets = scales::comma(sum(targets)),
        cumulative = scales::comma(sum(cumulative))) %>%
    mutate(
        targets = replace_na(as.character(targets), "-"),
        cumulative = replace_na(as.character(cumulative), "-")) %>%
    gt::gt() %>%
    # add custom title
    tab_header(
        title = "Democratic Republic of the Congo: Targets and Results") %>%
    tab_footnote(glue::glue("Source: Democratic Republic of the Congo Target Setting Tool ({today()}) | USAID SI Analytics")) %>%
    # relabel columns nicely
    cols_label(
        indicator = "Indicator",
        targets = "Targets",
        cumulative = "Results")
