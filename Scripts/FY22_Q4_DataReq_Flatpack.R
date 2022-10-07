# PROJECT:  Learn to tameDP
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Generating a flatpack file from DRC data
# REF ID:   7ed99574 
# LICENSE:  MIT
# DATE:     2022-10-07
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(extrafont)
  library(glitr)
  library(glamr)
  library(gophr)
  library(tameDP)
  library(here)
  library(readxl)
  library(skimr)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "7ed99574"

# IMPORT ------------------------------------------------------------------
  
  filepath_dp <- list.files(here("Data/DataPacks/"), 
                            pattern = ".xlsx")
  filepath_fp <- <- list.files(here("Data/FlatPacks/"), 
                               pattern = ".xlsx")

# MUNGE -------------------------------------------------------------------
  
  # convert dp > fp via tameDP
  df_fp_mech <- tame_dp(filepath_dp, type = "PSNUxIM")
  
  # convert dp > fp via app
  df_fp_sgac <- read_xlsx(filepath_fp, 
                          guess_max = 64000)

  # compare the files
  combined_fp <- df_fp_sgac %>%
    left_join(df_fp_mech, 
              by = c(
                "ou" = "operatingunit", 
                "country_name" = "country", 
                "snu1", 
                "psnu", 
                "psnu_uid" = "psnuuid",
                "prioritization" = "snuprioritization", 
                "mechanism_code" = "mech_code", 
                "partner_desc" = "prime_partner_name",
                "funding_agency",
                "mechanism_desc" = "mech_name",               
                "fiscal_year",              
                "support_type" = "indicatortype",           
                "indicator",                
                "disagg_type" = "standardizeddisaggregate",
                "numerator_denominator" = "numeratordenom",         
                "age" = "ageasentered",           
                "sex",                      
                "hts_modality" = "modality",                
                "resultstatus_inclusive" = "statushiv",                
                "resultstatus_specific" = "otherdisaggregate",       
                "target_value" = "targets"))