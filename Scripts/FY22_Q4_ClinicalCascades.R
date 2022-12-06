# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  To generate SNU level cascade plots
# REF ID:   fab3899d
# LICENSE:  MIT
# DATE:     2022-11-10
# UPDATED:  2022-12-01

# DEPENDENCIES -----------------------------------------------------------------

# Libraries
library(gagglr)
library(tidyverse)
library(scales)
library(sf)
library(glue)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(selfdestructin5)
library(gt)
library(cascade)
library(ggpattern)

# GLOBAL VARIABLES -------------------------------------------------------------

ref_id <- "fab3899d"

# SI specific paths/functions

# metadata
path <- si_path() %>%
    return_latest("MER_Structured_Datasets_OU_IM_FY20-23_20221114_v1_1")

ou_df <- read_msd(path)

get_metadata(path)

# PSNU
psnu_df <- si_path() %>%
    return_latest("Genie_PSNU_IM_Democratic_Republic_of_the_Congo_Frozen_149c4e61-c0d2-4668-b44a-6a593e40a538") %>%
    read_msd()

genie_path <- path
curr_fy <- metadata$curr_fy
curr_pd <-metadata$curr_pd
data_source <- metadata$source

# IMPORT -----------------------------------------------------------------------
drc_df <- ou_df %>%
    filter(
        country == "Democratic Republic of the Congo",
        fiscal_year == "2022")

drc_usaid_df <- ou_df %>%
    filter(
        country == "Democratic Republic of the Congo",
        funding_agency == "USAID",
        fiscal_year == "2022")

drc_psnu_df <- psnu_df %>%
    filter(fiscal_year == "2022")

drc_usaid_psnu_df <- psnu_df %>%
    filter(
        funding_agency == "USAID",
        fiscal_year == "2022")

# MUNGE ------------------------------------------------------------------------

# Generate all cascade plots for country-wide data
batch_cascade_plot(drc_df,
  imgpath = "Images/cascade/all", imgtype = ".png")

# Generate all cascade plots for usaid specific data
batch_cascade_plot(drc_usaid_df,
                   imgpath = "Images/cascade/usaid", imgtype = ".png")

# # export plots individually if sizing looks funky
# return_cascade_plot(drc_usaid_df)
# #
# si_save(paste0("kp_cascade.png"),
#          path = "Images/cascade/usaid",
#          scale = 1.6)

# Return Standard Cascade Plots for each SNU,
# Note: these are all the plots you can choose from
# Please enter the cascade you would like to create.
# 1:Standard
# 2:Standard Female
# 3:Standard Male
# 4:Pediatric
# 5:Pediatric Female
# 6:Pediatric Male
# 7:AYP (15-24 years old)
# 8:AYP Female
# 9:AYP Male
# 10:Adults
# 11:Adults Female
# 12:Adults Male
# 13:KP

# All agencies

# Haut Katanga
batch_cascade_plot(drc_psnu_df %>%
                       filter(snu1 == "Haut Katanga"),
                   imgpath = "Images/cascade/Haut_Katanga", imgtype = ".png")

# export plots individually if sizing looks funky
return_cascade_plot(drc_psnu_df %>%
                        filter(snu1 == "Haut Katanga"))

si_save(paste0("hk_std_cascade.png"),
        path = "Images/cascade/Haut_Katanga",
        scale = 1.6)

# Kinshasa
batch_cascade_plot(drc_psnu_df %>%
                       filter(snu1 == "Kinshasa"),
                   imgpath = "Images/cascade/Kinshasa", imgtype = ".png")


# Lualaba
batch_cascade_plot(drc_psnu_df %>%
                       filter(snu1 == "Lualaba"),
                   imgpath = "Images/cascade/Lualaba", imgtype = ".png")


# USAID specific

# Haut Katanga
batch_cascade_plot(drc_usaid_psnu_df  %>%
                       filter(snu1 == "Haut Katanga"),
                   imgpath = "Images/cascade/Haut_Katanga/usaid", imgtype = ".png")

# Kinshasa
batch_cascade_plot(drc_usaid_psnu_df  %>%
                       filter(snu1 == "Kinshasa"),
                   imgpath = "Images/cascade/Kinshasa/usaid", imgtype = ".png")


# Lualaba
batch_cascade_plot(drc_usaid_psnu_df  %>%
                       filter(snu1 == "Lualaba"),
                   imgpath = "Images/cascade/Lualaba/usaid", imgtype = ".png")


# end