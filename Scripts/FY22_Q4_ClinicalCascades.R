# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  To generate SNU level cascade plots
# REF ID:   fab3899d
# LICENSE:  MIT
# DATE:     2022-11-10
# UPDATED:  2022-11-10

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
    return_latest("Genie_PSNU_IM_Democratic_Republic_of_the_Congo")

get_metadata(path)

# PSNU
psnu_df <- si_path() %>%
    return_latest("Genie_PSNU_IM_Democratic_Republic_of_the_Congo") %>%
    read_msd()

genie_path <- path
curr_fy <- metadata$curr_fy
curr_pd <-metadata$curr_pd
data_source <- metadata$source

# IMPORT -----------------------------------------------------------------------

df_msd <- psnu_df %>%
  filter(
    funding_agency == "USAID",
    fiscal_year == "2022")

# MUNGE ------------------------------------------------------------------------

# return standard cascade (1) data as an example
std_casc_df <- return_cascade(df_msd, 1)

# Generate all cascade plots for country-wide data, most recent Q
batch_cascade_plot(df_msd,
  imgpath = "Images/cascade/usaid", imgtype = ".png")

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

# Haut Katanga
batch_cascade_plot(df_msd %>%
                       filter(snu1 == "Haut Katanga"),
                   imgpath = "Images/cascade/Haut_katanga/usaid", imgtype = ".png")

# Kinshasa
batch_cascade_plot(df_msd %>%
                       filter(snu1 == "Kinshasa"),
                   imgpath = "Images/cascade/Kinshasa/usaid", imgtype = ".png")


# Lualaba
batch_cascade_plot(df_msd %>%
                       filter(snu1 == "Lualaba"),
                   imgpath = "Images/cascade/Lualaba/usaid", imgtype = ".png")


# end