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

merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(
  folderpath = merdata,
  pattern = "Genie_SITE_IM_Democratic_Republic_of_the_Congo")

# Grab metadata
get_metadata(file_path)

# IMPORT -----------------------------------------------------------------------

df_msd <- read_msd(file_path) %>%
  filter(
    fiscal_year == "2022")

# MUNGE ------------------------------------------------------------------------

# return standard cascade (1) data as an example
std_casc_df <- return_cascade(df_msd, 1)

# Generate all cascade plots for country-wide data, most recent Q
batch_cascade_plot(df_msd,
  imgpath = "Images/cascade/all", imgtype = ".png")

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
return_cascade_plot(df_msd %>%
                    filter(snu1 == "Haut Katanga"),
                    export = F)

# Un-comment and enter the name of the plot you selected in between " and the first _
# ex: If you select plot 13, plot_file_name = glue("KP_Cascade_{metadata$curr_pd})
plot_file_name_HK <- glue("cascade/Haut_Katanga/Haut_Katanga_Standard_Cascade_{metadata$curr_pd}")
# plot_file_name_HK <- glue("cascade/Haut_Katanga/Haut_Katanga_KeyPopulations_{metadata$curr_pd}")

# need to be able to adjust the height and width
si_save(glue("Images/{plot_file_name_HK}_{ref_id}.png"),
        scale = 1.2)

# Kinshasa

return_cascade_plot(df_msd %>%
                    filter(snu1 == "Kinshasa"),
                    export = F)

# Un-comment and enter the name of the plot you selected in between " and the first _
# ex: If you select plot 13, plot_file_name = glue("KP_Cascade_{metadata$curr_pd})
plot_file_name_K <- glue("cascade/Kinshasa/Kinshasa_Standard_Cascade_{metadata$curr_pd}")
# plot_file_name_K <- glue("cascade/Kinshasa/Kinshasa_KeyPopulations_{metadata$curr_pd}")

# need to be able to adjust the height and width
si_save(glue("Images/{plot_file_name_K}_{ref_id}.png"),
        scale = .9)

# Lualaba

return_cascade_plot(df_msd %>%
                    filter(snu1 == "Lualaba"),
                    export = F)

# Un-comment and enter the name of the plot you selected in between " and the first _
# ex: If you select plot 13, plot_file_name = glue("KP_Cascade_{metadata$curr_pd})
plot_file_name_L <- glue("cascade/Lualaba/Lualaba_Standard_Cascade_{metadata$curr_pd}")
# plot_file_name_L <- glue("cascade/Lualaba/Lualaba_KeyPopulations_{metadata$curr_pd}")


# need to be able to adjust the height and width
si_save(glue("Images/{plot_file_name_L}_{ref_id}.png"),
        scale = 1.3)
# end