## PROJECT:  SI Support for DRC
## AUTHOR:   J. Hoehner | USAID
## LICENSE:  MIT
## PURPOSE:  DRC - Clinical Cascade Performance
## REF ID:   fb2bcd1f
## Date:     2022-11-03
## Update:   2022-11-03

# LIBRARIES --------------------------------------------------------------------

    library(tidyverse)
    library(cascade)
    library(gagglr)
    library(extrafont)
    library(glue)
    library(here)

# Global Variables -------------------------------------------------------------

    # reference id for this script
    # it is used in the filename of any output visuals so that we can find the
    # script used to produce it in our GitHub repository in the future
    ref_id <- "fb2bcd1f"

    # path to all MER data files
    data <- here("Data/MER/")

    # path to current OUxIM data file
    file_path <- return_latest(folderpath = data,
                               pattern = "Genie_OU_IM_Democratic_Republic_of_the_Congo_Frozen")

# Data -------------------------------------------------------------------------

    # get the data
    df_drc <- read_msd(file_path)

    # get information about the data for the final graph
    get_file_metadata(file_path)

# Choose a Cascade -------------------------------------------------------------

    plot_name

    # [1] "Standard"              # [2] "Standard Female"
    # [3] "Standard Male"         # [4] "Pediatric" # [5] "Pediatric Female"
    # [6] "Pediatric Male"        # [7] "AYP (15-24 years old)"
    # [8] "AYP Female"            # [9] "AYP Male"
    # [10] "Adults"               # [11] "Adults Female"
    # [12] "Adults Male"          # [13] "KP"

    # Return a cascade data frame (number corresponds to position in list)
    # 13 = KP cascade
    kp_cascade_data <- return_cascade(df_drc, 13)

# VIZ --------------------------------------------------------------------------

    # Plot the cascade
    # You will be prompted to enter a cascade number
    return_cascade_plot(df_drc, export = F)

    # Un-comment and enter the name of the plot you selected in between the ""
    # ex: plot_file_name = "KP_Cascade_FY22Q3"
    plot_file_name = "KP_Cascade_FY22Q3"

    # save the plot to the Images folder
    si_save(here(glue("Images/{plot_file_name}_{ref_id}.png")),
            height = 6,
            width = 13)
