# PROJECT:  sidr-congo
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  To generate SNU level cascade plots
# REF ID:   fab3899d
# LICENSE:  MIT
# DATE:     2022-11-10
# UPDATED:  2022-11-10

# DEPENDENCIES ------------------------------------------------------------

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

# GLOBAL VARIABLES --------------------------------------------------------

    ref_id <- "fab3899d"

    # SI specific paths/functions
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
                               pattern = "Genie_PSNU_IM_Democratic_Republic_of_the_Congo")

    # don't know what this is, need to ask TE
    # plhiv_path <- return_latest(folderpath = merdata,
    #                            pattern = "SUBNAT")

    # Grab metadata
    get_metadata(file_path)

# IMPORT ------------------------------------------------------------------

    df_msd <- read_msd(file_path) %>%
        filter(funding_agency == "USAID",
               fiscal_year == "2022")

    janitor::tabyl(df_msd$snu1)

# MUNGE -------------------------------------------------------------------

    # return standard cascade data
    return_cascade(df_msd, 1) %>% prinf()
    return_cascade(df_msd, 1) %>%
        prinf()

    # Generate all cascade plots for just USAID
    batch_cascade_plot(df_msd,
                       imgpath = "Images/cascade/usaid", imgtype =".png")

    # Return Standard Cascade Plots for each SNU

    # Haut Katanga

    return_cascade_plot(df_msd %>% filter(snu1 == "Haut Katanga"), export = F)

    # Un-comment and enter the name of the plot you selected in between " and the first _
    # ex: If you select plot 13, plot_file_name = glue("KP_Cascade_{metadata$curr_pd})
    plot_file_name = glue("cascade/Haut_Katanga/Haut_Katanga_Standard_Cascade_{metadata$curr_pd}")

    # need to be able to adjust the height and width
    si_save(glue("Images/{plot_file_name}_{ref_id}.png"),
            height = 9,
            width = 25)

    # Kinshasa

    return_cascade_plot(df_msd %>% filter(snu1 == "Kinshasa"), export = F)

    # Un-comment and enter the name of the plot you selected in between " and the first _
    # ex: If you select plot 13, plot_file_name = glue("KP_Cascade_{metadata$curr_pd})
    plot_file_name = glue("cascade/Kinshasa/Kinshasa_Standard_Cascade_{metadata$curr_pd}")

    # need to be able to adjust the height and width
    si_save(glue("Images/{plot_file_name}_{ref_id}.png"),
            height = 9,
            width = 20)

    # Lualaba

    return_cascade_plot(df_msd %>% filter(snu1 == "Lualaba"), export = F)

    # Un-comment and enter the name of the plot you selected in between " and the first _
    # ex: If you select plot 13, plot_file_name = glue("KP_Cascade_{metadata$curr_pd})
    plot_file_name = glue("cascade/Lualaba/Lualaba_Standard_Cascade_{metadata$curr_pd}")

    # need to be able to adjust the height and width
    si_save(glue("Images/{plot_file_name}_{ref_id}.png"),
            height = 9,
            width = 20)
# end