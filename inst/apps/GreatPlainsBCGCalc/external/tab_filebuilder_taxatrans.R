# File Builder Panel, taxa translate

function() {
  sidebarLayout(
    sidebarPanel(h2("File Builder: Taxa Translate and Attribute Assignment")
            , useShinyjs()

            , p("Only columns specified below will be carried through into the output file.")
            #, br()
            , h4("A. Upload a File")
            , p("If no file name showing below repeat 'Import File' in the left sidebar.")
            , p(textOutput("fn_input_display_taxatrans"))

            , h4("B. Select Calculation.")
            , uiOutput("UI_taxatrans_pick_official")

            , h4("C. User File Column Names")

            , h6("Required Fields")
            , p("If the default values are present they will be auto-populated.")
            # SampleID (really for group_by)
            , uiOutput("UI_taxatrans_user_col_sampid")
            , uiOutput("UI_taxatrans_user_col_taxaid")
            # N_Taxa (really for group_by)
            , uiOutput("UI_taxatrans_user_col_n_taxa")
            , uiOutput("UI_taxatrans_user_col_indexclass")

            , h6("Additional Fields")
            , p("There is one additional required field (BugGear) for Iowa bug samples.
                Optional fields (e.g., SiteID, collection date) can be selected as well.
                IMPORTANT! Do not include Life Stage or other fields that might
                cause a taxon to occur in more than one row for a given sample
                (which could lead to double-counting of that taxon in the richness metrics).")
            , uiOutput("UI_taxatrans_user_col_groupby")

            , h4("D. Run Operation")
            , p("This button will merge the user file with the official taxa file")
            , shinyjs::disabled(shinyBS::bsButton("b_calc_taxatrans"
                                                  , label = "Run Operation"))

            , h4("E. Download Output")
            , p("All input and output files will be available in a single zip file.")
            , shinyjs::disabled(downloadButton("b_download_taxatrans"
                                               , "Download Results"))

    )## sidebarPanel ~ END
       , mainPanel(
            tabPanel(title = "TaxaTrans_About"
                     ,includeHTML(file.path("www", "rmd_html"
                                            , "ShinyHTML_FB_TaxaTrans_1About.html"))
                     )## tabPanel ~ END
    )## mainPanel ~ END
  )##sidebarLayout ~ END


}##FUNCTION ~ END


# , mainPanel(
#   tabsetPanel(type = "tabs"
#               , tabPanel(title = "TaxaTrans_About"
#                          ,includeHTML(file.path("www"
#                                                 , "rmd_html"
#                                                 , "ShinyHTML_FB_TaxaTrans_1About.html"))
#               )
#               , tabPanel(title = "TaxaTrans_Output"
#                          ,includeHTML(file.path("www"
#                                                 , "rmd_html"
#                                                 , "ShinyHTML_FB_TaxaTrans_2Output.html"))
#               )
#   )## tabsetPanel ~ END
# )## mainPanel ~ END
