# Help Panel

function() {
  sidebarLayout(
    sidebarPanel(
      h2("Map Data")
      , p("This function will render points on a map.")
      , br()

      , h4("A. Upload a file.")
      , p("If no file name showing below repeat 'Import File' in the left sidebar.")
      , p(textOutput("fn_input_display_map"))

      , h4("B. Define Data Type")
      , uiOutput("UI_map_datatype")

      , h4("C. Define Column Names")
      , uiOutput("UI_map_col_xlong")
      , uiOutput("UI_map_col_ylat")
      , uiOutput("UI_map_col_sampid")
      # , uiOutput("UI_map_col_keep")

      , h4("D. Update Map")
      , p("After making changes above click the button below to update the map.")
      , bsButton("but_map_update", "Update Map")

      # , hr()
      # , includeHTML(file.path("www", "rmd_html", "ShinyHTML_Map.html"))
    )## sidebarPanel ~ END

    , mainPanel(
      leafletOutput("map_leaflet"
                    , height = "85vh")
    )## mainPanel ~ END
  )##sidebarLayout ~ END
}##FUNCTION ~ END
