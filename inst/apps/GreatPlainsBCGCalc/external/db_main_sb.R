#Sidebar----

# tabs
# sourced in global.R
# ref in db_main_body.R
# menu in db_main_sb.R

#sb_main <- function(id) {
function(id) {
  dashboardSidebar(
    width = 275
    , HTML("&nbsp;&nbsp;<font size=5><b>Steps</b></font>")
    , sidebarMenu(id = id
      , menuItem(text = "About"
               , tabName = "tab_about"
               , icon = icon("house")
                )## menuItem ~ About ~ END
      , menuItem(text = "Instructions"
                 , tabName = "tab_instruct"
                 , icon = icon("person-chalkboard")
                 )## menuItem ~ Instructions ~ END
      , menuItem(text = "Import Files"
                 , tabName = "tab_import"
                 , icon = icon("file-arrow-up")
                 , startExpanded = TRUE
                 )## menuItem ~ Import ~ END
      , menuItem(text = "Prepare Data"
                 , icon = icon("toolbox")
                 , menuSubItem("Introduction"
                               , tabName = "tab_filebuilder_intro"
                               , icon = icon("info")
                 )
                 , menuSubItem("Within the App: File Builder"
                               , tabName = "tab_filebuilder_taxatrans"
                               , icon = icon("language")
                               )
                 , menuSubItem("Outside the App"
                               , tabName = "tab_filebuilder_outsideapp"
                               , icon = icon("language")
                               )
                 , menuSubItem("Merge Files"
                               , tabName = "tab_filebuilder_mergefiles"
                               , icon = icon("code-merge")
                               )
                 )## menuItem ~ File Builder
      , menuItem(text = "Calculation"
                 , icon = icon("gears")
                 , tabName = "tab_calc"
                 , menuSubItem("BCG Models"
                               , tabName = "tab_calc_bcg"
                               , icon = icon("award"))
                 )## menuItem ~ BCG
      , menuItem(text = "Map"
                 , tabName = "tab_map"
                 , icon = icon("map"))## menuItem ~ Map
      , menuItem(text = "References"
                 , tabName = "tab_resources"
                 , icon = icon("book")
                 )## menuItem ~ References ~ END
      , menuItem(text = "Troubleshooting"
                 , tabName = "tab_troubleshoot"
                 , icon = icon("question")
                 )## menuItem ~ Troubleshooting ~ END
    )## sidebarMenu ~ END
  )## dashboardSidebar ~ END
}## FUNCTION ~ END
