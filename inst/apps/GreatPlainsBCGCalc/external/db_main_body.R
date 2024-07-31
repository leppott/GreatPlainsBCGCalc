# Main

# tabs
# sourced in global.R
# ref in db_main_body.R
# menu in db_main_sb.R

function(id) {

    tabItems(
      tabItem(tabName = "tab_about"
              , tab_code_about())
      , tabItem(tabName = "tab_instruct"
                , tab_code_instruct())
      , tabItem(tabName = "tab_import"
                , tab_code_import())
      , tabItem(tabName = "tab_filebuilder"
                , tab_code_filebuilder())
      , tabItem(tabName = "tab_filebuilder_intro"
                , tab_code_filebuilder_intro())
      , tabItem(tabName = "tab_filebuilder_taxatrans"
                , tab_code_filebuilder_taxatrans())
      , tabItem(tabName = "tab_filebuilder_outsideapp"
                , tab_code_filebuilder_outsideapp())
      , tabItem(tabName = "tab_filebuilder_mergefiles"
                , tab_code_filebuilder_mergefiles())
      , tabItem(tabName = "tab_calc_bcg"
                , tab_code_calc_bcg())
      , tabItem(tabName = "tab_map"
                , tab_code_map())
      , tabItem(tabName = "tab_resources"
                , tab_code_resources())
      , tabItem(tabName = "tab_troubleshoot"
                , tab_code_troubleshoot())
    )## tabItems
}## FUNCTION ~ END
