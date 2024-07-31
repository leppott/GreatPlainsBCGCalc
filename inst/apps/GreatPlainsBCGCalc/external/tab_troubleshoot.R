# About Panel

function() {
  tabPanel("tabpan_troubleshoot"
           , includeHTML(file.path("www", "rmd_html", "ShinyHTML_Troubleshoot.html"))
           #  , htmlOutput("html_about") # use with iframe
  )##tabPanel ~ END
}##FUNCTION ~ END
