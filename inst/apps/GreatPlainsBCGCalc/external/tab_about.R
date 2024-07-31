# About Panel

function() {
  mainPanel(
    tabsetPanel(type = "tabs"
                , tabPanel(title = "About"
                           ,includeHTML(file.path("www"
                                                  , "rmd_html"
                                                  , "ShinyHTML_About_1About.html"))
                )
                , tabPanel(title = "Study Area"
                           ,includeHTML(file.path("www"
                                                  , "rmd_html"
                                                  , "ShinyHTML_About_2StudyArea.html"))
                )
                , tabPanel(title = "Basic Information"
                           ,includeHTML(file.path("www"
                                                  , "rmd_html"
                                                  , "ShinyHTML_About_3BasicInfo.html"))
                )
                , tabPanel(title = "Funding"
                           ,includeHTML(file.path("www"
                                                  , "rmd_html"
                                                  , "ShinyHTML_About_4Funding.html"))
                )
    )## tabPanel ~ END
  )## mainPanel ~ END
}##FUNCTION ~ END
