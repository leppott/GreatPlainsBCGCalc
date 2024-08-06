#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# nolint start
library(shiny)
# nolint end

# Define server logic
shinyServer(function(input, output) {

  # INPUT Display Names ####

  output$fn_input_display_bcg <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_bcg

  output$fn_input_display_taxatrans <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_taxatrans

  # ~~~~IMPORT~~~~----
  # IMPORT ----
  file_watch <- reactive({
    # trigger for df_import()
    input$fn_input
  })## file_watch

  ## IMPORT, df_import ####
  df_import <- eventReactive(file_watch(), {
    # use a multi-item reactive so keep on a single line (if needed later)

    # input$df_import will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$fn_input

    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END

    sep_user <- input$sep

    # Define file
    fn_inFile <- inFile$datapath

    #message(getwd())
    message(paste0("Import, separator: '", input$sep,"'"))
    message(paste0("Import, file name: ", input$fn_input$name))

    # # Add "Results" folder if missing
    # boo_Results <- dir.exists(file.path(".", "results"))
    # if(boo_Results==FALSE){
    #   dir.create(file.path(".", "Results"))
    # }

    # Remove existing files in "results"
    clean_results()

    ### Mod, BCG_ATTR----
    # Read user imported file
    # Add extra colClasses parameter for BCG_Attr
    # the "i" values default to complex numbers
    # many permutations of BCG_Attr so check for it first then import

    df_header <- read.delim(fn_inFile
                            , header = TRUE
                            , sep = sep_user
                            , stringsAsFactors = FALSE
                            , na.strings = c("", "NA")
                            , nrows = 0)
    col_num_bcgattr <- grep("BCG_ATTR", toupper(names(df_header)))
    classes_df <- sapply(df_header, class)
    col_name_bcgattr <- names(df_header)[col_num_bcgattr]

    # 2023-12-04, modify for MN with multiple BCG_ATTR fields

    if (length(col_num_bcgattr) == 0) {
      # BCG_Attr present = FALSE
      # define classes = FALSE
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
    } else if (!"complex" %in%  as.vector(classes_df[col_num_bcgattr])) {
      # BCG_Attr present = TRUE
      # BCG_Attr Class is complex = FALSE
      # define classes on import = FALSE (change to text after import)
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
      #df_input[, col_num_bcgattr] <- as.character(df_input[, col_num_bcgattr])
      # doesn't work for multiple columns
      # df_input[, col_num_bcgattr] <- lapply(df_input[, col_num_bcgattr], as.character)
      # error for some files
      # use a loop :(
      for (b in col_num_bcgattr) {
        df_input[, b] <- as.character(df_input[, b])
      }## FOR ~ b
      #
    } else {
      # BCG_Attr present = TRUE
      # BCG_Attr Class is complex = TRUE
      # define classes on import = TRUE
      #classes_df <- sapply(df_header, class)
      classes_df[col_num_bcgattr] <- "character"
      df_input <- read.table(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA")
                             #, colClasses = c(col_name_bcgattr = "character"))
                             # , colClasses = classes_df)
                             , colClasses = classes_df[col_name_bcgattr])
    }## IF ~ col_num_bcgattr

    # Copy user files to results sub-folder
    copy_import_file(import_file = input$fn_input)

    ## button, enable, calc ----
    shinyjs::enable("b_calc_taxatrans")
    shinyjs::enable("b_calc_bcg")

    return(df_input)

  })##output$df_import ~ END

  ## IMPORT, df_import_DT ----
  output$df_import_DT <- DT::renderDT({
    df_data <- df_import()
  }##expression~END
  , filter = "top"
  , caption = "Table. Imported data."
  , options = list(scrollX = TRUE
                   , pageLength = 5
                   , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                   , autoWidth = TRUE)
  )##df_import_DT~END

  ## IMPORT, col names ----
  col_import <- eventReactive(file_watch(), {

    inFile <- input$fn_input

    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END

    # temp df
    df_temp <- df_import()
    # Column Names
    input_colnames <- names(df_temp)
    #
    return(input_colnames)

  })## col_import

  # ~~~~FILE BUILDER~~~~ ----
  # FB, TAXATRANS ----
  ## TaxaTrans, UI ----

  output$UI_taxatrans_pick_official <- renderUI({
    str_col <- "Calculation"
    selectInput("taxatrans_pick_official"
                , label = str_col
                , choices = c("", df_pick_taxoff[, "project"])
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_taxaid <- renderUI({
    str_col <- "Column, TaxaID (Scientific Name)"
    selectInput("taxatrans_user_col_taxaid"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "TaxaID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_drop <- renderUI({
    str_col <- "Columns to Drop"
    selectInput("taxatrans_user_col_drop"
                , label = str_col
                , choices = c("", names(df_import()))
                , multiple = TRUE)
  })## UI_colnames

  output$UI_taxatrans_user_col_n_taxa <- renderUI({
    str_col <- "Column, Taxa Count (number of individuals or N_Taxa)"
    selectInput("taxatrans_user_col_n_taxa"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "N_Taxa"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_groupby <- renderUI({
    str_col <- "Columns to Keep in Output"
    selectInput("taxatrans_user_col_groupby"
                , label = str_col
                , choices = c("", names(df_import()))
                , multiple = TRUE)
  })## UI_colnames

  output$UI_taxatrans_user_col_sampid <- renderUI({
    str_col <- "Column, Unique Sample Identifier (e.g., SampleID)"
    selectInput("taxatrans_user_col_sampid"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "SampleID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_indexclass <- renderUI({
    # str_col <- "BCG (Bugs and Fish): Column, Index Class (e.g., Index_Class)"
    str_col <- "Column, Index Class (e.g., Index_Class)"
    selectInput("taxatrans_user_col_indexclass"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "Index_Class"
                , multiple = FALSE)
  })## UI_colnames

  ## b_Calc_TaxaTrans ----
  observeEvent(input$b_calc_taxatrans, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 00, Initialize ----
      prog_detail <- "Calculation, Taxa Translator..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 6
      prog_sleep <- 0.25

      ## Calc, 01, Import User Data ----
      prog_detail <- "Import Data, User"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = input$fn_input)

      # result folder and files
      fn_abr <- abr_taxatrans
      fn_abr_save <- paste0("_", fn_abr, "_")

      # Add "reference" folder if missing
      path_results_ref <- file.path(path_results, dn_files_ref)
      boo_Results <- dir.exists(file.path(path_results_ref))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_ref))
      }
      # Add "Results" folder based on user selection later in this step

      # button, disable, download
      shinyjs::disable("b_download_taxatrans")

      # Import data
      # data
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      message(paste0("Import, file name, base: ", fn_input_base))
      df_input <- read.delim(inFile$datapath
                             , header = TRUE
                             , sep = input$sep
                             , stringsAsFactors = FALSE)
      # QC, FAIL if TRUE
      if (is.null(df_input)) {
        return(NULL)
      }

      ## Calc, 02, Gather and Test Inputs  ----
      prog_detail <- "QC Inputs"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Fun Param, Define
      sel_proj <- input$taxatrans_pick_official
      sel_user_taxaid <- input$taxatrans_user_col_taxaid
      #sel_col_drop <- unlist(input$taxatrans_user_col_drop)
      sel_user_ntaxa <- input$taxatrans_user_col_n_taxa
      sel_user_groupby <- unlist(input$taxatrans_user_col_groupby)
      sel_summ <- input$cb_TaxaTrans_Summ
      sel_user_indexclass <- input$taxatrans_user_col_indexclass

      fn_taxoff <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                  , "filename"]
      fn_taxoff_meta <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "metadata_filename"]
      col_taxaid_official_match <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                                  , "taxaid"]
      col_taxaid_official_project <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                                    , "calc_taxaid"]

      fn_taxoff_attr <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "attributes_filename"]
      fn_taxoff_attr_meta <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "attributes_metadata_filename"]
      col_taxaid_attr <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                        , "attributes_taxaid"]
      sel_user_sampid <- input$taxatrans_user_col_sampid

      sel_taxaid_drop <-  df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                     , "taxaid_drop"]
      dir_proj_results <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                         , "dir_results"]

      # include = yes; unique(sel_user_groupby)
      # include sampid, taxaid, and n_taxa so not dropped
      user_col_keep <- names(df_input)[names(df_input) %in% c(sel_user_groupby
                                                              , sel_user_sampid
                                                              , sel_user_taxaid
                                                              , sel_user_ntaxa
                                                              , sel_user_indexclass)]
      # flip to col_drop
      user_col_drop <- names(df_input)[!names(df_input) %in% user_col_keep]

      # Fun Param, Test

      if (sel_proj == "") {
        # end process with pop up
        msg <- "'Calculation' is missing!"
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        # validate(msg)
      }## IF ~ sel_proj

      if (is.na(fn_taxoff_meta) | fn_taxoff_meta == "") {
        # set value to NULL
        df_official_metadata <- NULL
      }## IF ~ fn_taxaoff_meta

      if (is.na(sel_user_ntaxa) | sel_user_ntaxa == "") {
        sel_user_ntaxa <- NULL
      }## IF ~ fn_taxaoff_meta

      if (is.null(sel_summ)) {
        sel_summ <- FALSE
      }## IF ~ sel_summ

      if (sel_taxaid_drop == "NULL") {
        sel_taxaid_drop <- NULL
      }## IF ~ sel_taxaid_drop

      if (is.null(sel_user_indexclass)) {
        sel_user_indexclass <- NULL
      } else if(is.na(sel_user_indexclass)) {
        sel_user_indexclass <- NULL
      } else if (sel_user_indexclass == "") {
        sel_user_indexclass <- NULL
      }## IF ~ sel_user_indexclass

      # if (is.na(sel_user_indexclass) | sel_user_indexclass == "") {
      #   sel_user_indexclass <- NULL
      # }## IF ~ sel_user_indexclass

      message(paste0("User response to summarize duplicate sample taxa = "
               , sel_summ))

      # Different result subfolder based on project (bugs/fish)
      # 2024-01-12
      if (sel_proj == "Great Plains BCG (Fish)") {
        dir_proj_results <- paste("Fish", dir_proj_results, sep = "_")
      } else if (sel_proj == "Great Plains BCG (IA Bugs)") {
        dir_proj_results <- paste("Bugs_IA", dir_proj_results, sep = "_")
      } else if (sel_proj == "Great Plains BCG (KS Bugs)") {
        dir_proj_results <- paste("Bugs_KS", dir_proj_results, sep = "_")
      } else if (sel_proj == "Great Plains BCG (MO Bugs)") {
        dir_proj_results <- paste("Bugs_MO", dir_proj_results, sep = "_")
      } else if (sel_proj == "Great Plains BCG (NE Bugs)") {
        dir_proj_results <- paste("Bugs_NE", dir_proj_results, sep = "_")
      } ## IF ~ sel_proj

      dn_files <- paste(abr_results, dir_proj_results, sep = "_")

      # Add "Results" folder if missing
      path_results_sub <- file.path(path_results, dn_files)
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      # QC index class
      if (is.null(sel_user_indexclass)) {
        # end process with pop up
        msg <- "'Index_Class' column name is required for BCG and is missing!"
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ sel_user_indexclass

      ## Calc, 03, Import Official Data (and Metadata)  ----
      prog_detail <- "Import Data, Official and Metadata"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      ## Data,  Official Taxa----
      url_taxoff <- file.path(url_bmt_base
                              , "taxa_official"
                              , "GP"
                              , fn_taxoff)
      temp_taxoff <- tempfile(fileext = ".csv")
      httr::GET(url_taxoff, write_disk(temp_taxoff))

      df_taxoff <- read.csv(temp_taxoff)

      ## Data, Official Taxa, Meta Data----
      if (!is.null(fn_taxoff_meta)) {
        url_taxoff_meta <- file.path(url_bmt_base
                                     , "taxa_official"
                                     , "GP"
                                     , fn_taxoff_meta)
        temp_taxoff_meta <- tempfile(fileext = ".csv")
        httr::GET(url_taxoff_meta, write_disk(temp_taxoff_meta))

        df_taxoff_meta <- read.csv(temp_taxoff_meta)
      }## IF ~ fn_taxaoff_meta

      ## Data, Official Attributes----
      if (!is.null(fn_taxoff_attr)) {
        url_taxoff_attr <- file.path(url_bmt_base
                                     , "taxa_official"
                                     , "GP"
                                     , fn_taxoff_attr)
        temp_taxoff_attr <- tempfile(fileext = ".csv")
        httr::GET(url_taxoff_attr, write_disk(temp_taxoff_attr))

        df_taxoff_attr <- read.csv(temp_taxoff_attr)
      }## IF ~ fn_taxoff_attr

      ## Data, Official Attributes, Meta Data----
      if (!is.null(fn_taxoff_meta)) {
        url_taxoff_attr_meta <- file.path(url_bmt_base
                                     , "taxa_official"
                                     , "GP"
                                     , fn_taxoff_attr_meta)
        temp_taxoff_attr_meta <- tempfile(fileext = ".csv")
        httr::GET(url_taxoff_attr_meta, write_disk(temp_taxoff_attr_meta))

        df_taxoff_attr_meta <- read.csv(temp_taxoff_attr_meta)
      }## IF ~ fn_taxaoff_meta


      ## Calc, 03, Run Function ----
      prog_detail <- "Calculate, Taxa Trans"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # function parameters
      df_user                 <- df_input
      df_official             <- df_taxoff
      df_official_metadata    <- df_taxoff_meta
      taxaid_user             <- sel_user_taxaid
      taxaid_official_match   <- col_taxaid_official_match
      taxaid_official_project <- col_taxaid_official_project
      taxaid_drop             <- sel_taxaid_drop
      col_drop                <- user_col_drop #NULL #sel_col_drop
      sum_n_taxa_boo          <- TRUE
      sum_n_taxa_col          <- sel_user_ntaxa
      sum_n_taxa_group_by     <- c(sel_user_sampid
                                   , sel_user_taxaid
                                   , sel_user_groupby)

      ## run the function ----
      taxatrans_results <- BioMonTools::taxa_translate(df_user
                                                       , df_official
                                                       , df_official_metadata
                                                       , taxaid_user
                                                       , taxaid_official_match
                                                       , taxaid_official_project
                                                       , taxaid_drop
                                                       , col_drop
                                                       , sum_n_taxa_boo
                                                       , sum_n_taxa_col
                                                       , sum_n_taxa_group_by
                                                       , trim_ws = TRUE
                                                       , match_caps = TRUE)

      ## Munge ----

      # Remove non-project taxaID cols
      # Specific to shiny project, not a part of the taxa_translate function
      # col_keep <- !names(taxatrans_results$merge) %in% col_drop_project
      # taxatrans_results$merge <- taxatrans_results$merge[, col_keep]

      # Attributes if have 2nd file
      if (!is.na(fn_taxoff_attr)) {

        df_ttrm <- taxatrans_results$merge
        # drop translation file columns
        col_keep_ttrm <- names(df_ttrm)[names(df_ttrm) %in% c(sel_user_sampid
                                                            , sel_user_taxaid
                                                            , sel_user_ntaxa
                                                            , "Match_Official"
                                                            , sel_user_groupby
                                                            , sel_user_indexclass)]
        df_ttrm <- df_ttrm[, col_keep_ttrm]

        # merge with attributes
        df_merge_attr <- merge(df_ttrm
                               , df_taxoff_attr
                               , by.x = taxaid_user
                               , by.y = col_taxaid_attr
                               , all.x = TRUE
                               , sort = FALSE
                               , suffixes = c("_xDROP", "_yKEEP"))
        # Drop duplicate names from Trans file (x)
        col_keep <- names(df_merge_attr)[!grepl("_xDROP$"
                                                , names(df_merge_attr))]
        df_merge_attr <- df_merge_attr[, col_keep]
        # KEEP and rename duplicate names from Attribute file (y)
        names(df_merge_attr) <- gsub("_yKEEP$", "", names(df_merge_attr))
        # Save back to results list
        taxatrans_results$merge <- df_merge_attr

        # QC check
        # testthat::expect_equal(nrow(df_merge_attr), nrow(df_ttrm))
        # testthat::expect_equal(sum(df_merge_attr[, sel_user_ntaxa], na.rm = TRUE)
        #                        , sum(df_ttrm[, sel_user_ntaxa], na.rm = TRUE))
      }## IF ~ !is.na(fn_taxoff_attr)

      # Reorder by SampID and TaxaID
      taxatrans_results$merge <- taxatrans_results$merge[
           order(taxatrans_results$merge[, sel_user_sampid]
                   , taxatrans_results$merge[, sel_user_taxaid]), ]

      # Add input filenames
      taxatrans_results$merge[, "file_taxatrans"] <- fn_taxoff
      taxatrans_results$merge[, "file_attributes"] <- fn_taxoff_attr


      # Resort columns
      col_start <- c(sel_user_sampid
                     , sel_user_taxaid
                     , sel_user_ntaxa
                     , "file_taxatrans"
                     , "file_attributes")
      col_other <- names(taxatrans_results$merge)[!names(taxatrans_results$merge)
                                                  %in% col_start]
      taxatrans_results$merge <- taxatrans_results$merge[, c(col_start
                                                             , col_other)]

      # Convert required file names to standard
      ## do at end so don't have to modify any other variables
      boo_req_names <- TRUE
      if (boo_req_names == TRUE) {
        names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                       %in% sel_user_sampid] <- "SampleID"
        names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                       %in% sel_user_taxaid] <- "TaxaID"
        names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                       %in% sel_user_ntaxa] <- "N_Taxa"
      }## IF ~ boo_req_names

      # Hack/Fix
      # Noteworthy NA causing issue later in Shiny app
      # 20231201, only if have Noteworthy
      if ("NOTEWORTHY" %in% toupper(taxatrans_results$merge)) {
        taxatrans_results$merge$Noteworthy <- ifelse(is.na(taxatrans_results$merge$Noteworthy)
                                                     , FALSE
                                                     , TRUE)
      }## IF ~ Noteworthy

      # need index class brought through


      ## Calc, 04, Save Results ----
      prog_detail <- "Save Results"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Save files

      ## File version names
      df_save <- data.frame(Calculation = sel_proj
                            , OperationalTaxonomicUnit = col_taxaid_official_project
                            , TranslationTable = fn_taxoff
                            , AttributeTable = fn_taxoff_attr)
      # fn_part <- paste0("_", abr_filebuilder, "_0taxasource", ".csv")
      fn_part <- "BCG_TaxaTranslator_source.csv"
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Taxa User
      # saved when imported

      # 2023-11-03, save original filenames
      # add taxatrans metadata

      ## Taxa Official
      file.copy(temp_taxoff
                , file.path(path_results_ref, fn_taxoff))

      ## Taxa Official, meta data
      file.copy(temp_taxoff_meta
                , file.path(path_results_ref, fn_taxoff_meta))

      ## Taxa Official, Attributes
      file.copy(temp_taxoff_attr
                , file.path(path_results_ref, fn_taxoff_attr))

      ## Taxa Official, Attributes, meta data
      file.copy(temp_taxoff_attr_meta
                , file.path(path_results_ref, fn_taxoff_attr_meta))

      ## translate - crosswalk
      df_save <- taxatrans_results$taxatrans_unique # df_taxoff_meta
      # fn_part <- paste0(fn_abr_save, "2taxamatch", ".csv")
      fn_part <- "BCG_TaxaTranslator_modify.csv"
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Non Match
      df_save <- data.frame(taxatrans_results$nonmatch)
      # fn_part <- paste0(fn_abr_save, "3nonmatch", ".csv")
      fn_part <- "BCG_TaxaTranslator_nonmatch.csv"
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Taxa Trans
      df_save <- taxatrans_results$merge
      # fn_part <- paste0(fn_abr_save, "4taxaattr", ".csv")
      fn_part <- "BCG_TaxaTranslator_TAXAATTR.csv"
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Calc, 05, Create Zip ----
      prog_detail <- "Create Zip File For Download"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Create zip file for download
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)


      ## Calc, 06, Clean Up ----
      prog_detail <- "Calculate, Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # button, enable, download
      shinyjs::enable("b_download_taxatrans")

      # time, end
      toc <- Sys.time()
      duration <- difftime(toc, tic)

      # pop up
      # Inform user about number of taxa mismatches
      ## calc number of mismatch
      df_mismatch <- data.frame(taxatrans_results$nonmatch)
      n_taxa_mismatch <- nrow(df_mismatch)
      msg <- paste0("Number of mismatch taxa = ", n_taxa_mismatch, "\n\n"
                    , "Any mismatched taxa in 'mismatch' file in results download.")
      shinyalert::shinyalert(title = "Task Complete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)

    }## expr ~ withProgress ~ END
    , message = "Calculating BCG"
    )## withProgress

  }##expr ~ ObserveEvent

  )##observeEvent ~ b_taxatrans_calc

  ## b_download_TaxaTrans ----
  output$b_download_taxatrans <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_taxatrans
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {##content~START

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ TaxaTrans

  # FB, MERGE FILES ----

  ## Merge, Import ----
  ### Merge, Import, FileWatch ----
  file_watch_mf1 <- reactive({
    # trigger for df_import()
    input$fn_input_mf1
  })## file_watch

  file_watch_mf2 <- reactive({
    # trigger for df_import()
    input$fn_input_mf2
  })## file_watch

  ### Merge, Import, df_import_mf1 ----
  df_import_mf1 <- eventReactive(file_watch_mf1(), {
    # use a multi-item reactive so keep on a single line (if needed later)

    # input$df_import_mf1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$fn_input_mf1

    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END

    sep_user <- input$sep

    # Define file
    fn_inFile <- inFile$datapath

    #message(getwd())
    # message(paste0("Import, separator: '", input$sep,"'"))
    message(paste0("Import, file name: ", inFile$name))

    # Remove existing files in "results"
    clean_results()

    #### Mod, BCG_ATTR----
    # Read user imported file
    # Add extra colClasses parameter for BCG_Attr
    # the "i" values default to complex numbers
    # many permutations of BCG_Attr so check for it first then import
    df_header <- read.delim(fn_inFile
                            , header = TRUE
                            , sep = sep_user
                            , stringsAsFactors = FALSE
                            , na.strings = c("", "NA")
                            , nrows = 0)
    col_num_bcgattr <- grep("BCG_ATTR", toupper(names(df_header)))
    classes_df <- sapply(df_header, class)
    col_name_bcgattr <- names(df_header)[col_num_bcgattr]

    if (identical(col_num_bcgattr, integer(0))) {
      # BCG_Attr present = FALSE
      # define classes = FALSE
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
    } else if (as.vector(classes_df[col_num_bcgattr]) != "complex") {
      # BCG_Attr present = TRUE
      # BCG_Attr Class is complex = FALSE
      # define classes on import = FALSE (change to text after import)
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
      df_input[, col_num_bcgattr] <- as.character(df_input[, col_num_bcgattr])
    } else {
      # BCG_Attr present = TRUE
      # define classes = TRUE
      classes_df <- sapply(df_header, class)
      classes_df[col_num_bcgattr] <- "character"
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA")
                             #, colClasses = classes_df)
                             #, colClasses = c(col_name_bcgattr = "character"))
                             , colClasses = classes_df[col_name_bcgattr])

    }## IF ~ col_num_bcgattr == integer(0)

    # result folder and files
    path_results_sub <- file.path(path_results, dn_files_input)
    # Add "Results" folder if missing
    boo_Results <- dir.exists(file.path(path_results_sub))
    if (boo_Results == FALSE) {
      dir.create(file.path(path_results_sub))
    }

    # Copy to "Results" sub-folder - Import "as is"
    file.copy(inFile$datapath
              , file.path(path_results_sub, inFile$name))

    # button, enable, calc
    shinyjs::enable("b_calc_mergefiles")

    # activate tab Panel with table of imported data
    updateTabsetPanel(session = getDefaultReactiveDomain()
                      , "MF_mp_tsp"
                      , selected = "tab_MF_1")

    # Return Value
    return(df_input)

  })##output$df_import_mf1 ~ END


  ### Merge, Import, df_import_mf2----
  df_import_mf2 <- eventReactive(file_watch_mf2(), {
    # use a multi-item reactive so keep on a single line (if needed later)

    # input$df_import_mf1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$fn_input_mf2

    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END

    # Define file
    fn_inFile <- inFile$datapath

    sep_user <- input$sep

    #message(getwd())
    #message(paste0("Import, separator: '", input$sep,"'"))
    message(paste0("Import, file name: ", inFile$name))

    # Move Results folder clean up to calc button
    # Assume import 2nd file after 1st

    #### Mod, BCG_ATTR----
    # Read user imported file
    # Add extra colClasses parameter for BCG_Attr
    # the "i" values default to complex numbers
    # many permutations of BCG_Attr so check for it first then import
    df_header <- read.delim(fn_inFile
                            , header = TRUE
                            , sep = sep_user
                            , stringsAsFactors = FALSE
                            , na.strings = c("", "NA")
                            , nrows = 0)
    col_num_bcgattr <- grep("BCG_ATTR", toupper(names(df_header)))
    classes_df <- sapply(df_header, class)
    col_name_bcgattr <- names(df_header)[col_num_bcgattr]

    if (identical(col_num_bcgattr, integer(0))) {
      # BCG_Attr present = FALSE
      # define classes = FALSE
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
    } else if (as.vector(classes_df[col_num_bcgattr]) != "complex") {
      # BCG_Attr present = TRUE
      # BCG_Attr Class is complex = FALSE
      # define classes on import = FALSE (change to text after import)
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
      df_input[, col_num_bcgattr] <- as.character(df_input[, col_num_bcgattr])
    } else {
      # BCG_Attr present = TRUE
      # define classes = TRUE
      classes_df <- sapply(df_header, class)
      classes_df[col_num_bcgattr] <- "character"
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA")
                             # , colClasses = classes_df)
                             #, colClasses = c(col_name_bcgattr = "character"))
                             , colClasses = classes_df[col_name_bcgattr])

    }## IF ~ col_num_bcgattr == integer(0)

    # result folder and files
    path_results_sub <- file.path(path_results, dn_files_input)
    # Add "Results" folder if missing
    boo_Results <- dir.exists(file.path(path_results_sub))
    if (boo_Results == FALSE) {
      dir.create(file.path(path_results_sub))
    }

    # Copy to "Results" sub-folder - Import "as is"
    file.copy(inFile$datapath
              , file.path(path_results_sub, inFile$name))

    # button, enable, calc
    shinyjs::enable("b_calc_mergefiles")

    # activate tab Panel with table of imported data
    updateTabsetPanel(session = getDefaultReactiveDomain()
                      , "MF_mp_tsp"
                      , selected = "tab_MF_2")

    # Return Value
    return(df_input)

  })##output$df_import_mf2 ~ END

  ### Merge, Import, df_import_mf1_DT ----
  output$df_import_mf1_DT <- DT::renderDT({
    df_data <- df_import_mf1()
  }##expression~END
  , filter = "top"
  , caption = "Table. MergeFile 1 (Samples)."
  , options = list(scrollX = TRUE
                   , pageLength = 5
                   , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                   , autoWidth = TRUE)
  )##df_import_mf1_DT ~ END

  ### Merge, Import, df_import_mf2_DT ----
  output$df_import_mf2_DT <- DT::renderDT({
    df_data <- df_import_mf2()
  }##expression~END
  , filter = "top"
  , caption = "Table. MergeFile 2 (Sites)."
  , options = list(scrollX = TRUE
                   , pageLength = 5
                   , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                   , autoWidth = TRUE)
  )##df_import_mf1_DT ~ END

  ## Merge, UI----

  output$UI_mergefiles_f1_col_merge <- renderUI({
    str_col <- "Merge Identifier, Primary File, Column Name"
    selectInput("mergefiles_f1_col_merge"
                , label = str_col
                # , choices = c("SiteID", "feature", "in progress")
                , choices = c("", names(df_import_mf1()))
                , selected = "SiteID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_mergefiles_f2_col_merge <- renderUI({
    str_col <- "Merge Identifier, Secondary File, Column Name"
    selectInput("mergefiles_f2_col_merge"
                , label = str_col
                #, choices = c("SiteID", "feature", "in progress")
                , choices = c("", names(df_import_mf2()))
                , selected = "SiteID"
                , multiple = FALSE)
  })## UI_colnames

  ## b_Calc_MergeFiles ----
  observeEvent(input$b_calc_mergefiles, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 00, Set Up Shiny Code ----

      prog_detail <- "Calculation, Merge Files..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 6
      prog_sleep <- 0.25

      ## Calc, 01, Initialize ----
      prog_detail <- "Initialize Data"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = input$fn_input_mf1)
      copy_import_file(import_file = input$fn_input_mf2)

      # result folder and files
      fn_abr <- abr_mergefiles
      fn_abr_save <- paste0("_", fn_abr, "_")
      path_results_sub <- file.path(path_results
                                    , paste(abr_results, fn_abr, sep = "_"))
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      # button, disable, download
      shinyjs::disable("b_download_mergefiles")

      ## Calc, 02, Gather and Test Inputs  ----
      prog_detail <- "QC Inputs"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # inputs
      ## file names
      fn_mf1 <- input$fn_input_mf1$name
      fn_mf2 <- input$fn_input_mf2$name
      ## column names
      col_siteid_mf1 <- input$mergefiles_f1_col_merge
      col_siteid_mf2 <- input$mergefiles_f2_col_merge
      ## file name base (file 1)
      fn_input_base <- tools::file_path_sans_ext(fn_mf1)

      # Stop if don't have both MF1 and MF2
      if (is.null(fn_mf1)) {
        msg <- "Merge File 1 filename is missing!"
        shinyalert::shinyalert(title = "Merge File Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)

      if (is.null(fn_mf2)) {
        msg <- "Merge File 2 filename is missing!"
        shinyalert::shinyalert(title = "Merge File Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)

      # Stop if colname for merge is NA
      if (col_siteid_mf1 == "") {
        msg <- "Merge File 1 merge column is missing!"
        shinyalert::shinyalert(title = "Merge File Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)

      if (col_siteid_mf2 == "") {
        msg <- "Merge File 2 merge column is missing!"
        shinyalert::shinyalert(title = "Merge File Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)

      ## Calc, 03, Run Function----
      suff_1x <- ".x"
      suff_2y <- ".y"
      df_merge <- merge(df_import_mf1()
                        , df_import_mf2()
                        , by.x = col_siteid_mf1
                        , by.y = col_siteid_mf2
                        , suffixes = c(suff_1x, suff_2y)
                        , all.x = TRUE
                        , sort = FALSE
      )
      # ***REPEAT*** same merge statement in DT statement for display on tab

      # move MF2 columns to the start (at end after merge)
      ## use index numbers
      ncol_1x <- ncol(df_import_mf1())
      ncol_merge <- ncol(df_merge)
      df_merge <- df_merge[, c(1, seq(ncol_1x + 1, ncol_merge), 2:ncol_1x)]

      ## Calc, 04, Save Results ----

      fn_merge <- paste0(fn_input_base, fn_abr_save, "RESULTS.csv")
      pn_merge <- file.path(path_results_sub, fn_merge)
      write.csv(df_merge, pn_merge, row.names = FALSE)


      ## Calc, 05, Clean Up----
      prog_detail <- "Calculate, Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      # # activate tab Panel with table of imported data
      # updateTabsetPanel(session = getDefaultReactiveDomain()
      #                   , "MF_mp_tsp"
      #                   , selected = "tab_MF_merge")


      ## Calc, 06, Zip Results ----
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)

      # button, enable, download
      shinyjs::enable("b_download_mergefiles")

      # time, end
      toc <- Sys.time()
      duration <- difftime(toc, tic)

      # pop up
      msg <- paste0("Elapse Time (", units(duration), ") = ", round(duration, 2))
      shinyalert::shinyalert(title = "Task Compete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)

    }## expr ~ withProgress ~ END
    , message = "Merging Files"
    )## withProgress ~ END
  }##expr ~ ObserveEvent ~ END
  )##observeEvent ~ b_calc_met_therm ~ END


  ## b_download_mergefiles ----
  output$b_download_mergefiles <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input_mf2
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_mergefiles
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ MergeFiles

  #~~~~CALC~~~~----

  # Calc, BCG, UI----

  output$UI_calc_bcg_user_col_buggear <- renderUI({
    str_col <- "Column, IA Bug Gear (BugGear)"
    selectInput("calc_bcg_user_col_buggear"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "BugGear"
                , multiple = FALSE)
  })## UI_colnames

  # Calc, BCG ----
  ## b_Calc_BCG ----
  observeEvent(input$b_calc_bcg, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 0, Set Up Shiny Code ----

      prog_detail <- "Calculation, BCG..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 12
      prog_sleep <- 0.25

      ## Calc, 1, Initialize ----
      prog_detail <- "Initialize Data"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = input$fn_input)

      # result folder and files
      # 2023-12-14, add community
      fn_comm <- input$si_community
      fn_abr <- abr_bcg
      fn_abr_save <- paste0("_", fn_abr, "_")
      path_results_sub <- file.path(path_results
                                    , paste(abr_results, fn_comm, fn_abr, sep = "_"))
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      # reference folder
      path_results_ref <- file.path(path_results, dn_files_ref)
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_ref))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_ref))
      }

      # button, disable, download
      shinyjs::disable("b_download_bcg")

      # data
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      message(paste0("Import, file name, base: ", fn_input_base))
      df_input <- read.delim(inFile$datapath
                             , header = TRUE
                             , sep = input$sep
                             , stringsAsFactors = FALSE)
      # QC, FAIL if TRUE
      if (is.null(df_input)) {
        return(NULL)
      }

      # QC, names to upper case
      names(df_input) <- toupper(names(df_input))

      # QC, Index_Name
      my_comm <- input$si_community
      if ((!"INDEX_NAME" %in% toupper(names(df_input))) & (my_comm == "Fish")) {
        df_input[, "INDEX_NAME"] <- "GP_Fish_BCG"
      } else if ((!"INDEX_NAME" %in% toupper(names(df_input))) & (my_comm == "Bugs_IA")) {
        df_input[,"INDEX_NAME"] <- "IA_Bugs_BCG"
      } else if ((!"INDEX_NAME" %in% toupper(names(df_input))) & (my_comm == "Bugs_KS")) {
        df_input[,"INDEX_NAME"] <- "KS_Bugs_BCG"
      } else if ((!"INDEX_NAME" %in% toupper(names(df_input))) & (my_comm == "Bugs_MO")) {
        df_input[,"INDEX_NAME"] <- "MO_Bugs_BCG"
      } else if ((!"INDEX_NAME" %in% toupper(names(df_input))) & (my_comm == "Bugs_NE")) {
        df_input[,"INDEX_NAME"] <- "NE_Bugs_BCG"
      }## IF ~ INDEX_NAME

      # QC, BMT community
      if (startsWith(my_comm, "Bugs_")) {
        BMT_comm <- "bugs"
      } else {
        BMT_comm <- "fish"
      }

      ## Calc, 2, Exclude Taxa ----
      prog_detail <- "Calculate, Exclude Taxa"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      message(paste0("User response to generate ExclTaxa = ", input$ExclTaxa))

      if (input$ExclTaxa) {
        ## Get TaxaLevel names present in user file
        phylo_all <- c("Kingdom"
                       , "Phylum"
                       , "SubPhylum"
                       , "Class"
                       , "SubClass"
                       , "Order"
                       , "SubOrder"
                       , "InfraOrder"
                       , "SuperFamily"
                       , "Family"
                       , "SubFamily"
                       , "Tribe"
                       , "Genus"
                       , "SubGenus"
                       , "Species"
                       , "Variety")
        phylo_all <- toupper(phylo_all) # so matches rest of file

        # case and matching of taxa levels handled inside of markExluded

        # Overwrite existing column if present
        # ok since user checked the box to calculate
        if ("EXCLUDE" %in% toupper(names(df_input))) {
          # save original user input
          df_input[, "EXCLUDE_USER"] <- df_input[, "EXCLUDE"]
          # drop column
          df_input <- df_input[, !names(df_input) %in% "EXCLUDE"]
        }## IF ~ Exclude

        # overwrite current data frame
        df_input <- BioMonTools::markExcluded(df_samptax = df_input
                                              , SampID = "SAMPLEID"
                                              , TaxaID = "TAXAID"
                                              , TaxaCount = "N_TAXA"
                                              , Exclude = "EXCLUDE"
                                              , TaxaLevels = phylo_all
                                              , Exceptions = NA)

        # Save Results
        # fn_excl <- paste0(fn_input_base, fn_abr_save, "1markexcl.csv")
        fn_excl <- "BCG_1markexcl.csv"
        dn_excl <- path_results_sub
        pn_excl <- file.path(dn_excl, fn_excl)
        write.csv(df_input, pn_excl, row.names = FALSE)

      }## IF ~ input$ExclTaxa


      ## Calc, 3, BCG Flag Cols ----
      # get columns from Flags (non-metrics) to carry through
      prog_detail <- "Calculate, Keep BCG Model Columns"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Rules - should all be metrics but leaving here just in case
      # Flags - not always metrics,
      # Index Name for import data
      import_IndexName <- unique(df_input$INDEX_NAME)
      # QC Flags for chosen BCG model (non-metrics)
      cols_flags <- unique(df_checks[df_checks$Index_Name == import_IndexName
                                     , "Metric_Name"])
      # can also add other columns to keep if feel so inclined
      cols_flags_keep <- cols_flags[cols_flags %in% names(df_input)]


      ## Calc, 3b, Rules ----
      prog_detail <- "Calculate, BCG Rules"
      message(paste0("\n", prog_detail))
      message(paste0("Community = ", input$si_community))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # filter for data Index_Name in data (drop 2 extra columns)
      df_rules <- df_bcg_models[df_bcg_models$Index_Name == import_IndexName
                                , !names(df_bcg_models) %in% c("SITE_TYPE", "INDEX_REGION")]
      # Save
      # fn_rules <- paste0(fn_input_base, fn_abr_save, "3metrules.csv")
      fn_rules <- "BCG_3metrules.csv"
      dn_rules <- path_results_sub
      pn_rules <- file.path(dn_rules, fn_rules)
      write.csv(df_rules, pn_rules, row.names = FALSE)

      ## Calc, 4, MetVal----
      prog_detail <- "Calculate, Metric, Values"
      message(paste0("\n", prog_detail))
      message(paste0("Community = ", input$si_community))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      #QC, BCG_Num to TOLVAL2 field
      if("BCG_NUM" %in% toupper(names(df_input))){
        df_input[, "TOLVAL2"] <- df_input$BCG_NUM

      } #END ~ if

      # Metric calculation
      ## Split calculation methods for Quant/Qual for IA and MO

      ## Quant vs. Qual ----
      # Metric Names
      df_metnames_xl <- readxl::read_excel(system.file("extdata/MetricNames.xlsx"
                                                    , package = "BioMonTools")
                                        , sheet = "MetricMetadata"
                                        , skip = 4
                                        , guess_max = 10^6)
      # Metric Names by Large-Rare TRUE/FALSE
      metnames_lr_t <- df_metnames_xl[df_metnames_xl[, "Bugs_LargeRare"] == TRUE
                                      , "METRIC_NAME", TRUE]  #303
      metnames_lr_f <- df_metnames_xl[df_metnames_xl[, "Bugs_LargeRare"] == FALSE
                                      , "METRIC_NAME", TRUE] #218

       # calc for different methods
      if (length(cols_flags_keep) > 0) {
        # keep extra cols from Flags (non-metric)
        fun_cols2keep <- cols_flags_keep
      } else {
        fun_cols2keep <- NULL
      }## IF ~ length(col_rules_keep)

      count_lr <- 999
      # N_TAXA; blank, na, 0, -99 to 999
      df_input <- df_input %>%
        dplyr::mutate(N_TAXA = dplyr::case_when(N_TAXA <= 0 ~ count_lr
                                                , is.na(N_TAXA) ~ count_lr
                                                , N_TAXA == "" ~ count_lr
                                                , .default = N_TAXA
        ))## mutate ~ case_when

      #
      if (my_comm == "Bugs_IA") {
        msg <- paste0("Index_Name, ", my_comm)
        message(msg)

        # Check if missing BUGGEAR
        boo_buggear <- "BUGGEAR" %in% toupper(names(df_input))
        if (!boo_buggear) {
          # pop up
          msg <- "Column is missing (IA data only, BUGGEAR)!"
          shinyalert::shinyalert(title = "BCG Calculation Error"
                                 , text = msg
                                 , type = "error"
                                 , closeOnEsc = TRUE
                                 , closeOnClickOutside = TRUE)
          validate(msg)
        }## IF ~ boo_buggear





        # create LR TRUE and LR FALSE datasets
        df_input_lr_t <- df_input
        df_input_lr_f <- df_input %>%
          dplyr::mutate(BUGGEAR_UC = toupper(BUGGEAR)) %>%
          dplyr::filter(BUGGEAR_UC != "QUALITATIVE")
        msg <- paste0("Data, Qual, dim = ", paste(dim(df_input_lr_t), collapse = ", "))
        message(msg)
        msg <- paste0("Data, Quant, dim = ", paste(dim(df_input_lr_f), collapse = ", "))
        message(msg)
        # calc
        if (nrow(df_input_lr_t) == 0) {
          # no large-rare samples so run on 'all' data
          df_metval <- BioMonTools::metric.values(df_input
                                          , fun.Community = BMT_comm
                                          , fun.MetricNames = metnames_lr_f
                                          , fun.cols2keep = fun_cols2keep
                                          , boo.Shiny = TRUE
                                          , verbose = TRUE
                                          , taxaid_dni = "DNI")

          } else {

          df_metval_lr_t <- BioMonTools::metric.values(df_input_lr_t
                                          , fun.Community = BMT_comm
                                          , fun.MetricNames = metnames_lr_t
                                          , fun.cols2keep = fun_cols2keep
                                          , boo.Shiny = TRUE
                                          , verbose = TRUE
                                          , taxaid_dni = "DNI")
          df_metval_lr_f <- BioMonTools::metric.values(df_input_lr_f
                                          , fun.Community = BMT_comm
                                          , fun.MetricNames = metnames_lr_f
                                          , fun.cols2keep = fun_cols2keep
                                          , boo.Shiny = TRUE
                                          , verbose = TRUE
                                          , taxaid_dni = "DNI")
          # merge
          # drop ni_total (default) from lr_t
          df_metval <- merge(df_metval_lr_t[, !(names(df_metval_lr_t) %in% "ni_total")]
                             , df_metval_lr_f
                             , by = c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"))
        }## IF ~ nrow(df_input_lr_t)
      #
      } else if (my_comm == "Bugs_MO") {
        msg <- paste0("Index_Name, ", my_comm)
        message(msg)

        # create LR TRUE and LR FALSE datasets
        df_input_lr_t <- df_input
        df_input_lr_f <- df_input %>%
          dplyr::filter(N_TAXA != count_lr)
        msg <- paste0("Data, Qual, dim = ", paste(dim(df_input_lr_t), collapse = ", "))
        message(msg)
        msg <- paste0("Data, Quant, dim = ", paste(dim(df_input_lr_f), collapse = ", "))
        message(msg)

        if (nrow(df_input_lr_t) == 0) {
          # no large-rare samples so run on 'all' data
          df_metval <- BioMonTools::metric.values(df_input
                                                  , fun.Community = BMT_comm
                                                  , fun.MetricNames = metnames_lr_f
                                                  , fun.cols2keep = fun_cols2keep
                                                  , boo.Shiny = TRUE
                                                  , verbose = TRUE
                                                  , taxaid_dni = "DNI")

        } else {

          df_metval_lr_t <- metric.values(df_input_lr_t
                                          , fun.Community = BMT_comm
                                          , fun.MetricNames = metnames_lr_t
                                          , fun.cols2keep = fun_cols2keep
                                          , boo.Shiny = TRUE
                                          , verbose = TRUE
                                          , taxaid_dni = "DNI")
          df_metval_lr_f <- BioMonTools::metric.values(df_input_lr_f
                                                       , fun.Community = BMT_comm
                                                       , fun.MetricNames = metnames_lr_f
                                                       , fun.cols2keep = fun_cols2keep
                                                       , boo.Shiny = TRUE
                                                       , verbose = TRUE
                                                       , taxaid_dni = "DNI")
          # merge
          # drop ni_total (default) from lr_t
          df_metval <- merge(df_metval_lr_t[, !(names(df_metval_lr_t) %in% "ni_total")]
                             , df_metval_lr_f
                             , by = c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"))
        }## IF ~ nrow(df_input_lr_t)


      } else {
        msg <- paste0("Index_Name, ", "NOT IA or MO.")
        message(msg)
        df_metval <- BioMonTools::metric.values(df_input
                                                , BMT_comm
                                                , fun.cols2keep = fun_cols2keep
                                                , boo.Shiny = TRUE
                                                , verbose = TRUE
                                                , taxaid_dni = "DNI")
      }## IF ~ my_comm



      ### Save Results ----

      # fn_metval <- paste0(fn_input_base, fn_abr_save, "2metval_all.csv")
      fn_metval <- "BCG_2metvall_all.csv"
      dn_metval <- path_results_sub
      pn_metval <- file.path(dn_metval, fn_metval)
      write.csv(df_metval, pn_metval, row.names = FALSE)

      ### Save Results (BCG) ----
      # Munge
      ## Model and QC Flag metrics only
      # cols_flags defined above
      cols_model_metrics <- unique(df_bcg_models[
        df_bcg_models$Index_Name == import_IndexName, "Metric_Name"])
      cols_req <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"
                    , "ni_total", "nt_total")
      cols_metrics_flags_keep <- unique(c(cols_req
                                          , cols_flags
                                          , cols_model_metrics))
      df_metval_slim <- df_metval[, names(df_metval) %in% cols_metrics_flags_keep]
      # Save
      # fn_metval_slim <- paste0(fn_input_base, fn_abr_save, "2metval_BCG.csv")
      fn_metval_slim <- "BCG_2metval_BCG.csv"
      dn_metval_slim <- path_results_sub
      pn_metval_slim <- file.path(dn_metval_slim, fn_metval_slim)
      write.csv(df_metval_slim, pn_metval_slim, row.names = FALSE)


      ## Calc, 5, MetMemb----
      prog_detail <- "Calculate, Metric, Membership"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      df_metmemb <- BCGcalc::BCG.Metric.Membership(df_metval, df_bcg_models)
      # Save Results
      # fn_metmemb <- paste0(fn_input_base, fn_abr_save, "3metmemb.csv")
      fn_metmemb <- "BCG_3metmemb.csv"
      dn_metmemb <- path_results_sub
      pn_metmemb <- file.path(dn_metmemb, fn_metmemb)
      write.csv(df_metmemb, pn_metmemb, row.names = FALSE)


      ## Calc, 6, LevMemb----
      prog_detail <- "Calculate, Level, Membership"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      df_levmemb <- BCGcalc::BCG.Level.Membership(df_metmemb, df_bcg_models)
      # Save Results
      # fn_levmemb <- paste0(fn_input_base, fn_abr_save, "4levmemb.csv")
      fn_levmemb <- "BCG_4levmemb.csv"
      dn_levmemb <- path_results_sub
      pn_levmemb <- file.path(dn_levmemb, fn_levmemb)
      write.csv(df_levmemb, pn_levmemb, row.names = FALSE)


      ## Calc, 7, LevAssign----
      prog_detail <- "Calculate, Level, Assignment"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      df_levassign <- BCGcalc::BCG.Level.Assignment(df_levmemb)
      # Save Results
      # fn_levassign <- paste0(fn_input_base, fn_abr_save, "5levassign.csv")
      fn_levassign <- "BCG_5levassign.csv"
      dn_levassign <- path_results_sub
      pn_levassign <- file.path(dn_levassign, fn_levassign)
      write.csv(df_levassign, pn_levassign, row.names = FALSE)


      ## Calc, 8, QC Flags----
      prog_detail <- "Calculate, QC Flags"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # 2023-12-06
      # Split if no flags so doesn't crash

      # Check if Flags exist for data
      col_index_metval <- c("INDEX_NAME", "INDEX_CLASS")
      col_index_checks <- c("Index_Name", "INDEX_CLASS")
      index_metval <- unique(df_metval[, col_index_metval])
      index_checks <- unique(df_checks[, col_index_checks])
      index_merge <- merge(index_metval, index_checks
                           , by.x = col_index_metval
                           , by.y = col_index_checks)

      if (nrow(index_merge) == 0) {

        # create dummy files
        str_nodata <- "No flags for the Index Name/Class combinations present in data"
        # Flags
        df_flags <- data.frame(x = str_nodata
                                   , CHECKNAME = "No Flags"
                                   , FLAG = NA)
        df_lev_flags <- df_levassign
        # Flags Summary
        df_lev_flags_summ <- data.frame(x = str_nodata)
        # Results
        df_results <- df_lev_flags[, !names(df_lev_flags) %in% c(paste0("L", 1:6))]
        # Flag Metrics
        df_metflags <- data.frame(x = str_nodata)

      } else {

        # Calc
        # df_checks loaded in global.R
        df_flags <- BioMonTools::qc.checks(df_metval, df_checks)
        # Change terminology; PASS/FAIL to NA/flag
        df_flags[, "FLAG"][df_flags[, "FLAG"] == "FAIL"] <- "flag"
        df_flags[, "FLAG"][df_flags[, "FLAG"] == "PASS"] <- NA
        # long to wide format
        df_flags_wide <- reshape2::dcast(df_flags
                                         , SAMPLEID ~ CHECKNAME
                                         , value.var = "FLAG")
        # Calc number of "flag"s by row.
        df_flags_wide$NumFlags <- rowSums(df_flags_wide == "flag", na.rm = TRUE)
        # Rearrange columns
        NumCols <- ncol(df_flags_wide)
        df_flags_wide <- df_flags_wide[, c(1, NumCols, 2:(NumCols - 1))]
        # Merge Levels and Flags
        df_lev_flags <- merge(df_levassign
                              , df_flags_wide
                              , by.x = "SampleID"
                              , by.y = "SAMPLEID"
                              , all.x = TRUE)
        # Flags Summary
        df_lev_flags_summ <- as.data.frame.matrix(table(df_flags[, "CHECKNAME"]
                                                        , df_flags[, "FLAG"]
                                                        , useNA = "ifany"))
        # Results
        df_results <- df_lev_flags[, !names(df_lev_flags) %in% c(paste0("L", 1:6))]
        ## remove L1:6

        # Flag Metrics
        col2keep_metflags <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"
                               , "METRIC_NAME", "CHECKNAME", "METRIC_VALUE"
                               , "SYMBOL", "VALUE", "FLAG")
        df_metflags <- df_flags[, col2keep_metflags]

      }## IF ~ check for matching index name and class


      # Save, Flags Summary
      # fn_levflags <- paste0(fn_input_base, fn_abr_save, "6levflags.csv")
      fn_levflags <- "BCG_6levflags.csv"
      dn_levflags <- path_results_sub
      pn_levflags <- file.path(dn_levflags, fn_levflags)
      write.csv(df_lev_flags_summ, pn_levflags, row.names = TRUE)

      # Save, Results
      # fn_results <- paste0(fn_input_base, fn_abr_save, "RESULTS.csv")
      fn_results <- "_BCG_RESULTS.csv"
      dn_results <- path_results_sub
      pn_results <- file.path(dn_results, fn_results)
      write.csv(df_results, pn_results, row.names = FALSE)

      # Save, Flag Metrics
      # fn_metflags <- paste0(fn_input_base, fn_abr_save, "6metflags.csv")
      fn_metflags <- "BCG_6metflags.csv"
      dn_metflags <- path_results_sub
      pn_metflags <- file.path(dn_metflags, fn_metflags)
      write.csv(df_metflags, pn_metflags, row.names = FALSE)


      ## Calc, 9, RMD----
      prog_detail <- "Calculate, Create Report"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      strFile.RMD <- file.path("external"
                               , "RMD_Results"
                               , "Results_BCG_Summary.Rmd")
      strFile.RMD.format <- "html_document"
      # strFile.out <- paste0(fn_input_base, fn_abr_save, "RESULTS.html")
      strFile.out <- "_BCG_RESULTS.html"
      dir.export <- path_results_sub
      rmarkdown::render(strFile.RMD
                        , output_format = strFile.RMD.format
                        , output_file = strFile.out
                        , output_dir = dir.export
                        , quiet = TRUE)

      ## Calc, 10, Save, Reference----
      prog_detail <- "Calculate, Save, Reference"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      path_results_ref <- file.path(path_results, dn_files_ref)

      ## Metric Flags
      # fn_save <- "MetricFlags.xlsx"
      # file_from <- temp_bcg_checks
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)

      ## Metric Names
      # fn_save <- "MetricNames.xlsx"
      # file_from <- temp_metricnames
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)

      ## BCG Rules
      # fn_save <- "Rules.xlsx"
      # file_from <- temp_bcg_models
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)

      ## Calc, 11, Clean Up----
      prog_detail <- "Calculate, Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      # Create zip file of results
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)

      # button, enable, download
      shinyjs::enable("b_download_bcg")

      # time, end
      toc <- Sys.time()
      duration <- difftime(toc, tic)

      # pop up
      msg <- paste0("Total Records (Input) = ", nrow(df_input)
                    , "\n\n"
                    , "Elapse Time (", units(duration), ") = ", round(duration, 2))
      shinyalert::shinyalert(title = "Task Complete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)

    }## expr ~ withProgress ~ END
    , message = "Calculating BCG"
    )## withProgress ~ END
  }##expr ~ ObserveEvent ~ END
  )##observeEvent ~ b_calc_bcg ~ END

  ## b_download_BCG ----
  output$b_download_bcg <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_bcg
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {##content~START

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ BCG

  #~~~~MAP~~~~----
  # MAP ----

  ## Map, UI ----

  output$UI_map_datatype <- renderUI({
    str_col <- "Select data type (calculation) to map."
    selectInput("map_datatype"
                , label = str_col
                , choices = c("", map_datatypes)
                , multiple = FALSE)
  })## UI_datatype

  output$UI_map_col_xlong <- renderUI({
    str_col <- "Column, Longitude (decimal degrees))"
    selectInput("map_col_xlong"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "Longitude"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_map_col_ylat <- renderUI({
    str_col <- "Column, Latitude (decimal degrees)"
    selectInput("map_col_ylat"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "Latitude"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_map_col_sampid <- renderUI({
    str_col <- "Column, SampleID (unique station or sample identifier)"
    selectInput("map_col_sampid"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "SampleID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_map_col_mapval <- renderUI({
    str_col <- "Column, Value to Map (e.g., BCG, MTTI, or metric value)"
    selectInput("map_col_mapval"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "SampleID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_map_col_keep <- renderUI({
    str_col <- "Additional Columns to Keep in Map Popup"
    selectInput("map_col_keep"
                , label = str_col
                , choices = c("", names(df_import()))
                , multiple = TRUE)
  })## UI_colnames

  ## Map, Leaflet ----
  output$map_leaflet <- renderLeaflet({

    # data for plot
    df_map <- df_import()

    # Map
    #leaflet() %>%
    leaflet(data = df_map) %>%
      # Groups, Base
      # addTiles(group="OSM (default)") %>%  #default tile too cluttered
      addProviderTiles("CartoDB.Positron"
                       , group = "Positron") %>%
      addProviderTiles(providers$OpenStreetMap
                       , group = "Open Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery
                       , group = "ESRI World Imagery") %>%
    # Layers, Control
    addLayersControl(baseGroups = c("Positron"
                                    , "Open Street Map"
                                    , "ESRI World Imagery"
                                    # , "USGS Imagery"
    )
    , overlayGroups = c("Ecoregions, Level III"
                        # , "BCG Class"
                        # , "NorWeST"
                        # , "NHD+ Catchments"
                        # , "NHD+ Flowlines"
    )
    ) %>%
      # Layers, Hide
      # hideGroup(c("Ecoregions, Level III"
      #            # , "BCG Class"
      #            # , "NorWeST"
      #            # , "NHD+ Catchments"
      #            # , "NHD+ Flowlines"
      # )) %>%
      # # Mini map
      addMiniMap(toggleDisplay = TRUE) #%>%
    # Legend
    # addLegend("bottomleft"
    #           , title = "L3 Ecoregions, BCG Valid"
    #           , colors = c("#000000", "#03F")
    #           , labels = c("TRUE", "FALSE")
    #           # , layerID = "Ecoregions, Level III"
    #           )



  })## map_leaflet ~ END

  ## Map, Leaflet, Proxy ----
  # update map based on user selections
  # tied to Update button
  # https://rstudio.github.io/leaflet/shiny.html
  # need a reactive to trigger, use map update button
  observeEvent(input$but_map_update, {

    ### Data ----
    df_map <- df_import()
    names_data <- names(df_map)

    no_narrative <- "No Narrative Designation"
    size_default <- 50

    ### Map_L_P, Gather and Test Inputs----
    sel_map_datatype   <- input$map_datatype
    sel_map_col_xlong  <- input$map_col_xlong
    sel_map_col_ylat   <- input$map_col_ylat
    sel_map_col_sampid <- input$map_col_sampid
    sel_map_col_keep   <- input$map_col_keep

    sel_map_col_mapval <- NA_character_
    sel_map_col_mapnar <- NA_character_
    sel_map_col_color  <- NA_character_

    if (is.null(sel_map_datatype) | sel_map_datatype == "") {
      # end process with pop up
      msg <- "'Data Type' name is missing!"
      shinyalert::shinyalert(title = "Update Map"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)
    }## IF ~ sel_map_datatype

    if (is.null(sel_map_col_xlong) | sel_map_col_xlong == "") {
      # end process with pop up
      msg <- "'Longitude' column name is missing!"
      shinyalert::shinyalert(title = "Update Map"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)
    }## IF ~ sel_map_col_xlong

    if (is.null(sel_map_col_ylat) | sel_map_col_ylat == "") {
      # end process with pop up
      msg <- "'Latitude' column name is missing!"
      shinyalert::shinyalert(title = "Update Map"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)
    }## IF ~ sel_map_col_ylat

    if (is.null(sel_map_col_sampid) | sel_map_col_sampid == "") {
      # end process with pop up
      msg <- "'SampleID' column name is missing!"
      shinyalert::shinyalert(title = "Update Map"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)
    }## IF ~ sel_map_col_sampid

    ### Munge Data ----
    #### Munge, Val, Nar, Size
    if (sel_map_datatype == "BCG") {
      sel_map_col_mapval <- "BCG_Status"
      sel_map_col_mapnar <- "BCG_Status2"
    } else if (sel_map_datatype == "Fuzzy Temp Model") {
      sel_map_col_mapval <- "Therm_Class" #"Continuous_Therm"
      sel_map_col_mapnar <- "Therm_Class"
    } else if (sel_map_datatype == "MTTI") {
      sel_map_col_mapval <- "MTTI"
      sel_map_col_mapnar <- "Map_Nar"
      df_map[, sel_map_col_mapnar] <- NA_character_
      cut_brk <- c(-1, 16, 19, 21, 23, 9999)
      cut_lab <- c("< 16"
                   , "16 - 18.9"
                   , "19 - 20.9"
                   , "21 - 22.9"
                   , ">= 23")
      df_map[, sel_map_col_mapnar] <- cut(df_map[, sel_map_col_mapval]
                                          , breaks = cut_brk
                                          , labels = cut_lab
                                          , include.lowest = TRUE
                                          , right = FALSE
                                          , ordered_result = TRUE)
    } else if (sel_map_datatype == "BDI") {
      sel_map_col_mapval <- "Index"
      sel_map_col_mapnar <- "Index_Nar"
    } else if (sel_map_datatype == "Thermal Metrics, nt_ti_stenocold") {
      sel_map_col_mapval <- "nt_ti_stenocold"
      sel_map_col_mapnar <- "Map_Nar"
      df_map[, sel_map_col_mapnar] <- NA_character_
      cut_brk <- c(-1, 1, 3, 9999)
      cut_lab <- c("absent"
                   , "1 or 2"
                   , ">= 3")
      df_map[, sel_map_col_mapnar] <- cut(df_map[, sel_map_col_mapval]
                                          , breaks = cut_brk
                                          , labels = cut_lab
                                          , include.lowest = TRUE
                                          , right = FALSE
                                          , ordered_result = TRUE)
    } else if (sel_map_datatype == "Thermal Metrics, nt_ti_stenocold_cold") {
      sel_map_col_mapval <- "nt_ti_stenocold_cold"
      sel_map_col_mapnar <- "Map_Nar"
      df_map[, sel_map_col_mapnar] <- NA_character_
      cut_brk <- c(-1, 1, 3, 5, 10, 9999)
      cut_lab <- c("absent"
                   , "1 or 2"
                   , "3 or 4"
                   , "5 - 9"
                   , ">= 10
                   ")
      df_map[, sel_map_col_mapnar] <- cut(df_map[, sel_map_col_mapval]
                                          , breaks = cut_brk
                                          , labels = cut_lab
                                          , include.lowest = TRUE
                                          , right = FALSE
                                          , ordered_result = TRUE)
    } else if (sel_map_datatype == "Thermal Metrics, nt_ti_stenocold_cold_cool") {
      sel_map_col_mapval <- "nt_ti_stenocold_cold_cool"
      sel_map_col_mapnar <- "Map_Nar"
      df_map[, sel_map_col_mapnar] <- NA_character_
      cut_brk <- c(-1, 9, 20, 25, 30, 9999)
      cut_lab <- c("< 9"
                   , "9 - 19"
                   , "20 - 24"
                   , "25 - 29"
                   , ">= 30")
      df_map[, sel_map_col_mapnar] <- cut(df_map[, sel_map_col_mapval]
                                          , breaks = cut_brk
                                          , labels = cut_lab
                                          , include.lowest = TRUE
                                          , right = FALSE
                                          , ordered_result = TRUE)

    } else if (sel_map_datatype == "Thermal Metrics, pt_ti_stenocold_cold_cool") {
      sel_map_col_mapval <- "pt_ti_stenocold_cold_cool"
      sel_map_col_mapnar <- "Map_Nar"
      df_map[, sel_map_col_mapnar] <- NA_character_
      cut_brk <- c(-1, 20, 35, 50, 65, 9999)
      cut_lab <- c("< 20"
                   , "20 - 34.9"
                   , "35 - 49.9"
                   , "50 - 64.9"
                   , ">= 65")
      df_map[, sel_map_col_mapnar] <- cut(df_map[, sel_map_col_mapval]
                                          , breaks = cut_brk
                                          , labels = cut_lab
                                          , include.lowest = TRUE
                                          , right = FALSE
                                          , ordered_result = TRUE)

    } else if (sel_map_datatype == "Thermal Metrics, pi_ti_stenocold_cold_cool") {
      sel_map_col_mapval <- "pi_ti_stenocold_cold_cool"
      sel_map_col_mapnar <- "Map_Nar"
      df_map[, sel_map_col_mapnar] <- NA_character_
      cut_brk <- c(-1, 10, 30, 40, 55, 9999)
      cut_lab <- c("< 10"
                   , "10 - 29.9"
                   , "30 - 39.9"
                   , "40 - 54.9"
                   , ">= 55")
      df_map[, sel_map_col_mapnar] <- cut(df_map[, sel_map_col_mapval]
                                          , breaks = cut_brk
                                          , labels = cut_lab
                                          , include.lowest = TRUE
                                          , right = FALSE
                                          , ordered_result = TRUE)

    } else if (sel_map_datatype == "Thermal Metrics, pt_ti_warm_stenowarm") {
      sel_map_col_mapval <- "pt_ti_warm_stenowarm"
      sel_map_col_mapnar <- "Map_Nar"
      df_map[, sel_map_col_mapnar] <- NA_character_
      cut_brk <- c(-1, 5, 10, 15, 40, 9999)
      cut_lab <- c("< 5"
                   , "5 - 9.9"
                   , "10 - 14.9"
                   , "15 - 39.9"
                   , ">= 40")
      df_map[, sel_map_col_mapnar] <- cut(df_map[, sel_map_col_mapval]
                                          , breaks = cut_brk
                                          , labels = cut_lab
                                          , include.lowest = TRUE
                                          , right = FALSE
                                          , ordered_result = TRUE)

    } else if (sel_map_datatype == "Thermal Metrics, nt_ti_warm_stenowarm") {
      sel_map_col_mapval <- "nt_ti_warm_stenowarm"
      sel_map_col_mapnar <- "Map_Nar"
      df_map[, sel_map_col_mapnar] <- NA_character_
      cut_brk <- c(-1, 2, 9999)
      cut_lab <- c("NA"
                   , ">= 2")
      df_map[, sel_map_col_mapnar] <- cut(df_map[, sel_map_col_mapval]
                                          , breaks = cut_brk
                                          , labels = cut_lab
                                          , include.lowest = TRUE
                                          , right = FALSE
                                          , ordered_result = TRUE)

    }## IF ~ sel_datatype ~ END


    # QC, Value in data frame
    boo_map_col_mapval <- sel_map_col_mapval %in% names_data
    if (boo_map_col_mapval == FALSE) {
      # end process with pop up
      msg <- paste0("Map Value column name ("
                    , sel_map_col_mapval
                    , ") is missing!")
      shinyalert::shinyalert(title = "Update Data"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)
    }## IF ~ sel_map_col_sampid



    # Rename Columns to known values
    ## Add Jitter to Lat-Long to avoid overlap
    # 1 second ~ 1/3600 ~ 0.000278 ~ 37.5 meters
    # 7 seconds ~ 262.3 meters
    jit_fac <- 0/3600
    nrow_data <- nrow(df_map)
    noise_y <- runif(nrow_data, -jit_fac, jit_fac)
    noise_x <- runif(nrow_data, -jit_fac, jit_fac)

    df_map <- df_map %>%
      mutate(map_ID = df_map[, sel_map_col_sampid]
             # , map_ylat = jitter(df_map[, sel_map_col_ylat], jit_fac)
             # , map_xlong = jitter(df_map[, sel_map_col_xlong], jit_fac)
             , map_ylat = df_map[, sel_map_col_ylat] + noise_y
             , map_xlong = df_map[, sel_map_col_xlong] + noise_x
             , map_mapval = df_map[, sel_map_col_mapval]
             , map_mapnar = df_map[, sel_map_col_mapnar]
             , map_color = NA_character_
             , map_size = NA_real_
             , map_popup = paste0(as.character("<b>"), "SampleID: ", as.character("</b>"), df_map[, sel_map_col_sampid], as.character("<br>")
                                  , as.character("<b>"), "Latitude: ", as.character("</b>"), df_map[, sel_map_col_ylat], as.character("<br>")
                                  , as.character("<b>"), "Longitude: ", as.character("</b>"), df_map[, sel_map_col_xlong], as.character("<br>")
                                  , as.character("<b>"), "Data Type: ", as.character("</b>"), sel_map_datatype, as.character("<br>")
                                  , as.character("<b>"), "Value: ", as.character("</b>"), df_map[, sel_map_col_mapval], as.character("<br>")
                                  , as.character("<b>"), "Narrative: ", as.character("</b>"), df_map[, sel_map_col_mapnar], as.character("<br>")
             )
      )

    ### Munge, Color, Size, Legend
    # by index value or narrative
    if (sel_map_datatype == "BCG") {
      leg_title <- "Biological Condition Gradient"
      # cut_brk <- seq(0.5, 6.5, 1)
      # cut_lab <- c("blue", "green", "lightgreen", "gray", "orange", "red")
      # leg_col <- cut_lab
      # leg_nar <- paste0("L", 1:6)
      # df_map[, "map_color"] <- cut(df_map[, "map_mapval"]
      #                              , breaks = cut_brk
      #                              , labels = cut_lab
      #                              , include.lowest = TRUE
      #                              , right = FALSE
      #                              , ordered_result = TRUE)
      leg_col <- c("blue"
                   , "green"
                   , "darkgreen"
                   , "lightgreen"
                   , "yellow"
                   , "gray"
                   , "brown"
                   , "orange"
                   , "purple"
                   , "red"
                   , "#808080"
      )
      leg_nar <- c("1"
                   , "2"
                   , "2.5"
                   , "3"
                   , "3.5"
                   , "4"
                   , "4.5"
                   , "5"
                   , "5.5"
                   , "6"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapval == leg_nar[1] ~ leg_col[1]
                                     , map_mapval == leg_nar[2] ~ leg_col[2]
                                     , map_mapval == leg_nar[3] ~ leg_col[3]
                                     , map_mapval == leg_nar[4] ~ leg_col[4]
                                     , map_mapval == leg_nar[5] ~ leg_col[5]
                                     , map_mapval == leg_nar[6] ~ leg_col[6]
                                     , map_mapval == leg_nar[7] ~ leg_col[7]
                                     , map_mapval == leg_nar[8] ~ leg_col[8]
                                     , map_mapval == leg_nar[9] ~ leg_col[9]
                                     , map_mapval == leg_nar[10] ~ leg_col[10]
                                     , TRUE ~ leg_col[11]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- size_default
    } else if (sel_map_datatype == "Fuzzy Temp Model") {
      leg_title <- "Fuzzy Temp Model"
      ## v1
      # leg_col <- c("#00B0F0"
      #              , "#8EA9DB"
      #              , "#8EA9DB"
      #              , "#8EA9DB"
      #              , "#B4C6E7"
      #              , "#BDD7EE"
      #              , "#BDD7EE"
      #              , "#BDD7EE"
      #              , "#DDEBF7"
      #              , "#F2F2F2"
      #              , "#F2F2F2"
      #              , "#F2F2F2"
      #              , "#F8CBAD"
      #              , "#808080"
      # )
      ## v2
      # leg_col <- c(blues9[9]
      #              , blues9[8]
      #              , blues9[8]
      #              , blues9[8]
      #              , blues9[7]
      #              , blues9[6]
      #              , blues9[6]
      #              , blues9[6]
      #              , blues9[5]
      #              , blues9[4]
      #              , blues9[4]
      #              , blues9[4]
      #              , "#F8CBAD"
      #              , "#808080"
      # )
      ## v3
      leg_col <- c("#140AE6"
                   , "#0066FF"
                   , "#7B9BF5"
                   , "#7B9BF5"
                   , "#0AE1EC"
                   , "#9AF3FC"
                   , "#BEFEFB"
                   , "#DDFBFF"
                   , "#DDFBFF"
                   , "#C6FFB9"
                   , "#34FB25"
                   , "#FFFF66"
                   , "#FFFFE5"
                   , "#FFFFE5"
                   , "#E4DFEC"
                   , "#FFC000"
                   , "#808080"
      )
      leg_nar <- c("VeryCold"
                   , "VCold_Cold"
                   , "TIE_VCold_Cold"
                   , "TIE_Cold_VCold"
                   , "Cold_VCold"
                   , "Cold"
                   , "Cold_Cool"
                   , "TIE_Cold_Cool"
                   , "TIE_Cool_Cold"
                   , "Cool_Cold"
                   , "Cool"
                   , "Cool_Warm"
                   , "TIE_Cool_Warm"
                   , "TIE_Warm_Cool"
                   , "Warm_Cool"
                   , "Warm"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar == leg_nar[1] ~ leg_col[1]
                                     , map_mapnar == leg_nar[2] ~ leg_col[2]
                                     , map_mapnar == leg_nar[3] ~ leg_col[3]
                                     , map_mapnar == leg_nar[4] ~ leg_col[4]
                                     , map_mapnar == leg_nar[5] ~ leg_col[5]
                                     , map_mapnar == leg_nar[6] ~ leg_col[6]
                                     , map_mapnar == leg_nar[7] ~ leg_col[7]
                                     , map_mapnar == leg_nar[8] ~ leg_col[8]
                                     , map_mapnar == leg_nar[9] ~ leg_col[9]
                                     , map_mapnar == leg_nar[10] ~ leg_col[10]
                                     , map_mapnar == leg_nar[11] ~ leg_col[11]
                                     , map_mapnar == leg_nar[12] ~ leg_col[12]
                                     , map_mapnar == leg_nar[13] ~ leg_col[13]
                                     , TRUE ~ leg_col[14]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- size_default
    } else if (sel_map_datatype == "MTTI") {
      leg_title <- "MTTI"
      leg_col <- c("#00B0F0"
                   , "#9AF3FC"
                   , "#92D050"
                   , "#FFFF00"
                   , "#FFC000"
                   , "#808080"
      )
      leg_nar <- c("< 16"
                   , "16 - 18.9"
                   , "19 - 20.9"
                   , "21 - 22.9"
                   , ">= 23"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar == leg_nar[1] ~ leg_col[1]
                                     , map_mapnar == leg_nar[2] ~ leg_col[2]
                                     , map_mapnar == leg_nar[3] ~ leg_col[3]
                                     , map_mapnar == leg_nar[4] ~ leg_col[4]
                                     , map_mapnar == leg_nar[5] ~ leg_col[5]
                                     , TRUE ~ leg_col[6]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- df_map$map_mapval
    } else if (sel_map_datatype == "BDI") {
      leg_title <- "BioDiversity Index"
      cut_brk <- c(0, 20, 30, 999)
      cut_lab <- c("gray", "lightgreen", "blue")
      leg_col <- rev(cut_lab)
      leg_nar <- rev(c("Low", "Medium", "High"))
      df_map[, "map_color"] <- cut(df_map[, "map_mapval"]
                                   , breaks = cut_brk
                                   , labels = cut_lab
                                   , include.lowest = TRUE
                                   , right = FALSE
                                   , ordered_result = TRUE)
      df_map[, "map_size"] <- size_default
      # REVERSE ORDER FOR LEGEND


    } else if (sel_map_datatype == "Thermal Metrics, nt_ti_stenocold") {
      leg_title <- "cold stenotherm taxa"
      leg_col <- c("#00B0F0"
                   , "#9AF3FC"
                   , "#808080"
      )
      leg_nar <- c(">= 3"
                   , "1 or 2"
                   , "absent"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar == leg_nar[1] ~ leg_col[1]
                                     , map_mapnar == leg_nar[2] ~ leg_col[2]
                                     , TRUE ~ leg_col[3]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- df_map$map_mapval
    } else if (sel_map_datatype == "Thermal Metrics, nt_ti_stenocold_cold") {
      leg_title <- "cold stenotherm + cold taxa"
      leg_col <- c("#00B0F0"
                   , "#9AF3FC"
                   , "#92D050"
                   , "#FFFF00"
                   , "#808080"
      )
      leg_nar <- c(">= 10"
                   , "5 - 9"
                   , "3 or 4"
                   , "1 or 2"
                   , "absent"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar %in% leg_nar[1] ~ leg_col[1]
                                     , map_mapnar == leg_nar[2] ~ leg_col[2]
                                     , map_mapnar == leg_nar[3] ~ leg_col[3]
                                     , map_mapnar == leg_nar[4] ~ leg_col[4]
                                     , TRUE ~ leg_col[5]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- df_map$map_mapval
    } else if (sel_map_datatype == "Thermal Metrics, nt_ti_stenocold_cold_cool") {
      leg_title <- "# cold stenotherm + cold + cool taxa"
      leg_col <- c("#00B0F0"
                   , "#9AF3FC"
                   , "#92D050"
                   , "#FFFF00"
                   , "#FFC000"
                   , "#808080"
      )
      leg_nar <- c(">= 30"
                   , "25 - 29"
                   , "20 - 24"
                   , "9 - 19"
                   , "< 9"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar == leg_nar[1] ~ leg_col[1]
                                     , map_mapnar == leg_nar[2] ~ leg_col[2]
                                     , map_mapnar == leg_nar[3] ~ leg_col[3]
                                     , map_mapnar == leg_nar[4] ~ leg_col[4]
                                     , map_mapnar == leg_nar[5] ~ leg_col[5]
                                     , TRUE ~ leg_col[6]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- df_map$map_mapval
    } else if (sel_map_datatype == "Thermal Metrics, pt_ti_stenocold_cold_cool") {
      leg_title <- "% cold stenotherm + cold + cool taxa"
      leg_col <- c("#00B0F0"
                   , "#9AF3FC"
                   , "#92D050"
                   , "#FFFF00"
                   , "#FFC000"
                   , "#808080"
      )
      leg_nar <- c(">= 65"
                   , "50 - 64.9"
                   , "35 - 49.9"
                   , "20 - 34.9"
                   , "< 20"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar == leg_nar[1] ~ leg_col[1]
                                     , map_mapnar == leg_nar[2] ~ leg_col[2]
                                     , map_mapnar == leg_nar[3] ~ leg_col[3]
                                     , map_mapnar == leg_nar[4] ~ leg_col[4]
                                     , map_mapnar == leg_nar[5] ~ leg_col[5]
                                     , TRUE ~ leg_col[6]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- df_map$map_mapval
    } else if (sel_map_datatype == "Thermal Metrics, pi_ti_stenocold_cold_cool") {
      leg_title <- "% cold stenotherm + cold + cool indiv"
      leg_col <- c("#00B0F0"
                   , "#9AF3FC"
                   , "#92D050"
                   , "#FFFF00"
                   , "#FFC000"
                   , "#808080"
      )
      leg_nar <- c(">= 55"
                   , "40 - 54.9"
                   , "30 - 39.9"
                   , "10 - 29.9"
                   , "< 10"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar == leg_nar[1] ~ leg_col[1]
                                     , map_mapnar == leg_nar[2] ~ leg_col[2]
                                     , map_mapnar == leg_nar[3] ~ leg_col[3]
                                     , map_mapnar == leg_nar[4] ~ leg_col[4]
                                     , map_mapnar == leg_nar[5] ~ leg_col[5]
                                     , TRUE ~ leg_col[6]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- df_map$map_mapval
    } else if (sel_map_datatype == "Thermal Metrics, pt_ti_warm_stenowarm") {
      leg_title <- "% warm + warm stenotherm taxa"
      leg_col <- c("#00B0F0"
                   , "#9AF3FC"
                   , "#92D050"
                   , "#FFFF00"
                   , "#FFC000"
                   , "#808080"
      )
      leg_nar <- c("< 5"
                   , "5 - 9.9"
                   , "10 - 14.9"
                   , "15 - 39.9"
                   , ">= 40"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar == leg_nar[1] ~ leg_col[1]
                                     , map_mapnar == leg_nar[2] ~ leg_col[2]
                                     , map_mapnar == leg_nar[3] ~ leg_col[3]
                                     , map_mapnar == leg_nar[4] ~ leg_col[4]
                                     , map_mapnar == leg_nar[5] ~ leg_col[5]
                                     , TRUE ~ leg_col[6]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- df_map$map_mapval
    } else if (sel_map_datatype == "Thermal Metrics, nt_ti_warm_stenowarm") {
      leg_title <- "warm stenotherm taxa"
      leg_col <- c("#FFC000"
                   , "#808080"
      )
      leg_nar <- c(">= 2"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar == leg_nar[1] ~ leg_col[1]
                                     , TRUE ~ leg_col[2]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- df_map$map_mapval
    } else {
      leg_title <- NA
      df_map[, "map_color"] <- "gray"
      df_map[, "map_size"] <- size_default
      leg_col <- "gray"
      leg_nar <- no_narrative
    }## IF ~ sel_datatype ~ COLOR



    ### Map ----
    # Bounding box
    map_bbox <- c(min(df_map[, sel_map_col_xlong], na.rm = TRUE)
                  , min(df_map[, sel_map_col_ylat], na.rm = TRUE)
                  , max(df_map[, sel_map_col_xlong], na.rm = TRUE)
                  , max(df_map[, sel_map_col_ylat], na.rm = TRUE)
    )

    #~~~~~~~~~~~~~~~~~~~~~~
    # repeat code from base
    #~~~~~~~~~~~~~~~~~~~~~~
    # zoom levels, https://leafletjs.com/examples/zoom-levels/

    #leaflet() %>%
    leafletProxy("map_leaflet", data = df_map) %>%
      # Groups, Base
      # addProviderTiles("CartoDB.Positron"
      #                  , group = "Positron") %>%
      # addProviderTiles(providers$Stamen.TonerLite
      #                  , group = "Toner Lite") %>%
      # addProviderTiles(providers$OpenStreetMap
      #                  , group = "Open Street Map") %>%
      clearControls() %>%
      clearShapes() %>%
      clearMarkers() %>%
      # Groups, Overlay
      # addCircles(lng = ~map_xlong
      #            , lat = ~map_ylat
      #            , color = ~map_color
      #            , popup = ~map_popup
      #            , radius = ~map_size
      #            , group = "Samples") %>%
      addCircleMarkers(lng = ~map_xlong
                       , lat = ~map_ylat
                       , color = ~map_color
                       , popup = ~map_popup
                       #, radius = ~map_size
                       , fill = ~map_color
                       , stroke = TRUE
                       , fillOpacity = 0.75
                       , group = "Samples"
                       , clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5
                                                               , showCoverageOnHover = TRUE
                                                               , freezeAtZoom = 13)
      ) %>%
      # Test different points
      # addAwesomeMarkers(lng = ~map_xlong
      #                   , lat = ~map_ylat
      #                   , popup = ~map_popup
      #                   , clusterOptions = markerClusterOptions()) %>%
      # Legend
      addLegend("bottomleft"
                , colors = leg_col
                , labels = leg_nar
                , values = NA
                , title = leg_title) %>%
      # Layers, Control
      addLayersControl(baseGroups = c("Positron"
                                      , "Open Street Map"
                                      , "ESRI World Imagery")
                       , overlayGroups = c("Samples"
                                           , "Ecoregions, Level III"
                                           #, "BCG Class"
                                           # , "NorWeST"
                                           # , "NHD+ Catchments"
                                           # , "NHD+ Flowlines"
                       )
      ) %>%
      # Layers, Hide
      hideGroup(c("Ecoregions, Level III"
                  # , "BCG Class"
                  # , "NorWeST"
                  # , "NHD+ Catchments"
                  # , "NHD+ Flowlines"
      )) %>%
      # Bounds
      fitBounds(map_bbox[1], map_bbox[2], map_bbox[3], map_bbox[4])


  })## MAP, Leaflet, PROXY
})##shinyServer ~ END
