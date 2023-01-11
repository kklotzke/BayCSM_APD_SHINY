library(coda)
library(DT)
# library(future)
# library(ipc)
# library(promises)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
# plan(multicore)


source("Backend/BCSM Sampler.R")
source("Backend/Construct Classification.R")
source("Backend/Construct Table Results.R")
source("Backend/Construct Text Results.R")
source("Backend/Generate Data.R")
source("Backend/MCMC Plots.R")

# Server logic
server <- function(input, output, session) {
  
  reactiveData <- reactiveValues()
  reactiveFS <- reactiveValues()
  reactiveOutput <- reactiveValues()
  reactiveOutput$mcmcFinished <- FALSE

  usrwd <- paste0("www/",format(Sys.time(),"%Y%m%d%H%M%S"))
  dir.create(usrwd, recursive = TRUE)
  
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
  output$summaryData <- renderText({ "Not selected"})
  output$text_data_nodata <- renderText({ "No dataset selected." })
  observeEvent(input$button_data_sample, {
    #updateTabsetPanel(session, "menutabs",selected = "gettingstarted")
    reactiveData$dataMat <- GenerateData(as.numeric(input$select_data_N))$Y
    output$text_data_generated <- renderText({paste0("Data matrix generated (", nrow(reactiveData$dataMat), " rows ", ncol(reactiveData$dataMat) + 1, " columns)")})
    output$summaryData <- renderText({paste0(nrow(reactiveData$dataMat), " respondents ", ncol(reactiveData$dataMat), " items")})
    output$dataMatrix <- renderDT({
      df <- as.data.frame(reactiveData$dataMat)
      df <- cbind('Respondent' = 1:nrow(df), df) 
      datatable(df, selection = 'none', rownames = FALSE, filter = "none", escape = TRUE, 
                options = list(autoWidth = TRUE, dom = "tp", ordering = FALSE, 
                               paging = TRUE, pageLength = 25, scrollX = TRUE), class = "compact")
    })
    output$analyse_message <- NULL
    output$text_data_nodata <- NULL
    reactiveOutput$mcmcFinished <- FALSE
  })
  
  mcmcrunning <- FALSE
  observe({
    session$onSessionEnded(function() {
      unlink(usrwd, recursive = TRUE)
    })
    
    reactiveFS$CM <- NULL
    if (!is.null(input$checkFS)) {
      reactiveFS$f1 <- reactiveFS$f2 <- reactiveFS$f8 <- reactiveFS$PF <- FALSE
      for (ii in 1:length(input$checkFS)) {
        if (input$checkFS[ii] == "f1")
          reactiveFS$f1 <- TRUE
        else if (input$checkFS[ii] == "f2")
          reactiveFS$f2 <- TRUE
        else if (input$checkFS[ii] == "f8")
          reactiveFS$f8 <- TRUE
        else if (input$checkFS[ii] == "PF")
          reactiveFS$PF <- TRUE
      }
      reactiveFS$CM <- ConstructCM(f1 = reactiveFS$f1, f2 = reactiveFS$f2, f8 = reactiveFS$f8, PF = reactiveFS$PF)
      output$classMatrix <- renderDT({
        df <- as.data.frame(t(reactiveFS$CM$u1))
        df <- cbind('Item' = 1:nrow(df), df)
        datatable(df, selection = 'none', rownames = FALSE, filter = "none", escape = TRUE, 
                  options = list(width = "100%", dom = "tp", ordering = FALSE, 
                                 paging = TRUE, pageLength = 18, scrollX = TRUE), class = "compact")
      })
      output$classText <- NULL
      output$Nt <- renderText({ reactiveFS$CM$Nt })
      output$analyse_message <- NULL
      reactiveOutput$mcmcFinished <- FALSE
    }
    else {
      reactiveFS$CM <- ConstructCM(f1 = reactiveFS$tmp.f1, f2 = reactiveFS$tmp.f2, f8 = reactiveFS$tmp.f8, PF = reactiveFS$tmp.PF)
      output$classMatrix <- NULL
      output$classText <- renderText({ "Model not specified." })
      output$Nt <- renderText({ "-" })
    }
  })
  output$modelid <- output$modelid2 <- renderText({ reactiveFS$CM$modelid })
  output$results_message <- renderText({ "Run analyse first." })
  output$results_message_mcmc <- renderText({ "Analyse not finished." })
  shinyjs::hide("fr_results_tables")
  shinyjs::hide("fr_results_text")
  
  observeEvent(input$button_analyse_start, {
    if (!is.null(reactiveData$dataMat) & !is.null(reactiveFS$CM$u1)) {
      output$analyse_message <- output$results_message <- output$results_message_mcmc <- NULL
      reactiveOutput$bcsmfit <- SamplerBCSMShiny(Y = reactiveData$dataMat, u = reactiveFS$CM$u1, XG = input$slider_analyse_iter, a0 = 0.0001, b0 = 0.0001, print = FALSE
                                                 , ii.progress = 10, pbid = "progress_analyse", session = session)
      reactiveOutput$f1 <- isolate(reactiveFS$f1)
      reactiveOutput$f2 <- isolate(reactiveFS$f2)
      reactiveOutput$f8 <- isolate(reactiveFS$f8)
      reactiveOutput$PF <- isolate(reactiveFS$PF)
      reactiveOutput$dataMat <- isolate(reactiveData$dataMat)
      reactiveOutput$modelid <- isolate(reactiveFS$CM$modelid)
      reactiveOutput$u <- isolate(reactiveFS$CM$u1)
      reactiveOutput$Nt <- isolate(reactiveFS$CM$Nt)
      reactiveOutput$Prior <- isolate(input$select_analyse_prior)
      reactiveOutput$XG <- isolate(input$slider_analyse_iter)
      reactiveOutput$burnin <- isolate(input$slider_analyse_burnin)
      reactiveOutput$summaryData <- renderText({paste0(nrow(reactiveOutput$dataMat), " respondents ", ncol(reactiveOutput$dataMat), " items")})
      reactiveOutput$mcmcFinished <- TRUE
      
      output$text_results_data <- reactiveOutput$summaryData
      output$text_results_model <- renderText({ reactiveOutput$modelid })
      output$text_results_Nt <- renderText({ paste0("Dimensions: ", reactiveOutput$Nt )})
      output$text_results_prior <- renderText({ paste0("Prior: ", reactiveOutput$Prior )})
      output$text_results_iter <- renderText({ paste0("Iterations: ", reactiveOutput$XG )})
      output$text_results_burnin <- renderText({ paste0("Burnin: ", reactiveOutput$burnin, "%" )})
      output$text_results_hdata <- renderText({ "Data" })
      output$text_results_hmodel <- renderText({ "Model" })
      output$text_results_hmcmc <- renderText({ "MCMC" })
      output$results_message_mcmc <- renderText({ "Report not generated." })
      
      output$text_results_hscale <- NULL
      output$text_results_hitemc <- NULL
      
      shinyjs::hide("fr_results_tables")
      shinyjs::hide("fr_results_text")
      shinyjs::hide('resultsTable_f1')
      shinyjs::hide('resultsTable_f2')
      shinyjs::hide('resultsTable_f8')
      shinyjs::hide('resultsTable_PF')
      shinyjs::hide("results_html_mcmcplots")
      
      output$resultsTable_f1 <- NULL
      output$resultsTable_f2 <- NULL
      output$resultsTable_f8 <- NULL
      output$resultsTable_PF <- NULL
      
    }
    else if (is.null(reactiveData$dataMat) & is.null(reactiveFS$CM$u1)) {
      output$analyse_message <- renderText({ "Dataset not selected. Model not specified." })
    }
    else if (is.null(reactiveData$dataMat) & !is.null(reactiveFS$CM$u1)) {
      output$analyse_message <- renderText({ "Dataset not selected." })
    }
    else if (!is.null(reactiveData$dataMat) & is.null(reactiveFS$CM$u1)) {
      output$analyse_message <- renderText({ "Model not specified." })
    }
  })
  
  observeEvent(input$button_results_generate, {
    if (reactiveOutput$mcmcFinished) {
      output$results_message <- output$results_message_mcmc <- NULL
      output$text_results_hscale <- renderText({ "Scale" })
      reactiveOutput$list.scale <- list()
      reactiveOutput$list.itemc <- list()
      reactiveOutput$labels <- character(0)

      if (reactiveOutput$f1) {
        caption1 <- "Unidimensional (f1)" 
        fid1 <- c("f1")
        fdescr1 <- c("General math ability")
        reactiveOutput$labels <- c(reactiveOutput$labels, paste0(fid1, " - ", fdescr1))
        df1 <- ConstructTableResults(samples = as.matrix(reactiveOutput$bcsmfit$Samples.tcov[, fid1]), u = t(as.matrix(reactiveOutput$u[fid1, ])), XG = reactiveOutput$XG, burnin = reactiveOutput$burnin, 
                                     fid = fid1, fdescr = fdescr1, CI.type = isolate(input$select_results_CI), CI.prob = isolate(input$text_results_CIp), digits = 3)
        reactiveOutput$list.scale[[length(reactiveOutput$list.scale) + 1]] <- df1
        output$resultsTable_f1 <- renderDT({
          datatable(df1, caption = caption1, selection = 'none', rownames = FALSE, filter = "none", escape = TRUE, 
                    options = list(width = "100%", dom = "t", ordering = FALSE, 
                                   paging = FALSE, class = "compact", columnDefs = list(list(targets = 7, visible = FALSE)))) %>% formatStyle(
                                     'CI.lower', 'CI.ind',
                                     backgroundColor = styleEqual(c(1, -1), c('LightGreen', 'LightSalmon'))
                                   ) %>% formatStyle(
                                     'CI.upper', 'CI.ind',
                                     backgroundColor = styleEqual(c(1, -1), c('LightGreen', 'LightSalmon'))
                                   )
        })
        shinyjs::show('resultsTable_f1')
      }
      if (reactiveOutput$f2) {
        caption2 <- "Multidimensional (f2)" 
        fid2 <- paste0("f2.", 1:2)
        fdescr2 <- c("Informal mathematics", "Formal mathematics")
        reactiveOutput$labels <- c(reactiveOutput$labels, paste0(fid2, " - ", fdescr2))
        df2 <- ConstructTableResults(samples = as.matrix(reactiveOutput$bcsmfit$Samples.tcov[, fid2]), u = as.matrix(reactiveOutput$u[fid2, ]), XG = reactiveOutput$XG, burnin = reactiveOutput$burnin, 
                                     fid = fid2, fdescr = fdescr2, CI.type = isolate(input$select_results_CI), CI.prob = isolate(input$text_results_CIp), digits = 3)
        reactiveOutput$list.scale[[length(reactiveOutput$list.scale) + 1]] <- df2
        output$resultsTable_f2 <- renderDT({
          datatable(df2, caption = caption2, selection = 'none', rownames = FALSE, filter = "none", escape = TRUE, 
                    options = list(width = "100%", dom = "t", ordering = FALSE, 
                                   paging = FALSE, class = "compact", columnDefs = list(list(targets = 7, visible = FALSE)))) %>% formatStyle(
                                     'CI.lower', 'CI.ind',
                                     backgroundColor = styleEqual(c(1, -1), c('LightGreen', 'LightSalmon'))
                                   ) %>% formatStyle(
                                     'CI.upper', 'CI.ind',
                                     backgroundColor = styleEqual(c(1, -1), c('LightGreen', 'LightSalmon'))
                                   )
        })
        shinyjs::show('resultsTable_f2')
      }
      
      if (reactiveOutput$f8) {
        caption3 <- "Multidimensional (f8)" 
        fid3 <- paste0("f8.", 1:8)
        fdescr3 <- c("Numbering (IM)", "Number comparisons (IM)", "Calculation (IM)", "Concepts (IM)"
                    , "Numeral literacy (FM)", "Number facts (FM)", "Calculation (FM)" , "Concepts (FM)")
        reactiveOutput$labels <- c(reactiveOutput$labels, paste0(fid3, " - ", fdescr3))
        df3 <- ConstructTableResults(samples = as.matrix(reactiveOutput$bcsmfit$Samples.tcov[, fid3]), u = as.matrix(reactiveOutput$u[fid3, ]), XG = reactiveOutput$XG, burnin = reactiveOutput$burnin, 
                                     fid = fid3, fdescr = fdescr3, CI.type = isolate(input$select_results_CI), CI.prob = isolate(input$text_results_CIp), digits = 3)
        reactiveOutput$list.scale[[length(reactiveOutput$list.scale) + 1]] <- df3
        output$resultsTable_f8 <- renderDT({
          datatable(df3, caption = caption3, selection = 'none', rownames = FALSE, filter = "none", escape = TRUE, 
                    options = list(width = "100%", dom = "t", ordering = FALSE, 
                                   paging = FALSE, class = "compact", columnDefs = list(list(targets = 7, visible = FALSE)))) %>% formatStyle(
                                     'CI.lower', 'CI.ind',
                                     backgroundColor = styleEqual(c(1, -1), c('LightGreen', 'LightSalmon'))
                                   ) %>% formatStyle(
                                     'CI.upper', 'CI.ind',
                                     backgroundColor = styleEqual(c(1, -1), c('LightGreen', 'LightSalmon'))
                                   )
        })
        shinyjs::show('resultsTable_f8')
      }
      
      
      if (reactiveOutput$PF) {
        output$text_results_hitemc <- renderText({ "Item characteristic" })
        caption4 <- "Presentation format (PF)" 
        fid4 <- paste0("PF.", 1:3)
        fdescr4 <- c("Text", "Text+image", "Image")
        reactiveOutput$labels <- c(reactiveOutput$labels, paste0(fid4, " - ", fdescr4))
        df4 <- ConstructTableResults(samples = as.matrix(reactiveOutput$bcsmfit$Samples.tcov[, fid4]), u = as.matrix(reactiveOutput$u[fid4, ]), XG = reactiveOutput$XG, burnin = reactiveOutput$burnin, 
                                     fid = fid4, fdescr = fdescr4, CI.type = isolate(input$select_results_CI), CI.prob = isolate(input$text_results_CIp), digits = 3)
        reactiveOutput$list.itemc[[length(reactiveOutput$list.itemc) + 1]] <- df4
        output$resultsTable_PF <- renderDT({
          datatable(df4, caption = caption4, selection = 'none', rownames = FALSE, filter = "none", escape = TRUE, 
                    options = list(width = "100%", dom = "t", ordering = FALSE, 
                                   paging = FALSE, class = "compact", columnDefs = list(list(targets = 7, visible = FALSE)))) %>% formatStyle(
                                     'CI.lower', 'CI.ind',
                                     backgroundColor = styleEqual(c(1, -1), c('LightGreen', 'LightSalmon'))
                                   ) %>% formatStyle(
                                     'CI.upper', 'CI.ind',
                                     backgroundColor = styleEqual(c(1, -1), c('LightGreen', 'LightSalmon'))
                                   )
        })
        shinyjs::show('resultsTable_PF')
      }
      
      # Generate text
      resultsText <- ConstructTextResults(list.scale = reactiveOutput$list.scale, list.itemc = reactiveOutput$list.itemc)

      output$results_text_scalepos <- renderText({ resultsText$results_text_scalepos })
      output$results_text_itemcpos <- renderText({ resultsText$results_text_itemcpos })
      output$results_text_scaleneg <- renderText({ resultsText$results_text_scaleneg })
      output$results_text_itemcneg <- renderText({ resultsText$results_text_itemcneg })

      if (all(is.null(c(resultsText$results_text_scalepos, resultsText$results_text_scaleneg, resultsText$results_text_itemcpos, resultsText$results_text_itemcneg)))) {
        shinyjs::hide("fr_results_text")
      }
      
      shinyjs::show("fr_results_tables")
      shinyjs::show("fr_results_text")

      # MCMC
      reactiveOutput$usrwd <- usrwd
      samples <- reactiveOutput$bcsmfit$Samples.tcov
      colnames(samples) <- reactiveOutput$labels
      mcmcfilepath <- mcmcplot_new(coda::mcmc(samples), dir = reactiveOutput$usrwd, filename = "mcmcplots", title = "", style = "gray")
      output$results_html_mcmcplots <- renderUI({ includeHTML(mcmcfilepath) })
      shinyjs::show("results_html_mcmcplots")
    }
    else {
      output$text_results_data <- NULL
      output$text_results_model <- NULL
      output$text_results_Nt <- NULL
      output$text_results_prior <- NULL
      output$text_results_iter <- NULL
      output$text_results_burnin <- NULL
      output$text_results_hdata <- NULL
      output$text_results_hmodel <- NULL
      output$text_results_hmcmc <- NULL
      output$text_results_hscale <- NULL
      output$text_results_hitemc <- NULL
      output$resultsTable_f1 <- NULL
      output$resultsTable_f2 <- NULL
      output$resultsTable_f8 <- NULL
      output$resultsTable_PF <- NULL
      output$results_message <- renderText({ "Run analyse first." })
      output$results_message_mcmc <- renderText({ "Analyse not finished." })
      shinyjs::hide("fr_results_tables")
      shinyjs::hide("fr_results_text")
      shinyjs::hide("results_html_mcmcplots")
    }
  })
}