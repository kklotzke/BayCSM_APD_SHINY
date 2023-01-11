library(DT)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

## Header 
header <- dashboardHeader(title = "BayCSM Assessment Profile Diagnostics (SAGE Prototype)", 
                          titleWidth = 1050)

## Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(id = "menutabs" 
    , menuItem("Introduction", tabName = "introduction", icon = icon("clipboard"))
    , menuItem("Dataset", tabName = "data", icon = icon("database"))
    , menuItem("Model", tabName = "model", icon = icon("project-diagram")
             # , menuSubItem('Classification structure',
             #             tabName = 'model_class',
             #             icon = icon("chevron-right"))
             # menuSubItem('Create new model',
             #             tabName = 'model_create',
             #             icon = icon("chevron-right"))
             )
    , menuItem("Analyse", tabName = "analyse", icon = icon("play"))
    , menuItem("Results", tabName = "results", icon = icon("poll"))
  )
)

## Body content
body <- dashboardBody(
    useShinyjs(), 
    tags$head(tags$style("body{min-height: 611px;height: auto; max-width: 1024px !important;margin: auto; overflow-y: scroll;}")),
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo  {
          background-color: #367fa9 !important;
          font-size: 20px;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #367fa9 !important;
          font-size: 20px;
        }
        .skin-blue .main-header .navbar {
          background-color: #367fa9 !important;
        }
      '))),

    tabItems(
    # Introduction
    tabItem(tabName = "introduction"
            , fluidRow(
                column(width = 12, offset = 0, style='padding:0px;margin-right:-10px;'
                       , box(style='margin-bottom:-10px'
                         , p(
                         "Choosing the right measurement scale for an assessment is challenging: each item and section may measure a different characteristic of an individual, and often these dimensions show overlap. Scores reported on dimension-specific scales have a potential diagnostic value over a general score. However, unnecessarily reported subscores that do not carry additional information about the individual can lead to wrong institutional decisions based on a diagnostic profile that is contaminated with statistical artifacts. Therefore, determining the scales to report the scores on is an important task."
                         , align = "justify")
                         , p(
                         "Features of this web application include:"
                         , tags$ul(
                           tags$li("Model complex test scales through multidimensional, multi-factor compositions (unidimensional, bi-factor, tri-factor, ...)"),
                           tags$li("Test for the added value of subscores"),
                           tags$li("Detect inconsistency in response behavior within a scale dimension"),
                           tags$li("Control for interactions of item characteristics and respondents"),
                           tags$li("Generate a report and view model diagnostics")
                           ) , align = "justify")
                         , hr()
                         , p(
                           "This prototype has been developed for the SAGE Concept Grants program."
                           , align = "justify")
                         , width = 12))
            )
            , fluidRow(
                  column(width = 4, "")
                , column(width = 8, offset = 4, style='padding-right:5px'
                       , column(width = 6, style='margin-left:20px;', box(tags$b("Author")
                             , tags$br()
                             , "Konrad Klotzke"
                             , tags$br()
                             , tags$em("University of Twente")
                             , width = NULL))
                       , column(width = 6, style='margin-left:-20px; margin-right:0px;', box(tags$b("Author")
                            , tags$br()
                            , "Prof. Dr. Jean-Paul Fox"
                            , tags$br()
                            , tags$em("University of Twente")
                            , width = NULL))
                )
            )
           
    ),
    
    # Data
    tabItem(tabName = "data",
            tabBox(
              title = "",
              id = "tabset_model", width = 400,
              tabPanel("Select data", 
                fluidRow(
                  box(p("For illustration, in this prototype data is randomly generated under the test structure of the Test of Early Mathematics Ability-3 (TEMA-3; Ryoo et al., 2015). "
                      , "The students' simulated responses to 48 items are scored as correct (1) or incorrect (0). ", align = "justify")
                      , p("The first half of the test consists of items measureing the students' ability in 'Informal mathematics', and the second half measures the ability in 'Formal mathematics'. "
                      , "Items within the 'Informal mathematics' cluster can be categorized as 'Numbering', 'Number comparisons', 'Calculation' and 'Concepts'. " 
                      , "Within 'Formal mathematics', the subcategories are 'Numeral literacy', 'Number facts', 'Calculation' and 'Concepts'. ", align = "justify")
                      , p("Finally, the presentation format varies between items across all (sub)categories. Thereby, the problem description for an item contains either only text, text and images or only images. ", align = "justify")
                      , hr()
                      , p("Ryoo, Ji Hoon, et al. \"Examining factor structures on the Test of Early Mathematics Ability-3: A longitudinal approach.\" ", tags$i("Learning and Individual Differences"), " 41 (2015): 21-29.", align = "justify")
                      , width = 12)
                ), 
                fluidRow(
                  box(selectInput(inputId = "select_data_N", label = "Number of respondents", 
                                  choices = list("N = 750" = 750, "N = 1000" = 1000, "N = 1500" = 1500), 
                                  selected = 750),
                       column(width = 3, actionButton(inputId ="button_data_sample", label = "Sample data"))
                      ,column(width = 9, span(textOutput("text_data_generated"), style = "color:green"))
                      , width = 12)
                ),
              ),
              tabPanel("View data",
                         fluidRow(
                           box(DTOutput("dataMatrix")
                               , textOutput("text_data_nodata")
                               , width = 12),
                      )
              )
            )
              
            
    ), 
    
    # Model
    tabItem(tabName = "model",
            tabBox(
              title = "",
              id = "tabset_model", width = 400,
              tabPanel("Specify model", 
                       fluidRow(
                         box(p("BCSM can model multidimensional, multi-factor structures that underly complex measurement scales.
                               In this prototype, a preset of four factor structures is provided. 
                               Factor structures can be freely combined." , align = "justify") 
                               , p("For example, combining 'General math ability' and 'Informal/Formal mathematics' results in a bi-factor model
                               that allows to test for the added value of subscores on the two-dimensional scale over
                               a general score on an unidimensional scale.", align = "justify") 
                               , p("Including the 'Presentation format' factor structure in the model 
                               is an example of controlling for the interaction of respondents and item characteristics.", align = "justify")
                             , width = 12)
                       ),
                       fluidRow(
                         box(
                           checkboxGroupInput("checkFS", label = h3("Select factor structure(s)"), 
                                              choices = list("General math ability (1 Dimension - all items load on a single dimension)" = "f1", 
                                                             "Informal/Formal mathematics (2 Dimensions - items load on one of two dimensions)" = "f2", 
                                                             "Informal/Formal subcategories (8 Dimensions - items load on one of eight dimensions)" = "f8", 
                                                             "Presentation format (3 Dimensions - items load on one of three dimensions)" = "PF"),
                                              selected = 0)
                           , hr()
                           , column(2, h4("Model: "))
                           , column(10, h4(textOutput("modelid")))
                           , width = 12)
                       )),
              tabPanel("Classification structure",                        
                       fluidRow(box(p("In the classification structure, associated observations are grouped in covariance layers of the BCSM additive covariance structure. 
                                      Rows and columns represent item membership and covariance layers. 
                                      Each cell indicates whether or not observations to the item (row) are grouped in the covariance layer (column). 
                                      1: Observations are grouped, 0: Observations are not grouped.", align = "justify"),  width = 12)
                       ),        
                       fluidRow(box(
                              textOutput("classText")
                            , DTOutput("classMatrix")    
                            , width = 12)
                       
                       ))
            )
    ), 
    
    # Analyse
    tabItem(tabName = "analyse",
            fluidRow(
              box(h4("Summary")
                  , column(3, "Dataset: ")
                  , column(9, textOutput("summaryData"))
                  , column(3, "Model: ")
                  , column(9, textOutput("modelid2"))
                  , column(3, "Dimensions: ")
                  , column(9, textOutput("Nt"))
                  , width = 12)
            ), 
            fluidRow(
              box(h4("Markov chain Monte Carlo sampling")
                  , selectInput(inputId = "select_analyse_prior", label = h5("Prior distribution"), 
                                choices = list("Non-informative truncated shifted-IG" = "Non-informative truncated shifted-IG"), 
                                selected = "Non-informative truncated shifted-IG")
                  , sliderInput("slider_analyse_iter", label = h5("Iterations"), min = 500, 
                                max = 4000, value = 1000, step = 100)
                  , sliderInput("slider_analyse_burnin", label = h5("Burnin (%)"), min = 5, 
                                max = 50, value = 10)
                  , hr()
                  , progressBar(id = "progress_analyse", value = 0, title = "MCMC process: idle", display_pct = TRUE)
                  , column(3, actionButton(inputId ="button_analyse_start", label = "Run"))
                  , column(9, span(textOutput("analyse_message"), style = "color:red"))
                  #, actionButton(inputId ="button_analyse_stop", label = "Abort", onclick = "Shiny.onInputChange('stopMCMC', true)")
                  , width = 12)
            )
    ), 
    
    # Results
    tabItem(tabName = "results"
            , tabBox(
              title = "",
              id = "tabset_results", width = 800
              , tabPanel("Report", 
                       fluidRow(
                         box(
                              selectInput(inputId = "select_results_CI", label = "Credible interval", 
                                           choices = list("Highest posterior density (HPD)" = "HPD"), 
                                           selected = "HPD")
                             , textInput("text_results_CIp", label = "Probability", value = "95")
                             , actionButton(inputId ="button_results_generate", label = "Generate report")
                             , width = 6)
                       , box(
                             column(12, span(textOutput("results_message"), style = "color:red"))
                           , column(12, tags$b(textOutput("text_results_hdata")))
                           , column(12, textOutput("text_results_data"))
                           , column(12, tags$b(textOutput("text_results_hmodel")))
                           , column(12, textOutput("text_results_model"))
                           , column(12, textOutput("text_results_Nt"))
                           , column(12, tags$b(textOutput("text_results_hmcmc")))
                           , column(12, textOutput("text_results_prior"))
                           , column(12, textOutput("text_results_iter"))
                           , column(12, textOutput("text_results_burnin"))
                           , width = 6)
                        ),
                       fluidRow(
                           box (
                               h3(textOutput("text_results_hscale"))
                             , DTOutput("resultsTable_f1")
                             , DTOutput("resultsTable_f2")
                             , DTOutput("resultsTable_f8")
                             , h3(textOutput("text_results_hitemc"))
                             , DTOutput("resultsTable_PF")
                             , width = 12), id = "fr_results_tables"
                       ), 
                       fluidRow(
                         box (
                              column(12, (span(textOutput("results_text_scalepos"), style = "color:LightGreen")))
                            , column(12, (span(textOutput("results_text_itemcpos"), style = "color:LightGreen")))
                            , column(12, (span(textOutput("results_text_scaleneg"), style = "color:LightSalmon")))
                            , column(12, (span(textOutput("results_text_itemcneg"), style = "color:LightSalmon")))
                            , width = 12, background = "blue"), id = "fr_results_text"
                       )
              )
              , tabPanel("MCMC diagnostics",                        
                       fluidRow(
                         box(
                             column(12, textOutput("results_message_mcmc"))
                           , column(12, htmlOutput("results_html_mcmcplots"), align = "center")
                           , width = 12)
                       ),        
                      )
            )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin = "blue")

