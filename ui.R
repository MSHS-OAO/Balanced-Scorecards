library(shiny)
library(shinyWidgets)
library(shinydashboard)

# Default values for global filters -------------------------------------------------------------------------
default_campus <- sort(unique(metrics_final_df$Site))
campus_choices <- sort(unique(metrics_final_df$Site))
default_service <- sort(unique(metrics_final_df$Service))[1]
service_choices <- sort(unique(metrics_final_df$Service))
default_month <- unique(metrics_final_df$Reporting_Month)[length(unique(metrics_final_df$Reporting_Month))]
month_choices <- unique(metrics_final_df$Reporting_Month)


ui <- 
  fluidPage(
    
    tags$style(type = 'text/css', 
               '.navbar { background-color: #dddedd; color: black; font-size: 24px; font-weight: bold;}',
               '.navbar-default .navbar-brand{color: black; font-size: 24px;}'
    ),
    
    tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
    color:#fff;
    background:#221f72
    }
    .box.box-solid.box-primary{
    border-bottom-color:#ffffff;
    border-left-color:#ffffff;
    border-right-color:#ffffff;
    border-top-color:#ffffff;
    }
                    ")),
    
    tags$style(
      "h3{
      margin-top: -0.7em;
      }"
    ),
    
    tags$style(
      "h4{
      font-weight: bold;
      margin-top: -0.7em;
      }"
    ),
  navbarPage("MSHS KPI Dashboard",
             
             header = tagList(
               useShinydashboard()
             ),
             
             # First Tab - Summary - All Sites
             tabPanel("Summary", value = "summary",
                      fluidRow(
                        column(2, 
                               box(
                                 title = NULL, solidHeader = FALSE, width = 12,
                               pickerInput("selectedService", label = h4("Select Department:"), 
                                           choices = service_choices,
                                           multiple = FALSE,
                                           options = pickerOptions(
                                             liveSearch = TRUE,
                                             actionsBox = TRUE,
                                             dropupAuto = FALSE,
                                             size = 10),
                                           selected = default_service))),
                        column(2,
                               box(
                                 title = NULL, solidHeader = FALSE, width = 12,
                               pickerInput("selectedMonth", label = h4("Select Reporting Month:"), 
                                           choices = month_choices,
                                           multiple = FALSE,
                                           options = pickerOptions(
                                             liveSearch = TRUE,
                                             actionsBox = TRUE,
                                             dropupAuto = FALSE,
                                             size = 10),
                                           selected = default_month)))),
                      hr(), 
                      fluidRow(textOutput("siteSummary_title")),
                      tags$head(tags$style("#siteSummary_title{color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 30px}")), br(), br(),
                      fluidRow(
                        column(12, tableOutput("siteSummary_table") %>% 
                                 withSpinner(type = 8, color = "#dddedd"))
                      )
             ), # Close tabPanel Summary
             
             # Second Tab - All Sites by KPI
             tabPanel("Site Comparison", value = "comparison",
                      fluidRow(
                        column(2, 
                               box(
                                 title = NULL, solidHeader = FALSE, width = 12,
                               pickerInput("selectedCampus2", label = h4("Select Site:"),
                                           choices = campus_choices,
                                           multiple = TRUE,
                                           options = pickerOptions(
                                             liveSearch = TRUE,
                                             actionsBox = TRUE,
                                             dropupAuto = FALSE,
                                             size = 10),
                                           selected = default_campus))),
                        column(2, 
                               box(
                                 title = NULL, solidHeader = FALSE, width = 12,
                               pickerInput("selectedService2", label = h4("Select Department:"), 
                                           choices = service_choices,
                                           multiple = FALSE,
                                           options = pickerOptions(
                                             liveSearch = TRUE,
                                             actionsBox = TRUE,
                                             dropupAuto = FALSE,
                                             size = 10),
                                           selected = default_service))),
                        column(2,
                               box(
                                 title = NULL, solidHeader = FALSE, width = 12,
                               pickerInput("selectedMonth2", label = h4("Select Reporting Month:"), 
                                           choices = month_choices,
                                           multiple = FALSE,
                                           options = pickerOptions(
                                             liveSearch = TRUE,
                                             actionsBox = TRUE,
                                             dropupAuto = FALSE,
                                             size = 10),
                                           selected = default_month)))
                        ),
                      hr(), 
                      fluidRow(textOutput("siteComp_title")),
                      tags$head(tags$style("#siteComp_title{color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 30px}")), br(),
                      fluidRow(
                        column(12, tableOutput("siteComp_table") %>% 
                                 withSpinner(type = 8, color = "#dddedd"))
                      )
             ), # Close tabPanel Comparison
             
             # Third Tab - Breakout
             tabPanel("KPI Breakout", value = "breakout",
                      fluidRow(
                        column(2, 
                               box(
                                 title = NULL, solidHeader = FALSE, width = 12,
                               pickerInput("selectedCampus3", label = h4("Select Site:"),
                                           choices = campus_choices,
                                           multiple = FALSE,
                                           options = pickerOptions(
                                             liveSearch = TRUE,
                                             actionsBox = TRUE,
                                             dropupAuto = FALSE,
                                             size = 10),
                                           selected = "MSH"))),
                        column(2, 
                               box(
                                 title = NULL, solidHeader = FALSE, width = 12,
                               pickerInput("selectedService3", label = h4("Select Department:"), 
                                           choices = service_choices,
                                           multiple = FALSE,
                                           options = pickerOptions(
                                             liveSearch = TRUE,
                                             actionsBox = TRUE,
                                             dropupAuto = FALSE,
                                             size = 10),
                                           selected = default_service))),
                        column(2,
                               box(
                                 title = NULL, solidHeader = FALSE, width = 12,
                               pickerInput("selectedMonth3", label = h4("Select Reporting Month:"), 
                                           choices = month_choices,
                                           multiple = FALSE,
                                           options = pickerOptions(
                                             liveSearch = TRUE,
                                             actionsBox = TRUE,
                                             dropupAuto = FALSE,
                                             size = 10),
                                           selected = default_month)))
                        ),
                      hr(), 
                      fluidRow(textOutput("siteBreakout_title")),
                      tags$head(tags$style("#siteBreakout_title{color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 30px}")), br(), br(),
                      fluidRow(
                        column(12, tableOutput("siteBreakout_table") %>% 
                                 withSpinner(type = 8, color = "#dddedd"))
                      )
             ), # Close tabPanel Breakdout
             # Fourth Tab - Operational Metrics
             
             
             navbarMenu("Data",
                        tabPanel("Finance",
                                 span("Finance Data Upload", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"), br(), br(), hr(),
                                
                                        fluidRow(
                                          column(2,
                                                 textInput("name_finance", (labelMandatory("1. Please enter name:"))),
                                                 
                                          )
                                        ),
                                 tabBox(title = NULL, id = "tabset8", width = "100%", type = "pills",
                                        tabPanel("Finance", br(),
                                                 fileInput("finance_msh_msm_msq", label = "Please upload Finance data for MSH and MSQ"),
                                                 fileInput("finance_msbi_msb_msw", label = "Please upload Finance data for MSBI, MSB, MSM, and MSW"),
                                                 actionButton("submit_finance", label = "Submit")
                               
                                      ),
                                      tabPanel("Census Days", br(),
                                               fileInput("finance_census", label = "Please upload Census Days data"),
                                               actionButton("submit_finance", label = "Submit")
                                               ),
                                      tabPanel("Overtime", br(),
                                               fileInput("finance_overtime", label = "Please upload Overtime data"),
                                               actionButton("submit_finance", label = "Submit")
                                               )
                                 )
                        ),
                        tabPanel("Press Ganey", value = "press_ganey",
                                 span("Press Ganey Data Upload", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"), br(), br(), hr(),
                                 fileInput("press_ed", label = "Please upload Press Ganey ED data"),
                                 fileInput("press_nursing", label = "Please upload Press Ganey Nursing data"),
                                 fileInput("press_support", label = "Please upload Press Ganey Support Services data"),
                                 actionButton("submit_press", label = "Submit")),
                        tabPanel("Productivity", value = "productivity",
                                 span("Productivity Data Upload", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"), br(), br(), hr(),
                                 fileInput("productiviy_data", label = "Please upload Productivity data"), br(),
                                 fileInput("productiviy_data_nursing_radiology", label = "Please upload Nursing and Radiology Productivity data"),
                                 actionButton("submit_prod", label = "Submit")
                        ),
                        tabPanel("Operational Metrics - Biomed/Clinical Eningeering",
                                 span("Operational Metrics - Biomed/Clinical Eningeering", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"), br(), br(), hr(),
                                 tabBox(title = NULL, id = "tabset9", width = "100%", type = 'pills', 
                                        tabPanel("Disruptions and Issues", hr(),
                                                 fluidRow(
                                                   column(12,
                                                          div(
                                                            id = "form_biomed_disruptions",
                                                            fluidRow(
                                                              column(2,
                                                                     textInput("name_biomed_distruptions", (labelMandatory("Name:")), "")
                                                              )
                                                            )
                                                            #####Enter datatables here
                                                          )
                                                   )
                                                 )
                                        ),
                                        tabPanel("KPIs", hr(),
                                                 fluidRow(
                                                   column(12,
                                                          div(
                                                            id = "form_biomed_kpi",
                                                            fluidRow(
                                                              column(2,
                                                                     textInput("name_biomed_kpi", (labelMandatory("Name:")), "")
                                                              )
                                                            )
                                                            #####Enter datatables here
                                                          )
                                                   )
                                                 ))
                                 )
                                 
                        ),
                        tabPanel("Operational Metrics - Engineering",
                                 span("Operational Metrics - Eningeering", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"), br(), br(), hr(),
                                 tabBox(title = NULL, id = "tabset10", width = "100%", type = 'pills', 
                                        tabPanel("CM KPI", hr(),
                                                 fluidRow(
                                                   column(12,
                                                          div(
                                                            id = "form_engineering_kpi",
                                                            fluidRow(
                                                              column(2,
                                                                     textInput("name_engineering_kpi", (labelMandatory("1. Please enter name:"))),
                                                                     
                                                              )
                                                            ),
                                                            h2("2. Please enter data in the tables below"),
                                                            br(),
                                                            div(id = "header_custom_engineer",
                                                                h3("Please leave cell blank if data has not been recieved")
                                                            ),
                                                            h2("3. Once finished please click on the button below"),
                                                            #####Enter datatables here
                                                            h2("Engineering - CM KPI"),
                                                            rHandsontableOutput("engineering_kpi"),
                                                            hr(),
                                                            actionButton("submit_engineering", "Submit", class = "btn-primary"),
                                                          )
                                                   )
                                                 ))
                                 )
                        ),
                        tabPanel("Operational Metrics - Environmental Services", value = "evs",
                                 span("Operational Metrics - Environmental Services", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"), br(), br(), hr(),
                                 fluidRow(
                                   column(2,
                                          textInput("name_evs", (labelMandatory("1. Please enter name:"))),
                                          
                                   )
                                 ),
                                 tabBox(title = NULL, id = "tabset7", width = "100%", type = 'pills',      
                                        tabPanel("Turn around Time", hr(),
                                                 fileInput("evs_data", label = "Please upload Environmental Services data"),
                                                 actionButton("submit_evs", "Submit", class = "btn-primary"),
                                        )
                                 )
                                 ),
                        
                        tabPanel("Operational Metrics - Food Services", value = "operational",
                                 shinyjs::useShinyjs(),
                                 shinyjs::inlineCSS(appCSS),
                                 span("Operational Metrics - Food Services", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"), br(), br(), hr(),
                                 tabBox(title = NULL, id = "tabset7", width = "100%", type = "pills",
                                        tabPanel("Cost and Revenue", br(),
                                                 fileInput("food_cost_and_revenue", label = "Please upload Cost and Revenue data"),
                                                 actionButton("submit_food", label = "Submit")
                                        )
                                 )
                                 
                        ),
                        tabPanel("Operational Metrics - Lab", value = "lab",
                                 span("Operational Metrics - Lab",
                                      style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 br(),
                                 hr(),
                                 tabBox(title = NULL, id = "tabset7", width = "100%", type = 'pills',      
                                        tabPanel("Turnaround Time", hr(),
                                                 fileInput("lab_scc", label = "Please upload SCC lab data"),
                                                 fileInput("lab_sun", label = "Please upload Sunquest lab data"),
                                                 actionButton("submit_lab_tat", "Submit", class = "btn-primary"),
                                                 ),
                                        tabPanel("Proficiency Testing", hr(),
                                                 fluidRow(
                                                   column(12,
                                                          div(
                                                            id = "form",
                                                            fluidRow(
                                                              column(2,
                                                                     textInput("lab_pt_username",
                                                                               labelMandatory(
                                                                                 "1. Please enter name:")
                                                                               )
                                                                     )
                                                            ),
                                                            h2("2. Please enter data in the tables below."),
                                                            br(),
                                                            div(id = "header_custom",
                                                                h3("Please leave cell blank if data has not been received.")
                                                                ),
                                                            h2("3. Please click on Submit when finished."),
                                                            hr(),
                                                            h2("Lab & Blood Bank Proficiency Testing"),
                                                            rHandsontableOutput("lab_prof_test"),
                                                            hr()
                                                          ))
                                                 ),
                                                 actionButton("submit_lab_pt", "Submit", class = "btn-primary")
                                                 )
                                        )
                        ),
                        tabPanel("Operational Metrics - Patient Transport",
                                 span("Operational Metrics - Patient Transport", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"), br(), br(), hr(),
                                 tabBox(title = NULL, id = "tabset11", width = "100%", type = 'pills',
                                        tabPanel("Turnaround Time-Non Patient Transport", hr(),
                                                 fileInput("non_patient_transport", label = "Please upload Non Patient Transport Metrics data"),
                                                 actionButton("submit_npt_tat", "Submit", class = "btn-primary"),
                                        ),
                                        tabPanel("Turnaround Time-Patient Transport", hr(),
                                                 fileInput("patient_transport", label = "Please upload PTET Metrics data"),
                                                 actionButton("submit_pt_tat", "Submit", class = "btn-primary"),
                                        )
                                        
                    
                                 )
                        ),
                        tabPanel("Operational Metrics - Security",
                                 span("Operational Metrics - Security", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"), br(), br(), hr(),
                                 tabBox(title = NULL, id = "tabset8", width = "100%", type = 'pills', 
                                        tabPanel("Incident Reports", hr(),
                                                 fluidRow(
                                                   column(12,
                                                          div(
                                                            id = "form",
                                                            fluidRow(
                                                              column(2,
                                                                     textInput("sec_inc_rpts_username",
                                                                               labelMandatory(
                                                                                 "1. Please enter name:")
                                                                                )
                                                                     )
                                                              ),
                                                            h2("2. Please enter data in the table below."),
                                                            br(),
                                                            div(id = "header_custom",
                                                                h3("Please leave cell blank if data has not been received.")
                                                                ),
                                                            h2("3. Please click Submit when finished."),
                                                            hr(),
                                                            h2("Security Incident Reports"),
                                                            rHandsontableOutput("sec_inc_rpts"),
                                                            hr()
                                                          )
                                                   )
                                                   ),
                                                 actionButton("submit_sec_inc_rpts", "Submit", class = "btn-primary")
                                        ),
                                        tabPanel("Security Events", hr(),
                                                 fluidRow(
                                                   column(12,
                                                          div(
                                                            id = "form_security_events",
                                                            fluidRow(
                                                              column(2,
                                                                     textInput("sec_events_username",
                                                                               labelMandatory(
                                                                                 "1. Please enter name:")
                                                                               )
                                                                     )
                                                              ),
                                                            h2("2. Please enter data in the table below."),
                                                            br(),
                                                            div(id = "header_custom",
                                                                h3("Please leave cell blank if data has not been received.")
                                                                ),
                                                            h2("3. Please click submit when finished."),
                                                            hr(),
                                                            h2("Security Events"),
                                                            rHandsontableOutput("sec_events"),
                                                            hr()
                                                          )
                                                   )
                                                 ),
                                                 actionButton("submit_sec_events", "Submit", class = "btn-primary")
                                        )
                                 )
                        )
                        
             )# Close tabPanel Breakout
             
          ), # Close NavBar
          
  tags$style(HTML("
 .handsontable  .htDimmed {
 color: #000;
 }
                    ")),
  
  tags$style(HTML("
 .handsontable  .htRight.htNumeric {
 color: #000;
 }
                    ")),
  
  ##Tabset pills html
  tags$style(HTML("
        .nav-tabs-custom > .nav > li[class=active] > a {
           background-color: #d80b8c;
           color: #FFF;
           border-top-color:  #d80b8c;
        }")),
  
  tags$style(HTML("
        .nav-tabs-custom {
          font-size: 15px
        }")),
  
  tags$style(HTML("
        #submit_prod {
          background-color: #d80b8c;
          color: #FFFFFF;
        }")),
  
  tags$style(HTML("
        #submit_finance {
          background-color: #d80b8c;
          color: #FFFFFF;
        }")),
  
  tags$style(HTML("
        #submit_food {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),
 
  tags$style(HTML("
        #submit_evs {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),
 
  tags$style(HTML("
        #submit_engineering {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),
 
 tags$style(HTML("
        #submit_lab_tat {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),
 
 tags$style(HTML("
        #submit_lab_pt {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),
 
 tags$style(HTML("
        #submit_sec_inc_rpts {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),
 tags$style(HTML("
        #submit_sec_events {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")), 

  
  tags$style(HTML("
        #submit_press {
          background-color: #d80b8c;
          color: #FFFFFF;
        }")),
  
  tags$style(HTML("
        #submit_budget {
          background-color: #d80b8c;
          color: #FFFFFF;
        }")),
  
  tags$style(HTML("
        #submit_overtime {
          background-color: #d80b8c;
          color: #FFFFFF;
        }")),
  tags$style(HTML("
        #header_custom {
          color: #ff0000;
          margin-left: 50px;
        }")),
  tags$style(HTML("
        #header_custom_engineer {
          color: #ff0000;
          margin-left: 50px;
        }")),
  tags$style(HTML("
        #header_custom_evs {
          color: #ff0000;
          margin-left: 50px;
        }")),
  
  tags$style(type = 'text/css', 
             '.navbar { background-color: #dddedd; color: black; font-size: 24px; font-weight: bold;}',
             '.navbar-default .navbar-brand{color: black; font-size: 24px;}'
  ) 
             
             
  ) # Close navbarPage
