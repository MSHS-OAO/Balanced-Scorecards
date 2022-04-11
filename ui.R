library(shiny)
library(shinyWidgets)
library(shinydashboard)

# Default values for global filters -------------------------------------------------------------------------
default_campus <- sort(unique(metrics_final_df_new$Site))
campus_choices <- sort(unique(metrics_final_df_new$Site))
default_service <- sort(unique(metrics_final_df_new$Service))[1]
service_choices <- sort(unique(metrics_final_df_new$Service))
metric_group_choices <- sort(unique(metrics_final_df_new$Metric_Group))
default_metric_group <- metric_group_choices
# service_choices <- sort(
#   unique(
#     metrics_final_df$Service[
#       which(!(
#         metrics_final_df$Service %in% c("Emergency Department", "Nursing")
#         )
#         )
#       ]
#     )
#   )

# default_month <- unique(metrics_final_df$Reporting_Month)[length(unique(metrics_final_df$Reporting_Month))]
# month_choices <- unique(metrics_final_df$Reporting_Month)
default_month <- format(max(metrics_final_df$Reporting_Month_Ref, na.rm = TRUE), "%m-%Y")
month_choices <- format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")



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
                        # Finance Data Submission ----
                        tabPanel("Finance",
                                 span("Finance Data Upload", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                
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
                                               actionButton("submit_finance_ot", label = "Submit")
                                               )
                                 )
                        ),
                        tabPanel("Patient Experience", value = "patient_experience",
                                 span("Patient Experience Data Upload", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                 tabBox(title = NULL, id = "tabset20", width = "100%", type = 'pills',
                                        tabPanel("Monthly Data",
                                                 hr(),
                                                 fileInput("pt_exp_ed_monthly", label = "Please upload monthly Patient Experience ED data"),
                                                 hr(),
                                                 fileInput("pt_exp_nursing_monthly", label = "Please upload monthly Patient Experience Nursing data"),
                                                 hr(),
                                                 fileInput("pt_exp_support_monthly", label = "Please upload monthly Patient Experience Support Services data"),
                                                 actionButton("submit_monthly_pt_exp", label = "Submit")),
                                        tabPanel("YTD Data",
                                                 hr(),
                                                 fileInput("pt_exp_ed_ytd", label = "Please upload YTD Patient Experience ED data"),
                                                 hr(),
                                                 fileInput("pt_exp_nursing_ytd", label = "Please upload YTD Patient Experience Nursing data"),
                                                 hr(),
                                                 fileInput("pt_exp_support_ytd", label = "Please upload YTD Patient Experience Support Services data"),
                                                 actionButton("submit_ytd_pt_exp", label = "Submit"))
                                 )
                        ),
                        # Productivity Data Submission ----
                        tabPanel("Productivity", value = "productivity",
                                 span("Productivity Data Upload", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                 fileInput("productiviy_data", label = "Please upload Productivity data"), br(),
                                 fileInput("productiviy_data_nursing_radiology", label = "Please upload Nursing and Radiology Productivity data"),
                                 actionButton("submit_prod", label = "Submit")
                        ),
                        # Biomed Data Input Tab ------
                        tabPanel("Operational Metrics - Biomed/Clinical Engineering",
                                 span("Operational Metrics - Biomed/Clinical Engineering", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                 # Biomed D&I Data Submission  -----
                                 tabBox(title = NULL, id = "tabset9", width = "100%", type = 'pills', 
                                        tabPanel("Disruptions and Issues", hr(),
                                                 fluidRow(
                                                   column(12,
                                                          div(
                                                            id = "form_biomed_disruptions",
                                                            fluidRow(
                                                              column(2,
                                                                     textInput("name_biomed_distruptions", (labelMandatory("1. Please enter Name:")), "")
                                                              )
                                                            )
                                                            ,
                                                            h2("2. Please enter data in the tables below"),
                                                            br(),
                                                            div(id = "header_custom_biomed",
                                                                h3("Please leave cell blank if data has not been received")
                                                            ),
                                                            h2("3. Once finished please click on the button below"),
                                                            #####Enter datatables here
                                                            h2("Biomed/Clinical Engineering - Disruptions and Issues"),
                                                            
                                                            rHandsontableOutput("bimoed_di"),
                                                            hr(),
                                                            actionButton("submit_biomeddi", "Submit", class = "btn-primary")
                                                          )
                                                   )
                                                 )
                                        ),
                                        # Biomed KPI Data Submission ----
                                        tabPanel("KPIs", hr(),
                                                 fluidRow(
                                                   column(12,
                                                          div(
                                                            id = "form_biomed_kpi",
                                                            fluidRow(
                                                              column(2,
                                                                     textInput("name_biomed_kpi", (labelMandatory("1. Please enter Name:")), "")
                                                              )
                                                            ),
                                                            h2("2. Please enter data in the tables below"),
                                                            br(),
                                                            div(id = "header_custom_biomed",
                                                                h4("Please leave cell blank if data has not been received"),
                                                                br(),
                                                                h4("Please enter percentages (Documented Status, PM Compliance - High Risk Equipment, 
                                                                   and PM Compliance - All Medical Equipment)as 2 digit decimal between 0 and 1.")
                                                            ),
                                                            
                                                            h2("3. Once finished please click on the button below"),
                                                            #####Enter datatables here
                                                            h2("Biomed/Clinical Engineering - KPIs"),
                                                            
                                                            rHandsontableOutput("biomed_kpi"),
                                                            hr(),
                                                            actionButton("submit_biomedkpis", "Submit", class = "btn-primary")
                      
                                                          )
                                                   )
                                                 ))
                                 )
                                 
                        ),
                        # Engineering Data Submission ----
                        tabPanel("Operational Metrics - Engineering",
                                 span("Operational Metrics - Engineering", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
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
                                                                h4("Please leave cell blank if data has not been received"),
                                                                br(),
                                                                h4("Please enter percentages (EOC/Patient Care Work Order Completion Rate, and Work Order Completion Rate) as a decimal between 0 and 1.")
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
                        # Environmental Services Data Submission ----
                        tabPanel("Operational Metrics - Environmental Services",
                                 value = "evs",
                                 span("Operational Metrics - Environmental Services",
                                      style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                 tabBox(title = NULL, id = "tabset8", width = "100%", type = 'pills',      
                                        tabPanel("Turnaround Time",
                                                 hr(),
                                                 fileInput("evs_data",
                                                           label = "Please upload Environmental Services data"),
                                                 hr(),
                                                 actionButton("submit_evs",
                                                              "Submit",
                                                              class = "btn-primary"),
                                        )
                                 )
                                 ),
                        # Emergency Department Data Submission  ------
                        tabPanel("Operational Metrics - Emergency Department",
                                 span("Operational Metrics - Emergency Department", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                 tabBox(title = NULL, id = "tabset11", width = "100%", type = 'pills',
                                        tabPanel("ED KPIs", hr(),
                                                 fileInput("ed", label = "Please upload ED KPIs data"),
                                                 hr(),
                                                 actionButton("submit_ed", "Submit", class = "btn-primary")
                                        )
                                        
                                        
                                 )
                        ),
                        # Food Services Data Submission ----
                        
                        tabPanel("Operational Metrics - Food Services", value = "operational",
                                 shinyjs::useShinyjs(),
                                 shinyjs::inlineCSS(appCSS),
                                 span("Operational Metrics - Food Services", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                 tabBox(title = NULL, id = "tabset7", width = "100%", type = "pills",
                                        tabPanel("Cost and Revenue", br(),
                                                 fileInput("food_cost_and_revenue", label = "Please upload Cost and Revenue data"),
                                                 actionButton("submit_food", label = "Submit")
                                        )
                                 )
                                 # shinyjs::hidden(
                                 #   div(
                                 #     id = "thankyou_msg",
                                 #     h3("Your metric was submitted successfully."),
                                 #     actionLink("submit_another", "Submit another response")
                                 #   )
                                 # )
                                 
                        ),
                        # Imaging DR Ops Data Submission ----
                        tabPanel("Operational Metrics - Imaging", value = "operational",
                                 shinyjs::useShinyjs(),
                                 shinyjs::inlineCSS(appCSS),
                                 span("Operational Metrics - Imaging", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                 tabBox(title = NULL, id = "tabset7", width = "100%", type = "pills",
                                        tabPanel("Imaging - IR", br(),
                                                 fileInput("imaging_IR", label = "Please upload Imaging interventional radiology data"),
                                                 actionButton("submit_imaging", label = "Submit")
                                        ),
                                        tabPanel("Imaging - DR ED Chest X-Ray", br(),
                                                 fluidRow(
                                                   column(2,
                                                          textInput("imaging_xray_username",
                                                                    labelMandatory(
                                                                      "1. Please enter name:")
                                                          )
                                                   )
                                                 ),
                                                 fileInput("imaging_DR_XRay", label = "Please upload Imaging ED Chest X-Ray diagnostic radiology data"),
                                                 actionButton("submit_imagingxray", label = "Submit")
                                        ),
                                        tabPanel("Imaging - DR ED Head CT ", br(),
                                                 fluidRow(
                                                   column(2,
                                                          textInput("imaging_ct_username",
                                                                    labelMandatory(
                                                                      "1. Please enter name:")
                                                          )
                                                   )
                                                 ),
                                                 fileInput("imaging_DR_ct", label = "Please upload Imaging ED Head CT diagnostic radiology data"),
                                                 actionButton("submit_imagingct", label = "Submit")
                                        )
                                        
                                 )
  
                        ),
                        # Lab Data Submission ----
                        tabPanel("Operational Metrics - Lab", value = "lab",
                                 span("Operational Metrics - Lab",
                                      style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                 tabBox(title = NULL, id = "tabset7", width = "100%", type = 'pills',      
                                        tabPanel("Turnaround Time",
                                                 hr(),
                                                 fileInput("lab_scc", label = "Please upload SCC lab data"),
                                                 hr(),
                                                 fileInput("lab_sun", label = "Please upload Sunquest lab data"),
                                                 hr(),
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
                                                                h4("Please leave cell blank if data has not been received."),
                                                                br(),
                                                                h4("Please enter percentages (Proficiency Testing) as a decimal between 0 and 1."),
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
                        # Nursing Data Submission ----
                        tabPanel("Operational Metrics - Nursing",
                                 span("Operational Metrics - Nursing", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                 tabBox(title = NULL, id = "tabset11", width = "100%", type = 'pills',
                                        tabPanel("Nursing Indicators", 
                                                 hr(),
                                                 fileInput("nursing", label = "Please upload Nursing Indicators data"),
                                                 hr(),
                                                 actionButton("submit_nursing", "Submit", class = "btn-primary")
                                        )
                                        
                                        
                                 )
                        ),
                        # Patient Transport Data Submission ----
                        tabPanel("Operational Metrics - Patient Transport",
                                 span("Operational Metrics - Patient Transport", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
                                 tabBox(title = NULL, id = "tabset11", width = "100%", type = 'pills',
                                        tabPanel("Turnaround Time-Non Patient Transport", hr(),
                                                 fileInput("non_patient_transport", label = "Please upload Non Patient Transport Metrics data"),
                                                 actionButton("submit_npt_tat", "Submit", class = "btn-primary"),
                                        ),
                                        tabPanel("Turnaround Time-Patient Transport", hr(),
                                                 fileInput("patient_transport", label = "Please upload PTET Metrics data"),
                                                 actionButton("submit_pt_tat", "Submit", class = "btn-primary")
                                        )
                                        
                    
                                 )
                        ),
                        # Security Data Submission ----
                        tabPanel("Operational Metrics - Security",
                                 span("Operational Metrics - Security", style = "color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(), 
                                 span("Please only submit data if you have completed training on data submission for this tool.",
                                      style = "color:red; font-family:Calibri; font-weight: bold; 
                                      font-size: 20px; font-style:italic;
                                      margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 0px"),
                                 br(),
                                 hr(),
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
             ), # Close tabPanel Breakout
             
             # Fifth tab - Targets & Status Definitions --------
             tabPanel("Targets & Status Definitions", value = "targets",
                      fluidRow(
                        column(2, 
                               box(
                                 title = NULL, solidHeader = FALSE, width = 12,
                                 pickerInput("selectedService4",
                                             label = h4("Select Department:"), 
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
                                 pickerInput("selectedMetricGroup",
                                             label = h4("Select Metric Group:"),
                                             choices = metric_group_choices,
                                             multiple = TRUE,
                                             options = pickerOptions(
                                               liveSearch = TRUE,
                                               actionsBox = TRUE,
                                               dropupAuto = FALSE,
                                               size = 10),
                                             selected = default_metric_group)))
                      ),
                      hr(), 
                      fluidRow(textOutput("targetSummary_title")),
                      tags$head(tags$style("#targetSummary_title{color: #black; font-family:Calibri; font-weight: bold; 
                                           font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 30px}")), br(), br(),
                      fluidRow(
                        column(10, 
                               offset = 1,
                               DT::dataTableOutput("targetSummary_table"))
                      )
             ), # Close tabPanel Summary
             
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
  
  ##Tabset pills html ----
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
        #submit_npt_tat {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),
 
 tags$style(HTML("
        #submit_pt_tat {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),
 
 tags$style(HTML("
        #submit_nursing {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),
 
 
 tags$style(HTML("
        #submit_finance_ot {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),
 
 
 tags$style(HTML("
        #submit_ed {
          background-color: #d80b8c;
          color: #FFFFFF;
          border-color: #d80b8c;
        }")),

  
  tags$style(HTML("
        #submit_monthly_pt_exp {
          background-color: #d80b8c;
          color: #FFFFFF;
        }")),
 
 tags$style(HTML("
        #submit_ytd_pt_exp {
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
        #submit_biomedkpis {
          background-color: #d80b8c;
          color: #FFFFFF;
        }")),
 tags$style(HTML("
        #submit_imagingct {
          background-color: #d80b8c;
          color: #FFFFFF;
        }")),
 tags$style(HTML("
        #submit_imagingxray {
          background-color: #d80b8c;
          color: #FFFFFF;
        }")),
 tags$style(HTML("
        #submit_biomeddi {
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
        #header_custom_biomed {
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
