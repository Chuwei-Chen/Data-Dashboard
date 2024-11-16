#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(shinydashboard)
library(DT)
library(shinysurveys)

# Header setup
dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <- tags$a(href='https://gld.gu.se/en/our-research/ongoing-projects/boundaries-and-barriers-social-institutions-and-domestic-migration-in-southern-africa/'
)

survey_questions <- data.frame(
  question = c("Who are you?",
    "What category of feedback do you want to provide?",
               "What would you like to change about the app?",
               "Why is this change necessary/How will it improve the app?"),
  option = I(list(Name = c("Dave","Ellen","Erica","Prisca"),FeedbackCategories = c("App","Model","Data") , NULL, NULL)),
  input_type = c("select","select","text","text"),
  input_id = c("person","category","suggestion", "motivation"),
  dependence = NA,
  dependence_value = NA,
  required = TRUE
)


# UI definition
shinyUI(dashboardPage(title = "Resilency Project Dashboard",
                      dbHeader,
                      dashboardSidebar(
                        sidebarMenu(
                          id = "tabs",
                          menuItem("Homepage", tabName = "homepage", icon = icon("home")),
                          menuItem("Statistical Models",tabName = "models", icon = icon("chart-line")),
                          menuItem("Inference & Visualization", tabName = "infvis", icon = icon("bar-chart")),
                          menuSubItem("Social Capital", tabName = "socialcapital"),
                          menuSubItem("Special T-test", tabName = "specialttest"),
                          menuItem("Build Your Own Model",tabName = "BYOM", icon = icon("chart-line")),
                          menuItem("Feedback & Suggestions",tabName = "feedback", icon = icon("book"))
                        )
                      ),
                      dashboardBody(
                        tags$head(tags$style(HTML('
                                  * {font-family: Arial}
                                  
                                  .download_csv {
                                    background-color: #EF9307;
                                    color: #161B39;
                                    border-color: #161B39;
                                  }
                                  
                                  .download_dta {
                                    background-color:#EF9307;
                                    color: #161B39;
                                    border-color: #161B39;
                                  }
                                  
                                  .download_rds {
                                    background-color:#EF9307;
                                    color: #161B39;
                                    border-color: #161B39;
                                  }
                                  
                                  .download_codebook {
                                    background-color:#EF9307;
                                    color: #161B39;
                                    border-color: #161B39;
                                  }
                        
                                  input[type=checkbox] { accent-color: #EF9307; }
                                  input[type=radio] { accent-color: #EF9307; }
                                  
                                  .box.box-solid.box-primary>.box-header {
                                      color: #161B39;
                                      background-color: #EF9307;
                                  }

                                  .box.box-solid.box-primary {
                                    border-color: #161B39;
                                    background:#FFFFFF;
                                  }
                                  
                                  .box {
                                    overflow: scroll;
                                  }
                        
                                  /* logo */
                                  .skin-blue .main-header .logo {
                                    background-color: #161B39;
                                    color: #161B39;
                                  }

                                  /* logo when hovered */
                                  .skin-blue .main-header .logo:hover {
                                    background-color: #161B39;
                                  }

                                  /* navbar (rest of the header) */
                                  .skin-blue .main-header .navbar {
                                    background-color: #161B39;
                                  }        

                                  /* main sidebar */
                                  .skin-blue .main-sidebar {
                                    background-color: #161B39;
                                  }

                                  /* active selected tab in the sidebarmenu */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
                                    background-color: #EF9307;
                                    color: #161B39;
                                  }

                                  /* other links in the sidebarmenu */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a {
                                    background-color: #FFFFFF;
                                    color: #161B39;
                                  }

                                  /* other links in the sidebarmenu when hovered */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
                                    background-color: #EF9307;
                                  }

                                  /* body */
                                  .content-wrapper, .right-side {
                                    background-color: #FFFFFF;
                                  }

                                  /* toggle button when hovered */                    
                                  .skin-blue .main-header .navbar .sidebar-toggle:hover {
                                    background-color: #EF9307;
                                  }
                                '))),
                        
                        tabItems(
                          # Homepage tab ----
                          tabItem(tabName = "homepage",
                                  fluidRow(
                                    column(4,
                                           tags$img(src = 'ResiliencyLogo.jpg', height = '240', width = '240', alt = "AI generated logo for Migration Book. Internal Use Only.")
                                    ),
                                    column(6, offset = 1,
                                           tags$h1("Resiliency Project Dashboard"),
                                           tags$p("In this app you can review the full set of results for the resiliency project. If you have questions, contact Chuwei at chuwei.chen@gu.se")
                                    )
                                  ),
                                  tags$hr(style = "border-color: #161B39;"),
                                  tags$h3("Dataset Structure"),
                                  tags$p("The current dataset used in this dashboard consists of respondents who participated in both the LGPI 2019 survey and at least one round of the Covid Dataset."),
                                  div(
                                    uiOutput("sample_table"),
                                    style = "display: flex; justify-content: center;"
                                  ),
                                  tags$hr(style = "border-color: #161B39;"),
                                  tags$h3("How to use the dashboard"),
                                  tags$p("Navigate our app using the sidebar and you will find main tabs for Statistical Models, Inference & Visualization, Build Your Own Model and Feedback & Suggestions."),
                                  tags$p("Each section offers filters for you to choose your variable of interest to help you analyze data effectively. Below each page you could submit your comments/notes and we will keep record!")
                          ), # Close Homepage tab
                          
                          # Statisticals Models tab ----
                          tabItem(tabName = "models",
                                  tags$h1("Choose Results to Review"),
                                  tags$p("On this page you can review the regression results for each outcome variable. First you need to choose the category and then choose the variable. Noted that some questions were asked in more than one round and are differentiated by R1, R2 and R3."),
                                  div(
                                    uiOutput("predictors_table"),
                                    style = "display: flex; justify-content: center; align-items: center;"  # Center alignment
                                  ),
                                  tags$hr(style = "border-color: #161B39;"),
                                  fluidRow(
                                    column(4,
                                           div(style = "background-color: #79C9C6; padding: 20px;",
                                               tags$h2("Choose an Outcome Variable"),
                                               radioButtons(inputId = "reg_commind", 
                                                            label = "Do you want to review an outcome variable at the community or individual level?", 
                                                            choices = c("Community", "Individual"), 
                                                            inline = TRUE, 
                                                            selected = "No"),
                                               # Conditional dropdown for "Community" choice
                                               conditionalPanel(
                                                 condition = "input.reg_commind == 'Community'",
                                                 selectInput(inputId = "community_var_reg", 
                                                             label = "Select Community-Level Topic", 
                                                             choices = c("Restrictions", "Enforcement/Sanctions", "Assistance Program"))
                                               ),
                                               
                                               # Conditional dropdown for "Individual" choice
                                               conditionalPanel(
                                                 condition = "input.reg_commind == 'Individual'",
                                                 selectInput(inputId = "individual_var_reg", 
                                                             label = "Select Individual-Level Topic", 
                                                             choices = c("Knowledge of Covid", "Precautionary Behavior", "Reactionary Behavior", "Perception/Fears/Hope", "Social Welfare"))
                                               ),
                                               
                                               uiOutput("outcomevar_ui_reg")
                                           )
                                    ),
                                    column(6,
                                           actionButton(inputId = "show_info_reg", label = "Get Results"),
                                           tags$p(""),
                                           tags$div(style = "display: flex; align-items: center; border-style: solid; border-color: black;",
                                                    tags$p(tags$strong("Full Question Text: "), style = "margin: 0;"),
                                                    HTML("&nbsp;"),
                                                    textOutput("questionText_reg", inline = TRUE)
                                           ),
                                           tags$p(""),
                                           tags$div(style = "display: flex; align-items: center; border-style: solid; border-color: black;",
                                                    tags$p(tags$strong("Answer Choices: "), style = "margin: 0;"),
                                                    HTML("&nbsp;"),
                                                    textOutput("answerChoices_reg", inline = TRUE)
                                           )
                                    )),
                                  tags$h2("Regression Table"),
                                  tableOutput("reg_table"),
                                  tags$hr(style = "border-color: #161B39;"),
                                  fluidRow(
                                    column(4,
                                           tags$div(style = "border-style: solid; border-color: black; padding-left:10px; padding-right:10px; padding-bottom:10px;",
                                                    tags$h4("Comments"),
                                                    radioButtons(inputId = "person_model",label = "Who are you?",choices = c("Dave","Ellen","Erica","Prisca")),
                                                    textAreaInput(inputId = "notes_model", label = "Notes and Observations", value = "", rows = 5, placeholder = "Type Here"),
                                                    actionButton(inputId = "saveResultsModel", "Save",style="color: #161B39; background-color: #EF9307; border-color: #161B39")
                                           )
                                    ),
                                    column(8,
                                           DTOutput("stats_model_comments")   
                                    )
                                  )
                          ), # Close Review tab
                          
                          # Inference Visualization ----
                          tabItem(tabName = "infvis",
                            tags$h1("Statistical Inference & Visualization"),
                            tags$p("On this page, you can review the summary of each Covid behavior and the T-test results. First choose the outcome variable you are interested in and then check the results. A special T-test at the bottom shows the difference between villages in which local leaders were elected or appointed in terms of ELF, ESI, size etc."),
                            fluidRow(
                              column(4,
                                     div(style = "background-color: #79C9C6; padding: 20px;",
                                         tags$h2("Choose an Outcome Variable"),
                                         radioButtons(inputId = "infvis_commind", label = "Do you want to review an outcome variable at the community or individual level?", choices = c("Community", "Individual"), inline = TRUE, selected = "No"),
                                         # Conditional dropdown for "Community" choice
                                         conditionalPanel(
                                           condition = "input.infvis_commind == 'Community'",
                                           selectInput(inputId = "community_var", 
                                                       label = "Select Community-Level Topic", 
                                                       choices = c("Restrictions", "Enforcement/Sanctions", "Assistance Program"))
                                         ),
                                         
                                         # Conditional dropdown for "Individual" choice
                                         conditionalPanel(
                                           condition = "input.infvis_commind == 'Individual'",
                                           selectInput(inputId = "individual_var", 
                                                       label = "Select Individual-Level Topic", 
                                                       choices = c("Knowledge of Covid", "Precautionary Behavior", "Reactionary Behavior", "Perception/Fears/Hope", "Social Welfare"))
                                         ),
                                         
                                         uiOutput("outcomevar_ui")
                                     )
                              ),
                              column(6,
                                     actionButton(inputId = "show_info", label = "Get Results"),
                                     tags$p(""),
                                     tags$div(style = "display: flex; align-items: center; border-style: solid; border-color: black;",
                                              tags$p(tags$strong("Full Question Text: "), style = "margin: 0;"),
                                              HTML("&nbsp;"),
                                              textOutput("questionText", inline = TRUE)
                                     ),
                                     tags$p(""),
                                     tags$div(style = "display: flex; align-items: center; border-style: solid; border-color: black;",
                                              tags$p(tags$strong("Answer Choices: "), style = "margin: 0;"),
                                              HTML("&nbsp;"),
                                              textOutput("answerChoices", inline = TRUE)
                                     )
                              )),
                            tags$hr(style = "border-color: #161B39;"),
                            tags$h2("Visualization and Data Summary"),
                            fluidRow(
                              column(4, plotOutput("outcome_histogram")),  # Histogram
                              column(4, tableOutput("outcome_summary"))   # Summary Table
                            ),
                            tags$hr(style = "border-color: #161B39;"),
                            tags$h2("District Distribution"),
                            tags$p("Comment: Since the dataset we are using is a combination of LGPI and Covid Dataset, therefore, we lost many observations, thus resulting in very few observation in outcome variables. Many districts have less than 10 observation, which I think has a large influence on the real situation. I suggest that if we would like to see some covid behaviours' district distribution, we should use the Covid Panel dataset."),
                            plotOutput("district"),
                            tags$hr(style = "border-color: #161B39;"),
                            tags$h2("t-Test"),
                            tags$p("Below is a set of t-Tests testing if the average value of the outcome variable varies by: ELF, High Population vs. Low Population, High Barrier vs. Low  Barrier, High Boundary vs. Low Boundary", "Male vs. Female"),
                            tableOutput("ttest_results"),
                            tags$hr(style = "border-color: #161B39;"),
                            fluidRow(
                              column(4,
                                     tags$div(style = "border-style: solid; border-color: black; padding-left:10px; padding-right:10px; padding-bottom:10px;",
                                              tags$h4("Comments"),
                                              radioButtons(inputId = "personInfvis",label = "Who are you?",choices = c("Dave","Ellen","Erica","Prisca")),
                                              textAreaInput(inputId = "notesInfvis", label = "Notes and Observations", value = "", rows = 5, placeholder = "Type Here"),
                                              actionButton(inputId = "saveResultsInfvis", "Save",style="color: #161B39; background-color: #EF9307; border-color: #161B39")
                                     )
                              ),
                              column(8,
                                     DTOutput("Infvis_comments")   
                              )
                            )
                            ),
                          
                          # Subpage for Social Capital ----
                            tabItem(tabName = "socialcapital",
                                    tags$h1("Social Capital Indexes Inference and Visualization"),
                                    tags$p("This page presents the data summary of Social Capital Indexes and specific variables"),
                                    tags$h3("Index Explanation"),
                                    tags$ul(
                                      tags$li("ind_bc1: Individual Bonding Capital (Reciprocity)"),
                                      tags$li("ind_bc2: Individual Bonding Capital (Engagement)"),
                                      tags$li("vbc1: Village Bonding Capital (Reciprocity)"),
                                      tags$li("vbc2: Village Bonding Capital (Engagement)"),
                                      tags$li("ind_lsc1: Individual Linking Capital"),
                                      tags$li("lsc2: Linking Capital (Local Authorities to Supra-local Authorities)"),
                                      tags$li("vlsc1: Village Linking Capital"),
                                      tags$li("vindstrongldr: Strong Leaderships"),
                                      tags$li("vpll: Perceived Legitimate Leaderships")
                                    ),
                                    tags$hr(style = "border-color: #161B39;"),
                                    tags$h3("Data Summary for Social Capital Variables"),
                                    tags$hr(style = "border-color: #161B39;"),
                                    tags$h3("Inference and Visuliaztion of Social Indexes"),
                                    fluidRow(
                                      column(4,
                                             div(style = "background-color: #79C9C6; padding: 20px;",
                                             radioButtons(inputId = "socialcapitalindex",
                                                             label = "Which index you would like to review?",
                                                             choices = c("Individual Bonding Capital (Reciprocity)",
                                                             "Individual Bonding Capital (Engagement)",
                                                             "Village Bonding Capital (Reciprocity)",
                                                             "Village Bonding Capital (Engagement)",
                                                             "Individual Linking Capital",
                                                             "Linking Capital (Local Authorities to Supra-local Authorities)",
                                                             "Village Linking Capital",
                                                             "Strong Leaderships",
                                                             "Perceived Legitimate Leaderships"
                                                             ), 
                                                 inline = TRUE, 
                                                 selected = "Individual Bonding Capital (Reciprocity)"),
                                             actionButton(inputId = "socialcapital", label = "Get Results"))),
                                      column(8,
                                             plotOutput("socialcapital_table"))
                                    
                                    )),
                            
                          # Subpage for special t-test ----
                            tabItem(tabName = "specialttest",
                                       tags$h1("Special Statistical Test"),
                                       tags$p("Difference between villages in which local leaders were elected or appointed in terms of ELF, ESI, size etc."),
                                       tags$p("Note: for the advisory council/committee, there are only 5 observations has the value of 'Both'(elected and appointed), so that has been converted to NA. The value 1 stands for elected and value 2 stands for appointed."),
                                       
                                       # Special T-test inputs
                                       radioButtons(inputId = "infvis_ttest_special", 
                                                    label = "Which local leader you would like to review? The village head/neighborhood bloc leader or advisory council/committee?", 
                                                    choices = c("Village Head", "Advisory Council"), 
                                                    inline = TRUE, 
                                                    selected = "Village Head"),
                                       
                                       actionButton(inputId = "run_ttest_special", label = "Run T-test"),
                                       
                                       # Output for the t-test result
                                    tags$h3("T-test Results between where local leaders are elected or appointed"),
                                    tableOutput("ttest_results_special"),
                                      
                                    # Comments Area
                                    tags$hr(style = "border-color: #161B39;"),
                                    DTOutput("Specialtest_comments")
                              )

                        ) # Close tabItems
                      ) # Close Dashboardbody
)) # Close UI definition

