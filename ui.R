#UI
library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "SY Endpoint Tracking Dashboard", titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "main", icon = icon("dashboard")),
      menuItem("Download Report", tabName = "reports", icon = icon("download")),
      uiOutput('select_region_ui'),
      uiOutput('select_comm_ui'),
      hr(),
      textInput('ind_youthid', 'Find YouthID'),
      actionButton("search", "Search"),
      uiOutput('last_update')
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "main", 
              #dashboard header
              h2("Summary Statistics"),
              
              # Main Summary stats
              fluidRow(
                # Dynamic valueBoxes
                valueBoxOutput("primary_endpoint"),
                valueBoxOutput("pregnancy"),
                valueBoxOutput("survey")
              ),
              
              # Tracker and breakdowns
              fluidRow(
                box(status = 'primary',
                    solidHeader = TRUE,
                    htmlOutput("total_enrollment"),
                    uiOutput('endpoint_progress'),
                    title = 'EndPoint Summary Status'),
                
                box(status = 'primary',
                    solidHeader = TRUE,
                    title = 'Breakdowns',
                    selectInput("breakdown", label = 'Breakdown',
                                # add initial VL to this,
                                choices = c('None', 'Community', 'Gender', 'Participant Status')))
              ),
              fluidRow(
                box(title="Intervention End-Point Tracker", 
                    htmlOutput("enpdpoint_text_sum_int"),
                    plotOutput("plot1_intervention", click = 'enr_click_intervention')
                ),
                
                box(title="Control End-point Tracker", 
                    htmlOutput("enpdpoint_text_sum_con"),
                    plotOutput("plot1_control", click = 'enr_click_control')
                )
              )
      ),
      tabItem(tabName = 'reports',
              h2("Some Downloadable reports"),
              fluidRow(
                box(status = 'primary',
                    solidHeader = TRUE,
                    htmlOutput("total_sup_inter"),
                    uiOutput('endpoint_progress_intervention'),
                    title = 'Intervention Suppression Summary Status'),
                
                box(status = 'primary',
                    solidHeader = TRUE,
                    title = 'Control Suppression Summary Status',
                    htmlOutput("total_sup_contol"),
                    uiOutput('endpoint_progress_control'))
              ),
              fluidRow(
                box(title="Intervention EndPoint Tracker", 
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('endpoint_list_int')),
                    downloadButton("download1","Download csv")
                ),
                
                box(title="Control EndPoint Tracker", 
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('endpoint_list_con')),
                    downloadButton("download2","Download csv")
                )
              )
              )
      
      
    )
  )
)
