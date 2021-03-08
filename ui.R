#UI
#library(shiny)
library(shinydashboard)
#library(shinyWidgets)
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
                    uiOutput('enr_progress'),
                    title = 'End -point Sammary Status'),
                
                box(status = 'primary',
                    solidHeader = TRUE,
                    title = 'Breakdowns',
                    selectInput("subbreakdown", label = 'Sub-Breakdown',
                                # add initial VL to this,
                                choices = c('None', 'Gender', 'Age Group', 'Marital', 'Pregnancy', 'Satisfaction Survey')))
              ),
              fluidRow(
                box(title="Intervention End-point Tracker", 
                    htmlOutput("enroll_text_summary"),
                    plotOutput("plot1_enrollment")
                ),
                
                box(title="Control End-point Tracker", 
                    htmlOutput("enroll_sub"),
                    plotOutput("plot2_enrollment"),
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('enrollment_list')),
                    downloadButton("download1","Download csv")
                )
              )
      ),
      tabItem(tabName = 'reports',
              h2("Some Downloadable reports"))
    )
  )
)
