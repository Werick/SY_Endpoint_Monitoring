# Server

# Important file Paths
helperfilepath <- file.path("script","helper_functions.R")
communitypath <- file.path("data","community.csv")

# Load helper functions
source(helperfilepath)

#load study sites
df_comm_codes <- read.csv(file = communitypath, stringsAsFactors = FALSE)

server <- function(input, output, session) {
  
  # -------------------------------------------
  # SELECT REGION
  # -------------------------------------------
  
  output$select_region_ui = renderUI({
    selectInput("select_region", label = "Region: ",
                choices = c('All', 'Kenya', 'Uganda'))
  })
  
  
  # -------------------------------------------
  # SELECT COMMUNITY
  # -------------------------------------------
  
  output$select_comm_ui = renderUI({
    df_temp = df_comm_codes
    if (length(input$select_region != 0)){
      if (input$select_region != 'All'){
        #print(paste(input$select_region))
        df_temp = df_temp[df_temp$Region == input$select_region,]
      }
      
      
      selectInput("select_comm", label = "Community: ",
                  choices = c('All', sort(unique(df_temp$Community))))
    }
  })
  
  # -------------------------------------------
  # LAST UPDATE DATE
  # -------------------------------------------
  
  last_update_date <- Sys.Date()
  output$last_update = renderUI({
    HTML(sprintf('<br> &nbsp &nbsp &nbsp Last Updated on %s.', last_update_date))
  })
  
  
  # -------------------------------------------
  # MAIN SUMMARY STATISTIC
  # -------------------------------------------
  output$primary_endpoint <- renderValueBox({
    
    n<- 0 # Get the n length(enrollment_data()$studyid)
    x <- 0 # length(follow_up_data()[follow_up_data()$fuvnumber==3,]$studyid)
    result <- round(100 * x/n,1)
    valueBox(
      
      paste0(result,"%","(",x,"/",n,")"), "Primary Endpoint",
      color = "green"
    )
  })
  
  output$pregnancy <- renderValueBox({
    
    n<- 0 # Get the n length(enrollment_data()$studyid)
    x <- 0 # length(follow_up_data()[follow_up_data()$fuvnumber==3,]$studyid)
    result <- round(100 * x/n,1)
    valueBox(
      
      paste0(result,"%","(",x,"/",n,")"), "Pregnancy",
      color = "green"
    )
  })
  
  output$survey <- renderValueBox({
    
    n<- 0 # Get the n length(enrollment_data()$studyid)
    x <- 0 # length(follow_up_data()[follow_up_data()$fuvnumber==3,]$studyid)
    result <- round(100 * x/n,1)
    valueBox(
      
      paste0(result,"%","(",x,"/",n,")"), "Patient Satisfaction Survey",
      color = "green"
    )
  })
  
}