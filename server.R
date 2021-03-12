# Server
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(RODBC)
library(ggplot2)
library(dplyr)
library(DT)


# -------------------------------------------
# PATH OF Files
# -------------------------------------------
version = '2021.03.12'
file_path =  "C://SearchYouth//MSAccessDatabase//SY_Endpoint.mdb"


helperfilepath <- file.path("script","helper_functions.R")
communitypath <- file.path("data","community.csv")
enrollmentpath <- file.path("data","enrollment.csv")
endpointpath <- file.path("data","endpoint.csv")
psspath <- file.path("data","pss.csv")

# Load helper functions
source(helperfilepath)

#------------------------------------------------------------------------------
# Server setup can either be local or cloud
# Set this option to local if getting data from Access db on a local machine
# Set this option to cloud if getting data from csv - This option is prefered when using Shiny cloud server
#------------------------------------------------------------------------------
server_setup <- 'cloud' # change this local to run from access db

# Load data from CSV Files. This option will be used for the cloud web server


# Load data from Access DB or csv files
if (server_setup == 'local') {
  df_all_tables <- readData(file_path)
} else {
  df_endp <- read.csv(file = endpointpath, stringsAsFactors = FALSE)
  df_pss <- read.csv(file = psspath, stringsAsFactors = FALSE)
  df_all_tables <- list("endpoint" = df_endp,
                        "pss"  = df_pss)
}




#load study sites
df_comm_codes <- read.csv(file = communitypath, stringsAsFactors = FALSE)
df_enroll_list <- read.csv(file = enrollmentpath, stringsAsFactors = FALSE)



# Add region
df_enroll_list$Region <- apply(df_enroll_list,1,addRegion, c = df_comm_codes)

df_all_tables$endpoint$Community <- apply(df_all_tables$endpoint, 1, addCommunity, c = df_comm_codes)
df_all_tables$endpoint$Region <- apply(df_all_tables$endpoint, 1, addRegion, c = df_comm_codes)
df_all_tables$endpoint$outcome_0 <- with(df_all_tables$endpoint, 
                                         ifelse(outcome == 1,"VL Result Obtained",
                                                ifelse(outcome == 2,"Moved out",
                                                       ifelse(outcome == 3,"Lost to followup",
                                                              ifelse(outcome == 4,"Found-declined sample",
                                                                     ifelse(outcome == 5,"Withdrew consent",
                                                                            ifelse(outcome == 6,"Death","Other")))))))



df_all_tables$pss$Community <- apply(df_all_tables$pss, 1, addCommunity, c = df_comm_codes)
df_all_tables$pss$Region <- apply(df_all_tables$pss, 1, addRegion, c = df_comm_codes)

# Set the endpoint status
df_enroll_list$'Endpoint Status' <- apply(df_enroll_list, 1, getEndpointStatus, epstatus=df_all_tables$endpoint)
df_enroll_list$outcome <- apply(df_enroll_list, 1, getEndpointOutcome, epstatus=df_all_tables$endpoint)
#head(df_enroll_list)


server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # OBSERVE SELECTION EVENTS AND RETURN VALUES
  # ---------------------------------------------------------------------------
  enrgraphselect = reactiveVal(NULL)
  
  observeEvent(input$select_region, 
               enrgraphselect(NULL))
  
  observeEvent(input$select_comm, 
               enrgraphselect(NULL))
  
  observeEvent(input$breakdown, {
    enrgraphselect(NULL)
  })
  
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
  pss <- reactive({
    ret_val <- df_all_tables$pss
  })

  endpoint <- reactive({
    ret_val <- df_all_tables$endpoint
  })

  df_enr <- reactive({
    ret_val <- df_enroll_list
  })
  
  output$primary_endpoint <- renderValueBox({
    
    n <- length(df_enr()$youthid)
    x <- length(endpoint()$subjid)
    
    ## Check user selection on Region and Community
    if (length(input$select_region != 0)){
      if (input$select_region != 'All'){
        n <- length(df_enr()[df_enr()$Region == input$select_region,]$youthid)
        x <- length(endpoint()[endpoint()$Region == input$select_region,]$subjid)
      }
    }
    
    if (length(input$select_comm != 0)){
      if (input$select_comm != 'All'){
        
        n <- length(df_enr()[df_enr()$Community == input$select_comm,]$youthid)
        x <- length(endpoint()[endpoint()$Community == input$select_comm,]$subjid)
      }
    }
    
    result <- round(100 * x/n,1)
    valueBox(
      
      paste0(result,"%","(",x,"/",n,")"), "Completed Endpoint Evaluation",
      color = "green"
    )
  })
  
  output$pregnancy <- renderValueBox({
    
    n<-  length(df_enr()[df_enr()$Gender == 'Female',]$youthid)
    x <- 0 # length(follow_up_data()[follow_up_data()$fuvnumber==3,]$studyid)
    
    ## Check user selection on Region and Community
    if (length(input$select_region)!= 0){
      if (input$select_region != 'All'){
        #print(paste(unique(df_enr()$Region)))
        n <- length(df_enr()[df_enr()$Region == input$select_region & df_enr()$Gender =='Female',]$youthid)
      }
    }
    
    if (length(input$select_comm)!= 0){
      if (input$select_comm != 'All'){
        n <- length(df_enr()[df_enr()$Community == input$select_comm & df_enr()$Gender=='Female',]$youthid)
      }
    }
    
    result <- round(100 * x/n,1)
    valueBox(
      
      paste0(result,"%","(",x,"/",n,")"), "Reported Pregnancy",
      color = "green"
    )
  })
  
  output$survey <- renderValueBox({
    
    n <-  length(df_enr()$youthid)
    x <- length(pss()$subjid)
    
    ## Check user selection on Region and Community
    if (length(input$select_region != 0)){
      if (input$select_region != 'All'){
        n <- length(df_enr()[df_enr()$Region == input$select_region,]$youthid)
        x <- length(pss()[pss()$Region == input$select_region,]$subjid)
      }
    }
    
    if (length(input$select_comm != 0)){
      if (input$select_comm != 'All'){
        n <- length(df_enr()[df_enr()$Community == input$select_comm,]$youthid)
        x <- length(pss()[pss()$Community == input$select_comm,]$subjid)
      }
    }
    
    result <- round(100 * x/n,1)
    valueBox(
      
      paste0(result,"%","(",x,"/",n,")"), "Completed Patient Survey",
      color = "green"
    )
  })
  
  
  # -------------------------------------------
  # END-POINT EVALUATION PROGRESS SUMMARY
  # -------------------------------------------
  
  output$total_enrollment = renderUI({
    
    enr_count = length(df_enr()$youthid)
    area = 'all regions'
    if (!is.null(input$select_comm)){
      if (input$select_region == 'All' & input$select_comm == 'All'){
      }
      else if (!is.na(input$select_comm) & input$select_comm == 'All'){
        area = input$select_region
        enr_count = length(df_enr()[df_enr()$Region==input$select_region,]$youthid)
      }
      else{
        area = input$select_comm
        enr_count = length(df_enr()[df_enr()$Community==input$select_comm,]$youthid)
      }
    }
    
    
    HTML(paste("<font size='4'>", sprintf("There are %s total enrollments in %s.",
                                          enr_count, area),"</font><br><br>"))
    
  })
  
  output$endpoint_progress = renderUI({
    enr_count = length(df_enr()$youthid)
    if (enr_count != 0){
      new_ct = length(df_enr()[df_enr()[,'Endpoint Status'] != 'Incomplete',]$youthid)
      
      progressBar(
        id = "pb1",
        value = new_ct,
        total = enr_count,
        title = "Endpoint Completion Status",
        display_pct = TRUE
      )
    }
  })
  
  
  
  # ---------------------------------------------------------------------------
  # END-POINT EVALUATION PROGRESS GRAPH - INTERVENTION
  # ---------------------------------------------------------------------------
  output$plot1_intervention <- renderPlot({
    df_temp <- df_enr()
    
    if(length(input$select_region)!=0){
      if (input$select_region != 'All') {
        df_temp <- df_enr()[df_enr()$Region == input$select_region, ]
      }
    }
    
    
    if (length(input$select_comm) !=0) {
      if (input$select_comm != 'All') {
        df_temp <-  df_enr()[df_enr()$Community == input$select_comm, ]
      }
    }
    
    graphEndpoint(df_temp,'Intervention',input$breakdown)
  })
  
  output$plot1_control <- renderPlot({
    df_temp <- df_enr()
    
    if(length(input$select_region)!=0){
      if (input$select_region != 'All') {
        df_temp <- df_enr()[df_enr()$Region == input$select_region, ]
      }
    }
    
    
    if (length(input$select_comm) !=0) {
      if (input$select_comm != 'All') {
        df_temp <-  df_enr()[df_enr()$Community == input$select_comm, ]
      }
    }
    
    graphEndpoint(df_temp,'Control',input$breakdown)
  })
  
  #----------------------------------------------------------------------------
  # Intervention Clinics Status Message
  #----------------------------------------------------------------------------
  output$enpdpoint_text_sum_int <- renderUI({
    df_temp <- df_enr()[df_enr()$study_arm == 'Intervention',]
    enr_count = length(df_temp$youthid)
    completed_endpoint <- length(df_temp[df_temp$outcome != 'Not Done',]$youthid)
    area = 'all regions'
    if (!is.null(input$select_comm)){
      if (input$select_region == 'All' & input$select_comm == 'All'){
      }
      else if (!is.na(input$select_comm) & input$select_comm == 'All'){
        area = input$select_region
        enr_count = length(df_temp[df_temp$Region==input$select_region,]$youthid)
        completed_endpoint <- length(df_temp[df_temp$outcome != 'Not Done' & df_temp$Region==input$select_region,]$youthid)
      }
      else{
        area = input$select_comm
        enr_count = length(df_temp[df_temp$Community==input$select_comm,]$youthid)
        completed_endpoint <- length(df_temp[df_temp$outcome != 'Not Done' & df_temp$Community==input$select_comm,]$youthid)
      }
    }
    
    
    HTML(paste("<font size='3'>", sprintf("There are %s Intervention enrollments in %s. %s Completed Endpoint Evaluation",
                                          enr_count, area, completed_endpoint),"</font><br><br>"))
  })
  
  #----------------------------------------------------------------------------
  # Control Clinics Status Message
  #----------------------------------------------------------------------------
  output$enpdpoint_text_sum_con <- renderUI({
    df_temp <- df_enr()[df_enr()$study_arm == 'Control',]
    enr_count = length(df_temp$youthid)
    completed_endpoint <- length(df_temp[df_temp$outcome != 'Not Done',]$youthid)
    area = 'all regions'
    if (!is.null(input$select_comm)){
      if (input$select_region == 'All' & input$select_comm == 'All'){
      }
      else if (!is.na(input$select_comm) & input$select_comm == 'All'){
        area = input$select_region
        enr_count = length(df_temp[df_temp$Region==input$select_region,]$youthid)
        completed_endpoint <- length(df_temp[df_temp$outcome != 'Not Done' & df_temp$Region==input$select_region,]$youthid)
      }
      else{
        area = input$select_comm
        enr_count = length(df_temp[df_temp$Community==input$select_comm,]$youthid)
        completed_endpoint <- length(df_temp[df_temp$outcome != 'Not Done' & df_temp$Community==input$select_comm,]$youthid)
      }
    }
    
    
    HTML(paste("<font size='3'>", sprintf("There are %s Control enrollments in %s. %s Completed Endpoint Evaluation",
                                          enr_count, area, completed_endpoint),"</font><br><br>"))
  })
  
  #-------------------------------------------------------------------------------
  # Downloadable table with a list of all Intervention participants
  #-------------------------------------------------------------------------------
  output$endpoint_list_int <- DT::renderDataTable({
    df_temp <- df_enr()[df_enr()$study_arm == 'Intervention',]
    
    if(length(input$select_region)!=0){
      if (input$select_region != 'All') {
        df_temp <- df_temp[df_temp$Region == input$select_region, ]
      }
    }
    
    
    if (length(input$select_comm) !=0) {
      if (input$select_comm != 'All') {
        df_temp <-  df_temp[df_temp$Community == input$select_comm, ]
      }
    }
    
    #add functionality to the download button
    datatable(df_temp,
              filter = "top",
              callback = JS("$('div.dwnld').append($('#download1'));"),
              extensions = 'Buttons')
  })
  
  #-------------------------------------------------------------------------------
  # Download button for the table with a list of all Intervention participants
  #-------------------------------------------------------------------------------
  # Intervention List
  output$download1 <- downloadHandler(
    filename = function() {
      paste('intervention_list1-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(df_enr()[df_enr()$study_arm == 'Intervention',], con)
    }
  )
  
  #-------------------------------------------------------------------------------
  # Downloadable table with a list of all Control participants
  #-------------------------------------------------------------------------------
  output$endpoint_list_con <- DT::renderDataTable({
    df_temp <- df_enr()[df_enr()$study_arm == 'Control',]
    
    if(length(input$select_region)!=0){
      if (input$select_region != 'All') {
        df_temp <- df_temp[df_temp$Region == input$select_region, ]
      }
    }
    
    
    if (length(input$select_comm) !=0) {
      if (input$select_comm != 'All') {
        df_temp <-  df_temp[df_temp$Community == input$select_comm, ]
      }
    }
    
    #add functionality to the download button
    datatable(df_temp,
              filter = "top",
              callback = JS("$('div.dwnld').append($('#download2'));"),
              extensions = 'Buttons')
  })
  
  #-------------------------------------------------------------------------------
  # Download button for the table with a list of all Intervention participants
  #-------------------------------------------------------------------------------
  # Control List
  output$download2 <- downloadHandler(
    filename = function() {
      paste('control_list1-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(df_enr()[df_enr()$study_arm == 'Control',], con)
    }
  )
}