# Helper Functions
# -----------------------------------------------------------------------------
# Helper function to Read data from an ms-access db and return a list of 
# datasets
# -----------------------------------------------------------------------------
readData = function(file_path){
  # connect to database 
  con = odbcConnectAccess2007(file_path)
  
  # read datasets
  df_endpoint = sqlFetch(con, 'endpoint', stringsAsFactors=FALSE)
  df_pss = sqlFetch(con, 'pss', stringsAsFactors=FALSE)
  
  odbcCloseAll()
  to_return = list("endpoint" = df_endpoint,
                   "pss"  = df_pss)
}

# -----------------------------------------------------------------------------
# ADD REGION
# Helper function adding region given the community
# -----------------------------------------------------------------------------

addRegion <- function(df, c) {
  cur_community = df['Community']
  region <- c[c$Community==cur_community,'Region']
  #print(paste(region))
  return(region)
}


# -----------------------------------------------------------------------------
# ADD COMMUNITY
# Helper function adding community given the youthid
# -----------------------------------------------------------------------------

addCommunity <- function(df, c) {
  cur_subjid = substr(df['subjid'],2,3)
  community <- c[as.character(c$Clinic_code) == cur_subjid,'Community']
  #print(paste(community, cur_subjid))
  return(community)
}


# -----------------------------------------------------------------------------
# Get Endpoint Status
# Helper function to find if the endpoint evaluation was done
# -----------------------------------------------------------------------------

getEndpointStatus <- function(df, epstatus) {
  cur_subjid = df['youthid']
  endpoint <- 'Incomplete'
  df_endpoint <- epstatus
  
  if (nrow(df_endpoint)>0) {
    df_endpoint <- epstatus[epstatus$subjid == cur_subjid, ]
  }
  
  if(nrow(df_endpoint)>0) {
    endpoint <- 'Complete'
  } 
  
  return(endpoint)
  
}

# -----------------------------------------------------------------------------
# Get Endpoint tracking Status
# Helper function to find if the endpoint tracking outcome
# -----------------------------------------------------------------------------

getEndpointOutcome <- function(df, epstatus) {
  cur_subjid = df['youthid']
  endpoint_outcome <- 'Not Done'
  df_endpoint <- epstatus
  
  if (nrow(df_endpoint)>0) {
    df_endpoint <- epstatus[epstatus$subjid == cur_subjid, ]
  }
  
  if(nrow(df_endpoint)>0) {
    endpoint_outcome <- df_endpoint[,'outcome_0']
  } 
  
  return(endpoint_outcome)
  
}



# -----------------------------------------------------------------------------
# GRAPH Enpoint summary
# Helper function for graphing endpoint ascertainment on 
# the monitoring dashboard
# -----------------------------------------------------------------------------

graphEndpoint <- function(df_endpoint, intervention, breakdown) {
  if(!is.null(intervention)) {
    df_endpoint_subset <- df_endpoint[df_endpoint$study_arm == intervention, ]
  }
  
  #--------------------------------
  # No Breakdowns selected
  #--------------------------------
  if (breakdown == 'None') {
    if(nrow(df_endpoint_subset)>0) {
      # df_counts = data.frame(table(df_endpoint_subset$Region, df_endpoint_subset$`Endpoint Status`))
      # df_counts = arrange(df_counts, desc(Freq))
      # df_counts = df_counts[df_counts$Freq != 0,]
      
      ggplot(df_endpoint_subset, aes(x = Region, fill = `Endpoint Status`)) + 
        geom_bar(position = 'stack') + 
        theme(text = element_text(size=15), axis.text.x = element_text(angle=90,hjust=1))
    }
  } 
  #--------------------------------
  # Breakdowns selected
  #--------------------------------
  else if (breakdown == 'Gender') {
    ggplot(df_endpoint_subset, aes(x = Gender, fill = `Endpoint Status`)) + 
      geom_bar(position = 'stack') + 
      theme(text = element_text(size=15), axis.text.x = element_text(angle=90,hjust=1))
  }
  
  else if (breakdown == 'Community') {
    ggplot(df_endpoint_subset, aes(x = Community, fill = `Endpoint Status`)) + 
      geom_bar(position = 'stack') + 
      theme(text = element_text(size=15), axis.text.x = element_text(angle=90,hjust=1))
  }
  
  else if (breakdown == 'Participant Status') {
    ggplot(df_endpoint_subset[df_endpoint_subset$outcome != 'Not Done',], aes(x = Region, fill = outcome)) + 
      geom_bar(position = 'stack') + 
      theme(text = element_text(size=15), axis.text.x = element_text(angle=90,hjust=1))
  }
  
}