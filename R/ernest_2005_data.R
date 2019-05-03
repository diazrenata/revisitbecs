#' @title Download raw paper data
#'
#' @description Download datasets used in Ernest 2005.
#' Only downloads if the relevant folders do not exist in the `datapath`. 
#'
#' @param datapath Folder to store the data
#'
#' @return NULL
#'
#' @export

download_raw_paper_data <- function(datapath = here::here('data', 'paper', 'raw')) {
  
  if(!dir.exists(paste0(datapath, '/andrews-lter'))) {
    dir.create(paste0(datapath, '/andrews-lter'))
    
    download.file('http://andlter.forestry.oregonstate.edu/ltermeta/ltersearch/dataaccess.aspx?docid=WE02601_v1.csv', (paste0(datapath, '/andrews-lter/andrews.csv')))
    download.file('http://andlter.forestry.oregonstate.edu/mdaccess/metadownload.aspx?dbcode=WE026', (paste0(datapath, '/andrews-lter/andrews-metadata.pdf')))
  }
  
  if(!dir.exists(paste0(datapath, '/niwot'))) {
    
    dir.create(paste0(datapath, '/niwot'))
    
    download.file('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.43.3&entityid=0dfd551cf702d8801148da7d8727c1c7', (paste0(datapath, '/niwot/niwot.csv')))
    download.file('https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-nwt.43.3&contentType=application/xml', (paste0(datapath, '/niwot/niwot-metadata.xml')))
    
  }
  
  if(!dir.exists(paste0(datapath, '/sev'))) {
    
    dir.create(paste0(datapath, '/sev'))
    
    download.file('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-sev.8.297976&entityid=d70c7027949ca1d8ae053eb10300dc0e', (paste0(datapath, '/sev/sev.csv')))
    download.file('https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sev.8.297976&contentType=application/xml', (paste0(datapath, '/sev/sev-metadata.')))
    
  }
  
}


#' @title Process raw data in to the appropriate format. 
#'
#' @description Process all the datasets 
#'#'
#' @return NULL
#'
#' @export

process_raw_data <- function() {
}


#' @title Process Andrews data in to the appropriate format. 
#'
#' @description Process Andrews smammal data
#'#'
#' @return NULL
#'
#' @export

process_andrews_data <- function() {
  
  
}

#' @title Process Niwot data in to the appropriate format. 
#'
#' @description Process Niwot smammal data
#' Initial cleaning from LTER website code generator
#'
#' @return NULL
#'
process_niwot_data <- function(datapath = here::here('data', 'paper', 'raw', 'niwot')){
  # FROM LTER WEBSITE #
  
  #Package ID: knb-lter-nwt.43.3 Cataloging System:https://pasta.lternet.edu.
  # Data set title: Small mammal herbivore trapping in alpine tundra..
  # Data set creator:  Jim Halfpenny -  
  # Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
  # Contact:    - Information Manager Niwot Ridge LTER/University of Colorado  - lternwt@colorado.edu
  # Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-nwt.43.3
  # Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 
  
  infile1  <- paste0(datapath, '/niwot.csv') 
  dt1 <-read.csv(infile1,header=F 
                 ,skip=0
                 ,sep=","  
                 ,quot='"' 
                 , col.names=c(
                   "collector",     
                   "date",     
                   "sex",     
                   "species",     
                   "animal.identification.number",     
                   "weight",     
                   "age.colon..adult",     
                   "age.colon..subadult",     
                   "age.colon..juvenile",     
                   "male.testes.position.colon..abdominal",     
                   "male.testes.position.colon..inguinal",     
                   "male.testes.position.colon..scrotal",     
                   "imperforate.colon..female.vaginal.condition",     
                   "perforate.colon..female.vaginal.condition",     
                   "plug.colon..female.vaginal.condition",     
                   "small.colon..female.nipple.size",     
                   "medium.colon..female.nipple.size",     
                   "large.colon..female.nipple.size",     
                   "female.lactating.condition",     
                   "female.pregnancy.condition",     
                   "condition.of.animal",     
                   "recapture.status",     
                   "grid",     
                   "trap.number",     
                   "habitat"    ), check.names=TRUE)
  
  
  # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
  
  if (class(dt1$collector)!="factor") dt1$collector<- as.factor(dt1$collector)                                   
  # attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
  tmpDateFormat<-"%m/%d/%Y"
  tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
  # Keep the new dates only if they all converted correctly
  if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
  rm(tmpDateFormat,tmp1date) 
  if (class(dt1$sex)!="factor") dt1$sex<- as.factor(dt1$sex)
  if (class(dt1$species)!="factor") dt1$species<- as.factor(dt1$species)
  if (class(dt1$animal.identification.number)!="factor") dt1$animal.identification.number<- as.factor(dt1$animal.identification.number)
  if (class(dt1$weight)=="factor") dt1$weight <-as.numeric(levels(dt1$weight))[as.integer(dt1$weight) ]
  if (class(dt1$age.colon..adult)=="factor") dt1$age.colon..adult <-as.numeric(levels(dt1$age.colon..adult))[as.integer(dt1$age.colon..adult) ]
  if (class(dt1$age.colon..subadult)=="factor") dt1$age.colon..subadult <-as.numeric(levels(dt1$age.colon..subadult))[as.integer(dt1$age.colon..subadult) ]
  if (class(dt1$age.colon..juvenile)=="factor") dt1$age.colon..juvenile <-as.numeric(levels(dt1$age.colon..juvenile))[as.integer(dt1$age.colon..juvenile) ]
  if (class(dt1$male.testes.position.colon..abdominal)!="factor") dt1$male.testes.position.colon..abdominal<- as.factor(dt1$male.testes.position.colon..abdominal)
  if (class(dt1$male.testes.position.colon..inguinal)!="factor") dt1$male.testes.position.colon..inguinal<- as.factor(dt1$male.testes.position.colon..inguinal)
  if (class(dt1$male.testes.position.colon..scrotal)!="factor") dt1$male.testes.position.colon..scrotal<- as.factor(dt1$male.testes.position.colon..scrotal)
  if (class(dt1$imperforate.colon..female.vaginal.condition)!="factor") dt1$imperforate.colon..female.vaginal.condition<- as.factor(dt1$imperforate.colon..female.vaginal.condition)
  if (class(dt1$perforate.colon..female.vaginal.condition)!="factor") dt1$perforate.colon..female.vaginal.condition<- as.factor(dt1$perforate.colon..female.vaginal.condition)
  if (class(dt1$plug.colon..female.vaginal.condition)!="factor") dt1$plug.colon..female.vaginal.condition<- as.factor(dt1$plug.colon..female.vaginal.condition)
  if (class(dt1$small.colon..female.nipple.size)!="factor") dt1$small.colon..female.nipple.size<- as.factor(dt1$small.colon..female.nipple.size)
  if (class(dt1$medium.colon..female.nipple.size)!="factor") dt1$medium.colon..female.nipple.size<- as.factor(dt1$medium.colon..female.nipple.size)
  if (class(dt1$large.colon..female.nipple.size)!="factor") dt1$large.colon..female.nipple.size<- as.factor(dt1$large.colon..female.nipple.size)
  if (class(dt1$female.lactating.condition)!="factor") dt1$female.lactating.condition<- as.factor(dt1$female.lactating.condition)
  if (class(dt1$female.pregnancy.condition)!="factor") dt1$female.pregnancy.condition<- as.factor(dt1$female.pregnancy.condition)
  if (class(dt1$condition.of.animal)!="factor") dt1$condition.of.animal<- as.factor(dt1$condition.of.animal)
  if (class(dt1$recapture.status)!="factor") dt1$recapture.status<- as.factor(dt1$recapture.status)
  if (class(dt1$grid)!="factor") dt1$grid<- as.factor(dt1$grid)
  if (class(dt1$trap.number)!="factor") dt1$trap.number<- as.factor(dt1$trap.number)
  if (class(dt1$habitat)!="factor") dt1$habitat<- as.factor(dt1$habitat)
  
  # MODIFICATIONS FROM LTER CODE #
}




#' @title Process Sevilleta data in to the appropriate format. 
#'
#' @description Process Sevilleta smammal data
#' Initial cleaning from LTER website code generator
#'
#' @return NULL
#'
process_sev_data <- function(datapath = here::here('data', 'paper', 'raw', 'sev')){
  # FROM LTER WEBSITE #
    # Package ID: knb-lter-sev.8.297976 Cataloging System:https://pasta.lternet.edu.
  # Data set title: Small Mammal Mark-Recapture Population Dynamics at Core Research Sites at the Sevilleta National Wildlife Refuge, New Mexico (1989 - present).
  # Data set creator:  Seth Newsome - University of New Mexico 
  # Metadata Provider:  Information Manager Sevilleta LTER -  
  # Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
  # Contact:  Information Manager Sevilleta LTER -    - data-use@sevilleta.unm.edu
  # Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sev.8.297976
  # Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
  #
  #install package tidyverse if not already installed
  if(!require(tidyverse)){ install.packages("tidyverse") }  
  library("tidyverse") 
  infile1  <- paste0(datapath, '/sev.csv') 
  # This creates a tibble named: dt1 
  dt1 <-read_delim(infile1  
                   ,delim=","   
                   ,skip=1 
                   , col_names=c( 
                     "year",   
                     "location",   
                     "season",   
                     "night",   
                     "web",   
                     "trap",   
                     "recap",   
                     "species",   
                     "sex",   
                     "age",   
                     "reprod",   
                     "mass"   ), 
                   col_types=list(
                     col_character(),  
                     col_character(),  
                     col_character(),  
                     col_character(),  
                     col_character(),  
                     col_character(),  
                     col_character(),  
                     col_character(),  
                     col_character(),  
                     col_character(),  
                     col_character(), 
                     col_number() ), 
                   na=c( "na", ".", " ","NA")  ) 
  # Observed issues when reading the data. An empty list is good!
  problems(dt1) 
 
}