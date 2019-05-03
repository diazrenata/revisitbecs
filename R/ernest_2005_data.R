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
    
    download.file('http://niwot.colorado.edu/data_csvs/smammals.jh.data.csv', (paste0(datapath, '/niwot/niwot.csv')))
    download.file('http://niwot.colorado.edu/meta_data/smammals.jh.meta.txt', (paste0(datapath, '/niwot/niwot-metadata.txt')))
    
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
#' @param datapath path to where data is 
#' @description Process Niwot smammal data
#' Initial cleaning from LTER website code generator
#'
#' @return NULL
#'
process_niwot_data <- function(datapath = here::here()){
  # Halfpenny, Jim. 2019. Small mammal disturbance data for Niwot Ridge from 1981-6-30 to 1990-8-23, yearly. http://niwot.colorado.edu 
  # http://niwot.colorado.edu/data/data/small-mammal-species-composition-data-for-niwot-ridge-1981-1990
  
  niwot_raw <- read.csv(paste0(datapath, '/data/paper/raw/niwot/niwot.csv'), stringsAsFactors = F)
  
  niwot <- niwot_raw %>% 
    filter(!is.na(weight)) %>%
    mutate(species = as.character(species)) %>%
    filter(species != "?MOUSE") %>%
    filter(species != "?PIKAS") %>%
    filter(species != "CHIPSP") %>%
    filter(species != "VOLESP") %>%
    select(collector, date, species, weight, condition, habitat)
  
  species_means <- niwot %>%
    select(species, weight) %>%
    group_by(species) %>%
    summarize(mean.weight = mean(weight), n.captures = n()) %>%
    ungroup()
  
  big_species <- species_means %>%
    filter(mean.weight > 400) %>%
    select(species)
  
  niwot <- niwot %>%
    filter(!(species %in% big_species$species))
  
  # Still 1 too many species, based on paper.
  # Removing pocket gophers because they were only trapped for the first two years? 
  niwot <- niwot %>%
    filter(species != 'THOTAL')
  
  niwot <- niwot %>% 
    select(species, weight)
  
  processedpath = paste0(datapath, '/data/paper/processed/niwot')
  if(!dir.exists(processedpath)) {
    dir.create(processedpath)
  }
  
  write.csv(niwot, paste0(processedpath, '/niwot_processed.csv'))
 
  return(TRUE)
   
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