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



