#########GET THE DATA FROM THE PDF FILES##################
library(RCurl)
library(rvest)
library(httr)
library(tabulizer)
library(dplyr)
######### HEALTHCARE MINISTRY COVID OFFICIAL WEBSITE##################
url.mscbs<-"http://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm"
mscbs.webpage <- getURL(url.mscbs, .opts=list(followlocation=TRUE, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))
######### GET THE LAST PUBLISED DOCUMENT ######################################
tmp <- read_html(mscbs.webpage)
tmp <- html_nodes(tmp, "section")
tmp <- html_nodes(tmp, "li")
######## ALWAYS SECOND PLACE ON PUBLISHED LINKS ###############################
tmp <- tmp[[2]]
tmp <- html_nodes(tmp, "a")
url.pdf <- html_attr(tmp,"href")
remove(tmp)
####### BUILD THE URL FILE ####################################################
document <- paste0(url.mscbs,url.pdf)
document <- gsub(".htm", "/../", document)
document
####### GET THE INCREMENTAL NUMBER OF LAST FILE TO ITERATE ####################
####### ALL FILES FOLLOW THE NAME PATTERN Actualizacion_$X_COVID-19.pdf #######
pattern="_(.*?)_"
result <- regmatches(url.pdf,regexec(pattern,url.pdf))
maxdoc <- as.numeric(result[[1]][[2]])
cnt <-0
cnt2 <- 0
covidComunidades <- list()
###### ITERATE OVER ALL THE PUBLISHED DOCUMENTS ###############################
set_config(config(ssl_verifypeer = 0L))
for(i in 36:maxdoc) {
  tryCatch({
    url <- gsub(as.character(maxdoc),as.character(i), document)
    if(!file.exists(paste0(as.character(i),"_covid.pdf")))
    {GET(url, write_disk(paste0(as.character(i),"_covid.pdf")))}
    print(url)
    ### DOWNLOAD THE PDF IN CASE YOU WANT TO KEEP IT###
    out <- extract_tables(paste0(as.character(i),"_covid.pdf"))
    exit <- 1>2#SET FALSE
    cnt2 <- cnt2+1
    for (f in 1:length(out)){
      tmp <- out[f]
      tableTemp <- as.data.frame(tmp)
      for (g in 1:length(tableTemp)){
        if (tableTemp[g,1]=='CCAA') {covidComunidades[[cnt2]]<-tableTemp 
        exit = TRUE}
      }
      print(f)
      if (exit) break
    }
    ##### COMMENT THE FOLLOWING LINE IN CASE YOU WANT TO KEEP ALL THE PDFs ###
    file.remove(paste0(as.character(i),"_covid.pdf"))

    }, warning = function(w) {print(paste0("Document ",i," ",w))}
  , error = function(e) {print(paste("Document ",i," ",e))}
  )
}
remove(out,result,tableTemp,tmp,cnt,cnt2,g,f,i,pattern,exit,mscbs.webpage,url.mscbs,url.pdf)

dayThree <- covidComunidades[[3]]
