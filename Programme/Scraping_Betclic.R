
#Scrapping all côtes betclic

library(packrat)
library(knitr)

knitr::opts_knit$set(root.dir = '/Users/dflouriot/R/Paris_Sportifs/')


packrat::set_opts(symlink.system.packages = TRUE)
#packrat::init('/Users/dflouriot/R/Paris_Sportifs/')
packrat::on(project =  '/Users/dflouriot/R/Paris_Sportifs/',auto.snapshot = T )

library(Rcrawler)
library(xml2)
library(httr)
library(XML)
library(data.table)
library(iterators)
library(foreach)
library(doParallel)
library(curl)
library(rvest)
library(stringr)
library(RCurl)
library(dplyr)
library(esquisse)
library(cronR)
library(caret)
library(MASS)
library(pls)
library(jsonlite)  

#Extraction sur l'api de betclic tous les matchs de foot  
betclic=GET('https://sportapi.begmedia.com/api/v2/events?sortBy=1&sportIds=1&isLive=false&FetchLiveTopMarkets=false',
            accept_json(),
            add_headers("accept"="application/json, text/plain","range"="Item=0-1000","x-client"="{\"Context\":\"eyJ0eXAiOiJKV1QiLCJhbGciOiJHbG9iYWwuTW9iaWxlLkFwaS5BdXRoLkFwaS5TaGEyNTZBbGdvcml0aG0ifQ.IntcIkxlZ2lzbGF0aW9uXCI6XCJGclwiLFwiU2l0ZVwiOlwiRnJGclwiLFwiTGFuZ3VhZ2VcIjpcIkZyXCIsXCJDaGFubmVsSWRcIjpcIkJldGNsaWNGclwiLFwiVW5pdmVyc2VcIjpcIlNwb3J0c1wiLFwiTm90QmVmb3JlXCI6XCIyMDE5LTA0LTE2VDA4OjI1OjQwLjYzNTE5NjlaXCIsXCJFeHBpcmF0aW9uVGltZVwiOlwiMjAxOS0wNC0xNlQwODozMDo0MC42MzUxOTY5WlwifSI.CJJN-nIljX3B09l92ICRFx4jdI9TLu_HA4452VEnAVI\",\"Auth\":\"eyJ0eXAiOiJKV1QiLCJhbGciOiJHbG9iYWwuTW9iaWxlLkFwaS5BdXRoLkFwaS5TaGEyNTZBbGdvcml0aG0ifQ.IntcIklwXCI6XCIxNjUuMjI1Ljc3LjE0NVwiLFwiVXNlcklkXCI6LTEsXCJTZXNzaW9uXCI6bnVsbCxcIkNvdW50cnlDb2RlXCI6bnVsbCxcIkxhbmd1YWdlQ29kZVwiOm51bGwsXCJDdXJyZW5jeUNvZGVcIjpudWxsLFwiSXNBZG1pblwiOmZhbHNlLFwiSXNMb2dnZWRGcm9tQm9cIjpmYWxzZSxcIklzTGF1bmNoZXJcIjpmYWxzZSxcIlJlZ3VsYXRvcklkXCI6MixcIk5vdEJlZm9yZVwiOlwiMjAxOS0wNC0xNlQwODoyNTo0MC42MzUxOTY5WlwiLFwiRXhwaXJhdGlvblRpbWVcIjpcIjIwMTktMDQtMTZUMTA6Mjc6NDAuNjM1MTk2OVpcIn0i.B_C5a5JPMw3_MCMSxtiKJWkbMzj7kYD4T1vETrqc-84\",\"Device\":null}")) %>%
  read_html() %>%
  html_text() 

#Attention au format de sortie : Ce n'est pas forcément un data frame mais un dataframe avec des listes... 
betclic1= fromJSON(betclic)
extract_elastic1= betclic1[,10][,6][,c('name','id')]
extract_elastic2= betclic1[,10][,c('name','id')]
extract_elastic=cbind(extract_elastic1,extract_elastic2)
names(extract_elastic)=c('sport','id_sport','competition','id_competition')

betclic1=betclic1 %>%
  dplyr::select(id,date,name,relativeDesktopUrl) %>%
  cbind(extract_elastic)
#glimpse(betclic2)

#Autimatisation du scrapping   
bectclic_cote_scrap_fin=NULL

for (u in betclic1[,"relativeDesktopUrl"]) { 
  #u='italie-bosnie-herzegovine-m2036832'
  #u='allemagne-bundesliga-2018-2019-m1723982'
  
  print(u)
  
  #Les informations utiles sur le scrapping
  reference=betclic1 %>%
    filter(relativeDesktopUrl==u)
  
  #Extraction des parties du code HTML qui nous intéressent : id ==  market_marketTypeCode
  betclic_scrap=read_html(paste0('https://www.betclic.fr/',u))%>% 
    html_nodes(xpath = "//*[contains(@id,'market_marketTypeCode')]") 
  
  #Extraction de toutes les côtés pour un match
  betclic_cote_final=NULL
  
  for (i in 1:length(betclic_scrap)) {
    
    #i=1
    #print(i)
    
    #Test pour savoir s'il y a une erreur ou non dans le scrapping de la page
    tryerror=''
    tryerror=try(betclic_scrap  %>%
                   .[[i]]   %>%
                   xml_child(2) %>%
                   xml_child(1) %>%
                   html_table(),silent=T)
    # tryerror[[1]][1]!="Error in html_table.xml_node(.) : html_name(x) == \"table\" n'est pas TRUE\n"
    
    #Si pas d'erreur alors go !
    if (is.data.frame(tryerror)==TRUE) {
      
      #Sélection des tables de côtes
      betclic_cote_temp=betclic_scrap %>%
        .[[i]]   %>%
        xml_child(2) %>%
        xml_child(1) %>%
        html_table()
      
      #Sélection des deux identifiants importants : id et lib
      id_cote_temp=betclic_scrap %>%
        .[[i]]   %>%
        html_attr("id")
      
      id_cote_temp1=betclic_scrap %>%
        .[[i]]   %>%
        html_attr("data-track-bloc-title")
      
      id_cote=as.data.frame(cbind(id_cote_temp,id_cote_temp1)) 
      names(id_cote)=c('id_cote','lib_cote')
      
      #Remise en ligne de tous les paris sortis de chaque table
      betclic_cote_base=NULL
      for (c in 1:length(betclic_cote_temp)) {
        
        betclic_cote_tempo=as.data.frame(betclic_cote_temp[,c])
        
        betclic_cote_tempo$id_cote=id_cote[1,"id_cote"] 
        betclic_cote_tempo$lib_cote=id_cote[1,"lib_cote"] 
        
        betclic_cote_base=rbind(betclic_cote_base,betclic_cote_tempo)
      }
      betclic_cote_final=rbind(betclic_cote_final,betclic_cote_base)
    } else { 
      
      # betclic_cote_temp=betclic_scrap %>%
      #  
      #  .[[i]]   %>%
      #  xml_child(2) %>%
      #      xml_child(1) %>%
      #   xml_child(1) %>%
      #  html_table()
      
    }
    
    
  }
  
  #Remise en forme globale de la sortie Scrapping  
  names(betclic_cote_final)=c('cote','cote_id','cote_lib')
  betclic_cote_final1 = betclic_cote_final %>%
    filter(cote!= '' & is.na(cote)==F) %>%
    mutate(cote=gsub('---','0',as.character(cote)),cote_id=as.character(cote_id),cote_lib=as.character(cote_lib)) %>%
    mutate(pari= do.call(rbind, strsplit(cote, ' (?=[^ ]+$)', perl=TRUE))[,1],cote= do.call(rbind, strsplit(cote, ' (?=[^ ]+$)', perl=TRUE))[,2]) %>%
    mutate(cote=as.numeric(as.character(gsub(',','\\.',cote))),
           id_match=reference$id,
           date_match=reference$date,
           name_match=reference$name,
           lien_scrap=reference$relativeDesktopUrl,
           sport=reference$sport,
           id_sport=reference$id_sport,
           competition=reference$competition,
           id_competition=reference$id_competition,
           date_scrap=as.Date(Sys.Date(),"%d-%m-%Y"))
  
  #Table de sortie d'erreur 
  bectclic_cote_scrap_fin=rbind(bectclic_cote_scrap_fin,betclic_cote_final1)
}


save(bectclic_cote_scrap_fin,file =paste0('/Users/dflouriot/R/Paris_Sportifs/Data/Betclic_Scrapping_',as.Date(Sys.Date(),"%d-%m-%Y"),'.RData'))







