library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)
library(shinyBS)
library(shinyjs)
library(cartography)


options(encoding = "UTF-8")

#for popup function
folder <- tempfile()
dir.create(folder)

ChefLieu1317 <- readRDS("ChefLieu1317.Rds")

Diocese1317 <- readRDS("Diocese1317.Rds")

T0New <- readRDS("T0New.Rds")

T0Impl <- readRDS("T0Impl.Rds")

Liens <- readRDS("liens.Rds")

Ecole <- readRDS("ecole.Rds")

#liste input
listObs <- list("Coutumiers",
                "Règles")

listOrdr <- list("Bénédictins",
                 "Chanoines réguliers",
                 "Monachisme érémitique",
                 "Hospitaliers et militaires",
                 "Mendiants",
                 "Clercs réguliers")

listStat <- list("Régulier",
                 "Séculier",
                 "Séculier communautaire",
                 "Autre")

listEcol <- list("École dépendante",
                 "École capitulaire",
                 "École du monastère")

listComm <- list("Masculine",
                 "Double",
                 "Féminine")

listRel <- list("A",
                "D",
                "H",
                "C")

colorSpat <- "#9B372F"
colorAttr <- "#9C7B36"
colorTempo <- "#294B59"

#Couleur links
Liens$modAgreg <- as.factor(Liens$modAgreg)
liensPal <- colorFactor(c("#8976B5","#CF6529","#5CA866","#69583E"),Liens$modAgreg)


#Fonctions ----

graphique_tapis <- function(carac,T0New){
  #Debut preparation
  T0NewTapis<-T0New %>% 
    mutate(date_stopC=ifelse(date_stopC>1800,1800,date_stopC)) %>% 
    mutate(date_stopC=ifelse(date_stopC==date_startC,date_stopC+5,date_stopC)) %>% #pour y voir qqchose
    mutate(dateC=(date_stopC+date_startC)/2)  #pour l'utilisation de linerange dans ggplot
  
  ListemodT0<-T0NewTapis %>% 
    group_by(caracNew,modaNiv1,modalite) %>% 
    summarise(n=n())
  
  #########Choix du point de vue= PdV
  PdV=carac
  
  TPdV<-filter(T0NewTapis,caracNew==PdV)
  
  TImplPdV<-TPdV %>% 
    group_by(idimplantation) %>% 
    summarise (dateminPdV=min(date_startC,na.rm=TRUE),
               datemaxPdV=max(date_stopC,na.rm=TRUE),
               nbEtats=n()) %>% 
    left_join(T0Impl,by="idimplantation")
  
  ###########proto Algo
  DateAmpl<-50   # amplitude des classes
  
  
  #initialisation fichier de W
  TPdVW<-TPdV %>% 
    mutate (modaW=modaNiv1) #Choix de travail sur modNiv2 de categories de caracNew
  TPdVW$idimplantation<-as.factor(TPdVW$idimplantation)
  
  couleur<-filter(T0New,caracNew==PdV) %>% 
    mutate(modaW=modaNiv1,modaW_Color=modaNiv1_Color) %>% 
    group_by(modaW,modaW_Color) %>% 
    summarise(nmodalite=n()) %>% 
    ungroup()
  #verification de la coherence entre les 2 sources
  couleur[!(couleur$modaW %in% TPdVW$modaW),]
  
  cols<-couleur$modaW_Color
  names(cols)<-couleur$modaW
  
  #decoupage pour courbe de frequence
  estdans<-function(min,max,d1,d2) {
    ifelse(((min>=d1) & (min<=d2)),1,
           ifelse(((min<d1)& (max>d1)),1,0))
  }
  Bmin<-summarise(filter(TPdVW,!is.na(date_start_min)),min=min(date_start_min))$min
  Bmax<-summarise(filter(TPdVW,!is.na(date_stop_max)),max=max(date_stop_max))$max
  DateClass<-seq(Bmin,Bmax,DateAmpl)
  
  TPdVWdis<-select(TPdVW,idfactoid,idimplantation, usual_name,date_startC,date_stopC,DureeFact,modaW)
  
  Vzero<-rep(0,nrow(TPdVWdis))
  i<-1
  for (i in 1:length(DateClass)) {
    TPdVWdis<-cbind(TPdVWdis,Vzero)
    colnames(TPdVWdis)[7+i]<-paste("A",DateClass[i],sep="")
    TPdVWdis[7+i]<-ifelse(((TPdVWdis$date_startC>=DateClass[i]) & (TPdVWdis$date_startC<=DateClass[i+1])),1,
                          ifelse(((TPdVWdis$date_startC<DateClass[i])& (TPdVWdis$date_stopC>DateClass[i])),1,0))
  }
  NcumulModa<-select(TPdVWdis,-c(1:6)) %>% 
    gather(Date,Freq,-modaW) %>% 
    mutate(Date=as.numeric(substr(Date,2,nchar(Date)))) %>% 
    filter(!is.na(Freq) & Freq>0)%>% 
    group_by(modaW,Date) %>% 
    summarize(Freq=sum(Freq,na.rm=TRUE)) %>% 
    arrange(Date,desc(Freq))  
  g<-ggplot(NcumulModa,aes(Date,fill=modaW,weight=Freq))+
    geom_bar(width = 50)+
    scale_fill_manual(values=cols)+
    xlab(paste("Périodes de ",DateAmpl," ans",sep=""))+
    theme(legend.position = "none")
  
  return(g)
}


chronogramme <- function(idimpl){
  
  T0NewTapis<-T0New %>% 
    mutate(date_stopC=ifelse(date_stopC>1800,1800,date_stopC)) %>% 
    mutate(date_stopC=ifelse(date_stopC==date_startC,date_stopC+5,date_stopC)) %>% #pour y voir qqchose
    mutate(dateC=(date_stopC+date_startC)/2)  #pour l'utilisation de linerange dasn ggplot
  
  
  coulChro<-T0New %>% 
    mutate(modaW=modaNiv1,modaW_Color=modaNiv1_Color) %>% 
    group_by(caracNew,modaW,modaW_Color) %>% 
    summarise(nmodalite=n()) %>% 
    ungroup()
  
  colsChro<-coulChro$modaW_Color
  names(colsChro)<-coulChro$modaW
  
  
  T0_1impl<-filter(T0NewTapis,idimplantation==idimpl) %>% 
    filter(caracNew != "Relations")
  
  
  titre<-paste(T0_1impl$usual_name[1]," (",T0_1impl$idimplantation[1],")",sep="")
  
  
  p<- ggplot(T0_1impl,aes(caracNew,dateC,colour=str_wrap(modaNiv1,10)))+
    ggtitle(titre)+
    geom_linerange(aes(ymin=date_startC,ymax=date_stopC),size=7)+
    scale_y_continuous(name="Date",breaks=seq(500,1900,200))+
    scale_color_manual(values=colsChro)+
    coord_flip()+
    theme(plot.margin = unit(c( 0.1, 0.1, 0.1, 0.1), "cm"),
          legend.position = 'bottom',
          legend.justification="left",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          legend.key = element_rect("white"),
          panel.background = element_rect(fill = "white"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.title =element_blank(),
          text = element_text(size=8),
          plot.title = element_text(size=10, face="bold"))
  
  
  return(p)
}







