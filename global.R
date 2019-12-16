library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)
library(shinyBS)
library(shinyjs)
library(cartography)
library(htmltools) #for leaflet label


options(encoding = "UTF-8")

#for popup function
folder <- tempfile()
dir.create(folder)

###################################
#     CHANGEMENTS DE DONNEES      #
###################################
moda <- "modalite"

color <- "modaNiv1_Color"

carac <- "caracNew"

### Granularité modaniv 2 // mi-dec 19
      modalite <- "modaNiv1"
      donnee <- readRDS("Data/T0NewChgt20191213.Rds")
      
      Ecole <- donnee[donnee[[carac]] %in%  "École", ]
      
      donnee <- donnee[donnee[[carac]] %in%  c("Ordres","Statuts","Type de communauté"), ]
      
      CaracHist<-read.csv(file = "Data/NewModelCaracModalitesColor4.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
      donnee <- merge(CaracHist[,c(moda,color)], donnee, by.x=moda,by.y=moda)
      

### Granularité modaniv 2 // dec 19
      # modalite <- "modaNiv1"
      # donnee <- readRDS("Data/T0New20191202.Rds")
      # 
      # Ecole <- donnee[donnee[[carac]] %in%  "École", ]
      # 
      # donnee <- donnee[donnee[[carac]] %in%  c("Observance","Ordres","Statuts","Type de communauté"), ]
      # 
      # CaracHist<-read.csv(file = "Data/NewModelCaracModalitesColor3.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
      # donnee <- merge(CaracHist[,c(moda,color)], donnee, by.x=moda,by.y=moda)


### Granularité modaniv 2 // nov 19
      # modalite <- "modaNiv2"
      # donnee <- readRDS("Data/T0New20191120.Rds")
      # 
      # Ecole <- donnee[donnee[[carac]] %in%  "École", ]
      # 
      # donnee <- donnee[donnee[[carac]] %in%  c("Observance","Ordres","Statuts","Type de communauté"), ]
      # 
      # CaracHist<-read.csv(file = "Data/NewModelCaracModalitesColor3.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
      # donnee <- merge(select(CaracHist,modalite, modaNiv1_Color), donnee, by.x="modalite",by.y="modalite")


     
### Granularité modAgreg
      # modalite <- "modAgreg"
      # donnee <- readRDS("T0NewBase.Rds")
      # 
      # Ecole <- donnee[donnee[[carac]] %in%  "École", ]
      # 
      # donnee <- donnee[donnee[[carac]] %in%  c("Observance","Ordres","Statuts","Type de communauté"), ]
      # 
      # CaracHist<-readRDS(file = "CaracHist.Rds")
      # donnee <- merge(select(CaracHist,modalite, modaNiv1, modaNiv1_Color), donnee, by.x="modalite",by.y="modalite")





# donnee <- merge(select(CaracHist,modalite, modaNiv1, modaNiv1_Color), donnee, by.x="modalite",by.y="modalite")


###################################



ChefLieu1317 <- readRDS("ChefLieu1317.Rds")

Diocese1317 <- readRDS("Diocese1317.Rds")

T0Impl <- readRDS("T0Impl.Rds")

Liens <- readRDS("liens.Rds")

Ecole <- readRDS("ecole.Rds")




#Couleur links
Liens$modAgreg <- as.factor(Liens$modAgreg)
liensPal <- colorFactor(c("#8976B5","#CF6529","#5CA866","#69583E"),Liens$modAgreg)


#Fonctions ----

# Pdv <- "Observance"

graphique_tapis <- function(Pdv,donnee){
  
  T0NewTapis<-donnee %>% 
    mutate(date_stopC=ifelse(date_stopC>1800,1800,date_stopC)) %>% 
    mutate(date_stopC=ifelse(date_stopC==date_startC,date_stopC+5,date_stopC)) %>% #pour y voir qqchose
    mutate(dateC=(date_stopC+date_startC)/2)  #pour l'utilisation de linerange dasn ggplot
  
  ListemodT0<-T0NewTapis %>% 
    group_by(T0NewTapis[[carac]],modaNiv1,modalite) %>% 
    summarise(n=n())
  ListeMod<-CaracHist
  
  # TPdV<-filter(T0NewTapis,caracNew==Pdv)
  TPdV <- T0NewTapis[T0NewTapis[[carac]] == Pdv, ]
  
  TPdVimpl<-TPdV %>% 
    group_by(idimplantation) %>% 
    summarise (dateminPdV=min(date_start_min,na.rm=TRUE),
               datemaxPdV=max(date_stop_max,na.rm=TRUE),
               nbEtats=n()) %>% 
    left_join(T0Impl,by="idimplantation")
  
  ###########proto Algo
  DateAmpl<-50   # amplitude des classes
  
  
  #initialisation fichier de W
  TPdVW<-TPdV %>% 
    mutate (modaW=!!sym(modalite)) #Choix de travail sur modNiv2 de catégories de caracNew
  TPdVW$idimplantation<-as.factor(TPdVW$idimplantation)
  
  couleur<-filter(CaracHist,CaracHist[[carac]]==Pdv) %>% 
    mutate(modaW=!!sym(modalite),modaW_Color=modaNiv1_Color) %>% 
    group_by(modaW,modaW_Color) %>% 
    summarise(nmodalite=n()) %>% 
    ungroup()
  #vérification de la cohérence entre les 2 sources
  couleur[!(couleur$modaW %in% TPdVW$modaW),]
  
  cols<-couleur$modaW_Color
  names(cols)<-couleur$modaW
  
  #découpage pour courbe de fréquence
  estdans<-function(min,max,d1,d2) {
    ifelse(((min>=d1) & (min<=d2)),1,
           ifelse(((min<d1)& (max>d1)),1,0))
  }
  Bmin<-summarise(filter(TPdVW,!is.na(date_start_min)),min=min(date_start_min))$min
  Bmax<-summarise(filter(TPdVW,!is.na(date_stop_max)),max=max(date_stop_max))$max
  DateClass<-seq(floor(Bmin/10)*10,Bmax,DateAmpl)
  
  
  TPdVWdis<-select(TPdVW,idfactoid,idimplantation, usual_name,date_startC,date_stopC,DureeFact,modaW)
  
  Vzero<-rep(0,nrow(TPdVWdis))
  i<-2
  for (i in 1:length(DateClass)) {
    TPdVWdis<-cbind(TPdVWdis,Vzero)
    colnames(TPdVWdis)[7+i]<-paste("A",DateClass[i],sep="")
    TPdVWdis[7+i]<-ifelse(((TPdVWdis$date_stopC<DateClass[i]) | (TPdVWdis$date_startC >DateClass[i+1])),0,1)
    
  }
  
  NcumulModa<-select(TPdVWdis,-c(1,3:6)) %>% 
    group_by(idimplantation,modaW) %>% 
    summarise_all(max) %>% 
    ungroup() %>% 
    select(-idimplantation) %>% 
    gather(Date,Freq,-modaW) %>% 
    mutate(Date=as.numeric(substr(Date,2,nchar(Date)))) %>% 
    filter(!is.na(Freq) & Freq>0)%>% 
    group_by(modaW,Date) %>% 
    summarize(Freq=sum(Freq,na.rm=TRUE)) %>% 
    arrange(Date,desc(Freq))  
  
  sum(TPdVWdis$A400,na.rm=TRUE)
  ggplot(NcumulModa)+
    geom_bar(aes(x=Date, Y=Freq))
  
  g<-ggplot(NcumulModa)+
    geom_bar(width = 50, aes(x=Date,fill=modaW,y=Freq), stat="identity")+
    scale_fill_manual(values=cols)+
    xlab(paste("Périodes de ",DateAmpl," ans",sep=""))+
    theme(legend.position = 'none')
  
  return(g)
}


chronogramme <- function(idimpl){
  
  T0NewTapis<-donnee %>% 
    mutate(date_stopC=ifelse(date_stopC>1800,1800,date_stopC)) %>% 
    mutate(date_stopC=ifelse(date_stopC==date_startC,date_stopC+5,date_stopC)) %>% #pour y voir qqchose
    mutate(dateC=(date_stopC+date_startC)/2)  #pour l'utilisation de linerange dasn ggplot 
  
  
  coulChro<-donnee %>% 
    mutate(modaW=!!sym(modalite),modaW_Color=!!sym(color)) %>% 
    group_by(!!sym(carac),modaW,modaW_Color) %>% 
    summarise(nmodalite=n()) %>% 
    ungroup()
  
  colsChro<-coulChro$modaW_Color
  names(colsChro)<-coulChro$modaW
  
  
  T0_1impl<-filter(T0NewTapis,idimplantation==idimpl)
  
  
  # T0_1impl<-filter(T0NewTapis,idimplantation==idimpl) %>% 
  #   filter(T0NewTapis[[carac]] != "Relations")
  
  titre<-paste(T0_1impl$usual_name[1]," (",T0_1impl$idimplantation[1],")",sep="")
  
  
  p<- ggplot(T0_1impl,aes_string(carac,"dateC",colour=modalite))+
    ggtitle(titre)+
    geom_linerange(aes(ymin=date_startC,ymax=date_stopC),size=7)+
    scale_y_continuous(name="Date",breaks=seq(500,1900,200))+
    scale_color_manual(values=colsChro)+
    coord_flip()+
    theme(plot.margin = unit(c( 0.1, 0.1, 0.1, 0.1), "cm"),
          legend.position = 'none',
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


showPopup <- function(id, lat, lng) {
  chrngr <- chronogramme(id)
  svg(filename= paste(folder,"plot.svg", sep = "/"), 
      width = 400*0.01 , height = 200*0.01 )
  print(chrngr)
  dev.off()
  
  content <- paste(readLines(paste(folder,"plot.svg",sep="/")), collapse = "")
  
  leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id, options = popupOptions(maxWidth = 500))
}





