library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)
library(shinyBS)
library(shinyjs)
library(cartography)

options(encoding = "UTF-8")

ChefLieu1317 <- readRDS("ChefLieu1317.Rds")

Diocese1317 <- readRDS("Diocese1317.Rds")


T0New <- readRDS("T0New.Rds")

Liens <- readRDS("liens.Rds")

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
                "X")

colorSpat <- "#9B372F"
colorAttr <- "#9C7B36"
colorTempo <- "#294B59"

#Couleur links
Liens$modAgreg <- as.factor(Liens$modAgreg)
liensPal <- colorFactor(c("#8976B5","#CF6529","#5CA866","#69583E"),Liens$modAgreg)