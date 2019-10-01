T0NewSimplf <- T0New %>% select(caracNew,modAgreg,idimplantation)

T0NewSel <- filter(T0NewSimplf, modAgreg %in% "Bénédictins")
# T0NewSel <- filter(T0NewSel, TypeEntiete != "École")

idImplSel <- unique(T0NewSel$idimplantation)

T0New <- filter(T0NewSimplf, idimplantation %in% idImplSel)

T0NewAffiche <- filter(T0New, caracNew %in% "Statuts")