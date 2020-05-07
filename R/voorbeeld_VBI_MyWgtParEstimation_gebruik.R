# VOORBEELD GEBRUIK FUNCTIE "My.WgtParEstimation"


# aanmaak  analyseset - voorbeeld -----

##samenvoegen tabellen ----
tblData2 <- tblData %>%
  #left_join(tblPlotDetails, by = c("IDPlots", "Periode")) %>%
  #left_join(PlotWeights, by = c("IDPlots", "IDSegments", "Periode")) %>%
  left_join(StrataDyn, by = c("IDPlots", "IDSegments", "Periode")) %>%
  left_join(StrataStat, by = c("IDPlots"))


##selectie variabelen ----
hulpvariabelen <- c("IDPlots", "IDSegments","Periode", "IDGroup", "PlotWeight", "SegmentWeight", "Reeks","DateDendro","StartPeriode")
respons <- c("v11_Volume_ha", "v12_VolWithBranches_ha")
strata <- c("LandUse","StandType","OwnerType","ForestTypeGroupCode", "ForestTypeGroup", "ForestTypeGroupTxt","ForestTypeMegaGroupCode","UBBP","UBBP_MeerDan10Jr","Regio","Bosgroep","reservaat")

analyseSet <- tblData2[c(hulpvariabelen, respons,strata)] #6675


##verwijderen ontbrekende waarden voor volume ----
# --> dwz alle segmenten met NA voor volume worden uit de analyse geweerd
analyseSet <- analyseSet[!is.na(analyseSet$v11_Volume_ha),] #5528 @Leen: 6586
analyseSet <- analyseSet[!is.na(analyseSet$v12_VolWithBranches_ha),] #5528 @Leen: 6586


# analyseset ----
# @ Els: ik heb bovenstaande analyseset verstuurd via mail
      # twee mogelijke variabelen zijn v11_... en v12_...
      # mogelijke strata zijn Ownertype, Landuse, regio ...


# gebruik met één variabele en zonder strata -----
p1 <- My.WgtParEstimation(analyseSet,"v11_Volume_ha",Periode=1,MaxReeks=10)
p2 <- My.WgtParEstimation(analyseSet,"v11_Volume_ha",Periode=2,MaxReeks=10)


Resultaat <- rbind (p1, p2)
#colnames(Resultaat)
Resultaat[10:13] <- round(Resultaat[10:13],3)
Resultaat


# wegschrijven resultaat naar access-db ----
My.ResultsToDatabase(results = Resultaat
                     , dbHandle = dbResultaten
                     , tblName = "tblResultaten"
                     , scriptName = NaamScript
                     , type="D"
                     , description = "6.1.a.Houtvoorraad (m³/ha)"
                     , forestedge = "met randplots"
                     , scriptLocation = "3DendroKwant"
                     , datasource = dbAnalyseDataTxt
                     , datasource_hash = as.character(md5(dbAnalyseData))
                     , request_from = Verzoek
                     , run_by = run)



# met strata ------
p1 <- My.WgtParEstimation(analyseSet, "v11_Volume_ha", UseStrata = T, Strata = "Ownertyoe", Periode=1)
p2 <- My.WgtParEstimation(analyseSet,"v11_Volume_ha", UseStrata = T, Strata = "Ownertyoe",Periode=2)


# met strata en meerdere variabelen ----
p1 <- My.WgtParEstimation(beuk,c("v11_Volume_ha","v12_VolWithBranches_ha"),UseStrata = c(T,T),Strata = rep("Ownertyoe",2), Periode=1)
p2 <- My.WgtParEstimation(beuk,c("v11_Volume_ha","v12_VolWithBranches_ha"),UseStrata = c(T,T),Strata = rep("Ownertyoe",2), Periode=2)





