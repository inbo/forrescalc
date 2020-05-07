#'  Berekening gewogen gemiddelde en 95% betrouwbaarheidsintervallen op basis van gewogen variantie
#'
#' Langere functiebeschrijving
#' @param Data Analysedataset
#' @param VariableName vector met de namen van de responsvariabelen (die de parameterschattingen bevatten) (als string)
#' @param Periode Welke bosinventaris (1, 2, ...)
#' @param MinReeks Eerste jaarreeks binnen een periode van 1 bosinventarisatie (12 jaar, dus tss 1 en 12)
#' @param MaxReeks Laatste jaarreeks binnen een periode van 1 bosinventarisatie (12 jaar, dus tss 1 en 12)
#' @param MinYear Eerste jaar van de geanalyseerde periode
#' @param MaxYear Laatste jaar van de geanalyseerde periode
#' @param UseStrata Gebruik je strata (logische vector) even lang als de vector VariableName
#' @param Strata Welke strata wil je gebruiken (per respons gedefinieerd in variableName)
#' @return
#' @importFrom
#' @examples

My.WgtParEstimation<-function(Data,VariableName,Periode=NA,MinReeks=1,MaxReeks=12,MinYear=1,MaxYear=9999,UseStrata=rep(FALSE,1000),Strata){


  if (is.na(Periode)){
    DataSel<-Data[Data$Reeks<=MaxReeks
                  & Data$Reeks>=MinReeks
                  & Data$Year>=MinYear
                  & Data$Year<=MaxYear,, drop = FALSE]
  } else {
    DataSel<-Data[Data$Periode==Periode
                  & Data$Reeks<=MaxReeks
                  & Data$Reeks>=MinReeks
                  & Data$Year>=MinYear
                  & Data$Year<=MaxYear,, drop = FALSE]
  }

  i<-1
  #Doorloop de functie voor iedere gedefinieerde respons
  for (Var in VariableName ){

    #Indien we per strata werken
    if (UseStrata[i]){
      s<-1

      #Loop over alle strata
      for(Stratum in levels(DataSel[,Strata[i]])){

        DataStrat <- DataSel[!is.na(DataSel[,Strata[i]])&(DataSel[,Strata[i]]==Stratum),, drop=FALSE]

        Variable <-DataStrat[,Var]

        if (nrow(DataStrat)>0){

          wgt.mean <- sum(DataStrat$Weight * Variable, na.rm=TRUE)/(sum(DataStrat$Weight * (!is.na(Variable)),na.rm=TRUE))

        } else {

          wgt.mean<-NA

        }

        if (nrow(DataStrat)>1){

          V1<-sum(DataStrat$Weight * (!is.na(Variable)),na.rm=T)

          V2<-sum(((DataStrat$Weight) * (!is.na(Variable)))^2,na.rm=T)

          wgt.var <-sum(DataStrat$Weight*(Variable-wgt.mean)^2)/(V1-(V2/V1))

          lci<-wgt.mean-1.96*sqrt(wgt.var)/sqrt(V1)

          uci<-wgt.mean+1.96*sqrt(wgt.var)/sqrt(V1)

        }else {

          wgt.var <-NA

          lci<-NA

          uci<-NA

        }

        outputS<-data.frame(variabele=Var,
                            strata=Strata[i],
                            stratumNaam=Stratum,
                            periode=Periode,
                            minYear=ifelse(nrow(DataStrat)>0,min(DataStrat$Year,na.rm=TRUE),NA),
                            maxYear=ifelse(nrow(DataStrat)>0,max(DataStrat$Year,na.rm=TRUE),NA),
                            minReeks=ifelse(nrow(DataStrat)>0,min(DataStrat$Reeks,na.rm=TRUE),NA),
                            maxReeks=ifelse(nrow(DataStrat)>0,max(DataStrat$Reeks,na.rm=TRUE),NA),
                            nbObservaties=length(DataStrat$IDPlots),
                            wgt.mean=wgt.mean,
                            wgt.var=wgt.var,
                            llci=lci,
                            ulci=uci)
        if (s<=1){
          outputT<-outputS
        } else {
          outputT<-rbind (outputT,outputS)
        }
        s<-s+1
      }

    } else { #Indien we niet met strata werken
      Variable <-DataSel[,Var]
      wgt.mean <- sum(DataSel$Weight * Variable, na.rm=TRUE)/sum(DataSel$Weight * (!is.na(Variable)),na.rm=TRUE)
      V1<-sum(DataSel$Weight * (!is.na(Variable)),na.rm=T)
      V2<-sum(((DataSel$Weight)* (!is.na(Variable)))^2,na.rm=T)
      wgt.var <-sum(DataSel$Weight*(Variable-wgt.mean)^2,na.rm=T)/(V1-(V2/V1))
      lci<-wgt.mean-1.96*sqrt(wgt.var)/sqrt(V1)
      uci<-wgt.mean+1.96*sqrt(wgt.var)/sqrt(V1)
      outputT<-data.frame(variabele=Var,
                          strata="",
                          stratumNaam="",
                          periode=Periode,
                          minYear=min(DataSel$Year),
                          maxYear=max(DataSel$Year),
                          minReeks=min(DataSel$Reeks),
                          maxReeks=max(DataSel$Reeks),
                          nbObservaties=nrow(DataSel),
                          wgt.mean=wgt.mean,
                          wgt.var=wgt.var,
                          llci=lci,
                          ulci=uci)

    }
    if (i<=1){
      output<-outputT
    } else {
      output<-rbind (output,outputT)
    }
    i<-i+1
  }
  data.frame(output)
}
