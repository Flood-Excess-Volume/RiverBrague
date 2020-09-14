#Compute the FEV of an hydrograph and return FEV and various features 
FEVcomputation<-function(flood,Qht,start,end)
{
  source("Sources/ExtractionTimeSeries.R")
  source("Sources/CalculVolume.R")

  if(max(flood[,2])>Qht)
  {#Time when Q>Qht
    #On the hydrograph
    StartFEV<-flood[(min(which(flood[,2]>Qht))-1),1]
    FinishFEV<-flood[(max(which(flood[,2]>Qht))+1),1]
    #Real time, interpolated
    TrealStart<-approx(x=c(flood[(min(which(flood[,2]>Qht))-0),2],flood[(min(which(flood[,2]>Qht))-1),2])
                       ,y=c(flood[(min(which(flood[,2]>Qht))-0),1],StartFEV)
                       ,xout=Qht)$y
    
    TrealFinish<-approx(x=c(flood[(max(which(flood[,2]>Qht))+1),2],flood[(max(which(flood[,2]>Qht))+0),2])
                        ,y=c(FinishFEV,flood[(max(which(flood[,2]>Qht))+0),1])
                        ,xout=Qht)$y
    #FEV volume
    FEV<-CalculVolume(flood,Qht)
    #Discharge Qm
    Qm<-round(FEV/(TrealFinish-TrealStart)+Qht,0)
    
    #Extraction of Q>Qht
    FEVpart<-ExtractionTimeSeries(flood,StartFEV,FinishFEV)
   
  }else{
    FEV<-Qm<-TrealFinish<-TrealStart<-0
  }
  return(data.frame(Qht,FEV,Qm,TrealFinish-TrealStart,TrealStart,TrealFinish))
}