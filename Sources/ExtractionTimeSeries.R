# Trim a time series from time=Start to time=Finish
ExtractionTimeSeries<-function(Dataset,Start,Finish)
  {
    Extract<-data.frame(NA)
    Extract<-Dataset[(min(which(Dataset[,1]>Start))):(max(which(Dataset[,1]<Finish))),]
    return(Extract)
  }
 