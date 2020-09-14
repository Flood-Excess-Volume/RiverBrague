# Compute the volume above the threshold discharge Qht of a flood hydrograph
CalculVolume<-function(flood,Qht=0)
{
  N<-length(flood[,1])
  volume<-0
  if(max(flood[,2])>Qht)
  {
    for(i in (1:N-1))
    {
      if(min(flood[i,2],flood[i+1,2])>Qht)
      {
        dvolume<-(flood[i,2]-Qht+flood[i+1,2]-Qht)/2*(as.numeric(flood[i+1,1])-as.numeric(flood[i,1]))
      }else
      {
        if(max(flood[i,2],flood[i+1,2])<Qht)
        {
          dvolume<-0
        }else
        {
          dvolume<-(max(flood[i,2],flood[i+1,2])-Qht)/2*abs((as.numeric(flood[i+1,1])-as.numeric(flood[i,1]))/(flood[i,2]-flood[i+1,2])*(flood[i+which.max(flood[i:(i+1),2])-1,2]-Qht))#*60
          # x11()
          # plot(flood[,1],flood[,2],type="b")
          # abline(h=Qht)
        }
      }
      volume<-volume+dvolume
    } 
  }
  
  return(round(volume,0))
}