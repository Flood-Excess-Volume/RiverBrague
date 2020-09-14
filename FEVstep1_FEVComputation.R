#Script computing the Flood Excess Volume (FEV) for current state and for project 
# state with giving room to the river. 
#
# see definition in Bokhove, O., Kelmanson, M. A., Kent, T., Piton, G. and Tacnet, JM.: 
# Communicating (nature-based) flood-mitigation schemes using flood-excess volume, 
# River Research and Applications, 35(9), 1402–1414, doi:10.1002/rra.3507, 2019.
#
# Script based on FEV_V8.R 
# Coded by Guillaume PITON guillaume.piton@inrae.fr 
# Funded by the NAIAD H2020 project (Grand Number: 730497)
# Version 0 - prepared and tested on Sept. 14, 2020
#
# Require the event hydrograph, the stage-discharge curves (current and project) 
# and the threshold discharge value for flooding.
#
# This is Step one, i.e. the full analysis is the FEV computation. 
# Step two is the square lake plotting. See attached script

#Cleaning environment
rm(list = ls())

#Set working directory
setwd("//grdata/Projets/NAIAD/5Analysis/05Hydrology/FEV/4GitHub")
#Create output directory
dir.create("./Output", showWarnings = FALSE)#, recursive = FALSE, mode = "0777")

#Source 
source("Sources/ExtractionTimeSeries.R")
source("Sources/CalculVolume.R")
source("Sources/FEVcomputation.R")

#Language English or French?
Language<-"English"


####################################################################
####################################################################
################# DATA LOADING----
####################################################################
####################################################################
{ 
  #discharge time series text file
  flood<-read.csv("Data/Oct2015Brague@Biot.csv",stringsAsFactors = F)
  #transform data in Posixct
  if(typeof(flood[,1])=="character"){flood[,1]<-as.POSIXct(flood[,1]
                                                           ,format="%Y-%m-%d %H:%M:%S"
                                                           ,tz="Europe/Paris")}
  
  #Starting and ending time of the hydrograph to consider 
  start<-as.POSIXct("2015-10-03 17:00", format="%Y-%m-%d %H:%M",tz="Europe/Paris")
  end<-as.POSIXct("2015-10-04 1:00", format="%Y-%m-%d %H:%M",tz="Europe/Paris")
  
  # Threshold discharge for flooding
  Qht<-135 
  
  #Estimate of peak flow upper and lower limit 
  #(will be used to compute uncertainty bounds on FEV)
  Qmaxinf<-185 #Lower bound
  Qmaxsup<-295 #Upper bound

#########################################Stage discharge relationships----
######################
##CURRENT SITUATION
#######################
  
#Stage - discharge relationship
H_Q<-read.csv("Data/Brague@BiotH_Q.csv") 

#Water stage - discharge functions interpolated on the data
H2Q<-function(Q){return(approx(H_Q$Q,H_Q$H,Q)$y)}
Q2H<-function(H){return(approx(H_Q$H,H_Q$Q,H)$y)}

#Treshold values updates
ht<-H2Q(Qht)

######################
##With giving room to the river
#######################
#Stage - discharge relationship (project state)
H_Qgrr<-read.csv("Data/Brague@BiotH_Qgrr15_0.5.csv")
W2<-15 #Widening in meters of this alternative rating curve

#New stage discharge functions
H2Qgrr<-function(Q){return(approx(H_Qgrr$Q,H_Qgrr$H,Q)$y)}
Q2Hgrr<-function(H){return(approx(H_Qgrr$H,H_Qgrr$Q,H)$y)}

#Computation of threshold discharge and max depth with project state
QhtGRR<-round(Q2Hgrr(ht),0) #New flood starting discharge
HmaxGRR<-round(H2Qgrr(max(flood$Qm3s)),2)
}


####################################################################
####################################################################
################# ACTUAL COMPUTATION----
####################################################################
####################################################################


##################################### FEV in natural state

FEV<-FEVcomputation(flood,Qht,start,end)
save(FEV,file="Output/FEV.RData")
print(paste0("FEV is ",round(FEV$FEV/10^6,2),"M m3"))

#uncertainties
Qmax<-max(flood$Qm3s,na.rm = T)
#Create new hydrograph with discharge increased proportionally 
# with discharge upper bound/peak discharge
floodsup<-data.frame(flood$Date,flood$Qm3s/Qmax*Qmaxsup);names(floodsup)<-names(flood)
FEVsup<-FEVcomputation(floodsup,Qht,start,end)
#Create new hydrograph with discharge decreased proportionally 
# with discharge lower bound/peak discharge
floodinf<-data.frame(flood$Date,flood$Qm3s/Qmax*Qmaxinf);names(floodsup)<-names(flood)
FEVinf<-FEVcomputation(floodinf,Qht,start,end)

#Give uncertainty range
print(paste0("FEV is ",round(FEV$FEV/10^6,2)," [",round(FEVinf$FEV/10^6,2),";",round(FEVsup$FEV/10^6,2)
             ,"] m3 (mean [lower bound; upper bound])"))

##################################### FEV in project state
FEVgrr<-FEVcomputation(flood,QhtGRR,start,end)
save(FEVgrr,file=paste0("Output/FEVgrr",W2,".RData"))

#uncertainties
FEVsupgrr<-FEVcomputation(floodsup,QhtGRR,start,end)
FEVinfgrr<-FEVcomputation(floodinf,QhtGRR,start,end)
#Give uncertainty range
print(paste0("FEV after giving-room-to-the-river is ",round(FEVgrr$FEV/10^6,2)," [",round(FEVinfgrr$FEV/10^6,2)
             ,";",round(FEVsupgrr$FEV/10^6,2),"] m3 (mean [lower bound; upper bound])"))



####################################################################
####################################################################
################# PLOTING---
####################################################################
####################################################################

#Plot FEV Current state-------------
if(Language=="English")
{
  Labels<-c(expression(paste("Discharge [",m^3,"/s]")),"Water stage [m]", "Time [hh:mm]","Stage - time measurement","Stage - discharge relationship","Hydrograph")
}else{
  Labels<-c(expression(paste("Debit [",m^3,"/s]")),"Hauteur d'eau [m]", "Temps [hh:mm]","Limnigramme","Courbe de tarage","Hydrogramme")
}
#Limits
Hmax<-H2Q(Qmax)
Qm<-FEV$Qm
Qlim<-c(0,Qmax*1.33)
Hlim<-c(0,Hmax*1.2)

  {png(file=paste0("Output/FEV_CURRENT.png"),height=17,width=15,units="cm",res=350,family="serif")
  {
    par(mar=c(0,0,0,0),oma=c(1,1,1,1),mfrow=c(2,2),mgp=c(2,0.6, 0),las=1,lty="solid",lwd=1,xaxs="i",yaxs="i",bty="o")
    par(yaxt="n")
    
    #Panel 1 : H-Q----
    #H-Q up to Qmax
    plot(H2Q(seq(0,Qmax,0.1)),seq(0,Qmax,0.1),type="l",ylim=Qlim,xlim=rev(Hlim),lwd=1.5)
    #H-Q Qmax up to Qmax sup
    lines(H2Q(seq(Qmax,Qmaxsup,0.1)),seq(Qmax,Qmaxsup,0.1),type="l",lwd=1.5,col="grey75")
    
    legend("bottomleft",legend=Labels[2]  ,bty="n")
    lines(c(0,H2Q(Qht),H2Q(Qht)),c(Qht,Qht,0),lty=2)
    lines(c(0,H2Q(Qmax),H2Q(Qmax)),c(Qmax,Qmax,0),lty=2)
    
    #Panel 2 Q(t)----
    par(yaxt="l")
    plot(flood$Date,flood$Qm3s,type="l",xlim=c(start,end),ylim=Qlim,lwd=1.2)
    FEVtimeSerie<-ExtractionTimeSeries(flood,FEV$TrealStart,FEV$TrealFinish)
    polygon(x=c(FEV$TrealStart,FEVtimeSerie$Date,FEV$TrealFinish),y=c(Qht,FEVtimeSerie$Qm3s,Qht)
            ,col="lightblue")
    polygon(x=c(flood$Date,rev(flood$Date))
            ,y=c(flood$Qm3s*Qmaxinf/max(flood$Qm3s),rev(flood$Qm3s*Qmaxsup/max(flood$Qm3s)))
            ,col=rgb(211, 211,211, 150, maxColorValue=255)
            ,border="grey95"
    )
    legend("bottomright",legend=Labels[3],bty="n")
    legend("topleft",title=Labels[1],legend="",bty="n")
    lines(c(start,FEV$TrealFinish),c(Qht,Qht),lty=2)
    lines(c(start,flood$Date[which.max(flood$Qm3s)]),c(Qmax,Qmax),lty=2)
    text(x=c(start,start,flood$Date[which.max(flood$Qm3s)]),y=c(Qm,Qht,Qmax)*1.05
         ,labels=c("",expression(Q[T]),expression(Q[p])),pos=c(4,4,4))
    text(x=mean(c(FEV$TrealStart,FEV$TrealFinish)),y=mean(c(Qm,Qht)),labels="FEV",cex=1.5)
    
    #Panel 3: H(t)----
    par(yaxt="n",xaxt="n")
    plot(H2Q(flood$Qm3s),flood$Date,type="l",ylim=c(end,start),xlim=rev(Hlim),lwd=2)
    legend("bottomright",legend=Labels[3],bty="n")
    lines(H2Q(c(Qht,Qht)),c(start,end),lty=2)
    lines(H2Q(c(Qmax,Qmax)),c(start,end),lty=2)
    text(x=H2Q(c(Qm,Qht,max(flood$Qm3s))),y=c(end,end,end)-1000,labels=c("",expression(h[T])
                                                                         ,expression(h[p]))
         ,pos=c(4,4,2))
    
    #Panel 4: empty, just axis and text----
    par(xaxt="n",yaxt="l")
    plot(flood$Date,flood$Date,type="n",ylim=c(end,start),xlim=c(end,start))
    
    legend(x="bottom",legend=c(Labels[4],Labels[5],Labels[6])
           ,lwd=c(2,1.5,1.2),bty="n")
    
    legend(x="top",ncol=1,title="",bty="n"
           ,legend=c(expression(" "),
                     expression(""),
                     bquote("FEV" %~~% .(round(FEV$FEV/10^6,3)) ~ "Mm3")
                     ,bquote(T[f] == .(round(FEV$TrealFinish...TrealStart/3600,1)) ~"h")
                     ,bquote(h[T] == .(round(H2Q(Qht),2)) ~"m")
                     ,bquote(Q[T] == .(Qht) ~"m3/s")
                     )
           ,text.col=c(rep(1,6), rep("grey33",5)))
    
}
  dev.off()
}

#Plot FEV GRR (Giving Room to the River)-------------
if(Language=="English")
{
  Labels<-c(expression(paste("Discharge [",m^3,"/s]")),"Water stage [m]", "Time [hh:mm]"
            ,"Stage - time measurement","Stage - discharge relationship","Hydrograph"
            ,"Stage - time measurement","Stage - discharge relationship"
            ,"Current","With GRR","m wider")
}else{
  Labels<-c(expression(paste("Debit [",m^3,"/s]")),"Hauteur d'eau [m]", "Temps [hh:mm]"
            ,"Limnigramme Actuel","Courbe de tarage actuelle","Hydrogramme"
            ,"Limnigramme Projet","Courbe de tarage projet"
            ,"Section actuelle","Section projet","m plus large")
}

#Limits
Qlim<-c(0,Qmax*1.33)
Hlim<-c(0,Hmax*1.2)
Taille<-12
{png(file=paste0("Output/FEVgrr-W",W2,"m.png"),
     height=Taille+2,width=Taille,units="cm",res=350
     ,family="serif")
  {
    par(mar=c(0,0,0,0),oma=c(1,1,1,1),mfrow=c(2,2),mgp=c(2,0.6, 0),las=1,lty="solid",lwd=1,xaxs="i",yaxs="i",bty="o")
    par(yaxt="n")
    LabelCurrent<-as.character(Labels[9])
    LabelProject<-as.character(Labels[10]);LabelProject2<-as.character(Labels[11])
    
    #Panel 1 : H-Q----
    #H-Q up to Qmax
    plot(H2Q(seq(0,Qmax,0.1)),seq(0,Qmax,0.1),type="l",ylim=Qlim,xlim=rev(Hlim),lwd=2.5)
    #H-Q Qmax up to Qmax sup
    lines(H2Q(seq(Qmax,Qmaxsup,0.1)),seq(Qmax,Qmaxsup,0.1),type="l",lwd=1.5,col="grey75")
    
    legend("bottomleft",legend=Labels[2],bty="n")
    lines(c(0,H2Q(Qht),H2Q(Qht)),c(Qht,Qht,0),lty=2)
    lines(c(0,H2Q(Qmax),H2Q(Qmax)),c(Qmax,Qmax,0),lty=2)
    
    #With GRR
    lines(seq(0,HmaxGRR,0.01),tapply(seq(0,HmaxGRR,0.01),(1:length(seq(0,HmaxGRR,0.01))),Q2Hgrr),lwd=2.5,lty=3,col="gray33")
    lines(seq(HmaxGRR,H2Qgrr(Qmaxsup),0.01),tapply(seq(HmaxGRR,H2Qgrr(Qmaxsup),0.01),(1:length(seq(HmaxGRR,H2Qgrr(Qmaxsup),0.01))),Q2Hgrr),lwd=1,lty=3,col="gray33")
    
    lines(c(ht,ht,0),c(Q2H(ht),FEVgrr$Qht,FEVgrr$Qht),lty=3,col="gray33")
    lines(c(0,H2Qgrr(Qmax),H2Qgrr(Qmax)),c(Qmax,Qmax,0),lty=3,col="gray33")
    text(HmaxGRR,Q2Hgrr(HmaxGRR),LabelProject,pos=3,col="grey33"
         ,srt=-atan(((max(flood$Qm3s*1.05)-max(flood$Qm3s)*.95)/max(flood$Qm3s)/(H2Qgrr(max(flood$Qm3s*1.05))-H2Qgrr(max(flood$Qm3s)*.95))*H2Qgrr(max(flood$Qm3s))))*180/3.14)
    text(H2Q(max(flood$Qm3s)),max(flood$Qm3s),LabelCurrent,pos=3
         ,srt=-atan(((max(flood$Qm3s*1.05)-max(flood$Qm3s)*.95)/max(flood$Qm3s)/(H2Q(max(flood$Qm3s*1.05))-H2Q(max(flood$Qm3s)*.95))*H2Q(max(flood$Qm3s))))*180/3.14)
    
    #Panel 2 Q(t)----
    par(yaxt="l")
    plot(flood$Date,flood$Qm3s,type="n",xlim=c(start,end),ylim=Qlim)
    legend("bottomright",legend=Labels[3],bty="n")
    legend("topleft",title=Labels[1],legend="",bty="n")
    #FEV polygon
    polygon(x=c(FEV$TrealStart,FEVtimeSerie$Date,FEV$TrealFinish),y=c(Qht,FEVtimeSerie$Qm3s,Qht)
            ,col="lightblue")
    #Uncertainty on hydrograph
    polygon(x=c(flood$Date,rev(flood$Date))
            ,y=c(flood$Qm3s*Qmaxinf/max(flood$Qm3s),rev(flood$Qm3s*Qmaxsup/max(flood$Qm3s)))
            ,col=rgb(211, 211,211, 150, maxColorValue=255)
            ,border="grey75")
    #Hydrograph
    lines(flood$Date,flood$Qm3s,lwd=2,lty=4)
    #Straight lines
    lines(c(start,FEV$TrealFinish),c(Qht,Qht),lty=2)
    lines(c(start,flood$Date[which.max(flood$Qm3s)]),c(Qmax,Qmax),lty=2)
    
    text(x=c(start,start,start),y=c(FEVgrr$Qht*1.05,Qht*.93,Qmax*1.05)
         ,labels=c(bquote(Q["T," ~.(LabelProject)]),bquote(Q["T," ~.(LabelCurrent)]),expression(Q[p])),pos=c(4,4,4))
    text(x=mean(c(FEV$TrealStart,FEV$TrealFinish)),y=mean(c(Qm,Qht)),labels="FEV",cex=1.5)
    
    #GRR
    lines(c(start,FEVgrr$TrealFinish),c(FEVgrr$Qht,FEVgrr$Qht),lty=3,col="grey33")
    
    #Panel 3: H(t)----
    par(yaxt="n",xaxt="n")
    plot(H2Q(flood$Qm3s),flood$Date,type="l",ylim=c(end,start),xlim=rev(Hlim),lwd=1.5)
    legend("bottomright",legend=Labels[3],bty="n")
    lines(H2Q(c(Qht,Qht)),c(start,end),lty=2)
    lines(H2Q(c(Qmax,Qmax)),c(start,end),lty=2)
    text(x=H2Q(Qht*1.05),y=end-1000,labels=bquote(h[T]),pos=c(4))
    text(x=H2Q(max(flood$Qm3s)),y=end-1000,labels=bquote(h["p"]),pos=c(2))

    
    #GRR
    lines(H2Qgrr(flood$Qm3s),flood$Date,lwd=1.5,lty=3,col="gray33")
    lines(H2Qgrr(c(Qmax,Qmax)),c(start,end),lty=3,col="gray33")
    text(x=H2Qgrr(Qmax)*1.03,y=(end-3000),labels=bquote(h["p," ~ .(LabelProject)]),pos=c(4),col="gray33")

    #Panel 4: empty, just axis and text----
    par(xaxt="n",yaxt="l")
    plot(flood$Date,flood$Date,type="n",ylim=c(end,start),xlim=c(end,start))
    legend(x="bottom",legend=c(Labels[4]
                               ,Labels[7]
                               ,Labels[5]
                               ,Labels[8],Labels[6])
           ,lwd=c(1.5,1.5,2.5,2.5,2),lty=c(1,3,1,3,4),bty="n",col=c(1,"gray33",1,"gray33",1))
    
    legend(x="top",ncol=1,title="",bty="n"
                   ,legend=c(Labels[9]
                     ,bquote("FEV" %~~% .(round(FEV$FEV/10^6,3)) ~ "Mm3")
                     ,bquote(T[f] == .(round(FEV$TrealFinish...TrealStart/3600,1)) ~"h")
                     ,bquote(h[T] == .(round(H2Q(Qht),2)) ~"m")
                     ,bquote(Q[T] == .(Qht) ~"m3/s")
                     ,bquote(.(LabelProject) %~~% .(W2) ~ .(LabelProject2))
                     ,bquote("FEV" %~~% .(round(FEVgrr$FEV/10^6,3)) ~ "Mm3")
                     ,bquote(T[f] == .(round(FEVgrr$TrealFinish...TrealStart/3600,1)) ~"h")
                     ,bquote(Q[T] == .(FEVgrr$Qht) ~"m3/s")
                     )
           ,text.col=c(rep(1,5), rep("grey33",5)));rm(LabelCurrent,LabelProject,LabelProject2)
    
}
  dev.off()
}

