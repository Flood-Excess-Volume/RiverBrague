#Script plotting the square lake analysis of the Flood Excess Volume (FEV) 
# framework
#
# see definition in Bokhove, O., Kelmanson, M. A., Kent, T., Piton, G. and Tacnet, JM.: 
# Communicating (nature-based) flood-mitigation schemes using flood-excess volume, 
# River Research and Applications, 35(9), 1402–1414, doi:10.1002/rra.3507, 2019.
#
# Script based on SquareLake_V0.R 
# Coded by Guillaume PITON guillaume.piton@inrae.fr 
# Funded by the NAIAD H2020 project (Grand Number: 730497)
# Version 0 - prepared and tested on Sept. 14, 2020
#
# Require the output of the FEV computation (see attached script) 
# and the costs and FEV of each measures.
#
# Step one was the full analysis is the FEV computation. See attached script
# This is step two, i.e. the square lake plotting. 

#Cleaning environment
rm(list = ls())
#Set working directory
setwd("//grdata/Projets/NAIAD/5Analysis/05Hydrology/FEV/4GitHub")


####################################################################
####################################################################
################# DATA LOADING----
####################################################################
####################################################################
#FEV result of step one
load("Output/FEV.Rdata")

#Data of measures
Measures<-read.csv("Data/Measures.csv",sep=",", quote = "")
Nbmeasure<-length(!is.na(Measures$Name))

####################################################################
####################################################################
################# COMPUTATION----
####################################################################
####################################################################

#Square lake computation
SideLake<-round((0.5*FEV$FEV)^0.5,0)
SideMeasure<-Measures$FEV/2/SideLake

#Compute lowest cost efficacy for color shading control
LowestCostEff<-ceiling(max(Measures$Cost/Measures$FEV,na.rm=T)*1.01/50)*50

#Color ramp which is dependent on cost efficacy
ColorMaxDarkness<-1
ColorMinDarkness<-0.18
ColorsPlain<-col2rgb(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) #"blue","orange","green","purple"))
ColorSerie<-rep(NA,Nbmeasure)
for (i in (1:Nbmeasure))
{
  ColorSerie[i]<-rgb(ColorsPlain[1,i]/255
                      ,ColorsPlain[2,i]/255
                      ,ColorsPlain[3,i]/255
                      ,(Measures$Cost[i]/Measures$FEV[i])/LowestCostEff*(ColorMaxDarkness-ColorMinDarkness)+ColorMinDarkness)
}

#Function writing text on plott
TextCost<-function(X0,X1,Y0,Cost,Effectiness,Measure,adj=c(0.5, 1.5))
{
  arrows(x0=X0, y0=Y0, x1 = X1, y1 = Y0, length = 0.1,code = 3)
  #Lower arrow
  text(SideLake/2#(X0+X1)/2
       ,Y0,paste(round(Cost,-5)/1000000,"M€ = ",round((Cost)/(100*Effectiness)/1000,0),"k€/1% FEV")
       ,adj=adj,cex=0.8)
  #Upper arrow
  text(SideLake/2#(X0+X1)/2
       ,Y0,paste(Measure,":",round((X1-X0)/SideLake*100,0),"% of FEV"),cex=0.8,adj=(adj-c(0,2)))
}

####################################################################
####################################################################
################# ACTUAL PLOTING----
####################################################################
####################################################################

  Extention<-1+Nbmeasure*0.1
  png(file=paste0("Output/Lake",".png"),height=12.2,width=10,units="cm",res=350,family="serif")
  {
    par(mar=c(2,2.5,1,0.9),mgp=c(1,0.6, 0),las=1,lty="solid",lwd=1,xaxs="i",yaxs="i",bty="o")
    
    plot(c(0,ceiling(SideLake/25)*25),c(0,ceiling(SideLake/25)*25*Extention),type="n",bty="o",xlab="x (m)",ylab="",asp=1,)

    X<-0
    for (i in (1:Nbmeasure)) #Each measure
    {
      rect(X,0,X+SideMeasure[i],SideLake,col=ColorSerie[i])
      X<-X+SideMeasure[i]
    }
    X<-0
    for (i in (1:Nbmeasure)) #Each measure
    {
      TextCost(X0=X,X1=X+SideMeasure[i],Y0=i*SideLake*0.6/Nbmeasure,Cost=Measures$Cost[i]
               ,Effectiness =  Measures$FEV[i]/FEV$FEV,paste(Measures$Feature[i]),adj=c(0.9,1.5))
      X<-X+SideMeasure[i]
      
    }
    rect(0,0,SideLake,SideLake,lwd=2)#Whole lake
    TextCost(X0=0,X1=sum(SideMeasure),Y0=0.8*SideLake,Cost=sum(Measures$Cost),1,"Total")
    
    legend("topright"#Side,SideLake*Extention,xjust=1
           ,title=" ",legend=rev(Measures$Name),bty="n",cex=0.93,title.adj=0.8)
    ramp<-c(ceiling(min(Measures$Cost/Measures$FEV)/5)*5,LowestCostEff/2,LowestCostEff)/LowestCostEff
     for (i in (1:Nbmeasure))
    {
      rect(SideLake*0.05,SideLake*(1+i*0.06)/Extention*1.3,SideLake*0.15,SideLake*(1+i*0.06)/Extention*1.34
           ,col=rgb(ColorsPlain[1,i]/255,ColorsPlain[2,i]/255,ColorsPlain[3,i]/255,ramp[1]*(ColorMaxDarkness-ColorMinDarkness)+ColorMinDarkness))
       rect(SideLake*0.20,SideLake*(1+i*0.06)/Extention*1.3,SideLake*0.30,SideLake*(1+i*0.06)/Extention*1.34
            ,col=rgb(ColorsPlain[1,i]/255,ColorsPlain[2,i]/255,ColorsPlain[3,i]/255,ramp[2]*(ColorMaxDarkness-ColorMinDarkness)+ColorMinDarkness))
       rect(SideLake*0.35,SideLake*(1+i*0.06)/Extention*1.3,SideLake*0.45,SideLake*(1+i*0.06)/Extention*1.34
            ,col=rgb(ColorsPlain[1,i]/255,ColorsPlain[2,i]/255,ColorsPlain[3,i]/255,ramp[3]*(ColorMaxDarkness-ColorMinDarkness)+ColorMinDarkness))
       
          text(SideLake*(0.10+(i-1)*.15),SideLake*Extention*.93,paste(ramp[i]*LowestCostEff),pos=3,cex=0.8)
    }
    text(SideLake*(0.27+(3-1)*.15),SideLake*Extention*.93,"k€/1% FEV",pos=3,cex=0.8)
  }
  
  dev.off()
