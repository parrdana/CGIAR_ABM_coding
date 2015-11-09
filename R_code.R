library(tidyr)
library(dplyr)
library(gdata)


file.create("SWAT_flag.txt")
system("test.exe",wait=FALSE,invisible=FALSE) #load the simulaiton model and allow it to complete the simulation for the first year
n<-1

while(n<20) #20 is the number of years during the simulation period
{
  while (file.exists("SWAT_flag.txt")) #wait until the simulation for year n is completed
  {
  }
  
  
  ##################################################################################
  #SWAT output variables (ABM input variables)
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  v<-scan(file="data_exchange.txt","r") #receive the value of state variable of the simulation model at the end of year n and output them
  message<-paste("Value of state variable in SWAT at the end of year",n,"is:",v)
  write(message,"")
  
  #Stream flow
  #Storage, outflow and surface area
  #Water withdrawal
  #Crop yield
  
  
  
  ##################################################################################
  #Upper level constraints
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #DoE
  #DoA
  #DoEnv
  
  
  ##################################################################################
  #Post-calculation for (From sWAT output) ecosystem services
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  ###################################################################################
  #ABM output variables initialization (SWAT input variables)
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #Irrigation efficiency (subbasin/HRU level, 1 number)
  IRR_EFF<- array(rep(-999,312), c(12, 13, 2)) #(agent,subbasin,HRU) ,dimnames=list("NULL", "NULL", c("HR1","HR2"))
  IRR_EFF[,,2]<-0.4
  #Rice  irrigated area (subbasin/HRU level, 1 number)
  RICE_AREA<- array(rep(-999,312), c(12, 13, 2))
  #Rice  planting date (agent/subbasin level, 1 date)
  RICE_PD<-matrix(rep(-999,156),12) #(agent,subbasin)
  #Rice  harvest date (agent/subbasin level, 1 date)
  RICE_HD<-matrix(rep(-999,156),12)
  #Upland crops area (subbasin/HRU level, 1 number)
  UPL_AREA<- array(rep(-999,312), c(12, 13, 2))
  #Upland crops date (agent/subbasin level, 1 date)
  UPL_PD<-matrix(rep(-999,156),12)
  #Upland crops date (agent/subbasin level, 1 date)
  UPL_HD<-matrix(rep(-999,156),12)
  #Reservoir target volume (reservoir, 1 number for each month)
  RES_VOL<-array(rep(-999,312), c(12, 5, 12))#(agent,reservoir#,month)
  #Number of days to meet the target volume (reservoir, 1 number)
  RES_DAY<-array(rep(-999,312), c(12, 5, 12))#(agent,reservoir#,month)#for now this assumes 5 reserviors max per sub-basin
  
  #######################################################################################
  #number of agents
  aaa<-1:12
  
  #number of subbasin in each agent
  ss<-rep(0,12);ss[1]<-10;ss[2]<-2;ss[3]<-1;ss[4]<-7;ss[5]<-2;ss[6]<-12;ss[7]<-4;ss[8]<-5;ss[9]<-13;ss[10]<-2;ss[11]<-3;ss[12]<-3;
  
  #number of reseviors in each agent
  #rr<-rep(0,12);rr[1]<-10;rr[2]<-2;rr[3]<-1;rr[4]<-7;rr[5]<-2;rr[6]<-12;rr[7]<-4;rr[8]<-5;rr[9]<-13;rr[10]<-2;rr[11]<-3;rr[12]<-3;
  rr<-rep(3,12)#temporary
  
  #number of ecosystem hotspots in each agent
  #ee<-rep(0,12);ee[1]<-10;ee[2]<-2;ee[3]<-1;ee[4]<-7;ee[5]<-2;ee[6]<-12;ee[7]<-4;ee[8]<-5;ee[9]<-13;ee[10]<-2;ee[11]<-3;ee[12]<-3;
  ee<-rep(3,12)#temporary
  
  #number of HRU within each agent and subbasin
  hh<-matrix(rep(0,156),12)#12agents*13subbasins(max # of subbasins in an agent)=156
  hh[1,1]<-2;hh[1,2]<-2;hh[1,3]<-2;hh[1,4]<-2;hh[1,5]<-2;hh[1,6]<-2;hh[1,7]<-2;hh[1,8]<-2;hh[1,9]<-2;hh[1,10]<-2;
  hh[2,1]<-2;hh[2,2]<-2;
  hh[3,1]<-2;
  hh[4,1]<-2; hh[4,2]<-2; hh[4,3]<-2; hh[4,4]<-2; hh[4,5]<-2; hh[4,6]<-2; hh[4,7]<-2;
  hh[5,1]<-2;hh[5,2]<-2;
  hh[6,1]<-2;hh[6,2]<-2;hh[6,3]<-2;hh[6,4]<-2;hh[6,5]<-2;hh[6,6]<-2;hh[6,7]<-2;hh[6,8]<-2;hh[6,9]<-2;hh[6,10]<-2;hh[6,11]<-2;hh[6,12]<-2;
  hh[7,1]<-2;hh[7,2]<-2;hh[7,3]<-2;hh[7,4]<-2;
  hh[8,1]<-2;hh[8,2]<-2;hh[8,3]<-2;hh[8,4]<-2;hh[8,5]<-2;
  hh[9,1]<-2;hh[9,2]<-2;hh[9,3]<-2;hh[9,4]<-2;hh[9,5]<-2;hh[9,6]<-2;hh[9,7]<-2;hh[9,8]<-2;hh[9,9]<-2;hh[9,10]<-2;hh[9,11]<-2;hh[9,12]<-2;hh[9,13]<-2;
  hh[10,1]<-2;hh[10,2]<-2;
  hh[11,1]<-2;hh[11,2]<-2;hh[11,3]<-2;
  hh[12,1]<-2;hh[12,2]<-2;hh[12,3]<-2;
  
  for (a in aaa){#agent loop
    message<-paste("agent=",a)
    write(message,"")
    sss<-1:ss[a]#index for subbasin loop for specific agent "a"
    rrr<-1:rr[a]#index for reservior loop
    eee<-1:ee[a]#index for ecosystem hotspot loop
    
    for (s in sss){#subbasin loop
      message<-paste("subbasin=",s)
      write(message,"")
      hhh<-1:hh[a,s]#index for HRU loop
      
      for (h in hhh){#HRU loop
        message<-paste("HRU=",h)
        write(message,"")
        
        ####################################################
        #if-then-else decision making at the HRU level
        #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        incremental_value<-sample(1:100, 1, replace=TRUE) #gnerate a random incremental value; replace this piece of code with operations on agents which could be based on the state variable from simulation model
        message<-paste("random incremental value produced at the end of year",n,"is",incremental_value)
        write(message,"")
        
      }#end HRU
    }#end subbasin
    
    for (r in rrr){#reservoir loop
      message<-paste("reservoir=",r)
      write(message,"")
    
      #####################################################
      #if-then-else decision making at the agent level
      #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    }#end reservoir
    
    for (e in eee){#ecosystem loop
      message<-paste("ecosystem=",e)
      write(message,"")
    
      #####################################################
      #if-then-else decision making at the agent level
      #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    }#end ecosystem
    
    #########################################################
    #save decision results for each agent
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
    
  }#end agent
  
  
  ##############################################################
  #Write ABM output (SWAT input) to data file
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  #need to make it so that it will not print out no data points (if there is only 1 HRU in a certain subbasin
  #then for second HRU in that subbasin data =-999) - probably should use NA options

  for (a in aaa){#agent loop
    sss<-1:ss[a]
    rrr<-1:rr[a]
    
    for (r in rrr){#reservoir loop
      #######################################################
      # DATA FRAME FOR RESERVIOR
      # data is specific just to agent (as well as resevior number and month)
      #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      RES_VOL_out1=RES_VOL[a,r,1];RES_VOL_out2=RES_VOL[a,r,2];RES_VOL_out3=RES_VOL[a,r,3];RES_VOL_out4=RES_VOL[a,r,4];RES_VOL_out5=RES_VOL[a,r,5];RES_VOL_out6=RES_VOL[a,r,6];RES_VOL_out7=RES_VOL[a,r,7];RES_VOL_out8=RES_VOL[a,r,8];RES_VOL_out9=RES_VOL[a,r,9];RES_VOL_out10=RES_VOL[a,r,10];RES_VOL_out11=RES_VOL[a,r,11];RES_VOL_out12=RES_VOL[a,r,12];
      RES_DAY_out1=RES_DAY[a,r,1];RES_DAY_out2=RES_DAY[a,r,2];RES_DAY_out3=RES_DAY[a,r,3];RES_DAY_out4=RES_DAY[a,r,4];RES_DAY_out5=RES_DAY[a,r,5];RES_DAY_out6=RES_DAY[a,r,6];RES_DAY_out7=RES_DAY[a,r,7];RES_DAY_out8=RES_DAY[a,r,8];RES_DAY_out9=RES_DAY[a,r,9];RES_DAY_out10=RES_DAY[a,r,10];RES_DAY_out11=RES_DAY[a,r,11];RES_DAY_out12=RES_DAY[a,r,12];
      
      if (a==1 & r==1) {
        #if the first row is the names it is easier to write out with a fixed width format and include the names (otherwise names are not fixed width)
        dfres<-data.frame("year","agent","reservior","T.Vol(Jan)","T.Vol(Feb)","T.Vol(Mar)","T.Vol(Apr)","T.Vol(May)","T.Vol(Jun)","T.Vol(Jul)","T.Vol(Aug)","T.Vol(Sep)","T.Vol(Oct)","T.Vol(Nov)","T.Vol(Dec)","T.Days(Jan)","T.Days(Feb)","T.Days(Mar)","T.Days(Apr)","T.Days(May)","T.Days(Jun)","T.Days(Jul)","T.Days(Aug)","T.Days(Sep)","T.Days(Oct)","T.Days(Nov)","T.Days(Dec)",stringsAsFactors =FALSE)
        dfres<-rbind(dfres,c(n,a,r,RES_VOL_out1,RES_VOL_out2,RES_VOL_out3,RES_VOL_out4,RES_VOL_out5,RES_VOL_out6,RES_VOL_out7,RES_VOL_out8,RES_VOL_out9,RES_VOL_out10,RES_VOL_out11,RES_VOL_out12,RES_DAY_out1,RES_DAY_out2,RES_DAY_out3,RES_DAY_out4,RES_DAY_out5,RES_DAY_out6,RES_DAY_out7,RES_DAY_out8,RES_DAY_out9,RES_DAY_out10,RES_DAY_out11,RES_DAY_out12))
        #colnames(dfres)=c("year","agent","reservior","T.Vol(Jan)","T.Vol(Feb)","T.Vol(Mar)","T.Vol(Apr)","T.Vol(May)","T.Vol(Jun)","T.Vol(Jul)","T.Vol(Aug)","T.Vol(Sep)","T.Vol(Oct)","T.Vol(Nov)","T.Vol(Dec)","T.Days(Jan)","T.Days(Feb)","T.Days(Mar)","T.Days(Apr)","T.Days(May)","T.Days(Jun)","T.Days(Jul)","T.Days(Aug)","T.Days(Sep)","T.Days(Oct)","T.Days(Nov)","T.Days(Dec)")
      }else{
        dfres<-rbind(dfres,c(n,a,r,RES_VOL_out1,RES_VOL_out2,RES_VOL_out3,RES_VOL_out4,RES_VOL_out5,RES_VOL_out6,RES_VOL_out7,RES_VOL_out8,RES_VOL_out9,RES_VOL_out10,RES_VOL_out11,RES_VOL_out12,RES_DAY_out1,RES_DAY_out2,RES_DAY_out3,RES_DAY_out4,RES_DAY_out5,RES_DAY_out6,RES_DAY_out7,RES_DAY_out8,RES_DAY_out9,RES_DAY_out10,RES_DAY_out11,RES_DAY_out12))
      }#endif
    }#end reservoir

    for (s in sss){#subbasin loop
      hhh<-1:hh[a,s]
      
      for (h in hhh){#HRU loop
        ####################################################################
        # DATA FRAME FOR CROPS
        # data is specific to HRU level except crop planting and harvesting dates which
        # are specific for just subbasins- for now they are repeated for each HRU identically
        #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IRR_EFF_out=IRR_EFF[a,s,h];RICE_PD_out=RICE_PD[a,s];RICE_AREA_out=RICE_AREA[a,s,h];RICE_HD_out=RICE_HD[a,s];UPL_PD_out=UPL_PD[a,s];UPL_AREA_out=UPL_AREA[a,s,h];UPL_HD_out=UPL_HD[a,s];
       
        if (a==1 & s==1 & h==1) {
          #if the first row is the names it is easier to write out with a fixed width format and include the names (otherwise names are not fixed width)
          dfcrops<-data.frame("year","agent","subbasin","HRU","Irr. Eff.","Rice PD","Rice Area","Rice HD","UPland PD","Upland Area","Upland HD",stringsAsFactors =FALSE)
          dfcrops<-rbind(dfcrops,c(n,a,s,h,IRR_EFF_out,RICE_PD_out,RICE_AREA_out,RICE_HD_out,UPL_PD_out,UPL_AREA_out,UPL_HD_out))
          #colnames(dfcrops)=c("year","agent","subbasin","HRU","Irr. Eff.","Rice PD","Rice Area","Rice HD","UPland PD","Upland Area","Upland HD")
        }else{
          dfcrops<-rbind(dfcrops,c(n,a,s,h,IRR_EFF_out,RICE_PD_out,RICE_AREA_out,RICE_HD_out,UPL_PD_out,UPL_AREA_out,UPL_HD_out))
        }#endif
      }#end HRU
    }#end subbasin
  }#end agent
  
  #writing out to files
  
  #using capture.output works but often creates extra rows if not all the columns appear on a single line
  #cat(capture.output(dfcrops), file = paste("ABMcrops_year",n,"output.txt"), sep = '\n')
  #cat(capture.output(dfres), file = paste("ABMres_year",n,"output.txt"), sep = '\n')
  
  #using write.fwf works but column names should be included in data rather than as names or else they do not have fixed width (only data does)
  #for now different files are created for each year but final will only have 1 interaction per year so will only need one file
  write.fwf(dfres, file=paste0("ABMres_year",n,"_output.txt"),sep=" ",quoteInfo=TRUE,append=FALSE,quote=FALSE,rownames=FALSE,colnames=FALSE,justify="left",width=12)
  write.fwf(dfcrops, file=paste0("ABMcrops_year",n,"_output.txt"),sep=" ",quoteInfo=TRUE,append=FALSE,quote=FALSE,rownames=FALSE,colnames=FALSE,justify="left",width=12)
  #creates warning about recycling "width" but seems to work properly
  
  #create flag after finishing writing data so SWAT will run next year
  file.create("SWAT_flag.txt")
  n<-n+1#year index
  
}#end year

#############################################
# wite decision results
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
