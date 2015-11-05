file.create("SWAT_flag.txt")
system("test.exe",wait=FALSE,invisible=FALSE) #load the simulaiton model and allow it to complete the simulation for the first year
n<-1

while(n<20) #20 is the number of years during the simulation period
{
  while (file.exists("SWAT_flag.txt")) #wait until the simulation for year n is completed
  {
  }
  
  
  ##################################################################################
  #Upper level constraints
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #DoE
  #DoA
  #DoEnv
  
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
  #Post-calculation for (From sWAT output) ecosystem services
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  ###################################################################################
  #ABM output variables initialization (SWAT input variables)
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #Irrigation efficiency (subbasin/HRU level, 1 number)
  IRR_EFF<- array(rep(-999,312), c(12, 13, 2)) #(agent,subbasin,HRU)
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
  RES_VOL<-#array(rep(-999,312), c(12, #, 12))#(agent,resevior#,month)
  #Number of days to meet the target volume (reservoir, 1 number)
  RES_DAY<-#array(rep(-999,312), c(12, #, 12))#(agent,resevior#,month)
  
  #######################################################################################
  #number of agents
  aaa<-1:12
  #number of subbasin in each agent
  ss<-rep(0,12);ss[1]<-10;ss[2]<-2;ss[3]<-1;ss[4]<-7;ss[5]<-2;ss[6]<-12;ss[7]<-4;ss[8]<-5;ss[9]<-13;ss[10]<-2;ss[11]<-3;ss[12]<-3;
  #number of HRU within each agent and subbasin
  hh<-matrix(rep(0,156),12)#12agents*13subbasins(max # of subbasins in an agent)=156
  hh[1,1]<-2;hh[1,2]<-2;hh[1,3]<-2;hh[1,4]<-2;hh[1,2]<-2;hh[1,6]<-2;hh[1,7]<-2;hh[1,8]<-2;hh[1,9]<-2;hh[1,10]<-2;
  hh[2,1]<-2;hh[2,2]<-2;
  hh[3,1]<-2;
  hh[4,1]<-2; hh[4,2]<-2; hh[4,3]<-2; hh[4,4]<-2; hh[4,2]<-2; hh[4,6]<-2; hh[4,7]<-2;
  hh[2,1]<-2;hh[2,2]<-2;
  hh[6,1]<-2;hh[6,2]<-2;hh[6,3]<-2;hh[6,4]<-2;hh[6,2]<-2;hh[6,6]<-2;hh[6,7]<-2;hh[6,8]<-2;hh[6,9]<-2;hh[6,10]<-2;hh[6,11]<-2;hh[6,12]<-2;
  hh[7,1]<-2;hh[7,2]<-2;hh[7,3]<-2;hh[7,4]<-2;
  hh[8,1]<-2;hh[8,2]<-2;hh[8,3]<-2;hh[8,4]<-2;hh[8,2]<-2;
  hh[9,1]<-2;hh[9,2]<-2;hh[9,3]<-2;hh[9,4]<-2;hh[9,2]<-2;hh[9,6]<-2;hh[9,7]<-2;hh[9,8]<-2;hh[9,9]<-2;hh[9,10]<-2;hh[9,11]<-2;hh[9,12]<-2;hh[9,13]<-2;
  hh[10,1]<-2;hh[10,1]<-2;
  hh[11,1]<-2;hh[11,2]<-2;hh[11,3]<-2;
  hh[12,1]<-2;hh[12,2]<-2;hh[12,3]<-2;
  
  for (a in aaa){#agent loop
    message<-paste("agent=",a)
    write(message,"")
    sss<-1:ss[a]#index for subbasin loop for specific agent "a"
    
    #if-then-else decision making at the agent level
    
    #for (r in rrr){#reservior loop
      
    #}
    
    for (s in sss){#subbasin loop
      message<-paste("subbasin=",s)
      write(message,"")
      hhh<-1:hh[a,s]#index for HRU loop
      
      for (h in hhh){#HRU loop
        message<-paste("HRU=",h)
        write(message,"")
        
        incremental_value<-sample(1:100, 1, replace=TRUE) #gnerate a random incremental value; replace this piece of code with operations on agents which could be based on the state variable from simulation model
        message<-paste("random incremental value produced at the end of year",n,"is",incremental_value)
        write(message,"")
      }
    }
  }
  

  
  
  write(incremental_value,file="data_exchange.txt") #write the incremental vaue to data exchange file and pass it to simulation model
  file.create("SWAT_flag.txt")
  n<-n+1
}
