library(tidyr)
library(dplyr)
library(magrittr)
library(gdata)


file.create("SWAT_flag.txt")
system("swat2012.exe",wait=FALSE,invisible=FALSE)
n<-1

while(n<22) #SWAT simulation period: 22 years
{
  while (file.exists("SWAT_flag.txt"))
  {
  }
  
  ###################################################################################
  #ABM output variables initialization (SWAT input variables)
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  nosb<-47;nohru<-376;nores<-10;#number of subbasins, hru's, reservoirs
  
  initcrops<-read.table("Crop_initial.txt",header = FALSE, sep = "")
  colnames(initcrops) <- c("SB_ID","HRU_ID","Area","PlantDate","PotIrri","Irri_TS","Irri_eff")
  i_rice_area <- initcrops[,3][seq(1, length(initcrops[,3]), 8)]
  r_rice_area <- initcrops[,3][seq(2, length(initcrops[,3]), 8)]
  i_upl_area <- initcrops[,3][seq(3, length(initcrops[,3]), 8)]
  r_upl_area <- initcrops[,3][seq(4, length(initcrops[,3]), 8)]
  forest_area <- initcrops[,3][seq(5, length(initcrops[,3]), 8)]
  grass_area <- initcrops[,3][seq(6, length(initcrops[,3]), 8)]
  urban_area <- initcrops[,3][seq(7, length(initcrops[,3]), 8)]
  wetland_area <- initcrops[,3][seq(8, length(initcrops[,3]), 8)]
  
  #for now all hru's are initialized with same irr_eff and all reservoirs with the same starg and ndtargr so no need to extract data
   res_ini <- read.csv(file="resini.csv")
  irr_eff<-vector(,nohru) 
  irr_eff[]<-0.5
  starg<-matrix(nrow=nores, ncol=12)
  starg[]<-99000
  ndtargr<-vector(,nores)
  ndtargr[]<-15
  
  
  #######################################################################################################
  #SWAT output variables (ABM input variables)
  ###### Streamflow 
  flow_mekong <- read.table("Flow_Mekong.txt")
  colnames(flow_mekong)[1:49] <- c("year","cal_day",paste0("Flow_SB_",1:47))
  
  ABM_flow <- flow_mekong %>% 
    tbl_df() %>%
    gather(key=Subbasin,value=Flow,-year,-cal_day) %>%
    mutate(SB_ID = extract_numeric(Subbasin)) %>%
    select(-Subbasin)
  
  ####### Reservoir Storage, outflow and surface area
  reservoir_mekong <- read.table("Reservoir_Mekong.txt")
  colnames(reservoir_mekong)[1:32] <- c("year","cal_day",paste0("Volume_Res",1:10),paste0("SurfArea_Res",1:10),paste0("Outflow_Res",1:10))
  
  ABM_reservoir <- reservoir_mekong %>% 
    tbl_df() %>% 
    gather(key=Att,value=Variable,-year,-cal_day) %>%
    separate(col=Att,into=c("Attr","Reservoir"),sep="_") %>%
    mutate(Reservoir = extract_numeric(Reservoir)) 
  
  ####### Crop yield
  crop_mekong  <- read.table("Crop_Mekong.txt")
  colnames(crop_mekong) <- c("year","SB_ID","HRU_ID","yield","IWW")
  
  ###############################
  #Upper level constraints
  #DoE
  #DoA
  #DoEnv
  
  #min_cy: this will be a dataframe that provides a minimum constraint on crop yield for each HRU where there is cropping
  #min_cprod: this will be a dataframe that provides a minimum constraint on crop production for each HRU where there is cropping
  #min_hydpow: this will be a dataframe that provides a minimum constraint on hydropower for each reservoir
  
  ################################
  #Post-calculation for (From SWAT output) ecosystem services
  
  # calculate water availability for domestic use
  # calculate water availability for industrial use
  # ecosystem requirements

  ############################################################################################################################
  ############################################################################################################################
  # Loops through agents, subbasins, HRU's and reservoir
  ############################################################################################################################
  ############################################################################################################################
  
  for (a in aaa){#agent loop
    message<-paste("agent=",a)
    write(message,"")

    for (s in sss){#subbasin loop
      message<-paste("subbasin=",s)
      write(message,"")

      for (h in hhh){#HRU loop
        message<-paste("HRU=",h)
        write(message,"")
        
        ####################################################
        #if-then-else decision making at the HRU level
        if (cy < min_cy[h]){irr.eff[h] <- irr.eff[h]*1.1} else {}
        
        if (cprod < min_cprod[h]){
          irr.eff[h] <- irr.eff[h]*1.1
          croparea[h] <- min(max_croparea[h],croparea[h]*1.1)
        } else {}
        
      }#end HRU
    }#end subbasin
    
    for (r in rrr){#reservoir loop
      message<-paste("reservoir=",r)
      write(message,"")
      
      #####################################################
      hydpow[r] <- streamflow * drop_hydpow[r]
      
      if (hydpow[r] < min_hydpow[r]){
        resvol[r] <- resvol[r]*0.9 # if hydropower generated is less than the minimum constraint, release more water from reservoir storage
      } else {}
      
      
    }#end reservoir
    
    for (e in eee){#ecosystem loop
      message<-paste("ecosystem=",e)
      write(message,"")
      

    }#end ecosystem
    
    #########################################################
    #save decision results for each agent

    
  }#end agent
  
############################################################################################################################
############################################################################################################################
# Write ABM output (SWAT input) to data file
############################################################################################################################
############################################################################################################################

  res<-cbind(starg,ndtargr)
  land_area<-cbind(i_rice_area,r_rice_area,i_upl_area,r_upl_area,forest_area,grass_area,urban_area,wetland_area)
  write.table(irr_eff,file="Irr_eff_by_R.txt",col.names = F, row.names = F)
  write.table(res,file="Reservoir_by_R.txt", col.names = F, row.names = F) 
  write.table(land_area,file="Landclass_Area_by_R.txt", col.names = F, row.names = F) 
  
  file.create("SWAT_flag.txt")
  n<-n+1
}
