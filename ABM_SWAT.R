library(tidyr)
library(dplyr)
library(magrittr)
library(gdata)


#trigger to keep track of long term hydropower generation and whether it falls below minimum
hptrig <- matrix(rep(FALSE,100),10); 
hptrigcount <- rep(0,10)#this count is used to ensure that it has been 10 years since start of simulation or 10 years since hydropower regulations have been altered


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
  
  crop_hru <- read.table("Crop_initial.txt")
  colnames(crop_hru) <- c("SB_ID","HRU_ID","Area","PlantDate","PotIrri","Irri_TS","Irri_eff")
  if (n<=3){#year 3 is the first year of simulation (we do not want to reinitialize values as Crop_initial.txt will not be updated)
    irice_area <- crop_hru[,3][seq(1, length(crop_ini[,3]), 8)]
    rrice_area <- crop_hru[,3][seq(2, length(crop_ini[,3]), 8)]
    iupl_area <- crop_hru[,3][seq(3, length(crop_ini[,3]), 8)]
    rupl_area <- crop_hru[,3][seq(4, length(crop_ini[,3]), 8)]
    forest_area <- crop_hru[,3][seq(5, length(crop_ini[,3]), 8)]
    grass_area <- crop_hru[,3][seq(6, length(crop_ini[,3]), 8)]
    urban_area <- crop_hru[,3][seq(7, length(crop_ini[,3]), 8)]
    wetland_area <- crop_hru[,3][seq(8, length(crop_ini[,3]), 8)]
    irr_eff <- crop_hru[,7]
  }

  res_ini <- read.table(file="Reservoir_initial.txt")
  if (n<=3){#year 3 is the first year of simulation (we do not want to reinitialize values as Reservoir_initial.txt will not be updated)
    starg <- res_ini[,6:17]
    ndtargr <- res_ini[,18]
  }
  
  ag_sb <- read.csv("MK_Agent_Sub_basins0907.csv")
  sb_char <- read.csv("Subbasins_char.csv") %>% 
    tbl_df() %>% 
    select(Subbasin,Area) %>% 
    left_join(ag_sb,by="Subbasin") %>% 
    rename(SB_ID = Subbasin, SB_Area=Area) %>% 
    left_join(crop_hru,by="SB_ID") %>% 
    select(Agent_ID,SB_ID,SB_Area,HRU_ID,Area:Irri_eff)
  
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
  
  readhydpow <- read.csv("reservoir_#_to_name_111815.csv",stringsAsFactors=FALSE)# mean annual energy (GW)
  mean_hydpow <- as.numeric(readhydpow$Mean.Annual.Energy..GW.) #(GW)
  if (n<=3){min_hydpow <- mean_hydpow *0.9}#first year of simulation is year 3 --> set min at 90% of mean for now
  if (n>3){min_hydpow <- min_hydpow*1.07}#increase 7% per year (first year of simulation is year 3)
  #power demands  expected to increase by about 7% per year between (2010 and 2030)
  
  #head <- #net available head is necessary for hydpow
  #hydpow = rho*g*Head*Q/1,000,000,000 for GW 

  ################################
  #Post-calculation for (From SWAT output) ecosystem services
  
  # calculate water availability for domestic use
  # calculate water availability for industrial use
  # ecosystem requirements
  
  ############################################################################################################################
  ############################################################################################################################
  # Loops through agents, subbasins, HRU's and reservoir and ecosystem
  ############################################################################################################################
  ############################################################################################################################
  
  #number of agents
  aaa<-1:12
  
  #number of subbasin in each agent
  ss<-rep(0,12);ss[1]<-9;ss[2]<-2;ss[3]<-1;ss[4]<-6;ss[5]<-1;ss[6]<-6;ss[7]<-4;ss[8]<-5;ss[9]<-5;ss[10]<-2;ss[11]<-3;ss[12]<-3;
  
  #number of reseviors in each agent
  rr<-rep(0,12);rr[1]<-4;rr[2]<-0;rr[3]<-0;rr[4]<-2;rr[5]<-1;rr[6]<-1;rr[7]<-0;rr[8]<-1;rr[9]<-1;rr[10]<-0;rr[11]<-0;rr[12]<-0;
  
  #number of ecosystem hotspots in each agent
  #ee<-rep(0,12);ee[1]<-10;ee[2]<-2;ee[3]<-1;ee[4]<-7;ee[5]<-2;ee[6]<-12;ee[7]<-4;ee[8]<-5;ee[9]<-13;ee[10]<-2;ee[11]<-3;ee[12]<-3;
  ee<-rep(3,12)#temporary
  
  #number of HRU within each agent and subbasin
  hh<-matrix(rep(8,108),12)#12agents*9subbasins(max # of subbasins in an agent)=108
  
  sn=0#for continuous subbasin index
  hn=0#for continuous HRU index
  rn=0#for continuous resevior index
  for (a in aaa){#agent loop
    message<-paste("agent=",a)
    write(message,"")
    sss<-1:ss[a]#index for subbasin loop for specific agent "a"
    rrr<-1:rr[a]#index for reservior loop
    eee<-1:ee[a]#index for ecosystem hotspot loop
    
    for (s in sss){#subbasin loop
      sn=sn+1#which subbasin out of 47 model is currently on
      message<-paste("subbasin=",s)
      write(message,"")
      hhh<-1:hh[a,s]#index for HRU loop
      
      for (h in hhh){#HRU loop
        hn=hn+1#which HRU out of 376 model is currently on
        message<-paste("HRU=",h)
        write(message,"")
        
        if (h==1){#irrigated rice
          ####################################################
          #if-then-else decision making at the HRU level
          if (cy[hn] < min_cy[hn]){irr.eff[hn] <- irr.eff[hn]*1.1} else {}
          eff_cost[hn] <- (irr.eff[hn]*.1)*costfactor #cost of changing irr_eff should be recorded
          
          if (cprod[hn] < min_cprod[hn]){
            irr.eff[hn] <- irr.eff[hn]*1.1
            eff_cost[hn] <- (irr.eff[hn]*.1)*costfactor #cost of changing irr_eff should be recorded
            change_area <- min(max_irice_area[sn],irice_area[sn]*1.1)-irice_area[sn]#area added to irice must be taken from another HRU
            irice_area[sn] <- min(max_irice_area[sn],irice_area[sn]*1.1)
            
            #which HRU's the additional crop area is taken from (weights add to 1) should depend on survey?
            rrice_area[sn] <- rrice_area[sn]-change_area*weight1
            rupl_area[sn] <- rupl_area[sn]-change_area*weight2
            forest_area[sn] <- forest_area[sn]-change_area*weight3
            grass_area[sn] <- grass_area[sn]-change_area*weight4
            #urban_area <- probably can't take from urban?
            #wetland_area <- probably don't want to take from wetland?
            
          } else {}
        }
        if (hh==3){#irrigated upland crop
          if (cy[hn] < min_cy[hn]){irr.eff[hn] <- irr.eff[hn]*1.1} else {}
          
          if (cprod[hn] < min_cprod[hn]){
            irr.eff[hn] <- irr.eff[hn]*1.1
            eff_cost[hn] <- (irr.eff[hn]*.1)*costfactor #cost of changing irr_eff should be recorded
            change_area <- min(max_iupl_area[sn],iupl_area[sn]*1.1)-iupl_area[sn]#area added to irice must be taken from another HRU
            iupl_area[sn] <- min(max_iupl_area[sn],iupl_area[sn]*1.1)
            
            #which HRU's the additional crop area is taken from (weights add to 1) should depend on survey?
            rrice_area[sn] <- rrice_area[sn]-change_area*weight1
            rupl_area[sn] <- rupl_area[sn]-change_area*weight2
            forest_area[sn] <- forest_area[sn]-change_area*weight3
            grass_area[sn] <- grass_area[sn]-change_area*weight4
            #urban_area <- probably can't take from urban?
            #wetland_area <- probably don't want to take from wetland?
            
          } else {}
        }
      }#end HRU
    }#end subbasin
    
    if (rr[a]!=0){
      for (r in rrr){#reservoir loop
        rn=rn+1#which reservior out of 10 the model is currently on
        message<-paste("reservoir=",r)
        write(message,"")
        
        hydpow[rn] <- sum(reservoir_mekong[which(reservoir_mekong$year==n),rn+22])*drop_hydpow[rn]
        #hydpow = rho*g*H*Q/1,000,000,000 for GW 
        hydpow[rn] <- 1000*9.8*head*sum(reservoir_mekong[which(reservoir_mekong$year==n),rn+22])
        
        hptrigcount[rn] <- hptrigcount[rn]+1
        
        if (hydpow[rn]< min_hydpow[rn]){
          #asign TRUE to most recent year and replace all other years with the value from the year before
          hptrig[rn,10]<-hptrig[rn,9];hptrig[rn,9]<-hptrig[rn,8];hptrig[rn,8]<-hptrig[rn,7];hptrig[rn,7]<-hptrig[rn,6];hptrig[rn,6]<-hptrig[rn,5];hptrig[rn,5]<-hptrig[rn,4];hptrig[rn,4]<-hptrig[rn,3];hptrig[rn,3]<-hptrig[rn,2];hptrig[rn,2]<-hptrig[rn,1];hptrig[rn,1]<-TRUE;
          if (all(hptrig[])==TRUE & hptrigcount[rn]>=10){#if hydropower generated is less than the minimum over last 10 years 
            #decrease number of days required to reach target and target storage during dry season
            starg[rn,-(4:10)] <- starg[rn,-(4:10)]*0.7
            ndtargr[rn] <- ndtargr[rn]-2
            hptrigcount[rn]=0#reset hydropower trigger count after changing reservoir managemnt practices to ensure it will not be changed for at least 10 more years (if ever again)
            
          }else if (sum(hptrig[rn,])==9 & hptrigcount[rn]>=10){#if hydropower generated is less than the minimum for 9 of last 10 years
            #decrease number of days required to reach target and target storage to a lesser extent than for 10/10 years
            starg[rn,-(4:10)] <- starg[rn,-(4:10)]*0.8
            ndtargr[rn] <- ndtargr[rn]-1
            hptrigcount[rn]=0#reset hydropower trigger count
            
          }else if (sum(hptrig[rn,])==8 & hptrigcount[rn]>=10){#if hydropower generated is less than the minimum for 8 of last 10 years
            #decrease number of days required to reach target and target storage to a lesser extent than for 9/10 years
            starg[rn,-(4:10)] <- starg[rn,-(4:10)]*0.9
            ndtargr[rn] <- ndtargr[rn]-1
            hptrigcount[rn]=0#reset hydropower trigger count
            
          }else{}
          
        }else{
          #asign FALSE to most recent year and replace all other years with the value from the year before
          hptrig[rn,10]<-hptrig[rn,9];hptrig[rn,9]<-hptrig[rn,8];hptrig[rn,8]<-hptrig[rn,7];hptrig[rn,7]<-hptrig[rn,6];hptrig[rn,6]<-hptrig[rn,5];hptrig[rn,5]<-hptrig[rn,4];hptrig[rn,4]<-hptrig[rn,3];hptrig[rn,3]<-hptrig[rn,2];hptrig[rn,2]<-hptrig[rn,1];hptrig[rn,1]<-FALSE;
        }
        
      }#end reservoir
    }#end if !0
    
    #########################################################
    #save decision results for each agent
    
  }#end agent
  
  ############################################################################################################################
  ############################################################################################################################
  # Write ABM output (SWAT input) to data file
  ############################################################################################################################
  ############################################################################################################################
  
  res<-cbind(starg,ndtargr)
  land_area<-cbind(irice_area,rrice_area,iupl_area,rupl_area,forest_area,grass_area,urban_area,wetland_area)
  write.table(irr_eff,file="Irr_eff_by_R.txt",col.names = F, row.names = F)
  write.table(res,file="Reservoir_by_R.txt", col.names = F, row.names = F) 
  write.table(land_area,file="HRU_Area_by_R.txt", col.names = F, row.names = F) 
  
  file.create("SWAT_flag.txt")
  n<-n+1
}