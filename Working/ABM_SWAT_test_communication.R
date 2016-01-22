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
  nosb<-47;nohru<-376;nores<-10;noyear=20#number of subbasins, hru's, reservoirs, years
  
  crop_hru <- read.table("Crop_initial.txt")
  colnames(crop_hru) <- c("SB_ID","HRU_ID","Area","PlantDate","PotIrri","Irri_TS","Irri_eff","Irri_minflow")

  if (n<=3){#year 3 is the first year of simulation (we do not want to reinitialize values as Crop_initial.txt will not be updated)
    irice_area <- crop_hru[,3][seq(1, length(crop_hru[,3]), 8)]
    rrice_area <- crop_hru[,3][seq(2, length(crop_hru[,3]), 8)]
    iupl_area <- crop_hru[,3][seq(3, length(crop_hru[,3]), 8)]
    rupl_area <- crop_hru[,3][seq(4, length(crop_hru[,3]), 8)]
    forest_area <- crop_hru[,3][seq(5, length(crop_hru[,3]), 8)]
    grass_area <- crop_hru[,3][seq(6, length(crop_hru[,3]), 8)]
    urban_area <- crop_hru[,3][seq(7, length(crop_hru[,3]), 8)]
    wetland_area <- crop_hru[,3][seq(8, length(crop_hru[,3]), 8)]
    irr_eff <- crop_hru[,7]
  }

  res_ini <- read.table(file="Reservoir_initial.txt")
  if (n<=3){#year 3 is the first year of simulation (we do not want to reinitialize values as Reservoir_initial.txt will not be updated)
    starg <- res_ini[,6:17]
    ndtargr <- res_ini[,18]
    maxout <-res_ini[,19:30]
    minout <- res_ini[,31:42]
  }
  
  ag_sb <- read.csv("MK_Agent_Sub_basins0907.csv")
  sb_char <- read.csv("Subbasins_char.csv") %>% 
    tbl_df() %>% 
    select(Subbasin,Area) %>% 
    left_join(ag_sb,by="Subbasin") %>% 
    rename(SB_ID = Subbasin, SB_Area=Area) %>% 
    left_join(crop_hru,by="SB_ID") %>% 
    select(Agent_ID,SB_ID,SB_Area,HRU_ID,Area:Irri_eff)
  
  ############################################
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
  
  #note: reservoir 1 and 7 are not actually reservoirs- created due to issues with SWAT
  
  ####### Crop yield
  crop_mekong  <- read.table("Crop_Mekong.txt")
  colnames(crop_mekong) <- c("year","SB_ID","HRU_ID","yield","IWW")
  

  #Read in survey weights for agriculture, hydropower, ecosystems
  readweights <- read.csv("Survey_Retabulated.csv",stringsAsFactors=FALSE)# mean annual energy (GW)
  weights <- data.frame(readweights$Agriculture..,readweights$Hydropower..,readweights$Ecosystem.Services..)
  colnames(weights)<-c("A_weight","H_weight","E_weight")

  ############################################################################################################################
  ############################################################################################################################
  # Loops through agents, subbasins, HRU's and reservoir and ecosystem
  ############################################################################################################################
  ############################################################################################################################
  
  #making variables such that yield and res storage should be 0 --> to check communication
  #starg[,] <- 15000000000#990000000000
  #starg[,] <- 99999999 #10^4 m^3
  #ndtargr[] <- 365
  #maxout[,] <-5000000000 #*10^5?
  #minout[,] <- 999999 #*10^5?
  #irr_minflow <- crop_hru$Irri_minflow
  irr_minflow[] <-999999
  #irr_eff[] <- 0.1
  ############################################################################################################################
  ############################################################################################################################
  # Write ABM output (SWAT input) to data file
  ############################################################################################################################
  ############################################################################################################################
  
  hru_out<-crop_hru[,-8]
  res<-cbind(starg,ndtargr,maxout,minout)
  irr_out<-cbind(irr_eff,irr_minflow)
  write.table(irr_out,file="Irr_eff_by_R.txt",col.names = F, row.names = F)
  write.table(res,file="Reservoir_by_R.txt", col.names = F, row.names = F) 
  write.table(hru_out,file="HRU_FR_by_R.txt", col.names = F, row.names = F) 
  
  #########################################################
  #save decision variables for each year for later analysis
  #way this is written now basically just combines all years of simulation and writes similarly as for SWAT input only including year
  
  hru_out_save<-cbind(n,crop_hru[,-8])
  res_save<-cbind(n,starg,ndtargr,maxout,minout)
  irr_out_save<-cbind(n,irr_eff,irr_minflow)
  write.table(irr_out_save,file="save_Irr_eff_by_R.txt",col.names = F, row.names = F, append =T)
  write.table(res_save,file="save_Reservoir_by_R.txt", col.names = F, row.names = F, append =T) 
  write.table(hru_out_save,file="save_HRU_FR_by_R.txt", col.names = F, row.names = F, append =T) 
  
  
  file.create("SWAT_flag.txt")
  n<-n+1
}