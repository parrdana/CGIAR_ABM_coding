library(tidyr)
library(dplyr)
library(magrittr)
library(gdata)

###################################################################################
# initialization 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
res_ini <- read.table(file="Reservoir_initial.txt")
starg <- res_ini[,6:17]
ndtargr <- res_ini[,18]
maxout <-res_ini[,19:30]
minout <- res_ini[,31:42]

#Need thresholds for decision changes
maxoutthres <-res_ini[,19:30]*0.5;
minoutthres <- res_ini[,31:42]+100000;
#!!!! warning !!!! these will need to be set more realistically (perhaps off of average monthly outflows once calibrated)

#day numbers in each month- used in ecossytem section
ndays<-matrix(0,ncol = 12, nrow = 2);ndays[1,1]<-1;ndays[2,1]<-31;ndays[1,2]<-32;ndays[2,2]<-59;ndays[1,3]<-60;ndays[2,3]<-90;ndays[1,4]<-91;ndays[2,4]<-120;ndays[1,5]<-121;ndays[2,5]<-151;ndays[1,6]<-152;ndays[2,6]<-181;
ndays[1,7]<-182;ndays[2,7]<-212;ndays[1,8]<-213;ndays[2,8]<-243;ndays[1,9]<-244;ndays[2,9]<-273;ndays[1,10]<-274;ndays[2,10]<-304;ndays[1,11]<-305;ndays[2,11]<-334;ndays[1,12]<-335;ndays[2,12]<-365;

### THIS CROP YIELD DATA DOES NOT CHANGE ########
ag_sb <- read.csv("MK_Agent_Sub_basins_v2.csv") %>% rename(SB_ID = Subbasin) #agent-subbasin relationship
cy_tar <- read.csv("TargetYields_rice_maize_v2.csv") %>% rename(SB_ID = subbasin) %>% #target crop yields
  gather(key=Crop,value=TarYields,-SB_ID) %>% 
  separate(Crop,into = c("CName","Unit"),sep = "_") %>% 
  mutate(TarYields = TarYields/907.2,
         HRU_ID = ifelse(CName == "IRICE",1,3)) %>%
  left_join(ag_sb,by="SB_ID") %>% 
  select(Agent_ID,SB_ID,HRU_ID,TarYields)

crop_hru <- read.table("Crop_initial.txt")
colnames(crop_hru) <- c("SB_ID","HRU_ID","AreaFrac","PlantDate","IrriHeat","Irri_TS","Irri_eff","Irri_minflow")

costfactor = 1000 #this is the cost of increasing efficiency by 1%

resnum=nrow(res_ini);#number of reservoirs
sbnum=nrow(ag_sb);#number of subbasins
hotnum=max(ag_sb$Hotspot_Number);#number of hotspots

#trigger to keep track of long term hydropower generation and crop production and whether they fall below minimum
hptrig <- matrix(rep(FALSE,resnum*10),resnum); croptrig <- matrix(rep(FALSE,sbnum*10),sbnum);
hptrigcount <- rep(0,resnum)#this count is used to ensure that it has been 10 years since start of simulation or 10 years since hydropower regulations have been altered

#counts number of years in a row with ecosystem insufficiencies (needs to be initialized before year loop)
IHAfailcount<-data.frame(matrix(0,ncol = 33, nrow = hotnum))#this stores the years in a row IHA values are insufficient
# the ncol above may need to be edited to match the number of IHA's examined in "IHAnew" + 1 (32+1=33)
IHAfailcount[,1]=1:hotnum;colnames(IHAfailcount)[1:ncol(IHAfailcount)]<-c("hotspot","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls");

########################################################################################
#Survey weights for agriculture, hydropower, ecosystems & Level of Cooperation for each agent
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#both "Level of Cooperation.csv" and "Irr_Eff_minmax.csv" are files that can be created by users of the webtool
readweights <- read.csv("Survey_Retabulated.csv",stringsAsFactors=FALSE)
readloc <- read.csv("Level of Cooperation.csv",stringsAsFactors=FALSE)
priorities <- data.frame(readloc$Agent_ID,readweights$Agriculture..,readweights$Hydropower..,readweights$Ecosystem.Services..,readloc$Level.of.Cooperation)
colnames(priorities)<-c("Agent_ID","A_weight","H_weight","E_weight","LOC")

irr_minmax <- read.csv("Irr_Eff_minmax_v2.csv",stringsAsFactors=FALSE)
colnames(irr_minmax)<-c("SB_ID","min_irr_eff","max_irr_eff")
#defines the starting place for irr_eff as well as the cap at the subbasin level


#%%%%%%%%%%%%%%%%%%%%%%%%  SWAT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
file.create("SWAT_flag.txt")
system("swat2012.exe",wait=FALSE,invisible=FALSE)
n<-1

while(n<22) #SWAT simulation period: 22 years - this part returns back to ABM
{
  while (file.exists("SWAT_flag.txt"))
  {
  }
  
  #################################################################################
  #SWAT output variables (ABM input variables)
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ###### Streamflow 
  flow_mekong <- read.table("Flow_Mekong.txt")
  colnames(flow_mekong)[1:ncol(flow_mekong)] <- c("year","cal_day",paste0("Flow_SB_",1:(ncol(flow_mekong)-2)))
  
  ABM_flow <- flow_mekong %>% 
    tbl_df() %>%
    gather(key=Subbasin,value=Flow,-year,-cal_day) %>%
    mutate(SB_ID = extract_numeric(Subbasin)) %>%
    select(-Subbasin)
  
  ####### Reservoir Storage, outflow and surface area
  reservoir_mekong <- read.table("Reservoir_Mekong.txt")
  colnames(reservoir_mekong)[1:ncol(reservoir_mekong)] <- c("year","cal_day",paste0("Volume_Res",1:resnum),paste0("SurfArea_Res",1:resnum),paste0("Outflow_Res",1:resnum))
  
  ABM_reservoir <- reservoir_mekong %>% 
    tbl_df() %>% 
    gather(key=Att,value=Variable,-year,-cal_day) %>%
    separate(col=Att,into=c("Attr","Reservoir"),sep="_") %>%
    mutate(Reservoir = extract_numeric(Reservoir)) 
  
  ####### Crops
  crop_mekong  <- read.table("Crop_Mekong.txt")
  colnames(crop_mekong) <- c("year","SB_ID","HRU_ID","Act_yield","IWW")

  ########################################################################################
  #Crops constraints and decision making
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (n == 1){ # for the first year, the initial area fraction and efficiency will be used, and updated subsequently
    user_Irri_eff<-left_join(crop_hru,irr_minmax,by="SB_ID") %>% select(min_irr_eff) %>% .$min_irr_eff
    #in the first year need to replace the starting irr_eff with what is input by the webtool users for each agent
    crop_eff <- select(crop_hru,SB_ID,HRU_ID,Irri_eff)%>%  mutate(Irri_eff = user_Irri_eff)
    crop_area <- select(crop_hru,SB_ID,HRU_ID,AreaFrac)
  } else {
    foo<-IRR_eff_by_R$New_Irri_eff
    crop_eff <- select(crop_hru,SB_ID,HRU_ID) %>%  mutate(Irri_eff = foo)
    crop_area <- select(crop_hru,SB_ID,HRU_ID) %>% mutate(AreaFrac = HRU_FR_by_R)
  }
  
  
  New_Eff <- NULL
  
  New_Eff <- filter(crop_mekong, year == n & HRU_ID %in% c(1,3)) %>% 
    select(SB_ID, HRU_ID,Act_yield) %>% 
    left_join(cy_tar,by=c("SB_ID", "HRU_ID")) %>% 
    left_join(crop_eff,by=c("SB_ID", "HRU_ID"))
  
  gg<- NULL;cap<- NULL
  for (a in 1:nrow(New_Eff)) {cap[a] <- irr_minmax$max_irr_eff[irr_minmax$SB_ID==New_Eff$SB_ID[a]]#the cap here is user defined max_irr_eff on a subbasin level
    gg[a] <- min(cap[a],New_Eff$Irri_eff[a]*1.1)}
    
  New_Eff %<>% mutate(New_Irri_eff = ifelse(Act_yield >= TarYields,Irri_eff,gg)) %>%
  #mutate(New_Irri_eff = Irri_eff) %>%
  mutate(AddEffCost = (New_Irri_eff-Irri_eff)*100*costfactor)  %>% mutate(Irri_eff = New_Irri_eff)
  #save New_Eff for later use with hpflag or a similar data frame if it gets changed

  IRR_eff_by_R <- left_join(crop_hru,New_Eff,by=c("SB_ID","HRU_ID")) %>%
  #mutate(min_irr_flow = 0) %>% 
  select(New_Irri_eff,Irri_minflow)
  #had to change Irri_minflow the above from 0 to value read in from crop_hru file so updates remain across years
  
  UpdateAreas <- NULL
  
  for (sb in 1:sbnum){
    temp <- mutate(crop_area,NewAreaFrac =AreaFrac) %>% 
      filter(SB_ID==sb) %>% 
      data.frame()
    
    val <- filter(New_Eff,SB_ID==sb & HRU_ID == 1) %>% .$New_Irri_eff
    
    if(val == 0.8){
      del_inc <- temp[temp$HRU_ID==1,"AreaFrac"]*0.1
      
      temp[temp$HRU_ID==5,"NewAreaFrac"] <- max(0,temp[temp$HRU_ID==5,"AreaFrac"] - 0.5*del_inc)
      temp[temp$HRU_ID==6,"NewAreaFrac"] <- max(0,temp[temp$HRU_ID==6,"AreaFrac"] - 0.5*del_inc)
      temp[temp$HRU_ID==1,"NewAreaFrac"] <- 1- sum(temp[-1,"NewAreaFrac"])
      
    }
    
    UpdateAreas <- bind_rows(UpdateAreas,temp)
  }
  
  HRU_FR_by_R <- UpdateAreas$NewAreaFrac
  
  #keeping track of annual crop insufficiencies as this extent affects ecoysystem decisions
  for (s in 1:sbnum){
    if(any(New_Eff$Act_yield[New_Eff$SB_ID==s]<New_Eff$TarYields[New_Eff$SB_ID==s])){
      croptrig[s,10]<-croptrig[s,9];croptrig[s,9]<-croptrig[s,8];croptrig[s,8]<-croptrig[s,7];croptrig[s,7]<-croptrig[s,6];croptrig[s,6]<-croptrig[s,5];croptrig[s,5]<-croptrig[s,4];croptrig[s,4]<-croptrig[s,3];croptrig[s,3]<-croptrig[s,2];croptrig[s,2]<-croptrig[s,1];croptrig[s,1]<-TRUE;
    }else{
      croptrig[s,10]<-croptrig[s,9];croptrig[s,9]<-croptrig[s,8];croptrig[s,8]<-croptrig[s,7];croptrig[s,7]<-croptrig[s,6];croptrig[s,6]<-croptrig[s,5];croptrig[s,5]<-croptrig[s,4];croptrig[s,4]<-croptrig[s,3];croptrig[s,3]<-croptrig[s,2];croptrig[s,2]<-croptrig[s,1];croptrig[s,1]<-FALSE;
    }
  }
  ###############################################################################
  # Hydropower generation calculation, constraints, and flags
  ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  if (n==1){
    readhydpow <- read.csv("reservoir_data_for_ABM_v2.csv",stringsAsFactors=FALSE)# mean annual energy (GW)
    mean_hydpow <- as.numeric(readhydpow$Mean.Annual.Energy..GWh.) #(GWh)
    capacity_hydpow <- as.numeric(readhydpow$Daily.Capacity..GWh.) #(GWh)
    min_hydpow <- mean_hydpow *0.9# set min at 90% of mean for now
    res_eff <- readhydpow$Efficiency
    res_a <- readhydpow$a
    res_b <- readhydpow$b
    res_c <- readhydpow$c}
  if (n>3){min_hydpow <- min_hydpow*1.07}#increase 7% per year (first year of simulation is year 3)
  #power demands  expected to increase by about 7% per year between (2010 and 2030)
  
  daily_res_Q <- reservoir_mekong[which(reservoir_mekong$year==n),paste0("Outflow_Res",1:resnum)]/86400;
  names(daily_res_Q)<-paste0("daily_Q_res",1:resnum)
  #daily outflow for each reservoir converted to m3/s from m3/day

  daily_res_head <- as.data.frame(t(res_a*t(reservoir_mekong[which(reservoir_mekong$year==n),paste0("Volume_Res",1:resnum)])^res_b+res_c));
  names(daily_res_head)<-paste0("daily_head_res",1:resnum)
  #daily mean head for each reservoir calculated from storage and empircal data
  
  #hydpow = u*rho*g*H*Q/1,000,000,000 for GW (kgm^2/s^3*10^9) then *24 to GWh
  #u = efficiency (in general 0.8) but taken from data if exists
  raw_hydpow <- data.frame(mapply('*',((1000*9.81*daily_res_head*daily_res_Q)/1000000000*24),res_eff));
  names(raw_hydpow)<-paste0("raw_hydpow_res",1:resnum)
  raw_hydpow <- as.matrix(raw_hydpow)
  #raw hydropower without accounting for capacity
  
  max_hydpow <- matrix(capacity_hydpow,nrow=ncol(raw_hydpow),ncol=nrow(raw_hydpow))
  max_hydpow <- t(max_hydpow)
  daily_hydpow <- pmin(raw_hydpow,max_hydpow)
  #need two matrices to use pmindaily
  colnames(daily_hydpow)<-paste0("daily_hydpow_res",1:resnum)
  #capacity is now accounted for as a maximum value
  
  hydpow <-colSums(daily_hydpow)
  names(hydpow)<-paste0("hydpow_res",1:resnum)
  #hydropower is now summed to annual for later management decisions (compare to annual means)
  
  nn=rep(n,nrow(readhydpow));resagent<-ag_sb$Agent_ID[ag_sb$SB_ID%in%readhydpow$subbasin..0106.setup.]
  hydpow1<-data.frame(cbind(nn,resagent,readhydpow$Reservoir.name,hydpow));names(hydpow1)<-c("year","Agent_ID","Reservoir","Hydro.Power")
  if (n==1){
    hydpow_save<-hydpow1
  }else{
    hydpow_save<-rbind(hydpow_save,hydpow1)}
  #hydropower needs to be saved for each year for later analysis (summary tables)
  
  hpflag <- rep(0,sbnum)#number of rows = number of subbasins
  res_exist <- rep(0,sbnum); 
  res_exist=ifelse(ag_sb$SB_ID %in% readhydpow$subbasin[readhydpow$Status=="Complete"],1,0)
  #the above exist does not include non-"complete" dams
  hpinfo <- data.frame(ag_sb$SB_ID,res_exist,hpflag);names(hpinfo)<-c("SB_ID","res_exist","hpflag")
  #this is used to see if hydropower requirements are not met on any particular year,
  #if they are not, then increase irr_eff in all subbasins in that agent such that there is more water for storage/hydropower
  New_Eff <- right_join(New_Eff,hpinfo,by="SB_ID")
  #the hpinfo dataframe is combined with the crops one for later ease
  
  ###############################################################################
  # Hydropower decisions (management changes)
  ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  rr=1:resnum
  for (r in rr){#reservoir loop
    
    if (readhydpow$Status[r]=="Complete"){#choose from Complete,UC,Planned,or Possible
      hptrigcount[r] <- hptrigcount[r]+1
      
      if (hydpow[r]< min_hydpow[r]){
        New_Eff$hpflag <- ifelse(New_Eff$SB_ID==readhydpow$subbasin[r],1,New_Eff$hpflag)#hp flag for later irr_eff change
        #asign TRUE to most recent year and replace all other years with the value from the year before
        hptrig[r,10]<-hptrig[r,9];hptrig[r,9]<-hptrig[r,8];hptrig[r,8]<-hptrig[r,7];hptrig[r,7]<-hptrig[r,6];hptrig[r,6]<-hptrig[r,5];hptrig[r,5]<-hptrig[r,4];hptrig[r,4]<-hptrig[r,3];hptrig[r,3]<-hptrig[r,2];hptrig[r,2]<-hptrig[r,1];hptrig[r,1]<-TRUE;
        if (all(hptrig[])==TRUE & hptrigcount[r]>=10){#if hydropower generated is less than the minimum over last 10 years 
          #decrease number of days required to reach target and target storage during dry season
          starg[r,-(5:10)] <- starg[r,-(5:10)]*0.7
          ndtargr[r] <- ndtargr[r]-4
          hptrigcount[r]=0#reset hydropower trigger count after changing reservoir managemnt practices to ensure it will not be changed for at least 10 more years (if ever again)
          
        }else if (sum(hptrig[r,])==9 & hptrigcount[r]>=10){#if hydropower generated is less than the minimum for 9 of last 10 years
          #decrease number of days required to reach target and target storage to a lesser extent than for 10/10 years
          starg[r,-(5:10)] <- starg[r,-(5:10)]*0.8
          ndtargr[r] <- ndtargr[r]-3
          hptrigcount[r]=0#reset hydropower trigger count
          
        }else if (sum(hptrig[r,])==8 & hptrigcount[r]>=10){#if hydropower generated is less than the minimum for 8 of last 10 years
          #decrease number of days required to reach target and target storage to a lesser extent than for 9/10 years
          starg[r,-(5:10)] <- starg[r,-(5:10)]*0.9
          ndtargr[r] <- ndtargr[r]-2
          hptrigcount[r]=0#reset hydropower trigger count
          
        }else{}
        
      }else{
        #asign FALSE to most recent year and replace all other years with the value from the year before
        hptrig[r,10]<-hptrig[r,9];hptrig[r,9]<-hptrig[r,8];hptrig[r,8]<-hptrig[r,7];hptrig[r,7]<-hptrig[r,6];hptrig[r,6]<-hptrig[r,5];hptrig[r,5]<-hptrig[r,4];hptrig[r,4]<-hptrig[r,3];hptrig[r,3]<-hptrig[r,2];hptrig[r,2]<-hptrig[r,1];hptrig[r,1]<-FALSE;
      }
    }#end if nan
  }#end for loop
  
  ##########################################################
  # If hydropower requirements not met (hpflag), then increase irr_eff 
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #OPTION 1: increase irr_eff in just that subbasin
  #mutate(New_Eff, New_Irri_eff = ifelse(hpflag==1,min(1,Irri_eff*1.1),Irri_eff)) %>% 
  #mutate(AddEffCost = (New_Irri_eff-Irri_eff)*100*costfactor)
  
  #OPTION 2:increase irr_eff in all subbasins in that agent
  cap<- NULL
  for (i in 1:nrow(New_Eff)){
    cap[i] <- irr_minmax$max_irr_eff[irr_minmax$SB_ID==New_Eff$SB_ID[i]]
    ifelse(New_Eff$Agent_ID[i] %in% New_Eff$Agent_ID[New_Eff$hpflag==1],
           New_Eff$New_Irri_eff[i] <- min(cap[i],New_Eff$Irri_eff[i]*1.1),
           New_Eff$New_Irri_eff[i] <-New_Eff$Irri_eff[i])
    
    New_Eff$AddEffCost[i] = New_Eff$AddEffCost[i]+(New_Eff$New_Irri_eff[i]-New_Eff$Irri_eff[i])*100*costfactor
      }
  
  #adjust irr eff in New_Eff for later use (above) and in IRR_eff_by_R for writing out (below)
  final_Irri_eff<-left_join(crop_hru,New_Eff,by=c("SB_ID","HRU_ID")) %>%   select(New_Irri_eff) %>% .$New_Irri_eff
  final_Irri_eff[is.na(final_Irri_eff)==T] <- 0
  IRR_eff_by_R<-mutate(IRR_eff_by_R,New_Irri_eff =final_Irri_eff)
  
  ###########################################################
  #Post-calculation for (From SWAT output) ecosystem services
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ############ ecosystem requirements - IHAs !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  columnend<-(sbnum+2); ecoyear<-flow_mekong[which(flow_mekong$year==n),3:columnend];
  #flow in m^3/s
   
  #Magnitude Timing (IHA 1:12 - mean streamflow for each calendar month m^3/s)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  jan<-as.vector(colMeans(ecoyear[1:31,]));feb<-as.vector(colMeans(ecoyear[32:59,]));mar<-as.vector(colMeans(ecoyear[60:90,]));apr<-as.vector(colMeans(ecoyear[91:120,]));may<-as.vector(colMeans(ecoyear[121:151,]));jun<-as.vector(colMeans(ecoyear[152:181,]));
  jul<-as.vector(colMeans(ecoyear[182:212,]));aug<-as.vector(colMeans(ecoyear[213:243,]));sep<-as.vector(colMeans(ecoyear[244:273,]));oct<-as.vector(colMeans(ecoyear[274:304,]));nov<-as.vector(colMeans(ecoyear[305:334,]));dec<-as.vector(colMeans(ecoyear[335:365,]));
  
  #Magnitude Duration (IHA 13:22 - annual max/min 1,3,7,30,90-day means)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  day1min<-apply(ecoyear,2,min); day1max<-apply(ecoyear,2,max)
  
  for (t in seq(1,365,90)){
    for (s in t:(t+89)){
      if (s==1){
        hold90<-colSums(ecoyear[s:(s+89),])
      }else{
        hold90new<-colSums(ecoyear[s:(s+89),])
        hold90<-rbind(hold90,hold90new)
      }
    }
  }
  day90max<-apply(hold90[1:276,],2,max);day90min<-apply(hold90[1:276,],2,min)
  #will be 276 saved 90day values in hold90 for 90day means
  
  for (t in seq(1,365,30)){
    for (s in t:(t+29)){
      if (s==1){
        hold30<-colSums(ecoyear[s:(s+29),])
      }else{
        hold30new<-colSums(ecoyear[s:(s+29),])
        hold30<-rbind(hold30,hold30new)
      }
    }
  }
  day30max<-apply(hold30[1:336,],2,max);day30min<-apply(hold30[1:336,],2,min)
  #will be 336 saved 30day values in hold30 for 30day means
  
  for (t in seq(1,365,7)){
    for (s in t:(t+6)){
      if (s==1){
        hold7<-colSums(ecoyear[s:(s+6),])
      }else{
        hold7new<-colSums(ecoyear[s:(s+6),])
        hold7<-rbind(hold7,hold7new)
      }
    }
  }
  day7max<-apply(hold7[1:359,],2,max);day7min<-apply(hold7[1:359,],2,min)
  #will be 359 saved 7day values in hold7 for 7day means
  
  for (t in seq(1,365,3)){
    for (s in t:(t+2)){
      if (s==1){
        hold3<-colSums(ecoyear[s:(s+2),])
      }else{
        hold3new<-colSums(ecoyear[s:(s+2),])
        hold3<-rbind(hold3,hold3new)
      }
    }
  }
  day3max<-apply(hold3[1:363,],2,max);day3min<-apply(hold3[1:363,],2,min)
  #will be 363 saved 3day values in hold3 for 3day means

  #Timing (IHA 23:24 - day of 1-day min and max)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  mindate<-apply(ecoyear,2,which.min); maxdate<-apply(ecoyear,2,which.max)
  
  #Magnitude Frequency Duration (IHA 25:28 - number of low/high pulses, mean duration of low/high pulses)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # to quantify these we need to establish thresholds for what constitutes a high or low pulse- e.g. 75percentile,25percentile of values over long period of time (pre-simulation)
  highpulse<-apply(ecoyear,2,max)*.8; lowpulse<-apply(ecoyear,2,max)*.2; #high and low pulse threshold NEED to be taken from historic data once obtained
  counthigh<-matrix(rep(0),nrow(ecoyear),ncol(ecoyear));countlow<-matrix(rep(0),nrow(ecoyear),ncol(ecoyear))#reinitialize each year
  for (i in 1:ncol(ecoyear)){
    counthigh[,i]<-ecoyear[,i]>highpulse[i];countlow[,i]<-ecoyear[,i]>lowpulse[i];
  }
  hipulse_no<-apply(counthigh,2,sum); lopulse_no<-apply(countlow,2,sum)
  hipulse_dur<-apply(counthigh,2,function(x){
    hirle<-rle(x);mean(hirle$lengths[hirle$values==TRUE])})
  lopulse_dur<-apply(countlow,2,function(x){
    lorle<-rle(x);mean(lorle$lengths[lorle$values==TRUE])})
    
  #Frequency Rate of Change (IHA 29:32 - mean of positive/negative differences between daily values, number of rises/falls)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ecochange1<-data.frame(diff(as.matrix(ecoyear[,]),lag=1));ecochange2<-data.frame(diff(as.matrix(ecoyear[,]),lag=1));
  ecochange1[ecochange1<=0]<-NA; ecochange2[ecochange2>=0]<-NA;
  mean_increase <- colMeans(ecochange1,na.rm=TRUE); mean_decrease <- colMeans(ecochange2,na.rm=TRUE);
  number_rises <- apply(ecochange1, 2, function(x) length(which(!is.na(x)))); number_falls <- apply(ecochange2, 2, function(x) length(which(!is.na(x))))
  
  if (n==1){
    IHA <- data.frame(n,ag_sb$SB_ID,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec,day1min,day1max,day3min,day3max,day7min,day7max,day30min,day30max,day90min,day90max,mindate,maxdate,hipulse_no,lopulse_no,hipulse_dur,lopulse_dur,mean_increase,mean_decrease,number_rises,number_falls)
    IHAnew <- data.frame(n,ag_sb$SB_ID,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec,day1min,day1max,day3min,day3max,day7min,day7max,day30min,day30max,day90min,day90max,mindate,maxdate,hipulse_no,lopulse_no,hipulse_dur,lopulse_dur,mean_increase,mean_decrease,number_rises,number_falls)
    colnames(IHA)[1:ncol(IHA)] <- c("year","sub_ID","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls")
    rownames(IHA) <- NULL
    colnames(IHAnew)[1:ncol(IHA)] <- c("year","sub_ID","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls")
    rownames(IHAnew) <- NULL
  }else{
    IHAnew <- data.frame(n,ag_sb$SB_ID,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec,day1min,day1max,day3min,day3max,day7min,day7max,day30min,day30max,day90min,day90max,mindate,maxdate,hipulse_no,lopulse_no,hipulse_dur,lopulse_dur,mean_increase,mean_decrease,number_rises,number_falls)
    colnames(IHAnew)[1:ncol(IHAnew)] <- c("year","sub_ID","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls")
    rownames(IHAnew) <- NULL
    IHA <- rbind(IHA,IHAnew)
  }
  #"IHAnew" holds IHA's for current year and "IHA" holds all years
 
  ############################################################################################################################
  # Ecosystem
  ############################################################################################################################
  #ecosystem requirements (IHA's)
  IHAreq <- read.csv("IHAreq.csv")
  #this file contains (for each hotspot) the targets of IHA's and the "acceptable" deviations from these targets
  #monthly flows are currently in m^3/s (montly mean rate)
  #this is a temporary fake file created based on the calculated IHAs -> this will be replaced by the actual data we obtain
  
  ############################################################################################################################
  #Checking requirements (IHA's)
  #if we want to limit which IHA's we care about, then reduce IHAnew to those values only
  ############################################################################################################################
  hotIHA<-data.frame(matrix(0,ncol = (ncol(IHAnew)-1), nrow = hotnum));#hotIHA contains the IHA information for each hotspot
  hotIHA[,1]=1:hotnum;colnames(hotIHA)[1:ncol(hotIHA)]<-c("hotspot","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls");
  IHAfail<-data.frame(matrix(0,ncol = (ncol(IHAnew)-1), nrow = hotnum));#this stores to what extent IHA values are insufficient (difference from target)
  IHAfail[,1]=1:hotnum;colnames(IHAfail)[1:ncol(IHAfail)]<-c("hotspot","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls");
  
  for (h in 1:hotnum){
    countnum=0;
    for (s in 1:sbnum){
      if (ag_sb$Hotspot_Number[s]==h){
        countnum=countnum+1;#most hotspots are spread across multiple subbasins
        hotIHA[h,2:ncol(hotIHA)]=hotIHA[h,2:ncol(hotIHA)]+IHAnew[s,3:ncol(IHAnew)];
      }
    }
    hotIHA[h,2:ncol(hotIHA)]=hotIHA[h,2:ncol(hotIHA)]/countnum;#this takes average of multiple subbasin IHA's
    
    #now need to compare current IHA up against IHA requirements
    for (i in 2:ncol(hotIHA)){
      if (((IHAreq[h,i]-IHAreq[h,(i+ncol(hotIHA)-1)]) <= hotIHA[h,i]) & (hotIHA[h,i] <= (IHAreq[h,i]+IHAreq[h,(i+ncol(hotIHA)-1)]))){
        #if calculated IHA is between target+/-deviation then reset year counter of problem years in a row
        IHAfailcount[h,i] <- 0;
      }else{#if NOT between target+/-deviation then store difference from target and count number of years in a row
        if (IHAfailcount[h,i]==0){#if this is the first year in a row with difficiency, store desired change
          IHAfail[h,i] <- IHAreq[h,i]-hotIHA[h,i];#this stores to what extent IHA is insufficient (desired change)
        }else{#if it is not the first year in a row then deficiency needs to be averaged over years before action taken
          IHAfail[h,i] <- (IHAfail[h,i]*IHAfailcount[h,i]+(IHAreq[h,i]-hotIHA[h,i]))/(IHAfailcount[h,i]+1);
        }
        #need to also establish that the sign (+or-) of insufficiency is same as last year to count years in a row
        if (n==1){
          IHAfailcount[h,i] <- IHAfailcount[h,i] +1;#this stores the number of years in a row with a problem
        }else{
          if (sign(IHAfail[h,i])==sign(IHAfail_last[h,i])){
            IHAfailcount[h,i] <- IHAfailcount[h,i] +1;
          }else{
            IHAfailcount[h,i] <- 0;#if insufficient in an opposite way, set counter back to zero
          }
        }
      }
    }
  }
  if (n==1){IHAfail_save<-cbind(n,IHAfail)}else{IHAfail_save <- rbind(IHAfail_save,cbind(n,IHAfail))}#in case we want to save all values for later analysis or summary table
  IHAfail_last <-IHAfail;#save this year for next year use (see if sign of insufficiency is same across year counting)
  
  ############################################################################################################################
  # Now to make decisions based on failures to meet requirements
  ############################################################################################################################
  resinternal <- read.csv("resinternal.csv")
  #this file contains (for each hotspot) which UPSTREAM dam within the SAME agent can be called for help (if any) (priotiry given to closest)
  #contains information for both present day and future scenarios (complete vs non-complete lists)
  
  nfail=3#number of years in a row with same IHA problem before action is taken
  scaling=0.2;#for 1 time change threshold in outlfow(0.2=20%increase)
  scaling2=0.1;#for 1 time change threshold in Irri_minflow
  
  #the following deals with monthly flow-based decisions
  for (h in 1:hotnum){#hotspot number
    for (m in 1:12){#month
      if ((IHAfail[h,m+1]!=0) & (IHAfailcount[h,m+1]==nfail)){#if there is a demand for more or less water "nfail" years in a row
        morl<-sign(IHAfail[h,m+1])#this indicates if there is a demand for more or less (morl) water
        if (!is.na(resinternal[h,3])){#if there is a reservoir within the agent
          rrr=resinternal$internal_res_complete[resinternal$Hotspot_Number==h]#reservoir in agent that can aid
          #!!!!!! warning !!!!!!! when changing to future scenario, need to change [h,3] to [h,5] (above if loop) and "internal_res_complete" (above) to "future_res_complete"
          if (sum(hptrig[rrr,1:nfail])>=2){#if hydropower insufficiency for 2 out of "nfail" years (can be altered) then need to vote
            rn<-runif(1, 1, 100);#vote
            if (rn<=priorities$E_weight[ag_sb$Agent_ID[ag_sb$SB_ID==readhydpow$subbasin..0106.setup.[readhydpow$Reservoir==rrr]]]){
              #ethan's idea was to lump non-ecosystem weights together against ecosystem
              currentout=0;#initialize
              for (nn in 1:nfail){#need to find average of last 3 years outflow before altering min or maxoutflow
                currentout1=reservoir_mekong[(reservoir_mekong$year==(n-nn+1)), 2*resnum+2+rrr];#daily outflow at current reservoir
                currentout1<-mean(currentout1[ndays[1,m]:ndays[2,m]]);#mean for this month at current reservoir
                currentout=currentout+currentout1;
              }
              currentout=currentout/nfail;#mean month's outflow in m^3/s (over last nfail years)
              changeout<-ifelse(IHAfail[h,m+1]<=currentout*scaling,IHAfail[h,m+1],currentout*scaling);#IHAfail also in terms of m^3/s
              #not only have an absolute threshold (minoutthres and maxoutthres below) but a threshold of 1 time change (above)
              if (morl>0){#if demand for more water
                request <- ifelse(currentout+changeout <= minoutthres[rrr,m],currentout+changeout,minoutthres[rrr,m]);
                #request increase minimum outflow but not over a set threshold (do we want to increase max also?- maybe if min hits threshold?)
                if (request>minout[rrr,m]){          #multiple hotspots can call for aid from same reservoir so only the greatest
                  minout[rrr,m] <- request;          #request will be instituted rather than being additive or replacing each other
                }
              }else{
                request <- ifelse(currentout-changeout >= maxoutthres[rrr,m],currentout-changeout,maxoutthres[rrr,m]);
                #request decrease maximum outflow but not over a set threshold
                if (request<maxout[rrr,m]){          #multiple hotspots can call for aid from same reservoir so only the greatest
                  maxout[rrr,m] <- request;          #request will be instituted rather than being additive or replacing each other
                }
              }
              IHAfailcount[h,m+1]=0;#after making decision, reset year counter for problem years in a row
            }else{}#do nothing if voted against and do not reset IHAfailcount
          
          }else{#if hydropower has no insufficiency then no need to vote for decision-> go ahead
            currentout=0;#initialize
            for (nn in 1:nfail){#need to find average of last 3 years outflow before altering min or maxoutflow
              currentout1=reservoir_mekong[(reservoir_mekong$year==(n-nn+1)), 2*resnum+2+rrr];#daily outflow at current reservoir
              currentout1<-mean(currentout1[ndays[1,m]:ndays[2,m]]);#mean for this month at current reservoir
              currentout=currentout+currentout1;
            }
            currentout=currentout/nfail;#mean month's outflow in m^3/s (over last nfail years)
            changeout<-ifelse(IHAfail[h,m+1]<=currentout*scaling,IHAfail[h,m+1],currentout*scaling);
            #not only have an absolute threshold (minoutthres below) but a threshold of 1 time change (above)
            if (morl>0){#if demand for more water
              request <- ifelse(currentout+changeout <= minoutthres[rrr,m],currentout+changeout,minoutthres[rrr,m]);
              #request increase minimum outflow but not over a set threshold (do we want to increase max also?- maybe if min hits threshold?)
              if (request>minout[rrr,m]){          #multiple hotspots can call for aid from same reservoir so only the greatest
                minout[rrr,m] <- request;          #request will be instituted rather than being additive or replacing each other
              }
            }else{
              request <- ifelse(currentout-changeout >= maxoutthres[rrr,m],currentout-changeout,maxoutthres[rrr,m]);
              #request decrease maximum outflow but not over a set threshold
              if (request<maxout[rrr,m]){          #multiple hotspots can call for aid from same reservoir so only the greatest
                maxout[rrr,m] <- request;          #request will be instituted rather than being additive or replacing each other
              }
            }
            IHAfailcount[h,m+1]=0;#after making decision, reset year counter for problem years in a row
          }
          
        }else{#if there is no reservoir within the agent then increase irri_minflow (where possible) (reduce irrigation)
          aaa<-unique(ag_sb$Agent_ID[ag_sb$Hotspot_Number==h]);#agents which contain hotspot
          for (aa in 1:length(aaa)){#each agent must be decided on separately bc hotspot often in 2 different agents with two different votes
            a<-aaa[aa];#current agent
            sss<-ag_sb$SB_ID[ag_sb$Hotspot_Number==h & ag_sb$Agent_ID==a];#subbasins within current agent which contain hotspot
            for (ss in 1:length(sss)){
              s<-sss[ss];#current subbbasin
              if (any(cy_tar$TarYields[cy_tar$SB_ID==s]>0)){#if that hotspot has agriculture (some with hotspots do not)
                if (sum(croptrig[s,1:nfail])>=2){#if crop production insufficiency for 2 out of nfail years (can be altered) in sb then need to vote
                  rn<-runif(1, 1, 100);#vote
                  if (rn<=priorities$E_weight[a]){
                    currentflow=0;
                    for (nn in 1:nfail){#need to find average of last 3 years flow before altering Irri_minflow
                      currentflow<-currentflow+IHA[(IHA$sub_ID==s)&(IHA$year==n-nn+1),m+2];
                    }
                    currentflow=currentflow/nfail;#mean monthly flow in subbasin (over last nfail years)
                    changeirr<-ifelse(IHAfail[h,m+1]<=currentflow*scaling2,IHAfail[h,m+1],currentflow*scaling2);
                    #have a threshold of 1 time change (above) (NOTE: but so far no absolute threshold- how?)
                    hru=(s-1)*8+1;#current HRU (bc Irri_minflow at HRU scale)
                    IRR_eff_by_R$Irri_minflow[hru]<-currentflow+changeirr;
                    IRR_eff_by_R$Irri_minflow[hru+2]<-currentflow+changeirr;
                    #always the same Irri_minflow for rice and maize but have to set both (hru's 1 and 3)
                    IHAfailcount[h,m+1]=0#after making decision, reset year counter for problem years in a row
                  }else{}#do nothing if voted against and do not reset IHAfailcount
            
                }else{#if crop production has no insufficiency then no need to vote for decision-> go ahead
                  currentflow=0;
                  for (nn in 1:nfail){#need to find average of last 3 years flow before altering Irri_minflow
                    currentflow<-currentflow+IHA[(IHA$sub_ID==s)&(IHA$year==n-nn+1),m+2];
                  }
                  currentflow=currentflow/nfail;#mean monthly flow in subbasin (over last nfail years)
                  changeirr<-ifelse(IHAfail[h,m+1]<=currentflow*scaling2,IHAfail[h,m+1],currentflow*scaling2);
                  #have a threshold of 1 time change (above) (NOTE: but so far no absolute threshold- how?)
                  hru=(s-1)*8+1;#current HRU (bc Irri_minflow at HRU scale)
                  IRR_eff_by_R$Irri_minflow[hru]<-currentflow+changeirr;
                  IRR_eff_by_R$Irri_minflow[hru+2]<-currentflow+changeirr;
                  #always the same Irri_minflow for rice and maize but have to set both (hru's 1 and 3)
                  IHAfailcount[h,m+1]=0;#after making decision, reset year counter for problem years in a row
                }
              }else{}#if no agriculture, cannot make any difference with irri_minflow
            }#subbasin loop end
          }#agent loop end
        }#if there is reservoir or not conditional end
      }else{} #there has not been a monthly flow insufficiency for this hotspot for nfail years in a row so do nothing
    }#month loop end
  }#hotspot loop end
  
  
  #save this data for summary tables
  nn<-rep(n,hotnum);
  eco_save_new<-cbind(nn,hotIHA,IHAreq[,2:ncol(IHAreq)]);
  colnames(eco_save_new)[1:ncol(eco_save_new)]<-c("year","hotspot","jan_calc","feb_calc","mar_calc","apr_calc","may_calc","jun_calc","jul_calc","aug_calc","sep_calc","oct_calc","nov_calc","dec_calc","1daymin_calc","1daymax_calc","3daymin_calc","3daymax_calc","7daymin_calc","7daymax_calc","30daymin_calc","30daymax_calc","90daymin_calc","90daymax_calc","dayofmin_calc","dayofmax_calc","hipulse_no_calc","lopulse_no_calc","hipulse_dur_calc","lopulse_dur_calc","ave_increa_calc","ave_decrea_calc","No. Rises_calc","No. Falls_calc","jan_tar","feb_tar","mar_tar","apr_tar","may_tar","jun_tar","jul_tar","aug_tar","sep_tar","oct_tar","nov_tar","dec_tar","1daymin_tar","1daymax_tar","3daymin_tar","3daymax_tar","7daymin_tar","7daymax_tar","30daymin_tar","30daymax_tar","90daymin_tar","90daymax_tar","dayofmin_tar","dayofmax_tar","hipulse_no_tar","lopulse_no_tar","hipulse_dur_tar","lopulse_dur_tar","ave_increa_tar","ave_decrea_tar","No. Rises_tar","No. Falls_tar","jan_allowed_dev","feb_allowed_dev","mar_allowed_dev","apr_allowed_dev","may_allowed_dev","jun_allowed_dev","jul_allowed_dev","aug_allowed_dev","sep_allowed_dev","oct_allowed_dev","nov_allowed_dev","dec_allowed_dev","1daymin_allowed_dev","1daymax_allowed_dev","3daymin_allowed_dev","3daymax_allowed_dev","7daymin_allowed_dev","7daymax_allowed_dev","30daymin_allowed_dev","30daymax_allowed_dev","90daymin_allowed_dev","90daymax_allowed_dev","dayofmin_allowed_dev","dayofmax_allowed_dev","hipulse_no_allowed_dev","lopulse_no_allowed_dev","hipulse_dur_allowed_dev","lopulse_dur_allowed_dev","ave_increa_allowed_dev","ave_decrea_allowed_dev","No. Rises_allowed_dev","No. Falls_allowed_dev");
  if(n==1){eco_save<-eco_save_new}else{eco_save<-rbind(eco_save,eco_save_new);}
  #saves: calculated IHAs,targets,and allowed deviations)
  ############################################################################################################################
  ############################################################################################################################
  # Write ABM output (SWAT input) to data file
  ############################################################################################################################
  ############################################################################################################################
  #IRR_eff_by_R is already constructed
  
  hru_out<-crop_hru[,-8];hru_out<-hru_out[,-3];hru_out<-cbind(hru_out,HRU_FR_by_R);hru_out<-hru_out[c(1,2,7,3,4,5,6)]
  #this uses the initial file as a template because the write out mimics that file. Irrigation minimum flow is removed since that is not suppose
  #to be written out in this file, however the initial areas are replaced with what was decided upon by the crop section of the ABM
  
  res<-cbind(starg,ndtargr,maxout,minout)

  write.table(IRR_eff_by_R,file="Irr_eff_by_R.txt",col.names = F, row.names = F)
  write.table(res,file="Reservoir_by_R.txt", col.names = F, row.names = F) 
  write.table(hru_out,file="HRU_FR_by_R.txt", col.names = F, row.names = F) 
  ################################################################
  #save decision variables for each year for later analysis
  #way this is written now basically just combines all years of simulation (by append) and writes similarly as for SWAT input only including year
  
  hru_out_save<-cbind(n,hru_out)
  res_save<-cbind(n,res)
  IRR_eff_by_R_save<-cbind(n,IRR_eff_by_R)
  write.table(IRR_eff_by_R_save,file="save_Irr_eff_by_R.txt",col.names = F, row.names = F, append =T)
  write.table(res_save,file="save_Reservoir_by_R.txt", col.names = F, row.names = F, append =T) 
  write.table(hru_out_save,file="save_HRU_FR_by_R.txt", col.names = F, row.names = F, append =T) 
  
  file.create("SWAT_flag.txt")
  n<-n+1
}

################################################################
while (file.exists("SWAT_flag.txt"))
{
}#still need to wait for SWAT to finish or wont have last year of data

############################################################################################################################
# Creating summary tables (NOTE: SWAT doesnt return to ABM for final year)
############################################################################################################################

crop_mekong  <- read.table("Crop_Mekong.txt")
colnames(crop_mekong) <- c("year","SB_ID","HRU_ID","Act_yield","IWW")
#have to reread final data since what has been read by ABM is only for 21 years (SWAT doesnt return to ABM for final year)
  
crop_mekongsum <- crop_mekong[crop_mekong$HRU_ID==1,c(1,2,4)]
#first remove all rows for non rice and colums for hru # and water withdrawl
  
crop_mekong1 <- crop_mekong[crop_mekong$HRU_ID==3,c(1,2,4)]
crop_mekongsum["M_Act_yield"] <- crop_mekong1$Act_yield
#now add maize

crop_mekongsum["Target"] <- New_Eff$TarYields[New_Eff$HRU_ID==1]
crop_mekongsum["M_Target"] <- New_Eff$TarYields[New_Eff$HRU_ID==3]
#now add targets
  
agentinfo <- rep(ag_sb$Agent_ID,n); crop_mekongsum["Agent_ID"] <- agentinfo; crop_mekongsum <- crop_mekongsum[c(1,7,2,3,4,5,6)]
#now add column for agent and rearange
  
#!!!crummy part about not returning to ABM after year 22 is that the hydropower and IHA's have to be calculated for that final year now and added to dataframes
reservoir_mekong <- read.table("Reservoir_Mekong.txt");colnames(reservoir_mekong)[1:ncol(reservoir_mekong)] <- c("year","cal_day",paste0("Volume_Res",1:resnum),paste0("SurfArea_Res",1:resnum),paste0("Outflow_Res",1:resnum))
daily_res_Q <- reservoir_mekong[which(reservoir_mekong$year==n),paste0("Outflow_Res",1:resnum)]/86400;names(daily_res_Q)<-paste0("daily_Q_res",1:resnum);
daily_res_head <- as.data.frame(t(res_a*t(reservoir_mekong[which(reservoir_mekong$year==n),paste0("Volume_Res",1:resnum)])^res_b+res_c));names(daily_res_head)<-paste0("daily_head_res",1:resnum)
raw_hydpow <- data.frame(mapply('*',((1000*9.81*daily_res_head*daily_res_Q)/1000000000*24),res_eff));names(raw_hydpow)<-paste0("raw_hydpow_res",1:resnum);raw_hydpow <- as.matrix(raw_hydpow)
max_hydpow <- matrix(capacity_hydpow,nrow=ncol(raw_hydpow),ncol=nrow(raw_hydpow));max_hydpow <- t(max_hydpow)
daily_hydpow <- pmin(raw_hydpow,max_hydpow);colnames(daily_hydpow)<-paste0("daily_hydpow_res",1:resnum)
hydpow <-colSums(daily_hydpow);names(hydpow)<-paste0("hydpow_res",1:resnum)
nn=rep(n,nrow(readhydpow));resagent<-ag_sb$Agent_ID[ag_sb$SB_ID%in%readhydpow$subbasin..0106.setup.]
hydpow1<-data.frame(cbind(nn,resagent,readhydpow$Reservoir.name,hydpow));names(hydpow1)<-c("year","Agent_ID","Reservoir","Hydro.Power");
hydpow_save<-rbind(hydpow_save,hydpow1);hydpow_save["Target"]<- min_hydpow; hydpow_save["Status"]<- readhydpow$Status; 
##########################################################################################################################################
#last year of IHA's for ecosystem summary file

flow_mekong <- read.table("Flow_Mekong.txt");colnames(flow_mekong)[1:ncol(flow_mekong)] <- c("year","cal_day",paste0("Flow_SB_",1:(ncol(flow_mekong)-2)))
columnend<-(sbnum+2); ecoyear<-flow_mekong[which(flow_mekong$year==n),3:columnend];
jan<-as.vector(colMeans(ecoyear[1:31,]));feb<-as.vector(colMeans(ecoyear[32:59,]));mar<-as.vector(colMeans(ecoyear[60:90,]));apr<-as.vector(colMeans(ecoyear[91:120,]));may<-as.vector(colMeans(ecoyear[121:151,]));jun<-as.vector(colMeans(ecoyear[152:181,]));
jul<-as.vector(colMeans(ecoyear[182:212,]));aug<-as.vector(colMeans(ecoyear[213:243,]));sep<-as.vector(colMeans(ecoyear[244:273,]));oct<-as.vector(colMeans(ecoyear[274:304,]));nov<-as.vector(colMeans(ecoyear[305:334,]));dec<-as.vector(colMeans(ecoyear[335:365,]));
day1min<-apply(ecoyear,2,min); day1max<-apply(ecoyear,2,max)
for (t in seq(1,365,90)){for (s in t:(t+89)){if (s==1){hold90<-colSums(ecoyear[s:(s+89),]);}else{hold90new<-colSums(ecoyear[s:(s+89),]);hold90<-rbind(hold90,hold90new);}}}
day90max<-apply(hold90[1:276,],2,max);day90min<-apply(hold90[1:276,],2,min)
for (t in seq(1,365,30)){for (s in t:(t+29)){if (s==1){hold30<-colSums(ecoyear[s:(s+29),]);}else{hold30new<-colSums(ecoyear[s:(s+29),]);hold30<-rbind(hold30,hold30new);}}}
day30max<-apply(hold30[1:336,],2,max);day30min<-apply(hold30[1:336,],2,min)
for (t in seq(1,365,7)){for (s in t:(t+6)){if (s==1){hold7<-colSums(ecoyear[s:(s+6),]);}else{hold7new<-colSums(ecoyear[s:(s+6),]);hold7<-rbind(hold7,hold7new);}}}
day7max<-apply(hold7[1:359,],2,max);day7min<-apply(hold7[1:359,],2,min)
for (t in seq(1,365,3)){for (s in t:(t+2)){if (s==1){hold3<-colSums(ecoyear[s:(s+2),]);}else{hold3new<-colSums(ecoyear[s:(s+2),]);hold3<-rbind(hold3,hold3new)}}}
day3max<-apply(hold3[1:363,],2,max);day3min<-apply(hold3[1:363,],2,min);mindate<-apply(ecoyear,2,which.min); maxdate<-apply(ecoyear,2,which.max)
highpulse<-apply(ecoyear,2,max)*.8; lowpulse<-apply(ecoyear,2,max)*.2; #high and low pulse threshold NEED to be taken from historic data once obtained
counthigh<-matrix(rep(0),nrow(ecoyear),ncol(ecoyear));countlow<-matrix(rep(0),nrow(ecoyear),ncol(ecoyear))#reinitialize each year
for (i in 1:ncol(ecoyear)){counthigh[,i]<-ecoyear[,i]>highpulse[i];countlow[,i]<-ecoyear[,i]>lowpulse[i];}
hipulse_no<-apply(counthigh,2,sum); lopulse_no<-apply(countlow,2,sum)
hipulse_dur<-apply(counthigh,2,function(x){hirle<-rle(x);mean(hirle$lengths[hirle$values==TRUE])});lopulse_dur<-apply(countlow,2,function(x){lorle<-rle(x);mean(lorle$lengths[lorle$values==TRUE])})
ecochange1<-data.frame(diff(as.matrix(ecoyear[,]),lag=1));ecochange2<-data.frame(diff(as.matrix(ecoyear[,]),lag=1));
ecochange1[ecochange1<=0]<-NA; ecochange2[ecochange2>=0]<-NA;
mean_increase <- colMeans(ecochange1,na.rm=TRUE); mean_decrease <- colMeans(ecochange2,na.rm=TRUE);
number_rises <- apply(ecochange1, 2, function(x) length(which(!is.na(x)))); number_falls <- apply(ecochange2, 2, function(x) length(which(!is.na(x))))
if (n==1){
  IHA <- data.frame(n,ag_sb$SB_ID,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec,day1min,day1max,day3min,day3max,day7min,day7max,day30min,day30max,day90min,day90max,mindate,maxdate,hipulse_no,lopulse_no,hipulse_dur,lopulse_dur,mean_increase,mean_decrease,number_rises,number_falls)
  IHAnew <- data.frame(n,ag_sb$SB_ID,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec,day1min,day1max,day3min,day3max,day7min,day7max,day30min,day30max,day90min,day90max,mindate,maxdate,hipulse_no,lopulse_no,hipulse_dur,lopulse_dur,mean_increase,mean_decrease,number_rises,number_falls)
  colnames(IHA)[1:ncol(IHA)] <- c("year","sub_ID","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls")
  rownames(IHA) <- NULL
  colnames(IHAnew)[1:ncol(IHA)] <- c("year","sub_ID","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls")
  rownames(IHAnew) <- NULL
}else{
  IHAnew <- data.frame(n,ag_sb$SB_ID,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec,day1min,day1max,day3min,day3max,day7min,day7max,day30min,day30max,day90min,day90max,mindate,maxdate,hipulse_no,lopulse_no,hipulse_dur,lopulse_dur,mean_increase,mean_decrease,number_rises,number_falls)
  colnames(IHAnew)[1:ncol(IHAnew)] <- c("year","sub_ID","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls")
  rownames(IHAnew) <- NULL;IHA <- rbind(IHA,IHAnew);}
hotIHA<-data.frame(matrix(0,ncol = (ncol(IHAnew)-1), nrow = hotnum));#hotIHA contains the IHA information for each hotspot
hotIHA[,1]=1:hotnum;colnames(hotIHA)[1:ncol(hotIHA)]<-c("hotspot","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls");
IHAfail<-data.frame(matrix(0,ncol = (ncol(IHAnew)-1), nrow = hotnum));#this stores to what extent IHA values are insufficient (difference from target)
IHAfail[,1]=1:hotnum;colnames(IHAfail)[1:ncol(IHAfail)]<-c("hotspot","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","1daymin","1daymax","3daymin","3daymax","7daymin","7daymax","30daymin","30daymax","90daymin","90daymax","dayofmin","dayofmax","hipulse_no","lopulse_no","hipulse_dur","lopulse_dur","ave_increa","ave_decrea","No. Rises","No. Falls");
for (h in 1:hotnum){countnum=0;for (s in 1:sbnum){if (ag_sb$Hotspot_Number[s]==h){countnum=countnum+1;hotIHA[h,2:ncol(hotIHA)]=hotIHA[h,2:ncol(hotIHA)]+IHAnew[s,3:ncol(IHAnew)];}};hotIHA[h,2:ncol(hotIHA)]=hotIHA[h,2:ncol(hotIHA)]/countnum;}#this takes average of multiple subbasin IHA's
nn<-rep(n,hotnum);eco_save_new<-cbind(nn,hotIHA,IHAreq[,2:ncol(IHAreq)]);
colnames(eco_save_new)[1:ncol(eco_save_new)]<-c("year","hotspot","jan_calc","feb_calc","mar_calc","apr_calc","may_calc","jun_calc","jul_calc","aug_calc","sep_calc","oct_calc","nov_calc","dec_calc","1daymin_calc","1daymax_calc","3daymin_calc","3daymax_calc","7daymin_calc","7daymax_calc","30daymin_calc","30daymax_calc","90daymin_calc","90daymax_calc","dayofmin_calc","dayofmax_calc","hipulse_no_calc","lopulse_no_calc","hipulse_dur_calc","lopulse_dur_calc","ave_increa_calc","ave_decrea_calc","No. Rises_calc","No. Falls_calc","jan_tar","feb_tar","mar_tar","apr_tar","may_tar","jun_tar","jul_tar","aug_tar","sep_tar","oct_tar","nov_tar","dec_tar","1daymin_tar","1daymax_tar","3daymin_tar","3daymax_tar","7daymin_tar","7daymax_tar","30daymin_tar","30daymax_tar","90daymin_tar","90daymax_tar","dayofmin_tar","dayofmax_tar","hipulse_no_tar","lopulse_no_tar","hipulse_dur_tar","lopulse_dur_tar","ave_increa_tar","ave_decrea_tar","No. Rises_tar","No. Falls_tar","jan_allowed_dev","feb_allowed_dev","mar_allowed_dev","apr_allowed_dev","may_allowed_dev","jun_allowed_dev","jul_allowed_dev","aug_allowed_dev","sep_allowed_dev","oct_allowed_dev","nov_allowed_dev","dec_allowed_dev","1daymin_allowed_dev","1daymax_allowed_dev","3daymin_allowed_dev","3daymax_allowed_dev","7daymin_allowed_dev","7daymax_allowed_dev","30daymin_allowed_dev","30daymax_allowed_dev","90daymin_allowed_dev","90daymax_allowed_dev","dayofmin_allowed_dev","dayofmax_allowed_dev","hipulse_no_allowed_dev","lopulse_no_allowed_dev","hipulse_dur_allowed_dev","lopulse_dur_allowed_dev","ave_increa_allowed_dev","ave_decrea_allowed_dev","No. Rises_allowed_dev","No. Falls_allowed_dev");
if(n==1){eco_save<-eco_save_new}else{eco_save<-rbind(eco_save,eco_save_new);}

###########################################################################
write.csv(crop_mekongsum, file = "crop_summary.csv",row.names = FALSE)
write.csv(hydpow_save, file = "hydropower_summary.csv",row.names = FALSE)
write.csv(eco_save, file = "ecosystem_summary2.csv",row.names = FALSE)
##########################################################################
