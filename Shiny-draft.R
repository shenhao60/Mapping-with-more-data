library(tidyverse)
library(lubridate)
library(tmap)
library(sp)
library(sf)
library(hurricaneexposuredata)
library(maps)

# Data input
data(county.fips)
county.fips=separate(county.fips,polyname,c('ID','U'),sep=':')%>%
  select(fips,ID)%>%
  unique()

M=st_as_sf(map('county',plot=F,fill=T))
colnames(county.fips)[2]=colnames(M)[1]
M=left_join(M,county.fips,'ID')
summary=read.csv('DisasterDeclarationsSummaries.csv',T)
detail=read.csv('PublicAssistanceFundedProjectsDetails.csv',T)

H_name=H_name_all$storm_id[100]
# Obtain hurricane data
H_name_all=force(hurr_tracks)%>%
  select(storm_id)%>%
  unique()
H_track=force(hurr_tracks)%>%
  filter(storm_id==H_name)
H_track$DT=ymd_hm(H_track$date)
t_H_track=cbind(H_track$longitude,H_track$latitude)%>%
  Line()%>%Lines(ID=H_name)%>%
  list()%>%SpatialLines()
H_rain=force(rain)%>%
  filter(storm_id==H_name)%>%
  group_by(fips)%>%
  summarise(storm_id=storm_id[1],precip=sum(precip))%>%
  filter(precip>20)%>%
  mutate(fips=as.numeric(fips))
H_rain=right_join(M,H_rain,'fips')
# Obtain FEMA data
FEMA_summary=summary%>%
  filter(incidentType=='Hurricane')
FEMA_summary$DT=ymd(substring(FEMA_summary$incidentBeginDate,1,10))
FEMA_summary=filter(FEMA_summary,
                    DT>=min(date(H_track$DT)),
                    DT<=max(date(H_track$DT)))%>%
  select(disasterNumber,
         disasterTime=DT,
         declarationType,
         declarationTitle,
         fipsStateCode,
         fipsCountyCode)%>%
  mutate(fips=fipsStateCode*1000+fipsCountyCode)%>%
  select(-fipsStateCode,-fipsCountyCode)

FEMA_detail=detail%>%
  filter(disasterNumber%in%FEMA_summary$disasterNumber)
FEMA_detail$fips=FEMA_detail$stateNumberCode*1000+FEMA_detail$countyCode
FEMA_detail=filter(FEMA_detail,
                   countyCode!=0,)%>%
  group_by(disasterNumber,fips)%>%
  summarise(projectAmount=sum(projectAmount,na.rm=T))

FEMA_summary=right_join(M,FEMA_summary,'fips')
FEMA_detail=right_join(M,FEMA_detail,'fips')

# Mapping

for(i in unique(FEMA_summary$declarationType)) {
  if (i==unique(FEMA_summary$declarationType)[1]){
    FEMA_map_s=FEMA_summary[FEMA_summary$declarationType==i,]%>%
      tm_shape(paste('Declaraation Type: ',i))+
      tm_polygons('red',0.5,title="Declaraation Type: ",legend.show=F)
  }
  else{
    FEMA_map_s=FEMA_map_s+
      FEMA_summary[FEMA_summary$declarationType==i,]%>%
      tm_shape(paste('Declaraation Type: ',i))+
      tm_polygons('red',0.5,title="Declaraation Type: ",legend.show=F)
  }
}

for(i in unique(FEMA_detail$disasterNumber)){
  if (i==unique(FEMA_detail$disasterNumber)[1]){
    FEMA_map_d=FEMA_detail[FEMA_detail$disasterNumber==i,]%>%
      tm_shape(paste('Project Number: ',i))+
      tm_polygons('projectAmount',0.5,title="Project Amount",legend.show=F)
  }
  else{
    FEMA_map_d=FEMA_map_d+
      FEMA_detail[FEMA_detail$disasterNumber==i,]%>%
      tm_shape(paste('Project Number: ',i))+
      tm_polygons('projectAmount',0.5,title="Project Amount",legend.show=F)
  }
}

HURR=tm_shape(H_rain,'Rainfall')+
  tm_polygons(col='precip',title="Rainfall (mm)")+
  tm_shape(t_H_track,'Track')+
  tm_lines(col='red')
FEMA_summary$disasterNumber
data.frame('Disaster Location'=FEMA_summary$ID,
           'Disaster Number'=FEMA_summary$disasterNumber,
           'Disaster Time'=FEMA_summary$disasterTime,
           'Disaster Type'=FEMA_summary$declarationType,
           'Disaster Title'=FEMA_summary$declarationTitle)
           