# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(tmap)
library(sp)
library(sf)
library(maps)
library(viridis)


# Load data
closest_dist=read.csv('closest_dist.csv',T)
hurr_tracks=read.csv('hurr_tracks.csv',T)
rain=read.csv('rain.csv',T)

detail=read.csv('PublicAssistanceFundedProjectsDetails.csv',T)
summary=read.csv('DisasterDeclarationsSummaries.csv',T)

# Basic mapping Data
data(county.fips)
county.fips=county.fips%>%
    separate(polyname,c('ID','U'),sep=':')%>%
    select(fips,ID)%>%unique()
M=st_as_sf(map('county',plot=F,fill=T))
M=left_join(M,county.fips,'ID')
## Extract name list of hurricane
H_name_all=hurr_tracks%>%select(storm_id)%>%unique()
H_name_all$storm_id2=H_name_all$storm_id
H_name_all=separate(H_name_all,storm_id2,c('storm','id'),sep='-')
## Dealing with special case 'Twenty-Two-2005'
H_name_all$storm[H_name_all$id=='Two']='Twenty-Two'
H_name_all$id[H_name_all$id=='Two']='2005'
H_name_all$id=as.numeric(H_name_all$id)


# Define UI for application that draws a histogram
ui=fluidPage(
    headerPanel('Hurricane & FEMA Data'),
    # Sidebar
    sidebarPanel(
        selectInput('year','Year',1999:2008,2005),
        uiOutput('ui_hurricane'),
        checkboxGroupInput('selection','Data Sources',
                           c('Hurricane Data'='hd',
                             'FEMA Summary'='fs',
                             'FEMA Details'='fd'),'hd'),
        helpText("Note: Be patient. The data files are large and the mapping",
                 "process is slow. When runing this shiny app for the first",
                 "time, please wait for at least 15 seconds.")
        ),
    # Main panel
    mainPanel(
        h3('Map of Hurricane & Emergency'),
        tmapOutput('map'),
        h3('Plot of Culmulative Rainfall >50mm'),
        plotOutput('plot'),
        h3('Data of Emergency summary'),
        dataTableOutput('data')
        )
    )
# Define server logic required to draw a histogram
server <- function(input,output) {
    # Reactive ui to select hurricane name with a giving year
    s_hurricane=reactive(sort(H_name_all$storm[H_name_all$id==input$year]))
    output$ui_hurricane=renderUI(
        selectInput('hurricane','Hurricane',s_hurricane(),'Cindy')
    )
    # Reactive ui to select additional data with a giving hurricane
    # Obtain hurricane name
    H_name=reactive({

        H_name_all$storm_id[H_name_all$storm==input$hurricane&
                                H_name_all$id==input$year]
    })
    # Obtain hurricane track
    H_track=reactive({
        validate(need(H_name() != "", "Please wait."))
        H_track=hurr_tracks%>%
            filter(storm_id==H_name())
        H_track$DT=ymd_hm(H_track$date)
        H_track
    })
    # Obtain hurricane rain data
    H_rain=reactive({
        H_rain=rain%>%
            filter(storm_id==H_name())%>%
            mutate(fips=as.numeric(fips))
        right_join(M,H_rain,'fips')
    })
    # Obtain FEMA summary data
    FEMA_summary=reactive({
        FEMA_summary=summary
        FEMA_summary=FEMA_summary%>%
            filter(DT>=min(date(H_track()$DT)),DT<=max(date(H_track()$DT)))%>%
            select(disasterNumber,disasterTime=DT,declarationType,
                   declarationTitle,fipsStateCode,fipsCountyCode)%>%
            mutate(fips=fipsStateCode*1000+fipsCountyCode)%>%
            select(-fipsStateCode,-fipsCountyCode)
        right_join(M,FEMA_summary,'fips')
    })
    # Obtain FEMA detail data
    FEMA_detail=reactive({
        FEMA_detail=detail%>%
            filter(disasterNumber%in%FEMA_summary()$disasterNumber)
        FEMA_detail$fips=FEMA_detail$stateNumberCode*1000+FEMA_detail$countyCode
        FEMA_detail=FEMA_detail%>%
            filter(countyCode!=0,)%>%
            group_by(disasterNumber,fips)%>%
            summarise(projectAmount=sum(projectAmount,na.rm=T))
        FEMA_detail=right_join(M,FEMA_detail,'fips')
    })
    # Mapping
    FEMA_map_s=reactive({
        for(i in unique(FEMA_summary()$declarationType)) {
            if (i==unique(FEMA_summary()$declarationType)[1]){
                FEMA_map_s=FEMA_summary()[FEMA_summary()$declarationType==i,]%>%
                    tm_shape(paste('Declaraation Type: ',i))+
                    tm_polygons('red',0.5,title="Declaraation Type: ",legend.show=F)
            }
            else{
                FEMA_map_s=FEMA_map_s+
                    FEMA_summary()[FEMA_summary()$declarationType==i,]%>%
                    tm_shape(paste('Declaraation Type: ',i))+
                    tm_polygons('red',0.5,title="Declaraation Type: ",legend.show=F)
            }
        }
        FEMA_map_s
    })
    FEMA_map_d=reactive({
        for(i in unique(FEMA_detail()$disasterNumber)){
            if (i==unique(FEMA_detail()$disasterNumber)[1]){
                FEMA_map_d=FEMA_detail()[FEMA_detail()$disasterNumber==i,]%>%
                    tm_shape(paste('Project Number: ',i))+
                    tm_polygons('projectAmount',0.5,title="Project Amount",legend.show=F,
                                palette=viridis(n=2,direction=-1,option="A"))
            }
            else{
                FEMA_map_d=FEMA_map_d+
                    FEMA_detail()[FEMA_detail()$disasterNumber==i,]%>%
                    tm_shape(paste('Project Number: ',i))+
                    tm_polygons('projectAmount',0.5,title="Project Amount",legend.show=F,
                                palette=viridis(n=2,direction=-1,option="A"))
            }
        }
        FEMA_map_d
    })
    HURR=reactive({
        t_H_track=cbind(H_track()$longitude,H_track()$latitude)%>%
            Line()%>%Lines(ID=H_name())%>%
            list()%>%SpatialLines()
        HURR=tm_shape(H_rain(),'Rainfall')+
            tm_polygons(col='precip',title="Rainfall (mm)")+
            tm_shape(t_H_track,'Track')+
            tm_lines(col='red')
    })
    map=reactive({
        if('hd'%in%input$selection) M1=HURR() else M1=NULL
        if('fs'%in%input$selection) M2=FEMA_map_s() else M2=NULL
        if('fd'%in%input$selection) M3=FEMA_map_d() else M3=NULL
        M1+M2+M3
    })
    plot=reactive({
        validate(need(H_name() != "", "Please wait."))
        H_dist=closest_dist%>%
            filter(storm_id==H_name(),storm_dist<500)
        
        H_rain_limit=H_rain()%>%
            filter(precip>50,fips%in%H_dist$fips)
        
        t_H_track=cbind(H_track()$longitude,H_track()$latitude)%>%
            Line()%>%Lines(ID=H_name())%>%
            list()%>%SpatialLines()
        ggplot()+
            geom_sf(data=H_rain())+
            geom_sf(data=H_rain_limit,mapping=aes(fill=precip))+
            scale_fill_steps(low='white',high='red', name='Rainfall (mm)')+
            geom_path(data=H_track(),mapping=aes(x=longitude,y=latitude))+
            ggtitle(H_name())+
            theme(plot.title=element_text(hjust=0.5),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  axis.title=element_blank(),
                  axis.text=element_blank(),
                  axis.ticks=element_blank())
    })
    data=reactive({
        validate(need(H_name() != "", "Please wait."))
        data.frame('Disaster Location'=FEMA_summary()$ID,
                   'Disaster Number'=FEMA_summary()$disasterNumber,
                   'Disaster Time'=FEMA_summary()$disasterTime,
                   'Disaster Type'=FEMA_summary()$declarationType,
                   'Disaster Title'=FEMA_summary()$declarationTitle)
    })
    output$map=renderTmap(map())
    output$plot=renderPlot(plot())
    output$data=renderDataTable(data())
}    


# Run the application 
shinyApp(ui = ui, server = server)
