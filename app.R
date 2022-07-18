if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('openxlsx')) install.packages('openxlsx'); library('openxlsx')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('shinyWidgets')) install.packages('shinyWidgets'); library('shinyWidgets')
#library(plyr) 
#library(dplyr)
#library(eeptools)
#library(reshape2)
#library(tidyverse)
#library(ggrepel)
#library(tibble)
#library(janitor)
if (!require('ggiraph')) install.packages('ggiraph'); library('ggiraph')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
if (!require('shiny')) install.packages('shiny'); library('shiny')
#library(plotly)
if (!require('DT')) install.packages('DT'); library('DT')
if (!require('data.table')) install.packages('data.table'); library('data.table')
#library(shinyjs)
#library(ggpubr)
if (!require('cowplot')) install.packages('cowplot'); library('cowplot')
if (!require('rhandsontable')) install.packages('rhandsontable'); library('rhandsontable')
if (!require('shinymanager')) install.packages('shinymanager'); library('shinymanager')

# define some credentials
credentials <- data.frame(
  user = c("ska","shiny", "shinymanager"), # mandatory
  password = c("123","azerty", "12345"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, NA, "2022-12-31"),
  admin = c(TRUE, FALSE, TRUE),
  comment = "Simple and secure authentification mechanism for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

cart = data.frame()
identification <- read_excel('../dataBase/id.xlsx', col_names = TRUE)
identification$Since1 <- as.Date(identification$Since1)
identification$Since2 <- as.Date(identification$Since2)
identification$Since3 <- as.Date(identification$Since3)
visits <- read_excel('../dataBase/visits1.xlsx', col_names = TRUE)
treatments <- read_excel('../dataBase/treatments.xlsx', col_names = TRUE)
csf <- read_excel('../dataBase/csf.xlsx', col_names = TRUE)
mri <- data.frame(read_excel('../dataBase/test_jan18_db.xlsx', sheet = 'MRI', col_names = TRUE))
spaceless <- function(x) {colnames(x) <- gsub(" ", "", colnames(x));x}
AdverseEvent <- spaceless(read_excel('../dataBase/test_jan18_db.xlsx', sheet = "Adverse Event", col_names = TRUE))
AdverseEvent <- data.frame(PatientID=AdverseEvent$PatientID, AdverseEvent=AdverseEvent$AdverseEvent, StartDate=AdverseEvent$StartDate)
FamilyH <- read_excel('../dataBase/test_jan18_db.xlsx', sheet = "Family_History", col_names = TRUE)
FamilyH <- spaceless(subset(FamilyH,select=-c(3:4,8)))
Relapses <- read_excel('../dataBase/test_jan18_db.xlsx', sheet = "Relapses", col_names = TRUE)
Relapses <- spaceless(subset(Relapses,select=c(1:6)))

dataLyneP <- read_excel('../dataBase/ListeLyne2.xlsx', col_names = TRUE)
dataLynePBMC <- read_excel('../dataBase/ListeLyne1.xlsx', sheet = "PBMC", col_names = TRUE)
dataLyneSerum <- read_excel('../dataBase/ListeLyne1.xlsx', sheet = "Serum", col_names = TRUE)
dataLyneLCR <- read_excel('../dataBase/ListeLyne1.xlsx', sheet = "LCR", col_names = TRUE)
dataLyneRNA <- read_excel('../dataBase/ListeLyne1.xlsx', sheet = "RNA-pellet", col_names = TRUE)

dat.serum = read.xlsx('../dataBase/ListeLyne1.xlsx',3)
dat.serum.noNA = subset(dat.serum,!is.na(PatientID))

dat.pbmc = read.xlsx('../dataBase/ListeLyne1.xlsx',2)
dat.pbmc.noNA = subset(dat.pbmc,!is.na(PatientID))

sub.serum = dat.serum.noNA[,c(1:8,19)]
colnames(sub.serum)=c('PatientID','PatientNum','DateSample','Location','Box','NbTubes','TubesLeft','Comments','Medication')
sub.serum$type ='serum'

sub.pbmc = dat.pbmc.noNA[,c(1:3,5:8,10,13)]
colnames(sub.pbmc)=c('PatientID','PatientNum','DateSample','Location','Box','NbTubes','TubesLeft','Comments', 'Medication')
sub.pbmc$type ='pbmc'

sub.sample = rbind(sub.serum,sub.pbmc)
id.noNA = subset(data.frame(identification), !is.na(PatientID))

plot_edss <- function(patient,show_mri=T,show_pbmc=T,show_serum=T,highlight_visits=c(),width=7,height=4){
  
  sub = subset(visits,PatientID==patient)
  Dob = as.character(subset(identification,PatientID==patient)$BirthDate)
  gender = as.character(subset(identification,PatientID==patient)$Gender)
  s2 = subset(treatments,(PatientID==patient) & (MSSpecific=="Yes"))
  
  s3 = subset(csf,PatientID==patient)
  s3=subset(reshape2::melt(subset(s3,PatientID==patient),id.vars='ExamDate'),grepl('IgGIndex|OligoclonalBanding|MatchedOCBinSerum|NbOligoclonalBanding',variable))
  s3=subset(s3,value!='')
  s3$value=ifelse(s3$variable=='IgGIndex',paste(s3$variable,s3$value,sep=':'),s3$value)
  
  med = as.character(subset(identification, PatientID==patient)$DoctorinCharge)
  s4=subset(reshape2::melt(subset(mri,mri$Patient.ID==patient),id.vars='Exam.Date'),grepl('McDonald|Nb.Lesions.T2|Enlarging',variable))
  s4$value=gsub(' ','',s4$value)
  s4=subset(s4,value!='')
  s4$text =paste(gsub('McDonald\\.Lesions\\.','',s4$variable),s4$value,sep=':')
  
  s5=subset(dataLynePBMC,PatientID==patient)
  s6=subset(dataLyneSerum,PatientID==patient)
  
  s7 = subset(Relapses,(PatientID==patient))
  s7=subset(reshape2::melt(subset(s7,PatientID==patient),id.vars='RelapseDate'),grepl('Duration|Recovery|Severity',variable))
  s7$value=ifelse(s7$variable=='Duration',paste(s7$variable,s7$value,sep=':'),s7$value)
  s7$value=ifelse(s7$variable=='Recovery',paste(s7$variable,s7$value,sep=':'),s7$value)
  s7$value=ifelse(s7$variable=='Severity',paste(s7$variable,s7$value,sep=':'),s7$value)
  
  edss_max = max(na.omit(sub$EDSS))
  
  
  sub[which(is.na(sub$dataUsedBy)),'dataUsedBy']=''
  sub[which(sub$dataUsedBy!=''),'dataUsedBy']=paste('data used by:',sub[which(sub$dataUsedBy!=''),'dataUsedBy'],sep=' ')
  
  if(nrow(s2)>0){
    
    vec = 1:nrow(s2)
    vec = vec/(max(vec)/edss_max)
    s2$y = vec
    s2$EndDate=as.character(s2$EndDate)
    s2[which(s2$EndDate=='#N/A'|is.na(s2$EndDate)),'EndDate']=as.character(max(as.Date(sub$VisitDate)))
    s2[which(s2$Discontinuation=='#N/A'|is.na(s2$Discontinuation)),'Discontinuation']="Discontinuation missing"
    sub$is_used=0
    sub[which(sub$dataUsedBy!=''),'is_used']=2
    
    
    pl = ggplot(sub[which(!is.na(sub$EDSS)),],aes(as.Date(VisitDate),EDSS))+
      geom_line()+
      geom_point_interactive(aes(fill=type, tooltip=paste(dataUsedBy, sep="\n"), data_id=rownames(sub[which(!is.na(sub$EDSS)),])),stroke=0.5,shape=21,col='black',size=3)+
      geom_point_interactive(data=sub[which(!is.na(sub$EDSS) & sub$is_used==2),],aes(fill=type, tooltip=paste(dataUsedBy, sep="\n"), data_id=rownames(sub[which(!is.na(sub$EDSS) & sub$is_used==2),])),stroke=2,shape=21,col='red',size=3)+
      geom_segment_interactive(data=s2,aes(x=as.Date(StartDate),xend=as.Date(EndDate),y=y,yend=y,col=Medication, tooltip=paste(Discontinuation, "StartDate :",StartDate,"EndDate :", EndDate, sep="\n")),arrow=arrow(length=unit(0.20,"cm")),size=1)+
      theme_bw()+xlab('')+ggtitle(paste0('PatientID = ',patient,'\n DoB = ',Dob, '; Gender = ',gender)) 
      
    
  }else{  
    
    pl = ggplot(sub[which(!is.na(sub$EDSS)),],aes(as.Date(VisitDate),EDSS))+
      geom_line()+
      geom_point_interactive(aes(fill=type, tooltip=paste(dataUsedBy, sep="\n"), data_id=rownames(sub[which(!is.na(sub$EDSS)),])),shape=21,size=3)+
      # geom_line(aes(y=cumsum),col='red',linetype='dashed')+
      xlab('')+
      theme_bw()+
      ggtitle(paste0('PatientID = ',patient,'\n DoB = ',Dob, '; Gender = ',gender))
  }
  if(nrow(s3)>0){
    info=list()
    s3$out=''
    for(i in unique(s3[,1])){
      info[[i]]<- paste(as.character(subset(s3,ExamDate==i)$value), collapse="\n")
      s3[which(s3[,1]==i),'out']=info[[i]]
    }
    pl = pl + 
      geom_point_interactive(data=s3,aes(x=as.Date(ExamDate,format = "%Y-%m-%d"),y=-0.5, tooltip=paste('CSF:',out, sep="\n"), data_id=out),shape=19,col='blue',size=4)+
      geom_vline(xintercept=as.Date(s3$ExamDate,format = "%Y-%m-%d"),color='red',linetype='dashed')
    
  }
  if(nrow(s7)>0){
    info=list()
    s7$out=''
    for(i in unique(s7[,1])){
      info[[i]]<- paste(as.character(subset(s7,RelapseDate==i)$value), collapse="\n")
      s7[which(s7[,1]==i),'out']=info[[i]]
    }
    pl = pl + 
      geom_point_interactive(data=s7,aes(x=as.Date(RelapseDate,format = "%d/%m/%Y"),y=-0.5, tooltip=paste('Relapses:',out, sep="\n"), data_id=out),shape=19,col='green',size=4)+
      geom_vline(xintercept=as.Date(s7$RelapseDate,format = "%d/%m/%Y"),color='blue',linetype='dashed')
    
  }
  if(nrow(s4)>0 & show_mri){
    info=list()
    s4$out=''
    for(i in unique(s4[,1])){
      info[[i]]<- paste(as.character(subset(s4,Exam.Date==i)$text), collapse="\n")
      s4[which(s4[,1]==i),'out']=info[[i]]
    }
    
    pl = pl +
      #geom_point(data=s4,aes(as.Date(Exam.Date,format = "%d/%m/%Y"),y=0),shape=18,size=5,color='blue')+
      geom_point_interactive(data=s4,aes(x=as.Date(Exam.Date,format = "%d/%m/%Y"),y=-0.5, tooltip=paste('MRI Results:',out, sep="\n"), data_id=out),shape=19,size=4)+
      geom_vline(xintercept=as.Date(s4$Exam.Date,format = "%d/%m/%Y"),color='blue',linetype='dashed')
  }
  if(nrow(s5)>0 & show_pbmc){
    pl = pl +
      geom_point_interactive(data=s5,shape=21,fill='tomato',aes(x=as.Date(PBMC,format = "%Y-%m-%d"),y=-0.5, tooltip=paste('PBMC Localisation:',Localisation1, LocalisationPBMC,"Tubes Restants:",`#tubesrestant`,sep="\n")),size=4)+
      geom_vline(xintercept=as.Date(s5$PBMC,format = "%Y-%m-%d"),color='green',linetype='dashed')
  }  
  if(nrow(s6)>0 & show_serum){
    pl = pl +
      geom_point_interactive(data=s6,shape=23,fill='goldenrod',aes(x=as.Date(Serum,format = "%Y-%m-%d"),y=-0.7, tooltip=paste("Serum Localisation:", LocalisationSerum, RackEtageBoite,"Tubes Restants",`#tubesrestants`,sep="\n")),size=4)+
      geom_vline(xintercept=as.Date(s6$Serum,format = "%Y-%m-%d"),color='green',linetype='dashed')
  } 
  if(length(highlight_visits)>0){
    sub.visits = subset(sub, biobankID  %in% highlight_visits)
    pl = pl + geom_vline(data=sub.visits, xintercept=as.Date(sub.visits$VisitDate,format = "%Y-%m-%d"),col='red') 
  }
  
  #deltaE = subset(visits,PatientID==patient)
  #deltaE$EDSS=as.numeric(as.character(deltaE$EDSS))
  #deltaE<-dplyr::filter(deltaE,  !is.na(EDSS))
  #deltas=c()
  #for(i in 1:nrow(deltaE)){
  #  if(i == 1){deltas=c(deltas,NA)
  #  deltas[1]=0
  #  }
  #  else if(is.na(deltaE[i,"EDSS"])|is.na(deltaE[i-1,"EDSS"])){deltas=c(deltas, NA)}
  #  else{deltas=c(deltas,deltaE[i,"EDSS"]-deltaE[i-1,"EDSS"])}
  #}
  #deltaE$delta <- unlist(deltas) 
  #dt <- ggplot(deltaE,aes(VisitDate,as.numeric(delta)))+geom_point()+geom_path()+ylab("DELTA-EDSS")+theme_bw()+geom_line(aes(color = "Test")) + scale_color_manual(values = NA) +theme(legend.text = element_blank(), legend.title = element_blank())
  pl+ylim(c(-0.5,edss_max))
  #figure <- plot_grid(pl, dt, align = "v",ncol = 1, nrow = 2 )
  return(pl)
}


ui <- secure_app(fluidPage(
  tags$h1("My secure application",align = "center"),
  HTML("<br>"),
  #verbatimTextOutput("auth_output"),
  fluidRow(
    box(width=12,
        column(width=2,sliderInput("range", "Age Range:", min = 5, max = 95, value = c(5,95))),
        column(width=2,sliderInput("range.OnSet", "Age At date of Onset:", min = 5, max = 95, value = c(5,95))),
        column(width=2,sliderInput("range.Diag", "Age At diagnosis:", min = 5, max = 95, value = c(5,95))),
        column(width=1,checkboxGroupInput('sex','Sex:',c('M','F'),selected=c('M','F'))),
        column(width=1,checkboxGroupInput('MStypes','Disease:',choices=na.omit(unique(data.frame(identification)$typeMS)),selected=na.omit(unique(data.frame(identification)$typeMS)))),
        column(width=1,checkboxGroupInput('diseaseProgression','Disease progression:',choices=(unique(data.frame(identification)$progressionAfPBMC)),selected=(unique(data.frame(identification)$progressionAfPBMC)))),
        column(width=1,checkboxGroupInput('sampleNT','Sample with new treatment:',choices=(unique(data.frame(dataLyneP)$treatmentAtPrel)),selected=(unique(data.frame(dataLyneP)$treatmentAtPrel)))),
        column(width=2,sliderInput("Range.ARR", "Range ARR:",min = 0, max = 6, value = c(0,6)))),
    box(width=12, 
        #box(width=3,selectInput('Med','Medication:',as.character(dataLyneSerum$Med))),
        box(width=3,pickerInput("Med","Medication:", choices=as.character(unique(dataLyneSerum$Med)), selected=dataLyneSerum$Med,options = list(`actions-box` = TRUE),multiple = T)),
        column(width=3, sliderInput('pbmc_n','Number of PBMC samples per patient:',min=0,max=10,value=1)),
        column(width=3,sliderInput("range.edss", "Range EDSS:",min = 0, max = 9, value = c(0,9))),
        box(width=3,pickerInput("Progression", "Rythme de progression:", choices=c(NA,sort(as.numeric(unique(data.frame(identification)$delta5YAfPBMC)))), selected=(unique(data.frame(identification)$delta5YAfPBMC)),options = list(`actions-box` = TRUE),multiple = T))),
    #column(width=3,sliderInput("Progression", "Rythme de progression:", min = -6.5, max = 6.5, value = c(-6.5,6.5)))),
    tags$h3("Please choose and click on the patient ID to view the visit graph and samples",align = "center"),
    #HTML("<br>"),
    box(width=12,DT::dataTableOutput("dt")),
    box(width=12,
        column(width=4,checkboxInput('show_mri','MRI',FALSE)),
        column(width=4,checkboxInput('show_pbmc','PBMC',TRUE)),
        column(width=4,checkboxInput('show_serum','SERUM',FALSE))),
    
    box(width=12,girafeOutput('edss',width="100%"),
        downloadButton("dndPlot", "Download",width="100%"),align = "center"),
    box(width=12,
        tabsetPanel(
          tabPanel('pbmc',rHandsontableOutput("table3")),
          tabPanel('serum',rHandsontableOutput("table")))),
    box(width=12,title='My Cart',
        textInput('name', "Name", value = "", width = NULL, placeholder = NULL),
        rHandsontableOutput("table2")),
    column(width=6,actionButton("cart", "Add", width = '100%')),
    column(width=6,actionButton("clear", "Clear", width = '100%')),
    
    column(width=12,downloadButton("downloadData", "Download",width="100%")))
  
  
))
server <- function(input, output) {
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  myReactives <- reactiveValues()
  myReactives$cart = data.frame()
  myReactives$pts = id.noNA
  myReactives$pt = ''
  # the datatable
  output$dt <- DT::renderDataTable({
    
    sampleNT = input$sampleNT
    sampleNT[sampleNT=='TRUE']=TRUE
    sampleNT[sampleNT=='FALSE']=FALSE
    
    n_pbmc = subset(dataLynePBMC, Med %in% input$Med)
    
    #n_serum = subset(dataLyneSerum, Med %in% input$Med)
    
    n_pbmc=table(n_pbmc$PatientID)
    #n_serum=table(n_serum$PatientID)
    
    pts = names(n_pbmc[n_pbmc>=input$pbmc_n])
    
    diseaseProg = input$diseaseProgression
    diseaseProg[diseaseProg=='']=NA
    diseaseProg[diseaseProg=='TRUE']=TRUE
    diseaseProg[diseaseProg=='FALSE']=FALSE
    
    sub.tab = subset(id.noNA, Gender %in% input$sex & progressionAfPBMC %in% diseaseProg & ageAtDateOfOnset<max(input$range.OnSet) & ageAtDateOfOnset>min(input$range.OnSet) & ageDiagnosis<max(input$range.Diag) & ageDiagnosis>min(input$range.Diag) & typeMS %in% input$MStypes & delta5YAfPBMC %in% as.numeric(input$Progression) & ARR>=min(as.numeric(input$Range.ARR)) & ARR<=max(as.numeric(input$Range.ARR)) & PatientID %in% pts )
    
    pID = unique(data.frame(subset(visits,EDSS>=min(input$range.edss) & EDSS<=max(input$range.edss) & ((isUsedBiobankPBMC==TRUE|isUsedBiobankSerum==TRUE))))[,1])
    bbID = unique(data.frame(subset(visits,EDSS>=min(input$range.edss) & EDSS<=max(input$range.edss) & ((isUsedBiobankPBMC==TRUE|isUsedBiobankSerum==TRUE))))[,'biobankID'])
    n_sample = unique(data.frame(subset(dataLyneP, treatmentAtPrel %in% input$sampleNT))[,1])
    
    sub.tab = subset(sub.tab, PatientID %in% pID)
    sub.tab = subset(sub.tab, PatientID %in% n_sample)
    
    AID = unique(data.frame(subset(visits,ageAtVisits>=min(input$range) & ageAtVisits<=max(input$range) & (isUsedBiobankPBMC==TRUE|isUsedBiobankSerum==TRUE)  ))[,1])
    
    sub.tab = subset(sub.tab, PatientID %in% AID)
    
    myReactives$bbID = bbID
    myReactives$pts = sub.tab
    DT::datatable(myReactives$pts, select="none",option=list(scrollX=T,pageLength =  5))})
  
  output$edss <-  renderGirafe({
    myReactives$pt = as.character(myReactives$pts[input$dt_cell_clicked$row,'PatientID'])
    
    if(is.null(input$dt_cell_clicked$row)){
      myReactives$pt = as.character(myReactives$pts$PatientID[1])
    }
    pl=plot_edss(myReactives$pt,input$show_mri,input$show_pbmc,input$show_serum, myReactives$bbID)
    #ggsave(paste0(myReactives$pt,'.edss_plot.pdf'),pl,width=7,height=4)
    girafe(ggobj = pl,options=list(rescale = FALSE),width_svg = 7, height_svg = 4)
  })
  
  output$table <- renderRHandsontable({
    if(is.null(input$dt_cell_clicked$row)){
      df = subset(sub.sample,type=='serum' & PatientID==as.character(myReactives$pts$PatientID[1]))
    }else{
      df = subset(sub.sample,type=='serum' & PatientID==as.character(myReactives$pts[input$dt_cell_clicked$row, 'PatientID']))
    }
    df$check = F
    
    df = df[,c('check',colnames(df)[1:(ncol(df)-1)])]
    mytab <- rhandsontable(df, colHeaders=colnames(df), rowHeaders=TRUE, selectCallback = TRUE)
    return(mytab)
  })
  
  output$table3 <- renderRHandsontable({
    if(is.null(input$dt_cell_clicked$row)){
      df = subset(sub.sample,type =='pbmc' & PatientID==as.character(myReactives$pts$PatientID[1]))
    }else{
      df = subset(sub.sample,type=='pbmc' & PatientID==as.character(myReactives$pts[input$dt_cell_clicked$row, 'PatientID']))
    }
    df$check = F
    df = df[,c('check',colnames(df)[1:(ncol(df)-1)])]
    mytab <- rhandsontable(df, colHeaders=colnames(df), rowHeaders=TRUE,selectCallback = TRUE)
    return(mytab)
  })
  output$selected=renderPrint({cat(input$table_select$select$rAll)})
  
  observeEvent(input$table3, {
    test_df = hot_to_r(input$table3)
    #print(test_df)
    # browser() # uncomment for debugging
  })
  
  output$table2=renderRHandsontable({
    out = subset(myReactives$cart,check==T)
    identification=data.frame(identification)
    rownames(identification)=identification$PatientID
    
    dup = !duplicated(out[,c('PatientNum','DateSample','type')])
    
    
    if(!'Gender' %in% colnames(out)){
      out = data.frame(identification[as.character(out$PatientID),c('Gender','BirthDate','typeMS')],cbind(out))
    }
    if(input$name ==''){
      name='Unknown'
    }else{
      name = input$name
    }
    out = data.frame(cbind(name,0,out))
    colnames(out)[1:2]=c('Name','NbTubeNeeded')
    dup = !duplicated(out[,c('PatientNum','DateSample','type')])
    
    
    #out$check = NULL
    mytab <- rhandsontable(out[dup,], colHeaders=colnames(out), rowHeaders=TRUE,selectCallback = TRUE)
    #myReactives$cart = out
    
    return(mytab)
    
  })
  
  observeEvent(input$cart,{
    
    if(nrow(myReactives$cart)==0){
      myReactives$cart = rbind(hot_to_r(isolate(input$table3)),hot_to_r(isolate(input$table)))
    }else{
      myReactives$cart = rbind(myReactives$cart,hot_to_r(isolate(input$table3)),hot_to_r(isolate(input$table)))
    }
  })
  observeEvent(input$clear,{
    myReactives$cart=data.frame(check=F,PatientNum=NA,DateSample=NA,type=NA)
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      'my_cart.xlsx'
    },
    content = function(file) {
      identification=data.frame(identification)
      rownames(identification)=identification$PatientID
      out = subset(myReactives$cart,check==T)
      if(!'Gender' %in% colnames(out)){
        out = data.frame(identification[as.character(out$PatientID),c('Gender','BirthDate','typeMS')],cbind(out))
      }
      if(input$name ==''){
        name='Unknown'
      }else{
        name = input$name
      }
      out = data.frame(cbind(name,0,out))
      colnames(out)[1:2]=c('Name','NbTubeNeeded')
      dup = !duplicated(out[,c('PatientNum','DateSample','type')])
      
      write.xlsx(out[dup,], file, row.names = FALSE)
    }
  )
  output$dndPlot <- downloadHandler(
    filename = function() {
      paste0(myReactives$pt,'.edss_plot.pdf')
    },
    content = function(file) {
      file.copy(paste0(myReactives$pt,'.edss_plot.pdf'), file, overwrite=TRUE)
    }
  )
  #observeEvent(input$savefile,
  #              {
  #                     #Convert to R object
  #                     identification=data.frame(identification)
  #                     rownames(identification)=identification$PatientID
  #                     out = subset(myReactives$cart,check==T)
  #                     if(!'Gender' %in% colnames(out)){
  #                        out = data.frame(identification[as.character(out$PatientID),c('Gender','BirthDate','typeMS')],cbind(out))
  #                     }
  #                     if(input$name ==''){
  #                      name='Unknown'
  #                     }else{
  #                      name = input$name
  #                     }
  #                     out = data.frame(cbind(name,0,out))
  #                     colnames(out)[1:2]=c('Name','NbTubeNeeded')
  #                     dup = !duplicated(out[,c('PatientNum','DateSample','type')])
  #
  #                     write.xlsx(out[dup,],'submissions/my_cart.xlsx')
  #                 
  #              }
  #          )
  
  
}

shinyApp(ui, server)