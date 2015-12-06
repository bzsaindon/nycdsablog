library(googleVis)
require(datasets)
library(dplyr)
library(ggplot2)
library(shiny)
library(ggvis)
library(ggthemes)
library(psych)
#getwd()
##LOAD DATA
#test
#ip_tmp <-read.csv("/Users/briansaindon/desktop/Files/NYCDSA/Project2/cms_state_app/data/Medicare_Charge_Inpatient_DRG100_DRG_Summary_by_DRGState_FY2013.csv", sep=",", fill=TRUE, header=TRUE)

stats <- do.call(rbind, lapply((describeBy(ip_tmp$Average.Covered.Charges, group=ip_tmp$DRG.Definition)), as.data.frame))
#stats$vars <-c(ip_tmp$DRG.Definition)
stats$vars <- rownames(stats)
gvisTable(stats)
#statscomparedata<-ip_tmp%>%filter(DRG.Definition==c("039 - EXTRACRANIAL PROCEDURES W/O CC/MCC", "069 - TRANSIENT ISCHEMIA"))
#statscomparedata <- ip_tmp[ip_tmp$DRG.Definition %in% c("039 - EXTRACRANIAL PROCEDURES W/O CC/MCC", "069 - TRANSIENT ISCHEMIA"), ]
#stats2 <- do.call(rbind, lapply((describeBy(statscomparedata$value, group=statscomparedata$DRG.Definition, skew=FALSE)), as.data.frame))
#gvisTable(stats2)
#end test

ip_tmp <-read.csv("data/Medicare_Charge_Inpatient_DRG100_DRG_Summary_by_DRGState_FY2013.csv", sep=",", fill=TRUE, header=TRUE)
ip <- rename(ip_tmp, region=Provider.State)

average_covered_charges_ip <- select(ip, Average.Covered.Charges, region, DRG.Definition) %>% 
  rename(., value = Average.Covered.Charges) 

average_total_payments_ip <- select(ip, Average.Total.Payments, region, DRG.Definition) %>% 
  rename(., value = Average.Total.Payments)

average_medicare_payments_ip <- select(ip, Average.Medicare.Payments, region, DRG.Definition) %>% 
  rename(., value = Average.Medicare.Payments)

total_discharges_ip <- select(ip, Total.Discharges, region, DRG.Definition) %>% 
  rename(., value = Total.Discharges)


shinyServer(function(input, output) {

    output$map2 <-
      renderGvis({
        datasetInput2 <- 
          reactive({
            switch(input$var,
                   "average_covered_charges_ip" = average_covered_charges_ip,
                   "average_total_payments_ip" = average_total_payments_ip,
                   "average_medicare_payments_ip" = average_medicare_payments_ip,
                   "total_discharges_ip" = total_discharges_ip)
          })
        
        mapdata2<-filter(datasetInput2(), DRG.Definition==input$drgvar)
        gvisGeoChart(mapdata2, "region", "value", 
                   options=list(title= "US Cost Variation",
                                region="US", 
                                displayMode="regions", 
                                resolution="provinces",
                                width=600, height=400))
    })
    
    output$histogram<-
      renderPlot({
        datasetInput3 <- 
          reactive({
            switch(input$var,
                   "average_covered_charges_ip" = average_covered_charges_ip,
                   "average_total_payments_ip" = average_total_payments_ip,
                   "average_medicare_payments_ip" = average_medicare_payments_ip,
                   "total_discharges_ip" = total_discharges_ip)
          })
        histdata<-filter(datasetInput3(), DRG.Definition==input$drgvar)
        ggplot(histdata, aes(x=value, fill=DRG.Definition)) + 
          geom_density(aes(x=value, fill=DRG.Definition), alpha=.3) + 
            theme_few() + theme(legend.position="bottom", legend.justification=c(0,1))+
        ylab("Density") +  
          xlab(input$var)+
          ggtitle(input$drgvar) +
          theme(legend.title=element_text(size=10)) +
          guides(shape=guide_legend(override.aes=list(size=100))) 
      })
    
    output$matrix<- 
      renderGvis({
        datasetInput30 <- 
          reactive({
            switch(input$var,
                   "average_covered_charges_ip" = average_covered_charges_ip,
                   "average_total_payments_ip" = average_total_payments_ip,
                   "average_medicare_payments_ip" = average_medicare_payments_ip,
                   "total_discharges_ip" = total_discharges_ip)
          })
      statsdata<-filter(datasetInput30(), DRG.Definition==input$drgvar)
      stats <- do.call(rbind, lapply((describeBy(statsdata$value, group=statsdata$DRG.Definition, skew=FALSE, range=FALSE)), as.data.frame))
      stats$vars <- rownames(stats)
      gvisTable(stats);
      })
      

    
    output$histogramcompare<-
      renderPlot({
        datasetInput4 <- 
          reactive({
            switch(input$var,
                   "average_covered_charges_ip" = average_covered_charges_ip,
                   "average_total_payments_ip" = average_total_payments_ip,
                   "average_medicare_payments_ip" = average_medicare_payments_ip,
                   "total_discharges_ip" = total_discharges_ip)
          })
        histcomparedata <- na.omit(datasetInput4()[datasetInput4()$DRG.Definition %in% c(input$drgcompare), ])
        
        ggplot(histcomparedata, aes(x=value, fill=DRG.Definition)) + 
          geom_density(aes(x=value, fill=DRG.Definition), alpha=.3) + 
          theme_few() + theme(legend.position="bottom", legend.direction="vertical")+
          ylab("Density") +  
          xlab(input$var)+
          ggtitle("Disease Level Comparison") +
          theme(legend.title=element_text(size=10)) +
          guides(shape=guide_legend(override.aes=list(size=100))) 
        
      })
    
    output$scatterplot<-
      renderPlot({
        scatterdata <- na.omit(ip[ip$region %in% c(input$state), ])
        
        ggplot(scatterdata, aes(x=Average.Covered.Charges, y=Average.Total.Payments, color=region)) + 
          geom_point(shape=1) +
           geom_line() +
          ggtitle("State Level Comparison - Covered Charges [Billed to Medicare] v Total Payments [Medicare Actual Payment") +
          theme(legend.title=element_text(size=10)) +
          guides(shape=guide_legend(override.aes=list(size=100))) + theme_bw()

      })
    
    output$scatterplot2<-
      renderPlot({
        
        scatterdata <- na.omit(ip[ip$region %in% c(input$state), ])
        ggplot(scatterdata, aes(x=Average.Covered.Charges, y=Average.Medicare.Payments, color=region)) + 
          geom_point(shape=1) +
          geom_line() +
          ggtitle("State Level Comparison - Covered Charges [Billed to Medicare]  v Medicare Payments") +
          theme(legend.title=element_text(size=10)) +
          guides(shape=guide_legend(override.aes=list(size=100))) + theme_bw()
        
        
      })
    output$matrix1<- 
      renderGvis({
        statscomparedata <- na.omit(ip[ip$DRG.Definition %in% c(input$drgcompare), ])
        stats3 <- do.call(rbind, lapply((describeBy(statscomparedata$Average.Covered.Charges, group=statscomparedata$DRG.Definition, skew=FALSE, range=FALSE)), as.data.frame))
        stats3$vars <- rownames(stats3)
        gvisTable(stats3);
      })
    
    output$histogramavgcov<-
      renderPlot({
        
        histcompare1<-na.omit(filter(ip, DRG.Definition == c(input$drgcompare)))
        
        ggplot(histcompare1, aes(x=Average.Covered.Charges, fill=DRG.Definition)) + 
          geom_density(aes(x=Average.Covered.Charges, fill=DRG.Definition), alpha=.3) + 
          theme_few() + theme(legend.position="bottom", legend.direction="vertical")+
          ylab("Density") +  
          xlab("Average Covered Charges")+
          ggtitle("Disease Level Comparison - Average Covered Charges [Billed to Medicare by Provider]") +
          theme(legend.title=element_text(size=10)) +
          guides(shape=guide_legend(override.aes=list(size=100))) 
        
      })
    
    output$matrix2<- 
      renderGvis({
        statscomparedata <- na.omit(ip[ip$DRG.Definition %in% c(input$drgcompare), ])
        stats3 <- do.call(rbind, lapply((describeBy(statscomparedata$Average.Total.Payments, group=statscomparedata$DRG.Definition, skew=FALSE, range=FALSE)), as.data.frame))
        stats3$vars <- rownames(stats3)
        gvisTable(stats3);
      })
    
    output$histogramavgtot<-
      renderPlot({
        
        histcompare2<-na.omit(filter(ip, DRG.Definition == c(input$drgcompare)))
        
        ggplot(histcompare2, aes(x=Average.Total.Payments, fill=DRG.Definition)) + 
          geom_density(aes(x=Average.Total.Payments, fill=DRG.Definition), alpha=.3) + 
          theme_few() + theme(legend.position="bottom", legend.direction="vertical")+
          ylab("Density") +  
          xlab("Average Total Payments")+
          ggtitle("Disease Level Comparison - Average Total Payments [Medicare + Beneficiary + 3rd Party]") +
          theme(legend.title=element_text(size=10)) +
          guides(shape=guide_legend(override.aes=list(size=100))) 
        
      })
    output$matrix3<- 
      renderGvis({
        statscomparedata <- na.omit(ip[ip$DRG.Definition %in% c(input$drgcompare), ])
        stats4 <- do.call(rbind, lapply((describeBy(statscomparedata$Average.Medicare.Payments, group=statscomparedata$DRG.Definition, skew=FALSE, range=FALSE)), as.data.frame))
        stats4$vars <- rownames(stats4)
        gvisTable(stats4);
      })
    
    output$histogramavgmed<-
      renderPlot({
        
        histcompare3<-na.omit(filter(ip, DRG.Definition == c(input$drgcompare)))
        
        ggplot(histcompare3, aes(x=Average.Medicare.Payments, fill=DRG.Definition)) + 
          geom_density(aes(x=Average.Medicare.Payments, fill=DRG.Definition), alpha=.3) + 
          theme_few() + theme(legend.position="bottom", legend.direction="vertical")+
          ylab("Density") +  
          xlab("Average Medicare Payments")+
          ggtitle("Disease Level Comparison - Average Medicare Payments") +
          theme(legend.title=element_text(size=10)) +
          guides(shape=guide_legend(override.aes=list(size=100))) 
        
      })
    
    output$matrix4<- 
      renderGvis({
        statscomparedata <- na.omit(ip[ip$DRG.Definition %in% c(input$drgcompare), ])
        stats5 <- do.call(rbind, lapply((describeBy(statscomparedata$Total.Discharges, group=statscomparedata$DRG.Definition, skew=FALSE, range=FALSE)), as.data.frame))
        stats5$vars <- rownames(stats5)
        gvisTable(stats5);
      })
    
    output$histogramtotdis<-
      renderPlot({
        
        histcompare4<-na.omit(filter(ip, DRG.Definition == c(input$drgcompare)))
        
        ggplot(histcompare4, aes(x=Total.Discharges, fill=DRG.Definition)) + 
          geom_density(aes(x=Total.Discharges, fill=DRG.Definition), alpha=.3) + 
          theme_few() + theme(legend.position="bottom", legend.direction="vertical")+
          ylab("Density") +  
          xlab("Total Discharges")+
          ggtitle("Disease Level Comparison - Total Discharges") +
          theme(legend.title=element_text(size=10)) +
          guides(shape=guide_legend(override.aes=list(size=100))) 
        
      })
    
})

 