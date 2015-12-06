require(datasets)
library(dplyr)
library(maps)
library(ggplot2)
library(shiny)
library(ggvis)
library(ggthemes)

ip_tmp <-read.csv("data/Medicare_Charge_Inpatient_DRG100_DRG_Summary_by_DRGState_FY2013.csv", sep=",", fill=TRUE, header=TRUE)
lst.drgs <- as.character(unique(ip_tmp$DRG.Definition))
data(state.regions)
lst.regions<-as.character(unique(state.abb))
 
library(shiny)
dashboardPage(
  dashboardHeader(
    title = "Title and sidebar 350 pixels wide",
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      conditionalPanel(condition="input.conditionedPanels == 'Single Value Analysis'",
                       
                       selectInput("var", 
                                   label = h4("Choose Cost Measure:"), 
                                   choices = c("average_covered_charges_ip", 
                                               "average_total_payments_ip",
                                               "average_medicare_payments_ip",
                                               "total_discharges_ip")),
                       selectInput("drgvar", 
                                   label = h4("Choose DRG:"), 
                                   choices = lst.drgs)),
      
      conditionalPanel(condition="input.conditionedPanels =='Disease Comparisons'", 
                       selectizeInput(
                         'drgcompare', 'Select Diseases to Compare', choices = lst.drgs, multiple = TRUE, select ="039 - EXTRACRANIAL PROCEDURES W/O CC/MCC"
                       )),
      
      conditionalPanel(condition="input.conditionedPanels == 'Payment Comparison'",
                       selectizeInput(
                         'state', 'Select States to Compare', choices = lst.regions, multiple = TRUE, select = "NY"
                       )),
      
      conditionalPanel(condition="input.conditionedPanels == 'Map'",
                       
                       selectInput("var", 
                                   label = h4("Choose Cost Measure:"), 
                                   choices = c("average_covered_charges_ip", 
                                               "average_total_payments_ip",
                                               "average_medicare_payments_ip",
                                               "total_discharges_ip")),
                       selectInput("drgvar", 
                                   label = h4("Choose DRG:"), 
                                   choices = lst.drgs))
    )
  ),
  dashboardBody(
    tabsetPanel(navbarPage("CMS Payment Data",
                           
                           
                           tabPanel("Single Value Analysis", tabPanel("Histogram", plotOutput("histogram")),
                                    uiOutput('matrix')
                           ),
                           tabPanel("Disease Comparisons", 
                                    #plotOutput("histogramcompare"),
                                    tabPanel("Average Cost", plotOutput("histogramavgcov")), uiOutput('matrix1'),
                                    tabPanel("Average Total Payment", plotOutput("histogramavgtot")), uiOutput('matrix2'),
                                    tabPanel("Average Medicare Payments", plotOutput("histogramavgmed")), uiOutput('matrix3'),
                                    tabPanel("Total Discharges", plotOutput("histogramtotdis")), uiOutput('matrix4'),
                                    tabPanel("Total Discharges Descriptives", tableOutput("summarytable"))),
                           tabPanel("Payment Comparison", plotOutput("scatterplot"),
                                    tabPanel("Total Payments v Total Discharges", plotOutput("scatterplot2"))),
                           tabPanel("Map",  htmlOutput("map2") 
                           ),
                           
                           
                           
                           id = "conditionedPanels"
    )
  )
  )
)


