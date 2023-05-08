#
#
#
# 
#
#    
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(magrittr)
library(rvest)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)


#cv_cases = read.csv("new.csv")
cv_cases = read.csv("cv_case_world.csv")

current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_today = subset(cv_cases, date==current_date) 
cv_today_reduced = subset(cv_today, cases>=1000)

cv_cases$date = as.Date(cv_cases$date)
cv_min_date="2020-02-02"
#current_date = "2023-01-01"#as.Date(max(cv_cases$date),"%Y-%m-%d")
#cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
index_min_date=as.numeric(as.POSIXct(cv_min_date))
current_date = "2023-04-01"#as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

shinyUI(fluidPage(
        
        # Application title
        titlePanel("Covid 19"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        #sliderInput("bins","Number of bins:",min = 1,max = 50,value = 30),
                        pickerInput("region_select", "Country/Region:",   
                                    choices = as.character(cv_today_reduced$country), 
                                    options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                    selected = as.character(cv_today_reduced$country)[c(76,82)],
                                    multiple = TRUE), 
                        pickerInput("outcome_select", "Outcome:",   
                                    choices = c("Cases", "Deaths","Vaccinated"), 
                                    selected = c("Cases (total)"),
                                    multiple = FALSE),
                        sliderInput("minimum_date",
                                    "Minimum date:",
                                    #min = Sys.Date() - 90,
                                    #max = Sys.Date(),
                                    #value =  c(Sys.Date() - 30, Sys.Date())),
                                    min = as.Date(cv_min_date,"%Y-%m-%d"),
                                    max = as.Date(current_date,"%Y-%m-%d"),
                                    value=c(as.Date(cv_min_date),as.Date(current_date) ),
                                    timeFormat="%d %b"),
                        dateRangeInput(
                                "daterange",
                                "Input date range",
                                start = as.Date(cv_min_date),#Sys.Date() - 30,
                                end = as.Date(current_date)#Sys.Date()
                        )
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        tabsetPanel(
                                tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
                                tabPanel("New", plotlyOutput("country_plot")),
                                tabPanel("Similarity", plotlyOutput("country_plot_cumulative_log")),
                                tabPanel("Twoway", plotOutput("plot"))
                                
                        )
                )
        )
))













