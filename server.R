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

if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

library(dtw)

#cv_cases = read.csv("new.csv")
cv_cases = read.csv("cv_case_world.csv")


current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_today = subset(cv_cases, date==current_date) 
cv_today_reduced = subset(cv_today, cases>=1000)

cv_cases$date = as.Date(cv_cases$date)
cv_min_date="2021-02-02"
current_date = "2023-01-01"#as.Date(max(cv_cases$date),"%Y-%m-%d")
#cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
index_min_date=as.numeric(as.POSIXct(cv_min_date))
current_date = "2023-01-01"#as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

########

# function to plot new cases by region
country_cases_plot = function(cv_cases, plot_start_date, plot_end_date) {
        #g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, group = 1,
        g = ggplot(cv_cases, aes(x = date, y = new_outcome, colour = region, group = 1,
                                 #text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome)
        )) + 
                xlim(c(plot_start_date,plot_end_date+5)) + xlab("Date")
        g1 = g + geom_line(alpha=0.5) +
                #geom_bar(position="stack", stat="identity") + 
                ylab("New (weekly)") + theme_bw()
}

#function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, plot_start_date, plot_end_date) {
        g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                                 text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
                xlim(c(plot_start_date,plot_end_date+1)) + xlab("Date")
        g1 = g + geom_line(alpha=0.8) + #geom_point(size = 1, alpha = 0.8) +
                ylab("Cumulative") + theme_bw() + 
                #scale_colour_manual(values=country_cols) +
                theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
        ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}




country_cases_cumulative_log = function(cv_cases, start_d,end_d)  {
        my_range <- unique(cv_cases$region)
        index_start_d=as.integer(match(start_d,cv_cases$date)/2)
        index_end_d=as.integer(match(end_d,cv_cases$date)/2-5)
        #print(index_start_d)
        #print(index_end_d)
        x=subset(cv_cases,country==my_range[1])$outcome[index_start_d:index_end_d]
        #print(length(x))
        y=subset(cv_cases,country==my_range[2])$outcome[index_start_d:index_end_d]
        #print(start_d)
        xy=dtw(x, y,keep = T)
        #print(y)
        #print(xy$M)
        max_arr=xy$M-2#length(xy$query)-2
        #print(max_arr)
        aa=dtw(x, y)$distance
        #aa=integer(aa)
        data <- data.frame(x=xy$query[0:max_arr,1],y = xy$reference[0:max_arr,1])
        g = ggplot(data, aes(x = x, y = y))+ggtitle(sprintf("The DTW distance between time series: %.5e", aa))+ 
                xlab(my_range[1])+ ylab(my_range[2])
        g1 = g + geom_line(alpha=0.8) 
}
country_dtw_two_way = function(cv_cases, start_d,end_d)  {
        my_range <- unique(cv_cases$region)
        index_start_d=as.integer(match(start_d,cv_cases$date)/2)
        index_end_d=as.integer(match(end_d,cv_cases$date)/2-5)
        x=subset(cv_cases,country==my_range[1])$outcome[index_start_d:index_end_d]
        y=subset(cv_cases,country==my_range[2])$outcome[index_start_d:index_end_d]
        xy=dtw(x, y,keep = T)
        dtwPlotTwoWay(xy,xlab=as.character(my_range[1]), ylab=as.character(my_range[2]))  
        #plot(x,y)
}

shinyServer(function(input, output, session) {
        country_reactive_db = reactive({
                db = cv_cases
                db$region = db$country
                
                #db$outcome = db$cases
                #db$new_outcome = db$new_cases
                
                if (input$outcome_select=="Cases") { 
                        db$outcome = db$cases
                        db$new_outcome = db$new_cases
                }
                
                if (input$outcome_select=="Deaths") { 
                        db$outcome = db$deaths 
                        db$new_outcome = db$new_deaths 
                }
                
                if (input$outcome_select=="Vaccinated") { 
                        db$outcome = db$vaccinations 
                        db$new_outcome = db$new_vaccinations 
                }
                #"vacinations","fully_vaccinated"
                
                db %>% filter(region %in% input$region_select)
        })
        
        ## Avoid chain reaction
        reactdelay <- 1
        change_slider <- reactiveVal(Sys.time())
        change_daterange <- reactiveVal(Sys.time())
        
        observeEvent(input$minimum_date, {
                if (difftime(Sys.time(), change_slider()) > reactdelay) {
                        change_daterange(Sys.time())
                        updateDateRangeInput(session,
                                             "daterange",
                                             start = input$minimum_date[[1]],
                                             end = input$minimum_date[[2]])
                }
        })
        observeEvent(input$daterange, {
                if (difftime(Sys.time(), change_daterange()) > reactdelay) {
                        change_slider(Sys.time())
                        updateSliderInput(session,
                                          "minimum_date",
                                          value = c(input$daterange[[1]], input$daterange[[2]]))
                }
        })
        
        # country-specific plots
        output$country_plot_cumulative <- renderPlotly({
                country_cases_cumulative(country_reactive_db(), input$daterange[[1]], input$daterange[[2]])
        })
        # country-specific plots
        output$country_plot <- renderPlotly({
                country_cases_plot(country_reactive_db(), input$daterange[[1]], input$daterange[[2]])
        })
        output$country_plot_cumulative_log <- renderPlotly({
                country_cases_cumulative_log(country_reactive_db(), input$daterange[[1]], input$daterange[[2]])
                #country_cases_cumulative_log(country_reactive_db())
        })
        output$plot <- renderPlot({
                country_dtw_two_way(country_reactive_db(), input$daterange[[1]], input$daterange[[2]])
        }) 
        
})