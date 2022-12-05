library(shiny)
library(shinythemes)
library(dplyr)
library(nycflights13)
library(ggplot2)
library(sjmisc)
library(readxl)
library(tidyverse)
library(tidyr)

my_data_17 = read_excel("./data/state_M2017_dl.xlsx")
states_17 = my_data_17 %>%
  group_by(STATE, ST) %>%
  summarise("17" = mean(as.numeric(H_MEAN), na.rm = T))

my_data_18 = read_excel("./data/state_M2018_dl.xlsx")
states_18 = my_data_18 %>%
  group_by(STATE) %>%
  summarise("18" = mean(as.numeric(H_MEAN), na.rm = T))

my_data_19 = read_excel("./data/state_M2019_dl.xlsx")
states_19 = my_data_19 %>%
  group_by(area_title) %>%
  summarise("19" = mean(as.numeric(h_mean), na.rm = T))

my_data_20 = read_excel("./data/state_M2020_dl.xlsx")
states_20 = my_data_20 %>%
  group_by(AREA_TITLE) %>%
  summarise("20" = mean(as.numeric(H_MEAN), na.rm = T))

my_data_21 = read_excel("./data/state_M2021_dl.xlsx")
states_21 = my_data_21 %>%
  group_by(AREA_TITLE) %>%
  summarise("21" = mean(as.numeric(H_MEAN), na.rm = T))

state_loc = read.csv("./data/statelatlong.csv")


result = states_17 %>%
  left_join(states_18, c("STATE" = "STATE")) %>%
  left_join(states_19, c("STATE" = "area_title")) %>%
  left_join(states_20, c("STATE" = "AREA_TITLE")) %>%
  left_join(states_21, c("STATE" = "AREA_TITLE")) %>%
  left_join(state_loc, c("ST" = "State"))

year_mean = data.frame(year = c("17","18","19","20","21"), 
                       mean_y = c(mean(result$"17"), mean(result$"18"), mean(result$"19"), mean(result$"20"), mean(result$"21")))

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "Wage",
    tabPanel("U.S.",
             sidebarPanel(
               tags$h3("Input:"),
               sliderInput(inputId = "bins",
                           label = "which year in 2000:",
                           min = 17,
                           max = 21,
                           value = 21),
               selectInput("select", h3("Select box"), 
                           choices = list("Map" = 1, "Graph" = 2), selected = 1)
               
             ), # sidebarPanel
             mainPanel(
               h3("Average hourly wage in 54 regions across the United States"),
               
               plotOutput(outputId = "distPlot")
               
             ) # mainPanel
             
    ), # Navbar 1, tabPanel
    tabPanel("States", 
             sidebarPanel(
               selectInput("select2", h3("Select Region"),
                           choices = (list("Alabama"  = 1, "Alaska" = 2, "Arizona" = 3, "Arkansas" = 4, "California" = 5, "Colorado" = 6,
                                           "Connecticut" = 7, "Delaware" = 8, "District of Columbia" = 9, "Florida" = 10,  "Georgia" = 11,
                                           "Guam" = 12, "Hawaii" = 13, "Idaho" = 14, "Illinois"  = 15, "Indiana" = 16, "Iowa" = 17, "Kansas" = 18,
                                           "Kentucky" = 19, "Louisiana"  = 20, "Maine" = 21, "Maryland" = 22, "Massachusetts"  = 23,
                                           "Michigan"  = 24,           "Minnesota"    = 25,        "Mississippi"  = 26,
                                           "Missouri"   = 27,          "Montana"   = 28,           "Nebraska"    = 29,         "Nevada"   = 30,            "New Hampshire" = 31,
                                           "New Jersey"   = 32,        "New Mexico"    = 33,       "New York"   = 34,          "North Carolina" = 35,      "North Dakota" = 36,
                                           "Ohio"   = 37,              "Oklahoma"   = 38,          "Oregon"  = 39,           "Pennsylvania"  = 40, "Puerto Rico" = 41,      "Rhode Island" = 42,
                                           "South Carolina"  = 43,     "South Dakota"   = 44,      "Tennessee"    = 45,        "Texas"     = 46,           "Utah"  = 47,
                                           "Vermont"       = 48,  "Virgin Islands" = 49,     "Virginia"    = 50,         "Washington"    = 51,       "West Virginia"  = 52,      "Wisconsin"   = 53,
                                           "Wyoming" = 54)), selected = 53)
             ),
             mainPanel(
               h3("Average hourly wage in "),

               plotOutput(outputId = "distPlot2")
             )
             )
  )
)

server <- function(input, output) {
    output$distPlot <- renderPlot({
    
    if (input$select == 1){
    
      temp = result%>%
        select(Longitude, Latitude, mean = as.character(input$bins))
      temp %>%
        ggplot(aes(Longitude, Latitude, color = mean, size = mean)) +
        scale_color_gradient(low="blue", high="red") +
        borders("state") +
        coord_map("conic") +
        geom_point() +
        coord_quickmap() +
        xlab(paste("mean wage in 20", as.character(input$bins), sep="")) +
        ylab("mean wage") +
        ggtitle(paste("National average hourly wage is ", mean(temp$mean)))#input$select))
      
    } else{
      a1=result %>%
        select(ST, STATE, mean = as.character(input$bins)) %>%
        arrange(desc(mean)) 
      a1%>%
        ggplot(aes(x = factor(STATE, level = a1$STATE), y = mean)) +
        geom_col() +
        geom_hline(yintercept=mean(a1$mean), linetype="dashed", color = "red") +
        xlab("Regions") +
        theme(axis.text.x = element_text(angle = 60, hjust=1)) +
        xlab(paste("mean wage in 20", as.character(input$bins), sep="")) +
        ggtitle(paste("National average hourly wage is ", mean(a1$mean)))
    }
    
  })
  output$distPlot2 <- renderPlot({
    temp_state = result$STATE[as.numeric(input$select2)]
    a2 = result[result$STATE == temp_state,] %>%
      pivot_longer(cols = 3:7, names_to = "year", values_to = "mean") %>%
      left_join(year_mean, by = ("year" = "year")) %>%
      mutate(year = as.numeric(paste("20",year, sep="")))
  
    ggplot(a2,aes(x = year)) +
      geom_line(aes(y = mean, color="region mean")) +
      geom_line(aes(y = mean_y, color = "US mean") , linetype="twodash") +
      scale_color_manual(values=c("region mean" = "blue", "US mean"="red")) +
      ggtitle(paste("Average hourly wage in ", temp_state))
    })
  
}

shinyApp(ui = ui, server = server)
