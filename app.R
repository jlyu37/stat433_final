library(shiny)
library(shinythemes)
library(dplyr)
library(nycflights13)
library(ggplot2)
library(sjmisc)
library(readxl)
library(tidyverse)
library(tidyr)
library(usmap)

my_data_17 = read_excel("./data/state_M2017_dl.xlsx") %>%
  mutate(TOT_EMP=gsub("\\*+", "0",TOT_EMP), H_MEAN=gsub("\\*+", "0",H_MEAN)) %>%
  mutate(H_MEAN=gsub("#+", "0",H_MEAN)) %>%
  transform(TOT_EMP = as.numeric(TOT_EMP), H_MEAN = as.numeric(H_MEAN))
my_data_17_d = my_data_17%>%
  filter(OCC_GROUP=="detailed") 
my_data_17_m = my_data_17%>%
  filter(OCC_GROUP=="major") 
states_17 = my_data_17_d %>%
  group_by(STATE) %>%
  summarise("17" = sum(TOT_EMP*H_MEAN)/sum(TOT_EMP[H_MEAN!=0]))
major_17 = my_data_17_m%>%
  group_by(OCC_TITLE) %>%
  summarise("17" = sum(TOT_EMP*H_MEAN)/sum(TOT_EMP[H_MEAN!=0]))
mean_17 = sum(my_data_17_m$TOT_EMP*my_data_17_m$H_MEAN)/sum(my_data_17_m$TOT_EMP)
  

my_data_18 = read_excel("./data/state_M2018_dl.xlsx") %>%
  mutate(TOT_EMP=gsub("\\*+", "0",TOT_EMP), H_MEAN=gsub("\\*+", "0",H_MEAN)) %>%
  mutate(H_MEAN=gsub("#+", "0",H_MEAN)) %>%
  transform(TOT_EMP = as.numeric(TOT_EMP), H_MEAN = as.numeric(H_MEAN))
my_data_18_d = my_data_18%>%
  filter(OCC_GROUP=="detailed") 
my_data_18_m = my_data_18%>%
  filter(OCC_GROUP=="major") 
states_18 = my_data_18_d %>%
  group_by(STATE) %>%
  summarise("18" = sum(TOT_EMP*H_MEAN)/sum(TOT_EMP[H_MEAN!=0]))
major_18 = my_data_18_m%>%
  group_by(OCC_TITLE) %>%
  summarise("18" = sum(TOT_EMP*H_MEAN)/sum(TOT_EMP[H_MEAN!=0]))
mean_18 = sum(my_data_18_m$TOT_EMP*my_data_18_m$H_MEAN)/sum(my_data_18_m$TOT_EMP)

my_data_19 = read_excel("./data/state_M2019_dl.xlsx")%>%
  mutate(tot_emp=gsub("\\*+", "0",tot_emp), h_mean=gsub("\\*+", "0",h_mean)) %>%
  mutate(h_mean=gsub("#+", "0",h_mean)) %>%
  transform(tot_emp = as.numeric(tot_emp), h_mean = as.numeric(h_mean))
my_data_19_d = my_data_19%>%
  filter(o_group=="detailed") 
my_data_19_m = my_data_19%>%
  filter(o_group=="major") 
states_19 = my_data_19_d %>%
  group_by(area_title) %>%
  summarise("19" = sum(tot_emp*h_mean)/sum(tot_emp[h_mean!=0]))
major_19 = my_data_19_m%>%
  group_by(occ_title) %>%
  summarise("19" = sum(tot_emp*h_mean)/sum(tot_emp[h_mean!=0]))
mean_19 = sum(my_data_19_m$tot_emp*my_data_19_m$h_mean)/sum(my_data_19_m$tot_emp)

my_data_20 = read_excel("./data/state_M2020_dl.xlsx")%>%
  mutate(TOT_EMP=gsub("\\*+", "0",TOT_EMP), H_MEAN=gsub("\\*+", "0",H_MEAN)) %>%
  mutate(H_MEAN=gsub("#+", "0",H_MEAN)) %>%
  transform(TOT_EMP = as.numeric(TOT_EMP), H_MEAN = as.numeric(H_MEAN))
my_data_20_d = my_data_20%>%
  filter(O_GROUP=="detailed") 
my_data_20_m = my_data_20%>%
  filter(O_GROUP=="major") 
states_20 = my_data_20_d %>%
  group_by(AREA_TITLE) %>%
  summarise("20" = sum(TOT_EMP*H_MEAN)/sum(TOT_EMP[H_MEAN!=0]))
major_20 = my_data_20_m%>%
  group_by(OCC_TITLE) %>%
  summarise("20" = sum(TOT_EMP*H_MEAN)/sum(TOT_EMP[H_MEAN!=0]))
mean_20 = sum(my_data_20_m$TOT_EMP*my_data_20_m$H_MEAN)/sum(my_data_20_m$TOT_EMP)
  
my_data_21 = read_excel("./data/state_M2021_dl.xlsx")%>%
  mutate(TOT_EMP=gsub("\\*+", "0",TOT_EMP), H_MEAN=gsub("\\*+", "0",H_MEAN)) %>%
  mutate(H_MEAN=gsub("#+", "0",H_MEAN)) %>%
  transform(TOT_EMP = as.numeric(TOT_EMP), H_MEAN = as.numeric(H_MEAN))
my_data_21_d = my_data_21%>%
  filter(O_GROUP=="detailed") 
my_data_21_m = my_data_21%>%
  filter(O_GROUP=="major") 
states_21 = my_data_21_d %>%
  group_by(AREA_TITLE) %>%
  summarise("21" = sum(TOT_EMP*H_MEAN)/sum(TOT_EMP[H_MEAN!=0]))
major_21 = my_data_21_m%>%
  group_by(OCC_TITLE) %>%
  summarise("21" = sum(TOT_EMP*H_MEAN)/sum(TOT_EMP[H_MEAN!=0]))
mean_21 = sum(my_data_21_m$TOT_EMP*my_data_21_m$H_MEAN)/sum(my_data_21_m$TOT_EMP)

state_loc = read.csv("./data/statelatlong.csv")

year_mean <- function(x) {
  if (x == 17){
    return(mean_17)
  }else if (x == 18){
    return(mean_18)
  }else if (x == 19){
    return(mean_19)
  }else if (x == 20){
    return(mean_20)
  }else if (x == 21){
    return(mean_21)
  }
}

result = states_17 %>%
  left_join(states_18, c("STATE" = "STATE")) %>%
  left_join(states_19, c("STATE" = "area_title")) %>%
  left_join(states_20, c("STATE" = "AREA_TITLE")) %>%
  left_join(states_21, c("STATE" = "AREA_TITLE")) %>%
  left_join(state_loc, c("STATE" = "City")) %>%
  mutate(state = State)

result2 = major_17 %>%
  full_join(major_18, c("OCC_TITLE" = "OCC_TITLE")) %>%
  full_join(major_19, c("OCC_TITLE" = "occ_title")) %>%
  full_join(major_20, c("OCC_TITLE" = "OCC_TITLE")) %>%
  full_join(major_21, c("OCC_TITLE" = "OCC_TITLE"))


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "Wage",
    tabPanel("States & Major",
             titlePanel("Selecting a major in a fixed state"),
             sidebarPanel(
               tags$h3("Input:"),
               sliderInput(inputId = "bins",
                           label = "which year in 2000:",
                           min = 17,
                           max = 21,
                           value = 21),
               selectInput("select_s", h3("Select Region"),
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
               h3("Weighted average hourly wage in 54 regions across the United States"),
               
               tabsetPanel(
                 tabPanel(
                   title = "US Mapping",
                   plotOutput(outputId = "distPlot")
                 ),
                 tabPanel(
                   title = "US Graph",
                  #  sidebarPanel(
                  #    selectInput("select", h3("Select box"), choices = list("Map" = 1, "Graph" = 2), selected = 1)
                  #  ),
                   plotOutput(outputId = "distPlot3")
                 ),
                 tabPanel(
                   title = "State Major",
                   #  sidebarPanel(
                   #    selectInput("select", h3("Select box"), choices = list("Map" = 1, "Graph" = 2), selected = 1)
                   #  ),
                   #sidebarPanel(
                   
                   #)
                   plotOutput(outputId = "distPlot4")
                 ),
                 tabPanel(
                   title = "State detail",
                   #  sidebarPanel(
                   #    selectInput("select", h3("Select box"), choices = list("Map" = 1, "Graph" = 2), selected = 1)
                   #  ),
                   #sidebarPanel(
                   
                   #)
                   plotOutput(outputId = "distPlot7")
                 )
               )
             ) # mainPanel
             
    ), # Navbar 1, tabPanel
    tabPanel("Major & States",
             titlePanel("Selecting a state in a fixed major"),
             sidebarPanel(
               tags$h3("Input:"),
               sliderInput(inputId = "bins1",
                           label = "which year in 2000:",
                           min = 17,
                           max = 21,
                           value = 21),
               selectInput("select_m", h3("Select Majors"),
                           choices = (list("Architecture and Engineering Occupations"  = 1, "Arts, Design, Entertainment, Sports, and Media Occupations" = 2, "Building and Grounds Cleaning and Maintenance Occupations" = 3, "Business and Financial Operations Occupations" = 4, "Community and Social Service Occupations" = 5, "Computer and Mathematical Occupations" = 6,
                                           "Construction and Extraction Occupations" = 7, "Education, Training, and Library Occupations" = 8, "Farming, Fishing, and Forestry Occupations" = 9, "Food Preparation and Serving Related Occupations" = 10,  "Healthcare Practitioners and Technical Occupations" = 11,
                                           "Healthcare Support Occupations" = 12, "Installation, Maintenance, and Repair Occupations" = 13, "Legal Occupations" = 14, "Life, Physical, and Social Science Occupations"  = 15, "Management Occupations" = 16, "Office and Administrative Support Occupations" = 17, "Personal Care and Service Occupations" = 18,
                                           "Production Occupations" = 19, "Protective Service Occupations"  = 20, "Sales and Related Occupations" = 21, "Transportation and Material Moving Occupations" = 22, "Educational Instruction and Library Occupations"  = 23)), selected = 1)
             ),
             mainPanel(
               h3("Weighted average hourly wage in different Majors"),

               tabsetPanel(
                 tabPanel(
                   title = "US",
                   plotOutput(outputId = "distPlot5")
                 ),
                 tabPanel(
                   title = "State",
                   #  sidebarPanel(
                   #    selectInput("select", h3("Select box"), choices = list("Map" = 1, "Graph" = 2), selected = 1)
                   #  ),
                   plotOutput(outputId = "distPlot6")
                 )
               )
               
             )
      )
  )
)

server <- function(input, output) {
    output$distPlot <- renderPlot({
    
    #if (input$select == 1){
    
       temp = result%>%
        select(state, mean = as.character(input$bins))
       mean_y = year_mean(input$bins)
      # temp %>%
      #   ggplot(aes(Longitude, Latitude, color = mean, size = mean)) +
      #   scale_color_gradient(low="blue", high="red") +
      #   borders("state") +
      #   coord_map("conic") +
      #   geom_point() +
      #   coord_quickmap() +
      #   xlab(paste("mean wage in 20", as.character(input$bins), sep="")) +
      #   ylab("mean wage") +
      #   ggtitle(paste("National average hourly wage is ", mean(temp$mean)))#input$select))
      
      plot_usmap(data = result, values = as.character(input$bins), color = "blue") +
        scale_fill_continuous(low = "white", high = "blue", name = "Weighted average of hourly wage") +
        labs(title = paste("mean wage in 20", as.character(input$bins), sep=""), subtitle = paste("National average hourly wage is ", mean_y)) +
        theme(legend.position = "right")
      
    #} else{
      # a1=result %>%
      #   select(ST, STATE, mean = as.character(input$bins)) %>%
      #   arrange(desc(mean)) 
      # a1%>%
      #   ggplot(aes(x = factor(STATE, level = a1$STATE), y = mean)) +
      #   geom_col() +
      #   geom_hline(yintercept=mean(a1$mean), linetype="dashed", color = "red") +
      #   xlab("Regions") +
      #   theme(axis.text.x = element_text(angle = 60, hjust=1)) +
      #   xlab(paste("mean wage in 20", as.character(input$bins), sep="")) +
      #   ggtitle(paste("National average hourly wage is ", mean(a1$mean)))
    #}
    
  })

  output$distPlot3 <- renderPlot({
    mean_y = year_mean(input$bins)
    
     a1=result %>%
        select(State, STATE, mean = as.character(input$bins)) %>%
        arrange(desc(mean)) 
      a1%>%
        ggplot(aes(x = factor(STATE, level = STATE), y = mean)) +
        geom_col() +
        geom_hline(yintercept=mean_y, linetype="dashed", color = "red") +
        xlab("Regions") +
        theme(axis.text.x = element_text(angle = 60, hjust=1)) +
        labs(title = paste("National average hourly wage is", mean_y), 
             subtitle = paste("mean wage in 20", as.character(input$bins), sep=""))
  })
  
  output$distPlot4 <- renderPlot({
    temp_state = result$STATE[as.numeric(input$select_s)]
    temp_y = input$bins
    temp = my_data_17_m %>%
      filter(STATE == temp_state)
    if (temp_y == 18){
      temp = my_data_18_m %>%
        filter(STATE == temp_state)
    } else if (temp_y == 19){
      temp = my_data_19_m %>%
        filter(area_title == temp_state) %>%
        rename(OCC_TITLE = occ_title, H_MEAN = h_mean)
    } else if (temp_y == 20){
      temp = my_data_20_m %>%
        filter(AREA_TITLE == temp_state)
    } else if (temp_y == 21){
      temp = my_data_21_m %>%
        filter(AREA_TITLE == temp_state)
    }
    mean_y = year_mean(input$bins)
    
    temp = temp %>%
      arrange(desc(H_MEAN)) 
    temp%>%
      ggplot(aes(x = factor(OCC_TITLE, level = OCC_TITLE), y = H_MEAN)) +
      geom_col()+
      geom_hline(yintercept=mean_y, linetype="dashed", color = "red") +
      theme(axis.text.x = element_text(angle = 60, hjust=1)) +
      ggtitle(paste("Average hourly wage in", temp_state)) +
      xlab("Different major") +
      ylab("Average hour wage")
  })
  output$distPlot5 <- renderPlot({
    mean_y = year_mean(input$bins1)
    
    a1=result2 %>%
      select(OCC_TITLE, mean = as.character(input$bins1)) %>%
      arrange(desc(mean)) 
    a1%>%
      ggplot(aes(x = factor(OCC_TITLE, level = OCC_TITLE), y = mean)) +
      geom_col() +
      geom_hline(yintercept=mean_y, linetype="dashed", color = "red") +
      xlab("Majors") +
      theme(axis.text.x = element_text(angle = 60, hjust=1)) +
      labs(title = paste("National average hourly wage is", mean(a1$mean)), 
           subtitle = paste("mean wage in 20", as.character(input$bins1), sep=""))
  })
  
  output$distPlot6 <- renderPlot({
    mean_y = year_mean(input$bins1)
    temp_y = input$bins1
    temp_major = result2$OCC_TITLE[as.numeric(input$select_m)]
    #h3(temp_major)
    temp = my_data_17_m %>%
      filter(OCC_TITLE == temp_major)
     if (temp_y == 18){
       temp = my_data_18_m %>%
         filter(OCC_TITLE == temp_major)
     
     } else if (temp_y == 19){
       temp = my_data_19_m %>%
         rename(OCC_TITLE = occ_title, H_MEAN = h_mean, STATE = area_title) %>%
         filter(OCC_TITLE == temp_major)
    } else if (temp_y == 20){
      temp = my_data_20_m %>%
        filter(OCC_TITLE == temp_major)%>%
        rename(STATE = AREA_TITLE)
    } else if (temp_y == 21){
      temp = my_data_21_m %>%
        filter(OCC_TITLE == temp_major)%>%
        rename(STATE = AREA_TITLE)
    }
    temp = temp %>%
      arrange(desc(H_MEAN)) 
    temp%>%
      ggplot(aes(x = factor(STATE, level = STATE), y = H_MEAN)) +
      geom_col()+
      geom_hline(yintercept=mean_y, linetype="dashed", color = "red") +
      theme(axis.text.x = element_text(angle = 60, hjust=1)) +
      labs(title = paste("Average hourly wage in", temp_major), subtitle = paste("in 20", temp_y, sep = "")) +
      xlab("Different States") +
      ylab("Average hour wage")
  })
  output$distPlot7 <- renderPlot({
    temp_state = result$STATE[as.numeric(input$select_s)]
    temp_y = input$bins
    temp = my_data_17_d %>%
      filter(STATE == temp_state)
    if (temp_y == 18){
      temp = my_data_18_d %>%
        filter(STATE == temp_state)
    } else if (temp_y == 19){
      temp = my_data_19_d %>%
        filter(area_title == temp_state) %>%
        rename(OCC_TITLE = occ_title, H_MEAN = h_mean, TOT_EMP = tot_emp)
    } else if (temp_y == 20){
      temp = my_data_20_d %>%
        filter(AREA_TITLE == temp_state)
    } else if (temp_y == 21){
      temp = my_data_21_d %>%
        filter(AREA_TITLE == temp_state)
    }
    mean_y = year_mean(input$bins)
    
    temp %>%
      ggplot() +
      geom_vline(xintercept = mean_y, linetype = "dotted", color = "red") +
      geom_step(aes(x = H_MEAN, y =TOT_EMP)) +
      labs(title = paste("Average hourly wage in", temp_state), subtitle = paste("in 20", temp_y, sep = "")) +
      ylab("Total emplement number") +
      xlab("Average hour wage")
  })
}

shinyApp(ui = ui, server = server)
