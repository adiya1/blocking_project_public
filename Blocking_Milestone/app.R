#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Blocking Milestone"),
   
   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
 #     sidebarPanel(
  #       sliderInput("bins",
   #                  "Number of bins:",
     #                min = 1,
    #                 max = 50,
      #               value = 30)
    #  ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
#)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
 #     x    <- faithful[, 2] 
  #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
   #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
    edited_students <- read_csv("edited_students_shiny.csv")
     
      edited_students %>% 
       na.omit(gender) %>%  
       # not considering empty rows in the gender column in this graph to observe the distribution of athletes and genders more easily
       mutate(athlete = as.character(athlete)) %>% 
       mutate(athlete = recode(athlete, "1" ="Athlete", "0" = "Non-Athlete")) %>% 
       ggplot(aes(x = athlete, fill = gender)) +
       # going to observe the comparison of athletes vs non-athletes and as well as that observe the gender distribution in these two groups
       geom_bar() +
       # using a bar graph to easily observe the comparison of the two groups
       scale_fill_manual("Gender", values = c("Male" = "blue", "Female" = "Red", "Other" = "green")) +
       # setting the colors for different genders to fill and assigning the legend label to be Gender
       labs(title = "Distribution of First-Year Student Athletes Depending on Gender",
            subtitle = "According to the survey, there are more female athletes than male athletes",
            caption = "Source: Blocking Project Survey") +
       xlab("Athlete Status") +
       ylab("Number of Students")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

