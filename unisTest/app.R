#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(tidyr)
library(dplyr)
library(magrittr) 
library(ggplot2)
library(wordcloud2)
library(shiny)
# Define UI
ui <- fluidPage(
  titlePanel("College Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_selector", "Select Plot Type:",
                  choices = c("Histogram", "Scatterplot", "Wordcloud")),
      conditionalPanel(
        condition = "input.plot_selector == 'Histogram'",
        selectInput("hist_var", "Select Variable for Histogram:",
                    choices = c("Apps", "Accept", "Enroll", "Top10perc", 
                                "Top25perc", "Outstate", "Room.Board", "Personal",
                                "S.F.Ratio", "perc.alumni", "Grad.Rate")),
        sliderInput(inputId = "binslider", label = "Bin Width", min = 0, 
                    max = 100, value = 40),
      ),
      
      #---------------- Word Cloud UI ----------------------------------
      conditionalPanel(
        condition = "input.plot_selector == 'Wordcloud'",
        selectInput("cloud_var1", "Selection to Limit Word Cloud",
                    choices = c("Grad.Rate", "Accept", "Enroll", "Top10perc", 
                                "Top25perc", "Outstate", "Room.Board", "Personal",
                                "S.F.Ratio", "perc.alumni", "Grad.Rate")),
        sliderInput("cloud_var1amt", "Cloud Slider 1",
                    min = 0, max = 50, value = c(0,50)),
        selectInput("cloud_var2", "Selection to Limit Word Cloud:",
                    choices = c("Apps", "Accept", "Enroll", "Top10perc", 
                                "Top25perc", "Outstate", "Room.Board", "Personal",
                                "S.F.Ratio", "perc.alumni", "Grad.Rate")),
        sliderInput("cloud_var2amt", "Cloud slider 2",
                    min = 0, max = 50, value = c(0,50)),
        selectInput("cloud_view", "Select Variable to View in Word Cloud:",
                    choices = c("Apps", "Accept", "Enroll", "Top10perc", 
                                "Top25perc", "Outstate", "Room.Board", "Personal",
                                "S.F.Ratio", "perc.alumni", "Grad.Rate") )
        
      )
    ),
    mainPanel(
      uiOutput("output_plot")
    )
  )
)


unis <- read.csv("College_Data.csv")  %>%
  mutate(Enroll.Rate = (Enroll / Accept)*100) %>% 
  mutate(Accept.Rate = (Accept / Apps)*100)
# Define server logic
server <- function(input, output, session) {
  
  output$output_plot <- renderUI({
    if (input$plot_selector == "Histogram") {
      hist_output <- plotOutput("hist_plot")
      return(hist_output)
    } else if (input$plot_selector == "Scatterplot") {
      scatterplot_output <- plotOutput("scatterplot_plot")
      return(scatterplot_output)
    } else if (input$plot_selector == "Wordcloud") {
      wordcloud_output <- plotOutput("wordcloud_plot")
      return(wordcloud_output)
    }
  })
  
  # Render a histogram plot based on the selected variable
  output$hist_plot <- renderPlot({
    ggplot(unis, aes(x = .data[[input$hist_var]], fill = factor(Private))) +
      geom_histogram(binwidth = input$binslider, color = "black") +
      scale_fill_manual(values = c("skyblue", "salmon"), labels = c("Public", "Private")) +
      labs(title = paste("Histogram of", input$hist_var),
           x = input$hist_var)
  })
  
  # when selected different variable change bin width settings
  observeEvent(input$hist_var, {
    max_val <- max(unis[[input$hist_var]])
    # Update the maximum value of the slider
    updateSliderInput(session, "binslider", max = max_val, value = max_val/10)
  })
  
  
  # -------------------- Word cloud server -------------------------
  output$wordcloud_plot <- renderPlot({
    # Filter the dataset based on the range sliders
    filter_unis <- subset(unis, unis[[input$cloud_var1]] >= input$cloud_var1amt[1] &
                            unis[[input$cloud_var1]] <= input$cloud_var1amt[2] &
                            unis[[input$cloud_var2]] <= input$cloud_var2amt[2] &
                            unis[[input$cloud_var2]] >= input$cloud_var2amt[1])
    
    # Convert word frequencies to a data frame
    word_freq_df <- data.frame(word = filter_unis$X, 
                               freq = filter_unis[[input$cloud_view]], stringsAsFactors = FALSE)
    
    # Plot the word cloud
    wordcloud2(word_freq_df, size = 1)
  })
  
  observeEvent(input$cloud_var1, {
    max_val <- max(unis[[input$cloud_var1]])
    # Update the maximum value of the slider
    updateSliderInput(session, "cloud_var1amt", paste("Range for",input$cloud_var1), 
                      max = max_val, value = c(max_val/2, max_val))
  })
  observeEvent(input$cloud_var2, {
    max_val <- max(unis[[input$cloud_var2]])
    # Update the maximum value of the slider
    updateSliderInput(session, "cloud_var2amt", paste("Range for",input$cloud_var2),
                      max = max_val, value = c(0, max_val/2))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
