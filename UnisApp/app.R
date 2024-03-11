#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(plotly)
library(tidyr)
library(dplyr)
library(magrittr) 
library(ggplot2)
library(wordcloud2)
library(shiny)
library(shinythemes)


# Define UI
ui <- navbarPage(
  theme = shinytheme("journal"),
  title = "College Explorer",
  tabsetPanel(
    # First Nav Bar option displaying project overview and meaningful vis
    tabPanel("Project Overview",
             tags$style(HTML("
               .project-overview-p {
                 margin-left: 100px;
                 margin-right: 100px;
                 margin-top:40px;
                 font-size:20px;
               }
               .images {
                  display: block;
                  margin-left:auto;
                  margin-right:auto;
                  height:400px;
               }
               .column {
                  float: left;
                  width: 50%;
                  padding: 5px;
              }

              .row::after {
                  content: '';
                  clear: both;
                  display: table;
              }
             ")),
             tags$h1("Private vs Public Colleges"),
             tags$p(class = "project-overview-p", 
             "The goal of this project is to see differences 
             between private and public colleges and what these differences 
             are through quantitative data. Expected users of this app include 
             students, parents, and school counselors looking at colleges with 
             certain characteristics and performances. The dataset being used 
             comes from kaggle being a data set from 2019 on 777 colleges in the 
             US. The variables found in the data set include:"),
             tags$ul(class = "project-overview-p",
               tags$li("X - College/University Name"),
               tags$li("Private - Yes or no on whether the school is private"),
               tags$li("Apps - Number of applications"),
               tags$li("Accept - Number of acceptances"),
               tags$li("Enroll - Number of new enrollments"),
               tags$li("Top10perc - Number of new students in top 10% in HS"),
               tags$li("Top25perc - Number of new students in top 25% in HS"),
               tags$li("F.Undergrad - Number of fulltime students"),
               tags$li("P.Undergrad - Number of parttime students"),
               tags$li("OutState - Out of state tuition"),
               tags$li("Room.Board - Room and board cost"),
               tags$li("Books - Estimated book costs"),
               tags$li("Personal - Estimated personal spending"),
               tags$li("PHD - Percentage of faculty with PHDs"),
               tags$li("Terminal - Percentage of faculty with Terminal degrees"),
               tags$li("S.F.Ratio - Student to faculty ratio"),
               tags$li("perc.alumni - Percentage of alumni who donate"),
               tags$li("Expend - Instructional expenditure per student"),
               tags$li("Grad.Rate - Graduation rate"),
             ),
             tags$p(class = "project-overview-p",
             "Key questions the app seeks to answer include differences in 
                    enrollment and acceptance rates between private and public 
                    colleges, correlations between variables like application count  
                    and graduation rates, through insights from visualizations including 
                    histograms, scatter plots, bar charts, and word clouds."),
             tags$img(class = "images", src ="PercAlumHist.png", alt="Percent Alum Histogram"),
             tags$p(class="project-overview-p",
                    "Through displaying the data is clear to see that within the 
                    histogram above private colleges tend to have more alumni 
                    donate back to their college having the peak frequency 
                    centered at 28%, while public colleges have a peak frequency 
                    at 14%. It interesting to see that private colleges have a 
                    higher percentage of alumni contribute and give back to their 
                    schooling. There are many different reasons this could be the 
                    case, but through visualization we can see a possible 
                    correlation."),
             tags$img(class = "images", src ="AlumSFScatter.png", alt="Percent Alum Histogram"),
             tags$p(class = "project-overview-p",
                    "With this scatter plot displaying the percentage of alumni 
                    donating to the college versus student to faculty ratio there 
                    is a clear implied line found as the percentage increases the 
                    ratio of students to faculty tends to decrease. As such there 
                    is an inverse correlation between the two. Commonly when the 
                    amount of faculty is closer to the student count the amount of 
                    alumni donating should also increase."),
             tags$img(class= "images", src="GradRateVPercAlumScatter.png"),
             tags$p(class = "project-overview-p",
                    "Similarly we can see when comparing the graduation rates 
                    of colleges vs the percentage of alumni donating to each 
                    school, public colleges tended to have a more horizontal 
                    correlation. Private colleges tend to have a positive trend 
                    where the higher the graduation rate is the higher the 
                    percentage of alumni can be. It is interesting to notice 
                    that very few colleges fall to the left of this line and 
                    alumni don't tend to give donations to private colleges with 
                    lower graduation rates."),
             div(class="row",
                 div(class="column",
             tags$img(class= "images", src="AcceptRateHist.png")),
                 div(class="column",
             tags$img(class= "images", src="AcceptGradRateScatter.png"))),
             tags$p(class = "project-overview-p",
                    "Looking at acceptance rates of colleges private colleges 
                    have a peak with an acceptance rate of 84% while public 
                    colleges have a peak at 75% found in the histogram above. 
                    This correlation tied with the graduation rates of colleges 
                    displays that private colleges with higher acceptance rates 
                    have a large array of graduation rates, but private colleges 
                    with lower acceptance rates tend to have higher graduation 
                    rates."),
             tags$p(class = "project-overview-p",
                    "Some improvements that could be made for the future include 
                    using updateSliderInput to display slider ranges for each 
                    variable for the word cloud. There was a conflict between 
                    outputting the word cloud and updating the slider input that 
                    couldn't be resolved so ultimately it made the overall 
                    experience more difficult to understand while using the 
                    wordcloud.The data displayed is the only available info displayed 
                    which could be more helpful knowing the college as well. Another 
                    issue is legend making when using ggplotly I would've liked 
                    to fix how the legend is made and working legends stop 
                    working when switching from plotOutput to plotlyOutput."),
             tags$h3("References"),
             tags$p(class="references",
                    "Munzner, Tamara. Visualization Analysis & Design. Boca Raton, FL: CRC Press/Taylor & Francis Group, 2015."),
             tags$p(class="references",
                    "Beeley, Chris. Web Application Development with R Using Shiny: Integrate the Power of R with the Simplicity of Shiny to Deliver Cutting-Edge Analytics over the Web, Second Edition. 2nd ed. Birmingham: PACKT Publishing, 2016."),
             tags$p(class="references",
                    " Mailund, Thomas. Beginning Data Science in R 4 : Data Analysis, Visualization, and Modelling for the Data Scientist. Second edition. New York, NY: Apress Media, LLC, 2022.")
    ),
    # Second Nav Bar option displaying interactable visualizations
    tabPanel("Visualization",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plot_selector", "Select Plot Type:",
                             choices = c("Histogram", "Scatter Plot", "Bar Chart", "Wordcloud")),
                 # Histogram Interaction UI
                 conditionalPanel(
                   condition = "input.plot_selector == 'Histogram'",
                   selectInput("hist_var", "Select Variable for Histogram:",
                               choices = c("Apps", "Accept", "Enroll", "Top10perc", 
                                           "Top25perc", "Outstate", "Room.Board", "Personal",
                                           "S.F.Ratio", "perc.alumni", "Grad.Rate", "Enroll.Rate","Accept.Rate")),
                   sliderInput(inputId = "binslider", label = "Bin Count", min = 1, 
                               max = 25, value = 10),
                   checkboxInput(inputId = "private_sel", label="Split histogram by type", value = FALSE, width = NULL)
                 ),
                 # Scatter Plot interaction UI
                 conditionalPanel(
                   condition = "input.plot_selector == 'Scatter Plot'",
                   selectInput("scat_xvar", "Select X Variable for Scatter Plot:",
                               choices = c("Apps", "Accept", "Enroll", "Top10perc", 
                                           "Top25perc", "Outstate", "Room.Board", "Personal",
                                           "S.F.Ratio", "perc.alumni", "Grad.Rate", "Enroll.Rate","Accept.Rate")),
                   selectInput("scat_yvar", "Select Y Variable for Scatter Plot:",
                               choices = c("Apps", "Accept", "Enroll", "Top10perc", 
                                           "Top25perc", "Outstate", "Room.Board", "Personal",
                                           "S.F.Ratio", "perc.alumni", "Grad.Rate", "Enroll.Rate","Accept.Rate")),
                   checkboxInput(inputId = "private_sel_scat", label="Split scatter plot by type", value = FALSE, width = NULL)
                 ),
                 # Bar Chart Interaction UI
                 conditionalPanel(
                   condition = "input.plot_selector == 'Bar Chart'",
                   selectInput("bar_var", "Select Variable for Bar Chart:",
                               choices = c("Apps", "Accept", "Enroll", "Top10perc", 
                                           "Top25perc", "Outstate", "Room.Board", "Personal",
                                           "S.F.Ratio", "perc.alumni", "Grad.Rate", "Enroll.Rate","Accept.Rate")),
                 ),
                 # Word Cloud Interaction UI
                 conditionalPanel(
                   condition = "input.plot_selector == 'Wordcloud'",
                   # slider1 selection and slider
                   selectInput("cloud_var1", "Selection to Limit Word Cloud",
                               choices = c("Grad.Rate", "Accept", "Enroll", "Top10perc", 
                                           "Top25perc", "Outstate", "Room.Board", "Personal",
                                           "S.F.Ratio", "perc.alumni", "Apps", "Enroll.Rate","Accept.Rate")),
                   sliderInput("cloud_slider1", "Limit Frequency",
                               min = 0, max = 1, value = c(0,1)),
                   #slider2 selection and slider
                   selectInput("cloud_var2", "Selection to Limit Word Cloud:",
                               choices = c("Apps", "Accept", "Enroll", "Top10perc", 
                                           "Top25perc", "Outstate", "Room.Board", "Personal",
                                           "S.F.Ratio", "perc.alumni", "Grad.Rate", "Enroll.Rate","Accept.Rate")),
                   sliderInput("cloud_slider2", "Limit Frequency",
                               min = 0, max = 1, value = c(0,0.5)),
                   # wordcloud display
                   selectInput("cloud_view", "Select Variable to View in Word Cloud:",
                               choices = c("Apps", "Accept", "Enroll", "Top10perc", 
                                           "Top25perc", "Outstate", "Room.Board", "Personal",
                                           "S.F.Ratio", "perc.alumni", "Grad.Rate", "Enroll.Rate","Accept.Rate") ),
                   checkboxInput(inputId = "private_sel_word", label="Split word cloud by type", value = FALSE, width = NULL)
                 )
             ),
             mainPanel(
               uiOutput("output_plot")
             ) # mainPanel End
           ) # sideBarLayout End
    ) # TabPanel End
  ) # TabsetPanel End
)

unis <- read.csv("College_Data.csv")  %>%
  mutate(Enroll.Rate = (Enroll / Accept)*100) %>% 
  mutate(Accept.Rate = (Accept / Apps)*100)

# Define server logic
server <- function(input, output, session) {
  # visualization display 
  output$output_plot <- renderUI({
    if (input$plot_selector == "Histogram") {
      hist_output <- plotlyOutput("hist_plot")
      return(hist_output)
    } else if (input$plot_selector == "Bar Chart") {
      barchart_output <- plotlyOutput("barchart_plot")
      return(barchart_output)
    } else if (input$plot_selector == "Scatter Plot") {
      scatter_output <- plotlyOutput("scatter_plot")
      return(scatter_output)
    } else if (input$plot_selector == "Wordcloud") {
      wordcloud_output <- wordcloud2Output("wordcloud_plot")
      return(wordcloud_output)
    }
  })
  
  # ------------------- Histogram ----------------------------
  output$hist_plot <- renderPlotly({
    # Ensure bin count is an integer
    bins <- round(input$binslider) 
    # splitting of histogram by private and public
    if (input$private_sel) { 
      ggplotly(ggplot(unis, aes(x = .data[[input$hist_var]])) +
        geom_histogram(data = subset(unis, Private == "Yes"), 
                       bins = bins, color = "black", fill = "skyblue") +
        geom_histogram(data = subset(unis, Private == "No"), 
                       bins = bins, color = "black", fill = "salmon") +
        labs(title = paste("Histograms of", input$hist_var),
             x = input$hist_var) +
        facet_wrap(~ Private, labeller = labeller(Private = c(Yes = "Private", No = "Public"))))
    } else {                # combine histogram
      ggplot(unis, aes(x = .data[[input$hist_var]], fill = factor(Private))) +
        geom_histogram(bins = bins, color = "black") +
        scale_fill_manual(values = c("skyblue", "salmon"), labels = c("Public", "Private")) +
        labs(title = paste("Histogram of", input$hist_var),
             x = input$hist_var,
             fill="Type of University")
    }
  })

  
  #------------------------ Scatter Plot -----------------------------
  output$scatter_plot <- renderPlotly({
    # splitting scatter plots display private and public
    if (input$private_sel_scat) {     
      p <- ggplot(unis, aes(text=X,x = .data[[input$scat_xvar]], y=.data[[input$scat_yvar]])) +
        geom_point(data = subset(unis, Private == "Yes"), 
                   color = "skyblue") +
        geom_point(data = subset(unis, Private == "No"), 
                   color = "salmon") +
        labs(title = paste("Scatter Plots of", input$scat_xvar, "vs.",input$scat_yvar),
             x = input$scat_xvar,y=input$scat_yvar) +
        facet_wrap(~ Private, labeller = labeller(Private = c(Yes = "Private", No = "Public")))
    } else {                           
      #combine scatter plot
      p <- ggplot(unis, aes(text=X,x = .data[[input$scat_xvar]], y = .data[[input$scat_yvar]])) +
        geom_point() +
        labs(title = paste("Scatter Plot of", input$scat_xvar, "vs.",input$scat_yvar),
             x = input$scat_xvar,
             y = input$scat_yvar)
    }
    ggplotly(p)
  })
  
  # ------------------------ Bar Chart --------------------------------
  output$barchart_plot <- renderPlotly({
    sortedUnis <- arrange(unis, desc(.data[[input$bar_var]]))
    # plot bar chart of top 10 private and public each
    p <- ggplot(data=sortedUnis,aes(x=.data[[input$bar_var]], y=X)) +
      geom_bar(data = subset(sortedUnis, Private == "Yes")%>% head(10), 
               fill = "skyblue",stat="identity") + 
      geom_bar(data = subset(sortedUnis, Private == "No")%>% head(10), 
               fill = "salmon", stat="identity")
    ggplotly(p)%>%
      layout(legend = list(title = "Type of University",
                           labels = list(Private = "Private", Public = "Public")))
  })
  
  
  # ------------------------ Word Cloud --------------------------------
  output$wordcloud_plot <- renderWordcloud2({
    # Get slider values
    var1_max <- input$cloud_slider1[2] * max(unis[[input$cloud_var1]])
    var1_min <- input$cloud_slider1[1] * max(unis[[input$cloud_var1]])
    var2_max <- input$cloud_slider2[2] * max(unis[[input$cloud_var2]])
    var2_min <- input$cloud_slider2[1] * max(unis[[input$cloud_var2]])
    #reduce out of bounds issues by increasing minimum in slider to smallest value in data
    if(var1_min < min(unis[[input$cloud_var1]])){
      var1_min <- min(unis[[input$cloud_var1]])
    }
    if(var2_min <  min(unis[[input$cloud_var2]])){
      var2_min <- min(unis[[input$cloud_var2]])
    }
    # filter word cloud by both values found of each slide bar
    filter_unis <- subset(unis, unis[[input$cloud_var1]] >= var1_min &
                            unis[[input$cloud_var1]] <= var1_max &
                            unis[[input$cloud_var2]] <= var2_max &
                            unis[[input$cloud_var2]] >= var2_min)
    # convert word frequencies to a data frame
    word_freq_df <- data.frame(word = filter_unis$X, 
                               freq = filter_unis[[input$cloud_view]],
                               stringsAsFactors = FALSE)
    if(input$private_sel_word){
    # create a vector of colors based on the "Private" variable
      colors <- ifelse(filter_unis$Private == "Yes", "skyblue", "salmon")
      
      # plot the word cloud with dynamically assigned colors
      wordcloud2(word_freq_df, size = 1, color = colors)
    }
    else{
      wordcloud2(word_freq_df,size=1)
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)