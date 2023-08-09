library(shiny)
library(tidyr)
library(ggplot2)
library(plotly)
library(dplyr)

boys_df <- read.csv("https://raw.githubusercontent.com/the-pudding/data/master/boybands/boys.csv")
bands_df <- read.csv("https://raw.githubusercontent.com/the-pudding/data/master/boybands/bands.csv")

xy_values <- left_join(bands_df, boys_df, by = "band")
xy_values$facial_hair[xy_values$facial_hair == ""] <- "no facial hair"

z_values <- select(xy_values, facial_hair, highest_pos) %>%
  group_by(facial_hair) %>% 
  filter(highest_pos == max(highest_pos, na.rm = TRUE))

#Server function----------------------------------------------------------------
server <- function(input, output) {
  
  #Plot 1-------------------------------------------------------------------------
  output$plot1 <- renderPlotly({
    #reactive function that will filter the z_values dataframe based on user input
    chart_data <- reactive({
      z_values %>%  
        filter(facial_hair %in% input$user_category)
    })
    
    # Make a bar graph
    my_plot <- ggplot(data = chart_data()) +
      geom_bar(mapping = aes(x = facial_hair, y = highest_pos, fill = facial_hair), stat = "identity") +
      ggtitle("Facial Hair vs. Song Position on Chart") +
      labs(x = "Types of Facial Hair", y = "Song Position on Billboard Chart", fill = "Facial Hair")
    
    # Make interactive plot
    my_plotly_plot <- ggplotly(my_plot) 
    
    return(my_plotly_plot)
  })
  
  
  #Plot 2-------------------------------------------------------------------------
  output$plot2 <- renderPlotly ({
    plot2_df <- bands_df %>% 
      filter(danceSpeed %in% input$user_speed)
    
    spd_plot <- ggplot(data = plot2_df, aes(highest_pos,
                                            fill = danceSpeed,
                                            color = danceSpeed))+
      geom_density(alpha = 0.3)+
      labs(title = "Dance Speed vs. Sucess of the song",
           x = "Billboard Position",
           y = "Density",
           fill = "",
           color = "Dance Speed")
    
    simons_plot <- ggplotly(spd_plot)
    
    return(simons_plot)
  })
  
 
  #Plot 3 -------------------------------------------------------------------------
  
  output$plot3 <- renderPlotly({
    boys_df <- read.csv("https://raw.githubusercontent.com/the-pudding/data/master/boybands/boys.csv")
    boys_df$dob <- as.numeric(str_sub(boys_df$dob, -4))
    new2_boys_df <- boys_df %>% replace_na(list(dob = 0)) %>% filter(dob != 0)
    a <- new2_boys_df[is.na(new2_boys_df)]
    new2_boys_df[is.na(new2_boys_df)] <- round(runif(length(a), min = 60, max = 80))
    
    x_data <- new2_boys_df %>%
      filter(dob >= input$dob_range[1], dob <= input$dob_range[2]) %>%
      filter(skin %in% input$skin_color)
    
    
    p <- ggplot(data = x_data) +
      geom_point(mapping = aes(x = dob, y = height, color = skin, shape = as.factor(skin)), alpha = .6) +
      labs(
        title = "Date Of Birth vs. Height",
        x = "Date Of Birth",
        y = "Height",
        color = "Skin Color",
        shape = ""
      )
    return(ggplotly(p))
  })
  
  #End of Server function----------------------------------------------------------
}
