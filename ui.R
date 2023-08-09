library(shiny)
library(tidyr)
library(dplyr)
library(plotly)
library(bslib)
library(stringr)

boys_df <- read.csv("https://raw.githubusercontent.com/the-pudding/data/master/boybands/boys.csv")
bands_df <- read.csv("https://raw.githubusercontent.com/the-pudding/data/master/boybands/bands.csv")

xy_values <- left_join(bands_df, boys_df, by = "band")
xy_values$facial_hair[xy_values$facial_hair == ""] <- "no facial hair"

z_values <- select(xy_values, facial_hair, highest_pos) %>%
  group_by(facial_hair) %>% 
  filter(highest_pos == max(highest_pos, na.rm = TRUE))

#Introduction Panel-------------------------------------------------------------
intro_panel <- tabPanel(
  title = "Introduction",
  HTML('<h1 class = "header"> Abstract </h1>'),
  HTML('<p> The main question our group is addressing is how different factors 
       contribute to the success of different boy bands. This question is important
       as the success of popular music and bands reflect the trends of the 
       entertainment and fashion industries. To address this question, we have 
       been examining data on various characteristics and mannerisms of 56 boybands
       and drawing conclusions on what general pattens lead to their successes. 
       Our project will include analyzing the successes of boybands hit singles 
       based on two different factors: the time and genre of the song as well as
       factoring in how the members physically look. Therefore, one of our 
       research questions is seeing whether the dance style of the boybands hit 
       song follows a trend. We want to see whether there is a correlation 
       between the dance styles throughout the 56 boybands and whether than 
       contributes to their songs popularity. Another research question is 
       whether the race of the members matters in how high up their song was on 
       the Billboard charts. Following up on that, we also want to see if their 
       physical attributes, such as hair color and accessories, have a factor in
       how high up the song was in the billboard charts. This may require extra 
       research on our end, but we also want to see when the band debuted and 
       how quickly they were able to attain a hit on the Billboard chart. 
       Something that motivated our questions is that boybands have changed over
       the past few decades, so it\'s interesting to see what catches the new 
       generations eyes. We think it\'s important because finding trends in pop 
       culture is something that can be used to predict the popularity of future
       boy bands. </p>'),
  
  HTML('<h2 class = "header2"> Dataset </h2>'),
  p("We found the data from", HTML('<a href ="https://github.com/the-pudding/data/tree/master/boybands"> The Pudding dataset. </a>'), "There were 40 people who collected this data, some being Adrian Blanco, Adriano Seghezzi, Alex Garcia, and Amanda Smith. The data was collected through five different articles written on Buzzfeed, Vogue, Billboard, Wikipedia, and Ranker. The data gathered from these sources were then verified against a Billboard chart. In regards to the physical characteristics of each member, data was collected by watching the music videos and recording characteristics through the footage. The Pudding\'s explanation for their data collections is to describe \'ideas debated in culture with visual essays,\' which can also be applied to this data collection. When combining both the bands and the boys\' datasets, there are 291 rows. For the bands dataset, there are 7 columns while there are 10 columns for the boys dataset. We don\'t think there are any ethical questions of power that need to be considered; if anything, one concern could be whether these bands receive different treatment based on factors like race and phenotypical characteristics. Some limitations that this data could have are that each member may look different in their various music videos, so choosing the way they look in just one may not be enough to gauge their popularity. The same can be said for their songs, because while the hits on Billboard may be a certain genre, the band itself may use alternate genres with their other songs."),
  
  HTML('<h3 class = "header3"> Limitation & Challenges </h3>'),
  HTML('<p> Through this project, there are mainly three possible ethical questions
       or limitations to consider with this dataset. The first challenge we might 
       encounter is how we handle the missing data. The physical characteristics
       of the members were collected by watching the footage of the music videos
       for their top hit, but that doesn\'t mean it accounts for the rest of their
       music videos. This way of recording can lead to many data inaccuracies, 
       as some of their outfits are not being recorded. Secondly, we may face 
       difficulties when grouping the characteristics of the band members together.
       Some members may have some similar features, like sharing the same hair 
       color, but that doesn\'t mean they are the same person. Finding a classification
       that matches all members as a whole might be challenging. Finally, it is 
       important to look for factors other than appearance, race, and song content
       that could potentially affect popularity. Other factors can also affect 
       the popularity of a band, such as the political relations at the time, 
       society\'s culture, the changing tastes of the people, and so on. This is
       a complex problem to analyze, yet it has to be taken into account. </p>'),
  br(),
  HTML('<center><img src = "https://content.api.news/v3/images/bin/9080c54cde959bfd5a9ece8476f9740d?width=650" width = "530" height = "370" alt = "BTS do tailoring with a twist at the 2019 Billboard Awards"></center>'),
  br(),
)


#First Plot Panel - defined checkbox as input
plot1_panel <- tabPanel(
  title = "Facial Hair vs. Song Position",
  titlePanel("Comparing Boy Band Members' Facial Hair and their Songs on the Chart"),
  sidebarLayout(sidebarPanel(
    checkboxGroupInput(inputId = 'user_category',
                       label = "Choose facial hair", 
                       choices = unique(z_values$facial_hair),
                       selected = "beard")), 
    mainPanel(plotlyOutput(outputId = "plot1"),
              p("This chart is a comparison between the facial hair that the members of the
bands had and the position of their highest charting song on the Billboard
chart. We used a bar graph because it measures categorical data and it groups
the information together cohesively. The graph looks at the lowest position
of the bands highest charting song in correlation to the members facial hairs
as a means to see whether the appearance of the members makes an impact on
how well their song does on the charts. This specific comparison was made in
order to test whether physical characteristics of the members plays a role in
how well their songs do - that is, whether the public is swayed by the
attractiveness of certain features compared to others. Something we did
find is that most of the facial hairs listed in the graph have a low ranking
on the Billboard chart. Interestingly enough, members with no facial hair can
still have a low ranking, which insinuates that its just certain types of
facial hair that brings lower chart rankings."))))


#Second Plot Panel -------------------------------------------------------------
plot2_df <- bands_df %>% 
  select(band, highest_pos, highest_pos_date, danceSpeed)

sibebar_spd_plot <- sidebarPanel(
  selectInput(
    inputId = "user_speed",
    label = "Select dance speed:",
    choices = bands_df$danceSpeed,
    selected = "pop",
    multiple = T
  ),
  helpText("Select the speed of the song group you would like to see displayed!")
)

main_plot2 <- mainPanel(
  plotlyOutput(outputId = "plot2"),
  p("This chart displays the dance speed of the song compared to the position
    of the song on the Billboard chart. Both pop and slow genres can be seen here,
    and the density refers to how many of each genre was categorized depending
    on the chart position. Pop songs seem to have a higher success rate from position
    0-25 on the chart, with their density going from 0.05 and sloping downwards.
    Slower songs tended to have more success when they were lower down on the chart,
    as evidenced by positions 60-100. Overall, it is clear that pop songs have more
    success with higher ranking songs, but this doesn't mean that slower songs
    have no success at all - they are just not as prevalent as pop songs.")
)

plot2_panel <- tabPanel(
  title = "Dance Speed Plot",
  titlePanel("How Dance Speed effects Song Success over Time"),
  sidebarLayout(
    sibebar_spd_plot,
    main_plot2
  )
)

#Third Plot Panel---------------------------------------------------------------
boys_df$dob <- as.numeric(str_sub(boys_df$dob, -4))
new_boys_df <- boys_df %>% replace_na(list(dob = 0)) %>% filter(dob != 0)

year_range <- range(new_boys_df$dob)
skin_color <- unique(new_boys_df$skin)

sidebar_content_p3 <- sidebarPanel(
  sliderInput(
    inputId = "dob_range",
    label = "Select date of birth range:",
    min = year_range[1],
    max = year_range[2],
    value = year_range
  ),
  
  selectInput(
    inputId = "skin_color",
    label = "Select skin color:",
    choices = skin_color,
    selected = "light",
    multiple = TRUE
  )
)

main_content <- mainPanel(
  plotlyOutput(outputId = "plot3"),
  p("The chart on this page has two primary purposes; the first is to find 
    out the height distribution of band members born in different years. 
    The second purpose is to determine the approximate number of band members
    with varying skin colors. With these two explorations, we can get a 
    general idea/impression of the band members' height and see the proportion
    of band members with different skin colors/races.")
)

plot3_panel <- tabPanel(
  title = "Height Distribution",
  titlePanel("Height distribution of band members by race and birth year"),
  sidebarLayout(
    sidebar_content_p3,
    main_content
  )
)

#Summary Panel------------------------------------------------------------------

summary_panel <- tabPanel(
  title = "Conclusion / Summary Takeaways",
  h3("Summary", align = "center"),
  p("When looking at each of the graphs, we can make separate conclusions 
  about each of the charts. For the facial hair comparison, something that
    we can determine is that members with facial hair tend to have songs
    that are lower on the Billboard chart. For example, a member with a goatee
    had a song that was 97th on the Billboard chart. This trend is constant
    when looking at the other facial hair types, showing that physical attributes
    and features hold weight in a songs popularity. In terms of song speed, 
    pop songs were more popular than slow songs. Higher charting songs had
    a density of roughly 0.05 for pop songs while slow songs were roughly less
    than 0.03. This could be because fans tend to gravitate towards more 
    uplifiting music rather than slower music. Something that we can take away
    from the height distribution of the members is that members born in more
    recent years tend to be shorter than members born in the 1980s. There are 
    a cluster of light skinned members between 68-75 inches, and the fact that
    there are less people of color can indicate that there is a majority
    of light skinned members in these boy bands. In general, these insights can
    tell us a lot about society and preferences. When we see trends in the
    skin tone and facial features, it's easy to make connections about how
    boy bands are judged more by their attractiveness rather than the content
    of their music. However, we should keep in mind that this may not be the only 
    factor causing a bands song to rise up in the charts."),
br(),
h3("Conclusion", align = "center"),
p("We can make many conclusions from the data given the different comparisons
we have decided to make. Some of these comparisons can be used to analyze
whether an upcoming boyband is going to be popular or not. For instance, if
they wanted to determine a song speed to use, they can go through the data
of previous boybands to see what kinds of song speed were number one on 
the Billboard chart. Alternatively, if we were to conclude from our research
that a band was more likely to get more listens if the lead singer had long hair, 
no facial hair, and more than one accessory, then designers and stylists 
would be able to adjust the look of their clients and create a more marketable
product. Our research has the potential to help the music and entertainment
industry better understand the rational behind the successes of boy bands
throughout the decades. Understanding trends can help us see what people are
finding to be more appealing, whether it be the members skin tones or the
style of their songs."),
br(),
HTML('<center><img src = "https://centennialbeauty.com/wp-content/uploads/2020/04/16939035311_21c7471acb_b.jpg" width = "500" height = "500" alt = "BTS do tailoring with a twist at the 2019 Billboard Awards"></center>'),
  br(),
)

#ui ----------------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  includeCSS("style.css"),
  navbarPage(
    title = "Final Project",
    intro_panel,
    plot1_panel,
    plot2_panel,
    plot3_panel,
    summary_panel
  )
)