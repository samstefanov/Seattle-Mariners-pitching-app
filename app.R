library(shiny)
library(tidyverse)
library(shinythemes)
library(markdown)

Sea <- read_csv("Mariners2023Rotation.csv")
Kirby <- read_csv("Kirby.csv")
Miller <- read_csv("Miller.csv")
Castillo <- read_csv("Castillo.csv")
Gilbert <- read_csv("Gilbert.csv")
Woo <- read_csv("Woo.csv")

Sea <- Sea %>%
  mutate(p_throws = fct_recode(stand, "Right" = "R", "Left" = "L")) %>%
  mutate(pitch_type = fct_recode(pitch_type, "Changeup" = "CH", "Slider" = "SL", "4-Seam" = "FF", "Sinker" = "SI", 
                                 "Curveball" = "CU", "Cutter" = "FC", "Splitter" = "FS", "Sweeper" = "ST", "Knuckle Curve" = "KC",
                                 "Knuckle Ball" = "KN", "Screwball" = "SC", "Slurve" = "SV", "IDK" = "", "Pitchout" = "PO", "Unknown Pitch" = "NA")) %>%
  mutate(type = fct_recode(type, "Strike" = "S", "Ball" = "B", "Ball in Play" = "X")) %>%
  mutate(events = fct_recode(events, "No event" = "", "Strikeout" = "strikeout", "Field out" = "field_out", "Single" = "single", "Force out" = "force_out",
                             "Field error" = "field_error", "Double" = "double", "Home run" = "home_run",
                             "Sac fly" = "sac_fly", "Double Play" = "grounded_into_double_play", "Hit by Pitch" = "hit_by_pitch")) %>%
  filter(pitch_type != "NA") %>%
  filter(game_type == "R")

Raleigh <-  Sea %>%
  filter(description %in% c("ball", "called_strike", "blocked_ball")) %>%
  mutate("catcher_name" = case_when(fielder_2 == 663728 ~  "Raleigh, Cal", fielder_2 == 620443 ~ "Torrens, Luis",
                                    fielder_2 == 657247 ~ "O'Keefe, Brian", fielder_2 == 608596 ~ "Murphy, Tom"))

ui <- fluidPage(
  titlePanel("Seattle Mariners Pitching 2023"),
  navbarPage("Tabs",
             theme = shinytheme("flatly"),
             tabPanel("Pitch Grid Map", sidebarLayout(
               sidebarPanel(
                 selectInput("player_name", "Player Name", choices = unique(Sea$player_name), selected = unique(Sea$player_name)),
                 sliderInput("inning", "Inning", min = 1, max = 9, value = 1, step = 1, ticks = FALSE),
                 sliderInput("outs_when_up", "Outs in Inning", min = 0, max = 2, value = 0, step = 1, ticks = FALSE),
                 selectInput("stand", "Batter Stance", choices = unique(Sea$stand), selected = unique(Sea$stand)),
               ),
               mainPanel(
                 plotOutput("scatterPlot", height = "500")
               )
             )),
             tabPanel("Pitch Velocity", sidebarLayout(
               sidebarPanel(
                 selectInput("player_name1", "Player Name", choices = unique(Sea$player_name), selected = unique(Sea$player_name)),
                 checkboxGroupInput("pitch_type1", "Pitch Types", choices = unique(Sea$pitch_type), selected = unique(Sea$pitch_type)),
                 sliderInput("inning1", "Inning", min = 1, max = 9, value = 1, step = 1, ticks = FALSE)
               ), 
               mainPanel(plotOutput("velocityPlot", height = "500"))
             )),
             tabPanel("Catcher Impact", sidebarLayout(
               sidebarPanel(
                 selectInput("catcher_name", "Catcher Name", choices = unique(Raleigh$catcher_name), selected = "Cal Raleigh"),
                 selectInput("player_name2", "Player Name", choices = unique(Raleigh$player_name), selected = unique(Raleigh$player_name)),
               ),
               mainPanel(plotOutput("catcherPlot", height = "500"))
             )),
             tabPanel("League Comparison", sidebarLayout(
               sidebarPanel(
                 selectInput("stat_type", "Select Stat", choices = c("Runs Above Average", "Runs Above Replacement", 
                                                                     "Innings Pitched (Starting)", "Expected Runs Allowed", "ERA+", "xRA With Defense"), selected = "Runs above average"),
               ),
               mainPanel(plotOutput("warPlot", height = "500"))
             )),
             tabPanel("About", mainPanel(includeMarkdown("about.Rmd"))))
)

server <- function(input, output) {
  
  SeaPitches <- reactive({
    subset(subset(subset(subset(Sea, stand %in% input$stand), inning %in% input$inning), outs_when_up %in% input$outs_when_up), player_name %in% input$player_name)
  })
  
  SeaVelo <- reactive({
    subset(subset(subset(Sea, inning %in% input$inning1), pitch_type %in% input$pitch_type1), player_name %in% input$player_name1)
  })
  
  SeaCatchers <- reactive({
    subset(subset(Raleigh, player_name %in% input$player_name2), catcher_name %in% input$catcher_name)
  })
  
  MLBpitchstat1 = bwar_pit23$runs_above_avg
  
  MLBPitcherStat <- reactive({
    if (input$stat_type == "Runs Above Average") {
      MLBpitchstat1 = bwar_pit23$runs_above_avg
    } else if (input$stat_type == "Innings Pitched (Starting)"){
      MLBpitchstat1 = bwar_pit23$IPouts_start
    } else if (input$stat_type == "Expected Runs Allowed") {
      MLBpitchstat1 = bwar_pit23$xRA
    } else if (input$stat_type == "ERA+") {
      MLBpitchstat1 = bwar_pit23$ERA_plus
    } else if (input$stat_type == "xRA With Defense") {
      MLBpitchstat1 = bwar_pit23$xRA_def_pitcher
    } else {
      MLBpitchstat1 = bwar_pit23$runs_above_rep
    }
    return(MLBpitchstat1)
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(SeaPitches(), aes(x = plate_x, y = plate_z, color = type)) +
      geom_point(size = 10) +
      labs(x = NULL, y = NULL, title = "Pitch Location Plot") +
      scale_color_manual(values = c("Strike" = "firebrick2", "Ball" = "seagreen4", "Ball in Play" = "navyblue")) +
      coord_fixed(ratio = 0.75)+
      geom_strikezone()
  })
  
  output$velocityPlot <- renderPlot({
    ggplot(data = SeaVelo(), aes(x = pitch_type, y = release_speed)) +
      geom_point(color = "navyblue") +
      labs(x = "Pitch Type", y = "Release Speed (mph)") +
      ylim(70, 100)
  })
  
  output$catcherPlot <- renderPlot({
    ggplot(SeaCatchers(), aes(x = plate_x, y = plate_z, color = description)) +
      geom_point(size = 10) +
      labs(x = NULL, y = NULL, title = "Pitch Location Plot") +
      scale_color_manual(values = c("called_strike" = "firebrick2", "ball" = "seagreen4", "blocked_ball" = "navyblue")) +
      coord_fixed(ratio = 0.75)+
      geom_strikezone()
  })
  
  output$warPlot <- renderPlot({
    ggplot(bwar_pit23, aes(x = MLBPitcherStat(), y = WAR)) +
      geom_point() +
      geom_label(aes(label = team_ID, fill = ifelse(team_ID == "SEA", "Seattle Mariners", "Other MLB teams"))) +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("ghostwhite", "seagreen4")) +
      xlab(input$stat_type) +
      labs(title = "MLB Starting Pitching WAR 2023 (Minimum 10 starts)")
  })
}

shinyApp(ui, server)