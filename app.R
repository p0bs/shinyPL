library(shiny)
library(DBI)
library(dplyr)
library(duckdb)
library(forcats)
library(formattable)
library(gt)
library(readr)
library(tidyr)


# Initiate ----

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "LOAD 'motherduck'")
dbExecute(con, "PRAGMA MD_CONNECT")

game_details <- read_rds("data/game_details.rds") |> 
  mutate(label = paste0(team_home, " vs. ", team_away)) |> 
  arrange(game)

values_choices <- setNames(
  game_details$game,
  game_details$label
  )


# Create the UI ----

ui <- fluidPage(

    # Application title
    titlePanel("Top 5 chances for this season"),

    # Sidebar with a select input for game choice 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "game_input", 
            label = "Game:", 
            choices = values_choices, 
            multiple = FALSE
            ),
          selectInput(
            inputId = "outcome_input", 
            label = "Prediction:", 
            choices = c(
              "Home win" = "3", 
              "Draw" = "1", 
              "Away win" = "0"
              ),
            multiple = FALSE
            ),
          actionButton(
            inputId = "button_go",
            label = "Go!"
            )
          ),
        # Print the number of the game
        mainPanel(
          gt_output("text_output")
          )
        )
    )


# Add server logic ----

server <- function(input, output) {

  input_game <- reactive({
    as.integer(input$game_input)
  })
  
  input_outcome <- reactive({
    as.integer(input$outcome_input)
  })
  
  output1 <- eventReactive(
    input$button_go, 
    {

      results_db <- tbl(con, "premLeague.simulations") |>
        filter(
          game == !!input_game(),
          is_home,
          points == !!input_outcome()
        ) |>
        distinct(sim) |>
        left_join(
          tbl(con, "premLeague.simulations"),
          join_by(sim == sim)
        ) |>
        summarise(
          points_total = sum(points),
          .by = c(sim, team)
        ) |>
        left_join(
          tbl(con, "premLeague.data_table_now"),
          join_by(team == team)
        ) |>
        mutate(
          points_sum = points_total + points_now
        ) |>
        select(sim, team, "points" = points_sum, goaldiff) |>
        group_by(sim) |>
        arrange(desc(points), desc(goaldiff), team) |>
        mutate(ranking = row_number()) |>
        ungroup() |>
        count(team, ranking) |>
        arrange(team, ranking) |>
        collect()
      
      n_results_db <- sum(results_db$n)/20
      
      results_db |> 
        mutate(
          team = as_factor(team),
          pc = n / n_results_db
        ) |> 
        select(team, ranking, pc) |> 
        tidyr::complete(team, ranking) |> 
        tidyr::replace_na(
          replace = list(
            team = NA_character_, 
            ranking = NA_integer_, 
            pc = 0L
          )
        ) |> 
        pivot_wider(
          id_cols = team, 
          names_from = ranking, 
          names_prefix = "p", 
          values_from = pc
        ) |> 
        mutate(
          top_five = p1 + p2 + p3 + p4 + p5
        ) |> 
        select(team, top_five) |> 
        filter(top_five >= 0.0001) |> 
        arrange(desc(top_five)) |> 
        mutate(top_five = percent(top_five, digits = 2)) |> 
        gt()
      
      }
  )
  

  output$text_output <- render_gt({
    
    output1() 
    
    })
  }


# Run the app ----

shinyApp(ui = ui, server = server)
