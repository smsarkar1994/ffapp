rm(list = ls())
library(shiny) 
library(httr)
library(ffscrapr)
library(jsonlite)
library(glue)
library(purrr)
library(tidyverse)


week_record <- function(points, wk, teamId, team_name, df, home) {
  wk <- as.numeric(wk)
  points <- as.numeric(points)
  
  
  df <- df %>%
    filter(week == wk)
  
  if (home == "HOME") {
    df <- df %>%
      filter(home_teamId != teamId)
  }
  
  if (home == "AWAY") {
    df <- df %>%
      mutate(away_points = ifelse(away_teamId == teamId, NA, away_points))
  }
  
  scores <- c(df$away_points, df$home_points)
  
  scores <- scores[!is.na(scores)]
  
  
  weekly_wins <- sum(ifelse(points > scores, 1, 0))
  weekly_losses <- 11-weekly_wins
  
  out <- data.frame(cbind(team_name, wk, weekly_wins, weekly_losses))
  
  return(out)
  
}


# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(h1("Fantasy Head 2 Head Records")),
  
  navbarPage("",
             tabPanel("Instructions",
                      includeMarkdown("how_to.rmd")
                      
                      
                      
                      
                      ), 
             tabPanel("App",
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                            fluidRow(
                              column(12,
                                     textInput("SWID", label = h4("SWID")))),
                            fluidRow(
                              column(12,
                                     textInput("espn_s2", label = h4("espn_s2")))),
                            
                            fluidRow(
                              column(12,
                                     textInput("league_id", label = h4("League ID")),
                                     submitButton("Submit")))
                          )),
                        
                        mainPanel(
                          htmlOutput("SWID"),
                          htmlOutput("espn_s2"), 
                          htmlOutput("league_id"),
                          htmlOutput("week"),
                          dataTableOutput("table")
                        )
                      ))
  ),
  

  

  


  

  
  )
  


    



server <- function(input, output) {
  dataInput <- reactive({
    if (input$SWID != "" & input$league_id != "" & input$espn_s2 != "") {
      sleague_id = 1881874
      SWID = "{546006A0-43F0-44F4-B752-66D6C99BBA1D}"
      sespn_s2 = "AEB7woA6FICKrStHw34LE4jIBEJyfY%2BJW6bo9yBg4cA0mhzSkgtq0E1elzih5mNfIuwm7Kk9Oh%2BKtg5hQA3vcCsqfs3uoXSzrdhyaSkZkjrx1pHlzh3pH6zhHhMZwDig%2BuaQ5vBfM0qdR3x7PqVHmV8rB%2F%2Bj83d6M%2B%2FmdTr%2Bewtfy31BPl8NqUrkKEDE8sfrJHPWQhTF007IkblVvqKhqCkgeB4CLrEo%2BmktL3W9pkPF7vR79vXtojd6xYRJVAWNGtLZudHlrX7LRf3IHazKEKwLcaTr8B8o59DgEbHprDcrCw%3D%3D"
      sseason = 2021
      
      # conn <- espn_connect(season = 2021, league_id = input$league_id,
      #                      espn_s2 = input$espn_s2, swid = input$SWID)

      conn <- espn_connect(season = 2021, league_id = sleague_id,
                           espn_s2 = sespn_s2, swid = SWID)
      
      scores <- espn_getendpoint(conn, view = "mMatchupScore")
      
      dat <- scores$content
      
      dat <- dat$schedule
      
      points <- dat %>%
        map_df(magrittr::extract, c("winner", "matchupPeriodId")) %>%
        bind_cols(
          dat %>%
            map("away") %>%
            map_df(magrittr::extract,c("teamId","totalPoints"))
        ) %>%
        bind_cols(
          dat %>%
            map("home") %>%
            map_df(magrittr::extract,c("teamId","totalPoints"))
        ) 
      
      
      colnames(points) <- c("winner", "week", "away_teamId", "away_points", "home_teamId", "home_points")

      mTeam <- espn_getendpoint(conn, view = "mTeam")
      mTeam <- mTeam$content
      team_names <- mTeam$teams
      
      teams <- team_names %>%
        map_df(magrittr::extract, c("abbrev", "id", "location", "nickname")) %>%
        mutate(team_name = paste(location, nickname)) %>%
        select(id, team_name)
      

    
      left_join(points, teams, by = c("away_teamId" = "id")) %>%
        rename(away_team = team_name) %>%
        left_join(teams, by = c("home_teamId" = "id")) %>%
        rename(home_team = team_name) %>%
        filter(winner != "UNDECIDED")
      
      
    } 
  })
  
  weekly_record <- reactive({
    if (input$SWID != "" & input$league_id != "" & input$espn_s2 != "") {
      
      
      away_record <- data.table::rbindlist(apply(dataInput(), 1, FUN = function(x) week_record(x["away_points"], x["week"], x["away_teamId"], 
                                                                                          x["away_team"], dataInput(), "AWAY")))
      home_record <- data.table::rbindlist(apply(dataInput(), 1, FUN = function(x) week_record(x["home_points"], x["week"], x["home_teamId"], 
                                                                                          x["home_team"], dataInput(), "HOME")))
      
      records <- bind_rows(away_record, home_record) %>%
        mutate(wk = as.numeric(wk), 
               weekly_wins = as.numeric(weekly_wins), 
               weekly_losses = as.numeric(weekly_losses))
      
      overall_records <- records %>%
        group_by(team_name) %>%
        summarise(tot_wins = sum(weekly_wins), 
                  tot_losses = sum(weekly_losses)) %>%
        mutate(win_pct = round(tot_wins/(tot_wins + tot_losses), 3)) %>%
        arrange(-tot_wins) 
      
      current_week = max(dataInput()$week)
      
      last_5 <- dataInput() %>%
        filter(week >= (current_week - 5) & week <= current_week) %>%
        pivot_longer(cols = c("away_points", "home_points"), 
                     names_to = "status", values_to = "points") %>%
        mutate(team_name = ifelse(status == "away_points", away_team, home_team)) %>%
        select(week, team_name, points)
      
      
      last_5 <- last_5 %>%
        group_by(team_name) %>%
        summarise(last5_avg = mean(points)) %>%
        mutate(last5_avg = round(last5_avg, 2))
      
      overall_records <- left_join(overall_records, last_5, by = "team_name")
      
      overall_records %>%
        rename(`Team Name` = team_name, `Total Wins` = tot_wins,
               `Total Losses` = tot_losses, `Win %` = win_pct, 
               `Last 5 Avg.` = last5_avg)
      
    }
    
  })
  
  max_week <- reactive(
    if (input$SWID != "" & input$league_id != "" & input$espn_s2 != "") {
      max(dataInput()$week)
    } else {
      NA
    }
  )

  output$table <- renderDataTable(
    weekly_record(), options = list(paging = FALSE, searching = FALSE)
  ) 
  
  
  # output$SWID <- renderUI({
  #   p(paste0("Your SWID token is: ", input$SWID),
  #     style = "word-wrap: break-word;")
  # })
  # 
  # output$espn_s2 <- renderUI({
  #   p(paste0("Your espn_s2 token is: ", input$espn_s2),
  #     style = "word-wrap: break-word;")
  # })
  # 
  # output$league_id <- renderUI({
  #   p(paste0("Your League ID is: ", input$league_id),
  #     style = "word-wrap: break-word;")
  # })
  
  output$week <- renderUI({
    p(paste0("Data as of week: ", max_week()),
      style = "word-wrap: break-word;")
  })
  
  
}

shinyApp(ui, server)