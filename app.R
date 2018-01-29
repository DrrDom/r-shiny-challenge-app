library(shiny)
library(dplyr)

# setup =====

# title
title <- "3rd Advanced Drug Design workshop competition - Olomouc 2018"

# max submissions
max_submissions <- 10

# number of entries in a submission
n_entries <- 100

# dir to store results
res_dir <- "challenge2018/results/"

# load groups
tmp <- read.table("data/tokens.txt", sep = "\t", stringsAsFactors = F)
groups <- tmp[, 2]
names(groups) <- tmp[, 1]
rm(tmp)

# load correct answers
answers <- read.table("data/correct_answers.txt", stringsAsFactors = F)[, 1]

# app =====

# Define UI for application

ui <- fluidPage(

  # Application title
  titlePanel(title),
  
  sidebarLayout(
    sidebarPanel(
      textInput("token", "Input your token"),
      textAreaInput("submit_text", "List of answers", "", width = "100%", rows = 10),
      h5("or"),
      fileInput("submit_file", "File with answers"),
      textInput("note", "Submission note"),
      actionButton("submit", "Submit", width = "100%",
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      verbatimTextOutput("submit_result")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Rules & description", includeHTML("data/challenge_description.htm")),
        tabPanel("Standings", 
                 mainPanel(
                   actionButton("refresh", "Refresh", width = "100px", style="background-color: #c0c0c0"),
                   fluidRow(
                     column(4, selectInput("submission_rank", "Show submisstions", c("best", "all"), width = "100%")),
                     column(6, selectInput("submission_group", "Show submisstions of", c("all groups", paste("Group", 1:length(groups))), width = "100%"))
                   ),
                   fluidRow(column(12, tableOutput("result_table"))),
                   width = 12
                 )
        ), id = "tabs"
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  globals <- reactiveValues(df = NULL)
  
  # click submit
  observeEvent(input$submit, {
    
    withProgress({
      
      if (input$token == "" | is.na(groups[input$token])) {
        output$submit_result <- renderText("Invalid token")
        return()
      }
      
      group = groups[input$token]
      savedir = paste0(res_dir, sub(" ", "_", group))
      if (dir.exists(savedir)) {
        files <- list.files(savedir)
        n_submissions <- sum(grepl("\\.txt$", files))
      } else {
        dir.create(savedir, recursive = TRUE)
        n_submissions <- 0
      }
      
      incProgress(1)

      if (n_submissions >= max_submissions) {
        output$submit_result <- renderText(paste0("Max number of submissions was reached: ", max_submissions))
        return()
      }
      
      if (input$submit_text == "" & is.null(input$submit_file)) {
        output$submit_result <- renderText("Enter mol IDs as text or\nselect file with mol IDs\nto upload")
        return()
      }
      
      incProgress(1)

      submission_file <- file.path(savedir, paste0("submission_", n_submissions + 1, ".txt"))
      if (!is.null(input$submit_file)) {
        file.copy(input$submit_file$datapath, submission_file, overwrite = TRUE)
      } else {
        write.table(input$submit_text, submission_file, quote = FALSE, row.names = FALSE, col.names = FALSE)
      }
      
      d <- read.table(submission_file, stringsAsFactors = FALSE)[, 1]
      d <- unique(d[d != ""])
      if (length(d) != n_entries) {
        file.remove(submission_file)
        output$submit_result <- renderText(paste0("You have to supply ", n_entries, " entries, only ", length(d), " unique ones were supplied"))
        return()
      }
      
      incProgress(1)
      
      score <- round(length(intersect(d, answers)) / length(d) / 0.37, 5)
      # score <- length(intersect(d, answers))
      
      res <- data.frame(group, score, strftime(Sys.time()), input$note)
      res_file <- file.path(savedir, paste0("submission_", n_submissions + 1, ".res"))
      write.table(res, res_file, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
      
      incProgress(1)

      output$submit_result <- renderText(paste0("Score = ", score))
    
      incProgress(1)
      
      updateTabsetPanel(session, "tabs","Standings")
      
    }, min = 0, max = 5, message = "Submitting...")
    
    refresh()
    
  })
  
  refresh <- function() {
    files <- list.files(res_dir, "submission_[0-9]+\\.res", recursive = TRUE, full.names = TRUE)
    if (length(files) > 0) {
      df <- list()
      withProgress({
        df <- lapply(files, function(f) {
          read.table(f, sep = "\t", header = FALSE, as.is = TRUE)
        })
      }, min = 1, max = length(files), value = 1, message = "Updating standings...")
      df <- do.call(rbind, df)
      colnames(df) <- c("Group", "Score", "Time", "Note")
      globals$df <- df
      output$result_table <- renderTable(dataset())
    }
  }
  
  # click refresh
  observeEvent(input$refresh, {
    refresh()
  })
  
  dataset <- reactive({
    df <- globals$df
    if (!is.null(df)) {
      if (input$submission_rank == "best") {
        df <- df %>% group_by(Group) %>% arrange(desc(Score)) %>% slice(1) %>% ungroup()
      }
      if (input$submission_group != "all groups") {
        df <- df %>% filter(Group == input$submission_group)
      }
      df <- df %>% arrange(desc(Score))
    }
    df
  })
  
  output$result_table <- renderTable(dataset())
  
}

# Run the application 
shinyApp(ui = ui, server = server)
