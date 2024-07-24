library(shiny)

app_ui <- function(request) {
  fluidPage(
    titlePanel("Shiny Server for R Code Execution"),
    sidebarLayout(
      sidebarPanel(
        fileInput("upload_code", "Upload R Script"),
        actionButton("execute_code", "Execute Code"),
        downloadButton("download_report", "Download Report")
      ),
      mainPanel(
        verbatimTextOutput("r_output"),
        uiOutput("plot_output")
      )
    )
  )
}
library(shiny)
library(httr)

app_server <- function(input, output, session) {
  code_content <- reactiveVal(NULL)
  
  observeEvent(input$upload_code, {
    req(input$upload_code)
    ext <- tools::file_ext(input$upload_code$name)
    if (ext != "R") {
      showNotification("Please upload a valid R script.", type = "error")
      return(NULL)
    }
    code_content(readLines(input$upload_code$datapath))
    showNotification("R script uploaded successfully.", type = "message")
  })
  
  observeEvent(input$execute_code, {
    req(code_content())
    code <- paste(code_content(), collapse = "\n")
    
    response <- POST(
      url = "https://example.com/api/execute_code",
      body = list(code = code),
      encode = "json"
    )
    
    if (response$status_code == 200) {
      result <- content(response, "parsed")
      output$r_output <- renderText(result$output)
      showNotification("Code executed successfully.", type = "message")
    } else {
      output$r_output <- renderText("Error executing code.")
      showNotification("Error executing code.", type = "error")
    }
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Report", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      req(code_content())
      tryCatch({
        rmd_content <- paste0("```{r}\n", paste(code_content(), collapse = "\n"), "\n```\n")
        writeLines(rmd_content, con = "report.Rmd")
        rmarkdown::render("report.Rmd", output_file = file)
        showNotification("Report generated successfully.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error generating report:", e$message), type = "error")
      })
    }
  )
}

run_app <- function() {
  shinyApp(ui = app_ui, server = app_server)
}
