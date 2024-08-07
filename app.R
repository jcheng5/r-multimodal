library(shiny)
library(bslib)
library(htmltools)
library(shinymedia)
library(jsonlite)
library(httr)

# Define UI
ui <- page_fluid(
  tags$head(
    tags$style(HTML("
      #footer {
        padding: 0.5em 0.7em;
        background-color: var(--bs-primary);
        color: white;
      }
      #footer a {
        color: white;
      }
    "))
  ),
  
  titlePanel("Multimodal Chat Demo"),
  
  shinymedia::input_video_clip("clip", reset_on_record = TRUE, 
                               class = "mt-3 mx-auto", 
                               style = css(width = "600px", max_width = "100%"),
                               video_bits_per_second = 256000,
                               audio_bits_per_second = 64000),
  
  uiOutput("response"),
  
  fixedPanel(
    bottom = 0, left = 0, right = 0, height = "auto", id = "footer",
    div(
      class = "mx-auto",
      style = css(width = "600px", max_width = "100%"),
      div(
        class = "float-left",
        "Built in R with ",
        a("Shiny", href = "https://shiny.posit.co/")
      ),
      div(
        class = "float-right",
        a(
          href = "https://github.com/jcheng5/multimodal",
          icon("github"), "View source code"
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  messages <- reactiveVal(list())
  
  chat_task <- reactive({
    req(input$clip)
    p <- Progress$new()
    tryCatch({
      result <- chat(input$clip, messages(), progress = p)
      messages(result$messages)
      result$audio_uri
    }, finally = p$close())
  }) %>% bindEvent(input$clip)
  
  output$response <- renderUI({
    if (is.null(input$clip)) {
      card(
        class = "mt-3 mx-auto",
        style = css(width = "600px", max_width = "100%"),
        markdown("
          **Instructions:** Record a short video clip to start chatting with GPT-4o.
          After it responds, you can record another clip to continue the conversation.
          Reload the browser to start a new conversation.

          Some ideas to get you started:

          * \"What do you think of the outfit I'm wearing?\"
          * \"Where does it look like I am right now?\"
          * \"Tell me an interesting fact about an object you see in this video.\"
        ")
      )
    } else {
      audio_uri <- chat_task()
      shinymedia::audio_spinner(src = audio_uri, autodismiss = FALSE)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
