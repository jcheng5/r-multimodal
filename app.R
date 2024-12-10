library(shiny)
library(bslib)
library(htmltools)
library(elmer)
library(shinymedia)
library(promises)

system_prompt <- paste(collapse = "\n", readLines("system_prompt.md", warn = FALSE))

# Define UI
ui <- page_fluid(
  tags$head(includeCSS("styles.css")),
  
  shinymedia::input_video_clip("clip", reset_on_record = FALSE,
    class = "mt-3 mx-auto", 
    style = css(width = "600px", max_width = "100%"),
    video_bits_per_second = 256000,
    audio_bits_per_second = 64000
  ),
  
  uiOutput("response"),
  
  fixedPanel(
    bottom = 0, left = 0, right = 0, height = "auto", id = "footer",
    div(
      class = "mx-auto",
      style = css(width = "600px", max_width = "100%"),
      div(class = "float-left", "Built in R with ", a("Shiny", href = "https://shiny.posit.co/")),
      div(class = "float-right",
        a(href = "https://github.com/jcheng5/multimodal", icon("github"), "View source code")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  show_intro <- TRUE

  chat <- chat_openai(
    model = "gpt-4o", 
    system_prompt = system_prompt
  )

  chat_task <- eventReactive(input$clip, {
    req(input$clip)
    p <- Progress$new()
    converse(chat, input$clip, progress = p) |>
      finally(\() {
        p$close()
      })
  })
  
  output$response <- renderUI({
    if (is.null(input$clip)) {
      if (show_intro) {
        show_intro <<- FALSE

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
      }
    } else {
      chat_task() %...>%
        shinymedia::audio_spinner(src = ., autodismiss = FALSE)
    }
  })
}

# Run the application 
shinyApp(ui, server)
