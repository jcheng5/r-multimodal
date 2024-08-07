library(shiny)
library(httr2)
library(jsonlite)
# library(av)
library(base64enc)
library(dotenv)

# Load environment variables from .env file
load_dot_env()

# Get OpenAI API key from environment variable
openai_api_key <- Sys.getenv("OPENAI_API_KEY")
if (openai_api_key == "") {
  stop("OPENAI_API_KEY not found in .env file")
}

whisper <- function(audio_file_path, model = "whisper-1") {
  req <- request("https://api.openai.com/v1/audio/transcriptions") %>%
    req_headers(
      Authorization = paste("Bearer", openai_api_key)
    ) %>%
    req_body_multipart(
      file = curl::form_file(audio_file_path),
      model = model
    ) %>%
    req_error(body = function(resp) stop("Error in Whisper API call: ", resp_body_json(resp)$error$message))

  resp <- req %>% req_perform()
  resp_body_json(resp)$text
}

chat <- function(video_data_uri, messages, progress = NULL) {
  # Function to update progress
  update_progress <- function(message, value) {
    if (!is.null(progress)) {
      progress$set(message = message, value = value)
    }
  }
  
  # 1. Parse the data URI and save video to a temporary file
  update_progress("Decoding input...", 0)
  video_data <- parse_data_uri(video_data_uri)
  mime_type <- video_data$mime_type
  ext <- head(names(mime::mimemap[mime::mimemap == mime_type]), 1)
  if (length(ext) == 0) {
    stop("Unrecognized content type '", mime_type, "'")
  }
  temp_video_file <- tempfile(fileext = ext)
  writeBin(video_data$bytes, temp_video_file)
  
  # 2. Extract audio and frames from the video
  update_progress("Extracting audio and frames...", 0.1)
  temp_audio_file <- tempfile(fileext = ".mp3")
  system2("ffmpeg", args = c("-i", temp_video_file, "-b:a", "64k", "-ac", "1", temp_audio_file))
  
  # Extract frames (2 fps)
  temp_frame_dir <- tempfile()
  dir.create(temp_frame_dir)
  av::av_encode_video(
    input = temp_video_file,
    output = file.path(temp_frame_dir, "frame%04d.jpg"),
    codec = "mjpeg",
    vfilter = "fps=2,scale='if(gt(iw,ih),512,-1)':'if(gt(ih,iw),512,-1)'"
  )
  
  # 3. Transcribe audio using Whisper API
  update_progress("Transcribing audio...", 0.2)
  user_prompt <- whisper(temp_audio_file)
  
  # 4. Prepare image data URIs
  image_uris <- lapply(list.files(temp_frame_dir, full.names = TRUE), from_file)
  
  # 5. Prepare the message for OpenAI API
  update_progress("Preparing API request...", 0.3)
  new_message <- list(
    role = "user",
    content = c(
      list(list(type = "text", text = user_prompt)),
      lapply(image_uris, function(uri) {
        list(
          type = "image_url",
          image_url = list(url = uri)
        )
      })
    )
  )
  
  all_messages <- c(messages, list(new_message))
  
  # Add system message
  system_message <- list(
    role = "system",
    content = paste(collapse = "\n", readLines("system_prompt.md", warn = FALSE))
  )

  all_messages <- c(list(system_message), all_messages)
  
  # cat(jsonlite::toJSON(all_messages, pretty=TRUE, auto_unbox = TRUE))

  # 6. Call OpenAI API
  update_progress("Waiting for response...", 0.4)
  openai_req <- request("https://api.openai.com/v1/chat/completions") %>%
    req_headers(
      Authorization = paste("Bearer", openai_api_key),
      "Content-Type" = "application/json"
    ) %>%
    req_body_json(list(
      model = "gpt-4o-mini",
      messages = all_messages,
      max_tokens = 300
    )) %>%
    req_error(body = function(resp) stop("Error in OpenAI API call: ", resp_body_json(resp)$error$message))
  
  openai_resp <- openai_req %>% req_perform()
  
  # Parse the response
  response_content <- resp_body_json(openai_resp)
  response_text <- response_content$choices[[1]]$message$content
  
  # 7. Text-to-speech conversion
  update_progress("Synthesizing audio...", 0.8)
  tts_req <- request("https://api.openai.com/v1/audio/speech") %>%
    req_headers(
      Authorization = paste("Bearer", openai_api_key),
      "Content-Type" = "application/json"
    ) %>%
    req_body_json(list(
      model = "tts-1",
      voice = "nova",
      input = response_text
    )) %>%
    req_error(body = function(resp) stop("Error in OpenAI TTS API call: ", resp_body_json(resp)$error$message))
  
  tts_resp <- tts_req %>% req_perform()
  
  # Save the audio to a temporary file and create a data URI
  temp_audio_file <- tempfile(fileext = ".mp3")
  writeBin(resp_body_raw(tts_resp), temp_audio_file)
  response_audio_uri <- from_file(temp_audio_file, "audio/mpeg")
  
  # 8. Clean up temporary files
  file.remove(temp_video_file, temp_audio_file)
  unlink(temp_frame_dir, recursive = TRUE)
  
  # 9. Return results
  update_progress("Done!", 1)
  list(
    audio_uri = response_audio_uri,
    messages = c(messages, list(list(role = "assistant", content = response_text)))
  )
}

# Helper functions (datauri and media_extractor equivalents)
from_file <- function(file_path, mime_type = NULL) {
  if (is.null(mime_type)) {
    mime_type <- mime::guess_type(file_path)
  }
  encoded_string <- base64enc::base64encode(readBin(file_path, "raw", file.info(file_path)$size))
  paste0("data:", mime_type, ";base64,", encoded_string)
}

parse_data_uri <- function(data_uri) {
  m <- regmatches(data_uri, regexec("^data:([^;]+).*?;base64,(.*)$", data_uri))
  if (length(m) == 0) {
    stop("Malformed data URI")
  }
  mime_type <- m[[1]][[2]]
  b64data <- m[[1]][[3]]
  decoded <- base64enc::base64decode(b64data)
  list(bytes = decoded, mime_type = mime_type)
}

as_tempfile <- function(data_uri) {
  parsed <- parse_data_uri(data_uri)
  temp_file <- tempfile(fileext = mime::guess_extension(parsed$mime_type))
  writeBin(parsed$bytes, temp_file)
  temp_file
}
