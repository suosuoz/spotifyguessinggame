#Import Libraries
library(shiny)
library(DT)
library(spotifyr)
library(dplyr)
library(stringdist)
library(shinyjs)
library(stringr)
library(httr)

#The HTML/CSS of the Website
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'open sans light', Verdana, sans-serif;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
        text-align: center;
        padding-bottom: 20px;
      }

    ")),
    
    #Allows "Submit" == press Enter Key
    tags$head(tags$script(src = "enter_button.js")) 
    
  ),
  
  headerPanel(
    "Spotify Guessing Game by suosuoz"
  ),
  
  sidebarPanel(
    p("This is your formal volume warning."),
    p("The goal of this game is to simply guess the artist of a song. If there are multiple artists on the track, just enter any one singer. You will have thirty seconds to guess each song. If time runs out, please hit the submit button to go to the next track. Once you finish a playlist, you can enter a new URI to keep playing or reenter the same URI to play it again. Finally, there is a little bit of leniency, so don't worry if you mispell an artist!"),
    br(),
    p("Instructions: "),
    
    tags$ol(
      tags$li("Open Spotify"),
      tags$li("Copy the Spotify URI link from a playlist (see image below) or use one of the codes below"),
      tags$li("Enter the playlist and start guessing!")
    ),
    
    p("Example playlist links to use:"),
    p("Alt Megalist: spotify:playlist:1x5H89xQejE46G1GOKqRo3"),
    p("00s Rock: spotify:playlist:37i9dQZF1DX3oM43CtKnRV"),
    p("90s Rock: spotify:playlist:37i9dQZF1DX1rVvRgjX59F"),
    p("Pop Hits: spotify:playlist:37i9dQZF1DWTwnEm1IYyoj"),
    
    
    HTML("<img style='padding: 10px 0;' src=spot.png width=300px />"),
    br(),
    p("*Not all tracks have listening previews and are therefore not included"), 
    width=5
  ),
  
  mainPanel(
    fluidRow(column(8, align="center", offset=2, 
                    textInput("txt2", label="Enter a Playlist Link",))),
    fluidRow(column(8, align="center", offset=2, 
                    actionButton("selectbutton", "Submit"))),
    br(),
    p(id = "jaja", align="center", "~~Please wait if you hit the button~~"),
    p(id = "jajaja", align="center", "~~Larger playlists can take around ten secs~~"),
    hr(),
    uiOutput("numsongs"),
    uiOutput("error"),
    br(),
    fluidRow(column(8, align="center", offset=2,
                    uiOutput("my_audio"))),
    fluidRow(column(8, align="center", offset=2,
                    uiOutput("newplay"))),
    br(),
    fluidRow(column(8, align="center", offset=2,
                    textInput("txt1", "Guess the Artist"))),
    fluidRow(column(8, align="center", offset=2,
                    actionButton("guessbutton", "Submit"))),
    br(),
    br(),
    fluidRow(column(12, align="center", offset=0,
                    verbatimTextOutput("value"))),
    DT::dataTableOutput("songtable"),
    br(),
    width=7
  )
)


#Functions to create HTML elements
get_audio_tag <- function(filename) {
  tags$audio(
    src = filename,
    type = "audio/mpeg",
    controls = "controls",
    autoplay = "autoplay"
  )
}
get_numsongs_tag <- function(nrowtot_allsongs, nrowtest) {
  xy <- paste0(nrowtest, " out of ", nrowtot_allsongs, " available songs were sampled*")
  tags$p(xy, align="center")
}
get_error <- function() {
  tags$p("Invalid Playlist. Try Again!", align="center")
}
get_newplay <- function() {
  tags$p("Select a new playlist to continue!",align="center")
}


#What runs when you enter the site
server <- function(input, output, session) {
  
  #Get my Spotify App ID and Secret
  Sys.setenv(SPOTIFY_CLIENT_ID = "YOUR_CLIENT_ID")
  Sys.setenv(SPOTIFY_CLIENT_SECRET = "YOUR_CLIENT_SECRET")
  
  
  #Hide elements to prevent errors
  shinyjs::hide("guessbutton")
  shinyjs::hide("txt1")
  
  
  #Initialize varables
  n <- reactiveValues()
  n$df <- data.frame("Song" = character(0),"Artist" = character(0),"Guess" = character(0),"Correct" = character(0),stringsAsFactors = FALSE)
  score <- 0
  guesses <- 0
  guesses2 <- 0
  test <- data.frame(char = character(), stringsAsFactors = FALSE)
  
  
  #What occurs when you select a Playlist
  observeEvent(input$selectbutton, {
    
    #This var tracks the number of plays in the current sample
    guesses2 <<- 0
    
    shinyjs::hide("error")
    datalist = list()
    
    
    #Use the user input. Cut off the first part of the copy
    updateTextInput(session, "txt2", value = "")
    ttt <- str_replace(input$txt2, "spotify:playlist:", "")
    
    
    #Check if the user's input a valid playlist (has the correct length and is the correct format (tested by seeing if it can go to a valid playlist))
    txtcheck <- TRUE
    url1 <- paste0("https://open.spotify.com/playlist/",ttt)
    
    if(str_length(ttt) != 22){
      txtcheck <- FALSE
    }
    if(http_error(url1)){
      txtcheck <- FALSE
    }

    
    #If the playlist is valid
    if (txtcheck == TRUE) {
      
      #Spotify can only get 100 songs at a time, so calculate how many times we need to get playlists
      runs <- get_playlist(ttt)$tracks$total %/% 100 + 1
      
      #Get all the songs in the playlist and add them to a dataframe
      for (j in 1:runs) {
        allsongs <- data.frame(get_playlist_tracks(ttt, offset = 100 * (j - 1)))
        datalist[[j]] <- allsongs
      }
      tot_allsongs <- dplyr::bind_rows(datalist)
      
      #Remove songs that do not have a preview
      whichna <- which(is.na(tot_allsongs$track.preview_url))
      tot_allsongs <- tot_allsongs[-c(whichna), ]
      
      #Sample either N or the min(if it is less than N) from the playlist
      samp <- sample(1:nrow(tot_allsongs), min(25, nrow(tot_allsongs)), replace = FALSE)
      test <- tot_allsongs[samp, ]
      
      #Guesses2 = 0. Choose the first song and play it
      songlink <- test[guesses2 + 1, ]$track.preview_url
      output$my_audio <- renderUI(get_audio_tag(songlink))
      shinyjs::show("my_audio")
      
      #Show how many songs were sampled out of the total in the playlist
      output$numsongs <- renderUI(get_numsongs_tag(nrow(tot_allsongs), nrow(test)))
      
      
      test <<- test
      shinyjs::show("guessbutton")
      shinyjs::show("txt1")
      shinyjs::show("numsongs")
      shinyjs::hide("newplay")
      shinyjs::hide("selectbutton")
      shinyjs::hide("txt2")
      shinyjs::hide("mytable")
      shinyjs::hide("jaja")
      shinyjs::hide("jajaja")
    } else{
      
      #If the playlist is not valid, show that there was an error
      output$error <- renderUI(get_error())
      shinyjs::show("error")
    }
  })
  
  
  #What occurs when the guess song button is pressed
  observeEvent(input$guessbutton, {
    
    #Allows "Submit" == press Enter Key
    value <- eventReactive(input$guessbutton, {
      input$txt1
    })
    
    
    
    
    #Record a guess
    guesses2 <<- guesses2 + 1
    guesses <<- guesses + 1
    
    #Use our dataset to get artist, song info
    rv <- reactiveValues(t = test[guesses2, ])
    updateTextInput(session, "txt1", value = "")
    
    #Get the artist
    aj <- rv$t[9]
    artist <- aj[[1]][[1]][3]
    
    #Get the song
    song <- rv$t[18]
    
    #Convert the user input and artist data to lowercase
    once2 <- tolower(input$txt1)
    twicex <- tolower(artist[1, 1])
    twicex2 <- tolower(artist[2, 1])
    twicex3 <- tolower(artist[3, 1])
    twicex4 <- tolower(artist[4, 1])
    twicex5 <- tolower(artist[5, 1])
    
    #Check if the user input matches any of the artists on the track. The text matching software is more lenient if the string is longer
    if (str_length(twicex) < 6 | is.na(twicex)) {
      txtcheckx <- amatch(once2, twicex, maxDist = 1) == 1
    } else{
      txtcheckx <- amatch(once2, twicex, maxDist = 4) == 1
    }
    if (str_length(twicex2) < 6 | is.na(twicex2)) {
      txtcheckx2 <- amatch(once2, twicex2, maxDist = 1) == 1
    } else{
      txtcheckx2 <- amatch(once2, twicex2, maxDist = 4) == 1
    }
    if (str_length(twicex3) < 6 | is.na(twicex3)) {
      txtcheckx3 <- amatch(once2, twicex3, maxDist = 1) == 1
    } else{
      txtcheckx3 <- amatch(once2, twicex3, maxDist = 4) == 1
    }
    if (str_length(twicex4) < 6 | is.na(twicex4)) {
      txtcheckx4 <- amatch(once2, twicex4, maxDist = 1) == 1
    } else{
      txtcheckx4 <- amatch(once2, twicex4, maxDist = 4) == 1
    }
    if (str_length(twicex5) < 6 | is.na(twicex5)) {
      txtcheckx5 <- amatch(once2, twicex5, maxDist = 1) == 1
    } else{
      txtcheckx5 <- amatch(once2, twicex5, maxDist = 4) == 1
    }
    
    
    #Convert NA values to FALSE
    if (is.na(txtcheckx)) {
      txtcheckx <- FALSE
    }
    if (is.na(txtcheckx2)) {
      txtcheckx2 <- FALSE
    }
    if (is.na(txtcheckx3)) {
      txtcheckx3 <- FALSE
    }
    if (is.na(txtcheckx4)) {
      txtcheckx4 <- FALSE
    }
    if (is.na(txtcheckx5)) {
      txtcheckx5 <- FALSE
    }
    
    
    #Updat the score and other vars depending on if the user was correct
    if (txtcheckx == TRUE | txtcheckx2 == TRUE | txtcheckx3 == TRUE | txtcheckx4 == TRUE | txtcheckx5 == TRUE) {
      score <<- score + 1
      y <- 0
      ret <- "Correct!"
      ret2 <- "Yes"
    } else{
      y <- 1
      ret <- "Wrong :("
      ret2 <- "No"
    }
    
    #Create a new row on the table
    newEntry <- observe({
      isolate(n$df[nrow(n$df) + 1, ] <-
                c(song, artist[1, 1], input$txt1, ret2))
    })
    
    #Print a message based on if the user was correct and how many artists there are
    output$value <- renderText({
      if (nrow(artist) == 1) {
        paste0(ret, " That was \"", song, "\" by ", artist, "\n", "Score: ", score, "/", guesses, "\n")
      } else if (nrow(artist) == 2) {
        paste0(ret, " That was \"", song, "\" by ", artist[1, 1], " & ", artist[2, 1], "\n", "Score: ", score, "/", guesses, "\n")
      } else if (nrow(artist) == 3) {
        paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], " & ", artist[3, 1], "\n", "Score: ", score, "/", guesses, "\n")
      } else if (nrow(artist) == 4) {
        paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], " & ", artist[4], "\n", "Score: ", score, "/", guesses, "\n")
      } else if (nrow(artist) == 5) {
        paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], ", ", artist[4, 1], " & ", artist[5, 1], "\n", "Score: ", score, "/", guesses, "\n")
      } else{
        paste0(ret, " That was \"", song, "\" by ", artist[1,1], " and others", "\n", "Score: ", score, "/", guesses, "\n")
      }
    })
    
    #Create the datatable of previous songs tested
    output$songtable <- DT::renderDataTable({
      DT::datatable(n$df, options = list(pageLength = 100,lengthChange=FALSE)) %>%
        formatStyle(columns = c(3:9), 'text-align' = 'center')
    })
    
    #Update to the next song in the sample
    songlink <- test[guesses2 + 1, ]$track.preview_url
    output$my_audio <- renderUI(get_audio_tag(songlink))
    
    #What to do if the sample has run out
    if (guesses2  == nrow(test)) {
      shinyjs::hide("guessbutton")
      shinyjs::hide("txt1")
      shinyjs::hide("my_audio")
      shinyjs::hide("numsongs")
      shinyjs::show("newplay")
      shinyjs::show("selectbutton")
      shinyjs::show("txt2")
      shinyjs::show("mytable")
      shinyjs::show("jaja")
      shinyjs::show("jajaja")
      output$newplay <- renderUI(get_newplay())
    }
  })
  
}
shinyApp(ui = ui, server = server)
