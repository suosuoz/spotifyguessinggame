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
  
  #Needed so we can run js code to 
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
    
    tags$title("Spotify Guessing Game"),
    
    #Allows "Submit" == press Enter Key
    tags$script(src = "enter_button.js"),
    tags$script(src = "enter_button1.js")
  ),
  
  
  
  headerPanel(
    HTML("<p>Spotify Guessing Game by suosuoz &nbsp;&nbsp;<a href='https://github.com/suosuoz/spotifyguessinggame' target='_blank'><img style='padding: 10px 0;' src=gh.png /></a></p> ")
  ),
  
  
  
  sidebarPanel(
    p("The goal of this game is to simply guess the artist or title of a song. If there are multiple artists listed on the song, you only need enter one of them. You will have thirty seconds to guess each song. If time runs out, please hit the submit button to go to the next track or submit an answer. Once you finish a playlist, you can enter a new playlist at the top, replay the same playlist, or switch to the other game type. Not all tracks have listening previews and  therefore cannot be included. Finally, there is a little bit of leniency, so don't worry if you mispell an artist or track name!"),
    br(),
    p("Retrieving a playlist: "),
    tags$ol(
      tags$li("Open Spotify"),
      tags$li("Copy and paste the Spotify URI or Playlist Link from your selected playlist (see image below for more details) into the text box  or use one of the example codes below"),
      tags$li("Hit the Submit button and start guessing!")
    ),
    br(),
    p("Example playlist links to use:"),
    p("Alt Megalist: 1x5H89xQejE46G1GOKqRo3"),
    p("00s Rock: 37i9dQZF1DX3oM43CtKnRV"),
    p("90s Rock: 37i9dQZF1DX1rVvRgjX59F"),
    p("Pop Hits: 37i9dQZF1DWTwnEm1IYyoj"),
    HTML("<img style='padding: 10px 0;' src=spot.png width=300px />"),
    width=5
  ),
  
  
  
  mainPanel(
    p(id = "head1", align="center", "Choose Your Game Type"),
    fluidRow(column(8, align="center", offset=2, actionButton("artistbutton", "Guess the Artist"),
                                                  actionButton("songbutton", "Guess the Song"))),
    br(),
    fluidRow(column(8, align="center", offset=2, textInput("txt2", label="Enter a Playlist Link",))),
    fluidRow(column(8, align="center", offset=2, actionButton("selectbutton", "Submit"))),
    fluidRow(column(8, align="center", offset=2, textInput("txt21", label="Enter a Playlist Link",))),
    fluidRow(column(8, align="center", offset=2, actionButton("selectbutton1", "Submit"))),
    br(),
    p(id = "jaja", align="center", "~~Please wait if you hit the Submit button~~"),
    p(id = "jajaja", align="center", "~~Larger playlists can take about ten seconds to load~~"),
    hr(),
    fluidRow(column(8, align="center", offset=2, actionButton("restart", "New Playlist"))),
    br(),
    br(),
    uiOutput("numsongs"),
    uiOutput("error"),
    uiOutput("error2"),
    br(),
    fluidRow(column(8, align="center", offset=2, uiOutput("my_audio"))),
    fluidRow(column(8, align="center", offset=2, tags$audio(id = "myaudio", controls = NA, autoplay = NA, src = ""))),
    fluidRow(column(8, align="center", offset=2, uiOutput("newplay"))),
    br(),
    fluidRow(column(8, align="center", offset=2, actionButton("playagainartist", "Use the Same Playlist"))),
    fluidRow(column(8, align="center", offset=2, actionButton("playagainsong", "Use the Same Playlist"))),
    br(),
    fluidRow(column(8, align="center", offset=2, actionButton("switchtoartist", "Switch to Artist"))),
    fluidRow(column(8, align="center", offset=2, actionButton("switchtosong", "Switch to Song"))),
    br(),
    fluidRow(column(8, align="center", offset=2, textInput("txt1", "Guess the Song Title"))),
    fluidRow(column(8, align="center", offset=2, actionButton("guessbutton", "Submit"))),
    fluidRow(column(8, align="center", offset=2, textInput("txt11", "Guess the Artist"))),
    fluidRow(column(8, align="center", offset=2, actionButton("guessbutton1", "Submit"))),
    br(),
    br(),
    fluidRow(column(12, align="center", offset=0, verbatimTextOutput("value"))),
    DT::dataTableOutput("songtable"),
    br(),
    width=7
  )
)





#Functions to create HTML elements
get_numsongs_tag <- function(nrowtot_allsongs, nrowtest, totallplaylist) {
  xy <- paste0("Your playlist has ", totallplaylist, " songs.", "\n", nrowtest, " out of ", nrowtot_allsongs, " available songs were sampled.")
  tags$p(xy, align="center")
}
get_error <- function() {
  tags$p("Invalid Playlist. Try Again!", align="center")
}
get_error2 <- function() {
  tags$p("All songs in the playlist do not have a preview!", align="center")
}
get_newplay <- function() {
  tags$p("Enter a new playlist above ",align="center")
}





#What runs when you enter the site
server <- function(input, output, session) {
  
  
  #Get my Spotify App ID and Secret
  Sys.setenv(SPOTIFY_CLIENT_ID = "182b5e445ae845498a60c3fa49b689a7")
  Sys.setenv(SPOTIFY_CLIENT_SECRET = "4b93c4ee1a2b4a0fb70eb0e56f7b7fa2")
  
  
  #Immediately lowers the media volume from 1 to 0.2 (default is too loud)
  runjs("document.getElementById('myaudio').volume = 0.2;")
  
  
  #Hide elements that aren't needed, prevents possible errors
  shinyjs::hide("restart")
  shinyjs::hide("playagainartist")
  shinyjs::hide("playagainsong")
  shinyjs::hide("guessbutton")
  shinyjs::hide("txt1")
  shinyjs::hide("myaudio")
  shinyjs::hide("txt2")
  shinyjs::hide("selectbutton")
  shinyjs::hide("jaja")
  shinyjs::hide("jajaja")
  shinyjs::hide("guessbutton1")
  shinyjs::hide("txt11")
  shinyjs::hide("txt21")
  shinyjs::hide("selectbutton1")
  shinyjs::hide("switchtoartist")
  shinyjs::hide("switchtosong")
  
  
  
  
  #Initialize varables
  
  #This is the history table
  n <- reactiveValues()
  n$df <- data.frame("Song" = character(0),"Artist" = character(0),"Guess" = character(0),"Correct" = character(0), "Round" = character(0),stringsAsFactors = FALSE)
  
  score <- 0
  score2 <- 0
  guesses <- 0
  guesses2 <- 0
  round <- 0
  test <- data.frame(char = character(), stringsAsFactors = FALSE)
  
  #This stores the playlist value so that the replay button works
  ttt_c <- NA
  
  
  #
  #
  #
  
  
  #Choose which gamemode is picked: song or artist
  observeEvent(input$songbutton, {
    shinyjs::show("txt2")
    shinyjs::show("selectbutton")
    shinyjs::show("jaja")
    shinyjs::show("jajaja")  
    shinyjs::hide("songbutton")
    shinyjs::hide("artistbutton")
    shinyjs::hide("head1")
  })
  
  observeEvent(input$artistbutton, {
    shinyjs::show("txt21")
    shinyjs::show("selectbutton1")
    shinyjs::show("jaja")
    shinyjs::show("jajaja")  
    shinyjs::hide("songbutton")
    shinyjs::hide("artistbutton")
    shinyjs::hide("head1")
  })
  
  
  #
  #
  #
  
  
  #Guess the Song
  #What occurs when you select a Playlist
  observeEvent(input$selectbutton, {
    shinyjs::hide("playagainsong")
    shinyjs::hide("switchtoartist")
    shinyjs::hide("error")
    shinyjs::hide("error2")
    
    #This var tracks the number of plays/score in the current sample
    guesses2 <<- 0
    score2 <<- 0
    round <<- round + 1
    
    #Initialize the list of songs in the playlist
    datalist = list()
    
    #Update the textbox to blank
    updateTextInput(session, "txt2", value = "")
    
    #Use the user input. Cut off the first part of the copy if the link is the URI, cut off the front and end if it is a playlist link
    ttt <- str_replace(input$txt2, "spotify:playlist:", "")
    ttt <- gsub("https://open.spotify.com/playlist/", "\\1",ttt)
    ttt <- gsub("\\?.*","\\1",ttt)
    
    #Check if the user's input is a valid playlist (has the correct length and is the correct format (tested by seeing if it links to a valid playlist))
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
      
      #Stores the playlist link for the replay button
      ttt_c <<- ttt
      
      #Spotify can only get 100 songs at a time, so calculate how many times we need to get playlists
      runs <- get_playlist(ttt)$tracks$total %/% 100 + 1
      
      #Get all the songs in the playlist and add them to a dataframe
      for (j in 1:runs) {
        allsongs <- data.frame(get_playlist_tracks(ttt, offset = 100 * (j - 1)))
        datalist[[j]] <- allsongs
      }
      tot_allsongs <- dplyr::bind_rows(datalist)
      
      totallplaylist <- nrow(tot_allsongs)
      

      #Remove songs that do not have a preview
      whichna <- which(is.na(tot_allsongs$track.preview_url))
      
      #If there are no NA, do not remove any songs
      if(!(length(whichna) == 0) ){
        tot_allsongs <- tot_allsongs[-c(whichna), ]    
      }
      
      #If all the songs in the playlist are invalid, error; else run correctly
      if(nrow(tot_allsongs) == 0){
        output$error <- renderUI(get_error())
        shinyjs::show("error")
        output$error2 <- renderUI(get_error2())
        shinyjs::show("error2")
      } else{
        
        #Sample either N or the number of valid songs in the playlist
        samp <- sample(1:nrow(tot_allsongs), min(25, nrow(tot_allsongs)), replace = FALSE)
        
        #Store the valid songs in TEST
        test <- tot_allsongs[samp, ]
        
        #Choose the first song and play it
        shinyjs::show("myaudio")
        songlink <- test[guesses2 + 1, ]$track.preview_url
        runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
        
                #Show how many songs were sampled out of the total in the playlist
        output$numsongs <- renderUI(get_numsongs_tag(nrow(tot_allsongs), nrow(test), totallplaylist))
        
        #Allow access outside of this function
        test <<- test
        
        #Show/Hide HTML elements
        shinyjs::show("restart")
        shinyjs::show("guessbutton")
        shinyjs::show("txt1")
        shinyjs::show("numsongs")
        shinyjs::hide("newplay")
        shinyjs::hide("selectbutton")
        shinyjs::hide("txt2")
        shinyjs::hide("mytable")
        shinyjs::hide("jaja")
        shinyjs::hide("jajaja")
      }
    } else{
      
      #If the playlist is invalid, show that there was an error
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

    
    #Record the number of guesses (total and round)
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
    
    
    #Clean the song string (remove all after a dash or parentheses. Change & and + to and)
    song2 <- song
    song2 <- str_replace(song2, " -.*", "")
    song2 <- str_replace(song2, " \\(.*", "")
    song2 <- str_replace(song2, "&","and")
    song2 <- str_replace(song2, "\\+","and")
    

    #Convert the user input and artist data to lowercase
    twicexx <- tolower(song2)
    once2 <- tolower(input$txt1)
    
    
    #Check if the user input matches the song title. The text matching software is more lenient if the string is longer
    if (str_length(twicexx) < 6 | is.na(twicexx)) {
      txtcheckxx <- amatch(once2, twicexx, maxDist = 2) == 1
    } else{
      txtcheckxx <- amatch(once2, twicexx, maxDist = 4) == 1
    }
    
    
    #Convert NA values to FALSE
    if (is.na(txtcheckxx)) {
      txtcheckxx <- FALSE
    }
    
    
    #Update the score and other vars depending on if the user was correct
    if (txtcheckxx == TRUE) {
      score <<- score + 1
      score2 <<- score2 + 1
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
      isolate(n$df[nrow(n$df) + 1, ] <- c(song, artist[1, 1], input$txt1, ret2, round))
    })
    
    
    #Print a message based on if the user was correct and how many artists there are
    output$value <- renderText({
      
      if(round < 2){
      
        if (nrow(artist) == 1) {
          paste0(ret, " That was \"", song, "\" by ", artist, "\n", "Score: ", score, "/", guesses, "\n")
        } else if (nrow(artist) == 2) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], " & ", artist[2, 1], "\n", "Score: ", score, "/", guesses, "\n")
        } else if (nrow(artist) == 3) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], " & ", artist[3, 1], "\n", "Score: ", score, "/", guesses, "\n")
        } else if (nrow(artist) == 4) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], " & ", artist[4, 1], "\n", "Score: ", score, "/", guesses, "\n")
        } else if (nrow(artist) == 5) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], ", ", artist[4, 1], " & ", artist[5, 1], "\n", "Score: ", score, "/", guesses, "\n")
        } else{
          paste0(ret, " That was \"", song, "\" by ", artist[1,1], " and others", "\n", "Score: ", score, "/", guesses, "\n")
        }
          
      } else{
        if (nrow(artist) == 1) {
          paste0(ret, " That was \"", song, "\" by ", artist, "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        } else if (nrow(artist) == 2) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], " & ", artist[2, 1], "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        } else if (nrow(artist) == 3) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], " & ", artist[3, 1], "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        } else if (nrow(artist) == 4) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], " & ", artist[4, 1], "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        } else if (nrow(artist) == 5) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], ", ", artist[4, 1], " & ", artist[5, 1], "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        } else{
          paste0(ret, " That was \"", song, "\" by ", artist[1,1], " and others", "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        }
          
      }
    })
    
    
    #Create the datatable of previous songs tested
    output$songtable <- DT::renderDataTable({
      DT::datatable(n$df, options = list(pageLength = 100,lengthChange=FALSE)) %>%
        formatStyle(columns = c(3:9), 'text-align' = 'center')
    })
    
    
    #Update to the next song in the sample
    songlink <- test[guesses2 + 1, ]$track.preview_url
    runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
    
    
    #What to do if the sample has run out
    if (guesses2  == nrow(test)) {
      shinyjs::hide("restart")
      shinyjs::hide("guessbutton")
      shinyjs::hide("txt1")
      shinyjs::hide("myaudio")
      shinyjs::hide("numsongs")
      shinyjs::show("newplay")
      shinyjs::show("selectbutton")
      shinyjs::show("txt2")
      shinyjs::show("mytable")
      shinyjs::show("jaja")
      shinyjs::show("jajaja")
      shinyjs::show("switchtoartist")
      shinyjs::show("playagainsong")
      output$newplay <- renderUI(get_newplay())
    }
  })
  
  
  
  #
  #
  #
  
  
  
  #Guess the Artist
  #What occurs when you select a Playlist
  observeEvent(input$selectbutton1, {
    shinyjs::hide("playagainartist")
    shinyjs::hide("switchtosong")
    shinyjs::hide("error")
    shinyjs::hide("error2")
    
    #This var tracks the number of plays/score in the current sample
    guesses2 <<- 0
    score2 <<- 0
    round <<- round + 1
    datalist = list()
    
    
    #Updates the text in the textbox to blank
    updateTextInput(session, "txt21", value = "")
    
    #Use the user input. Cut off the first part of the copy
    ttt <- str_replace(input$txt21, "spotify:playlist:", "")
    ttt <- gsub("https://open.spotify.com/playlist/", "\\1",ttt)
    ttt <- gsub("\\?.*","\\1",ttt)
    
    
    #Check if the user's input a valid playlist (has the correct length and is the correct format (tested by seeing if it links to a valid playlist))
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
      ttt_c <<- ttt
      #Spotify can only get 100 songs at a time, so calculate how many times we need to get playlists
      runs <- get_playlist(ttt)$tracks$total %/% 100 + 1
      
      #Get all the songs in the playlist and add them to a dataframe
      for (j in 1:runs) {
        allsongs <- data.frame(get_playlist_tracks(ttt, offset = 100 * (j - 1)))
        datalist[[j]] <- allsongs
      }
      tot_allsongs <- dplyr::bind_rows(datalist)
      
      totallplaylist <- nrow(tot_allsongs)
      

      #Remove songs that do not have a preview, necessary to redefine
      whichna <- which(is.na(tot_allsongs$track.preview_url))
      
      #If there is no NA, do not remove any songs
      if(!(length(whichna) == 0) ){
        tot_allsongs <- tot_allsongs[-c(whichna), ]    
      }
      
      #If all the songs in the playlist are invalid, error; else run correctly
      if(nrow(tot_allsongs) == 0){
        output$error <- renderUI(get_error())
        shinyjs::show("error")
        output$error2 <- renderUI(get_error2())
        shinyjs::show("error2")
      } else{
        
        #Sample either N or the number of valid songs in the playlist
        samp <- sample(1:nrow(tot_allsongs), min(25, nrow(tot_allsongs)), replace = FALSE)
        test <- tot_allsongs[samp, ]
        
        
        #Guesses2 = 0. Choose the first song and play it
        shinyjs::show("myaudio")
        songlink <- test[guesses2 + 1, ]$track.preview_url
        runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
        
        
        #Show how many songs were sampled out of the total in the playlist
        output$numsongs <- renderUI(get_numsongs_tag(nrow(tot_allsongs), nrow(test), totallplaylist))
        
        test <<- test
        
        shinyjs::show("restart")
        shinyjs::show("guessbutton1")
        shinyjs::show("txt11")
        shinyjs::show("numsongs")
        shinyjs::hide("newplay")
        shinyjs::hide("selectbutton1")
        shinyjs::hide("txt21")
        shinyjs::hide("mytable")
        shinyjs::hide("jaja")
        shinyjs::hide("jajaja")
      }
    } else{
      
      #If the playlist is not valid, show that there was an error
      output$error <- renderUI(get_error())
      shinyjs::show("error")
    }
  })
  
  
  #What occurs when the guess artist button is pressed
  observeEvent(input$guessbutton1, {
    
    #Allows "Submit" == press Enter Key
    value <- eventReactive(input$guessbutton1, {
      input$txt11
    })
    
    
    #Record a guess
    guesses2 <<- guesses2 + 1
    guesses <<- guesses + 1
    
    
    #Use our dataset to get artist, song info
    rv <- reactiveValues(t = test[guesses2, ])
    updateTextInput(session, "txt11", value = "")
    
    
    #Get the artist
    aj <- rv$t[9]
    artist <- aj[[1]][[1]][3]
    
    
    #Get the song
    song <- rv$t[18]
    
    
    #Convert the user input and artist data to lowercase
    once2 <- tolower(input$txt11)
    
    once2 <- str_replace(once2, "&","and")
    once2 <- str_replace(once2, "\\+","and")
    
    twicex <- tolower(artist[1, 1])
    twicex2 <- tolower(artist[2, 1])
    twicex3 <- tolower(artist[3, 1])
    twicex4 <- tolower(artist[4, 1])
    twicex5 <- tolower(artist[5, 1])
    
    
    #Check if the user input matches any of the artists on the track. The text matching software is more lenient if the string is longer
    if (str_length(twicex) < 6 | is.na(twicex)) {
      txtcheckx <- amatch(once2, twicex, maxDist = 2) == 1
    } else{
      txtcheckx <- amatch(once2, twicex, maxDist = 4) == 1
    }
    if (str_length(twicex2) < 6 | is.na(twicex2)) {
      txtcheckx2 <- amatch(once2, twicex2, maxDist = 2) == 1
    } else{
      txtcheckx2 <- amatch(once2, twicex2, maxDist = 4) == 1
    }
    if (str_length(twicex3) < 6 | is.na(twicex3)) {
      txtcheckx3 <- amatch(once2, twicex3, maxDist = 2) == 1
    } else{
      txtcheckx3 <- amatch(once2, twicex3, maxDist = 4) == 1
    }
    if (str_length(twicex4) < 6 | is.na(twicex4)) {
      txtcheckx4 <- amatch(once2, twicex4, maxDist = 2) == 1
    } else{
      txtcheckx4 <- amatch(once2, twicex4, maxDist = 4) == 1
    }
    if (str_length(twicex5) < 6 | is.na(twicex5)) {
      txtcheckx5 <- amatch(once2, twicex5, maxDist = 2) == 1
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
    
    
    #Update the score and other vars depending on if the user was correct
    if (txtcheckx == TRUE | txtcheckx2 == TRUE | txtcheckx3 == TRUE | txtcheckx4 == TRUE | txtcheckx5 == TRUE) {
      score <<- score + 1
      score2 <<- score2 + 1
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
      isolate(n$df[nrow(n$df) + 1, ] <- c(song, artist[1, 1], input$txt11, ret2, round))
    })
    
    
    #Print a message based on if the user was correct and how many artists there are
    output$value <- renderText({
      
      if(round < 2){
      
        if (nrow(artist) == 1) {
          paste0(ret, " That was \"", song, "\" by ", artist, "\n", "Score: ", score, "/", guesses, "\n")
        } else if (nrow(artist) == 2) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], " & ", artist[2, 1], "\n", "Score: ", score, "/", guesses, "\n")
        } else if (nrow(artist) == 3) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], " & ", artist[3, 1], "\n", "Score: ", score, "/", guesses, "\n")
        } else if (nrow(artist) == 4) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], " & ", artist[4, 1], "\n", "Score: ", score, "/", guesses, "\n")
        } else if (nrow(artist) == 5) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], ", ", artist[4, 1], " & ", artist[5, 1], "\n", "Score: ", score, "/", guesses, "\n")
        } else{
          paste0(ret, " That was \"", song, "\" by ", artist[1,1], " and others", "\n", "Score: ", score, "/", guesses, "\n")
        }
      } else{
        if (nrow(artist) == 1) {
          paste0(ret, " That was \"", song, "\" by ", artist, "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        } else if (nrow(artist) == 2) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], " & ", artist[2, 1], "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        } else if (nrow(artist) == 3) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], " & ", artist[3, 1], "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        } else if (nrow(artist) == 4) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], " & ", artist[4, 1], "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        } else if (nrow(artist) == 5) {
          paste0(ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], ", ", artist[4, 1], " & ", artist[5, 1], "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        } else{
          paste0(ret, " That was \"", song, "\" by ", artist[1,1], " and others", "\n", "Score: ", score, "/", guesses, "\n", "Score This Round: ",score2, "/",guesses2)
        }
        
        
        
      }
      
      
    })
    
    
    #Create the datatable of previous songs tested
    output$songtable <- DT::renderDataTable({
      DT::datatable(n$df, options = list(pageLength = 100,lengthChange=FALSE)) %>%
        formatStyle(columns = c(3:9), 'text-align' = 'center')
    })
    
    
    #Update to the next song in the sample
    songlink <- test[guesses2 + 1, ]$track.preview_url
    runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
    
    
    #What to do if the sample has run out
    if (guesses2  == nrow(test)) {
      shinyjs::hide("restart")
      shinyjs::hide("guessbutton1")
      shinyjs::hide("txt11")
      shinyjs::hide("myaudio")
      shinyjs::hide("numsongs")
      shinyjs::show("newplay")
      shinyjs::show("selectbutton1")
      shinyjs::show("txt21")
      shinyjs::show("mytable")
      shinyjs::show("jaja")
      shinyjs::show("jajaja")
      shinyjs::show("switchtosong")
      shinyjs::show("playagainartist")
      output$newplay <- renderUI(get_newplay())
    }
  })
  
  
  #
  #
  #
  
  
  #Replay Buttons
  observeEvent(input$playagainsong, {
    shinyjs::hide("playagainsong")
    shinyjs::hide("switchtoartist")
    shinyjs::hide("error")
    shinyjs::hide("error2")
    
    #This var tracks the number of plays in the current sample
    guesses2 <<- 0
    score2 <<- 0
    round <<- round + 1
    datalist = list()
    
    
    #We know that the playlist is already valid. Import that value
    ttt <- ttt_c
    txtcheck <- TRUE

    
    #If the playlist is valid
    if (txtcheck == TRUE) {
      ttt_c <<- ttt
      #Spotify can only get 100 songs at a time, so calculate how many times we need to get playlists
      runs <- get_playlist(ttt)$tracks$total %/% 100 + 1
      
      #Get all the songs in the playlist and add them to a dataframe
      for (j in 1:runs) {
        allsongs <- data.frame(get_playlist_tracks(ttt, offset = 100 * (j - 1)))
        datalist[[j]] <- allsongs
      }
      tot_allsongs <- dplyr::bind_rows(datalist)
      
      totallplaylist <- nrow(tot_allsongs)
      
      
      #Remove songs that do not have a preview, necessary to redefine
      whichna <- which(is.na(tot_allsongs$track.preview_url))
      
      #If there is no NA, do not remove any songs
      if(!(length(whichna) == 0) ){
        tot_allsongs <- tot_allsongs[-c(whichna), ]    
      }
      
      #If all the songs in the playlist are invalid, error; else run correctly
      if(nrow(tot_allsongs) == 0){
        output$error <- renderUI(get_error())
        shinyjs::show("error")
        output$error2 <- renderUI(get_error2())
        shinyjs::show("error2")
      } else{
        
        #Sample either N or the number of valid songs in the playlist
        samp <- sample(1:nrow(tot_allsongs), min(25, nrow(tot_allsongs)), replace = FALSE)
        test <- tot_allsongs[samp, ]
        
        
        #Guesses2 = 0. Choose the first song and play it
        shinyjs::show("myaudio")
        songlink <- test[guesses2 + 1, ]$track.preview_url
        runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
        
        
        #Show how many songs were sampled out of the total in the playlist
        output$numsongs <- renderUI(get_numsongs_tag(nrow(tot_allsongs), nrow(test), totallplaylist))
        
        
        test <<- test
        
        shinyjs::show("restart")
        shinyjs::show("guessbutton")
        shinyjs::show("txt1")
        shinyjs::show("numsongs")
        shinyjs::hide("newplay")
        shinyjs::hide("selectbutton")
        shinyjs::hide("txt2")
        shinyjs::hide("mytable")
        shinyjs::hide("jaja")
        shinyjs::hide("jajaja")
      }
    } else{
      
      #If the playlist is not valid, show that there was an error
      output$error <- renderUI(get_error())
      shinyjs::show("error")
    }
    
  })
  

  observeEvent(input$playagainartist, {
    shinyjs::hide("playagainartist")
    shinyjs::hide("switchtosong")
    shinyjs::hide("error")
    shinyjs::hide("error2")
    
    #This var tracks the number of plays in the current sample
    guesses2 <<- 0
    score2 <<- 0
    round <<- round + 1
    datalist = list()
    
    
    #Import the last playlist
    ttt <- ttt_c
    
    
    #Check if the user's input a valid playlist (has the correct length and is the correct format (tested by seeing if it links to a valid playlist))
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
      
      totallplaylist <- nrow(tot_allsongs)
      
      
      #Remove songs that do not have a preview, necessary to redefine
      whichna <- which(is.na(tot_allsongs$track.preview_url))
      
      #If there is no NA, do not remove any songs
      if(!(length(whichna) == 0) ){
        tot_allsongs <- tot_allsongs[-c(whichna), ]    
      }
      
      #If all the songs in the playlist are invalid, error; else run correctly
      if(nrow(tot_allsongs) == 0){
        output$error <- renderUI(get_error())
        shinyjs::show("error")
        output$error2 <- renderUI(get_error2())
        shinyjs::show("error2")
      } else{
        
        #Sample either N or the number of valid songs in the playlist
        samp <- sample(1:nrow(tot_allsongs), min(25, nrow(tot_allsongs)), replace = FALSE)
        test <- tot_allsongs[samp, ]
        
        
        #Guesses2 = 0. Choose the first song and play it
        shinyjs::show("myaudio")
        songlink <- test[guesses2 + 1, ]$track.preview_url
        runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
        
        
        #Show how many songs were sampled out of the total in the playlist
        output$numsongs <- renderUI(get_numsongs_tag(nrow(tot_allsongs), nrow(test), totallplaylist))
        
        test <<- test
        
        shinyjs::show("restart")
        shinyjs::show("guessbutton1")
        shinyjs::show("txt11")
        shinyjs::show("numsongs")
        shinyjs::hide("newplay")
        shinyjs::hide("selectbutton1")
        shinyjs::hide("txt21")
        shinyjs::hide("mytable")
        shinyjs::hide("jaja")
        shinyjs::hide("jajaja")
      }
    } else{
      
      #If the playlist is not valid, show that there was an error
      output$error <- renderUI(get_error())
      shinyjs::show("error")
    }
    
  })
  
  
  #
  #
  #
  
  
  #Restart during a current play through
  observeEvent(input$restart, {
    #Set the current song to NA so no sound is playing
    songlink <- NA
    runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
    
    #Show/Hide appropriate HTML elements
    shinyjs::show("head1")
    shinyjs::show("songbutton")
    shinyjs::show("artistbutton")
    shinyjs::hide("playagainsong")
    shinyjs::hide("playagainartist")
    shinyjs::hide("myaudio")
    shinyjs::hide("restart")
    shinyjs::hide("guessbutton")
    shinyjs::hide("txt1")
    shinyjs::hide("guessbutton1")
    shinyjs::hide("txt11")
    shinyjs::hide("numsongs")
  })
  
  #Swtich UI to Song verison
  observeEvent(input$switchtosong, {
    shinyjs::hide("playagainartist")
    shinyjs::hide("switchtosong")
    shinyjs::hide("selectbutton1")
    shinyjs::hide("txt21")
    shinyjs::show("selectbutton")
    shinyjs::show("txt2")
  })
  
  #Switch UI to Artist version
  observeEvent(input$switchtoartist, {
    shinyjs::hide("playagainsong")
    shinyjs::hide("switchtoartist")
    shinyjs::hide("selectbutton")
    shinyjs::hide("txt2")
    shinyjs::show("selectbutton1")
    shinyjs::show("txt21")
  })
  
}

shinyApp(ui = ui, server = server)
