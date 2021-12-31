#Spotify Guessing Game
#Matt Matsuo


###Import Libraries
library(shiny)
library(DT)
library(spotifyr)
library(dplyr)
library(stringdist)
library(shinyjs)
library(stringr)
library(httr)


#
#
#

#
#
#


###Setup the structure of the website with HTML/CSS
ui <- fluidPage(
  
  ###Allows use of javascript 
  useShinyjs(),
  
  
  #
  #
  #
  
  #
  #
  #
  
  
  ###Header - Green title with Github link
  tags$head
  (
    tags$style
    (HTML
      ("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      h1 
      {
        font-family: 'open sans light', Verdana, sans-serif;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
        text-align: center;
        padding-bottom: 20px;
      }
      ")
    ),
    
    tags$title("Spotify Guessing Game"),
    
    tags$script(src = "enter_button.js"),     #Allows the use of the Enter key in place of clicking the "Submit" button - gts_but_submit_guess
    tags$script(src = "enter_button1.js"),    #Allows the use of the Enter key in place of clicking the "Submit" button - gta_but_submit_guess
    tags$script(src = "enter_button2.js"),    #Allows the use of the Enter key in place of clicking the "Submit" button - gts_but_submit_numsongs
    tags$script(src = "enter_button3.js"),    #Allows the use of the Enter key in place of clicking the "Submit" button - gta_but_submit_numsongs
    tags$script(src = "enter_button4.js"),    #Allows the use of the Enter key in place of clicking the "Submit" button - gta_but_submit_playlist
    tags$script(src = "enter_button5.js"),    #Allows the use of the Enter key in place of clicking the "Submit" button - gts_but_submit_playlist
  ),
  
  headerPanel(HTML("<p>Spotify Guessing Game by suosuoz &nbsp;&nbsp;<a href='https://github.com/suosuoz/spotifyguessinggame' target='_blank'><img style='padding: 10px 0;' src=gh.png /></a></p> ")),
  
  
  #
  #
  #
  
  #
  #
  #
  
  
  ###Left Side Panel
  sidebarPanel
  (
    p("Test your music knowledge! Grap your favorite playlist and see if you actually know the music you claim to be a fan of."),
    br(),
    p("Follow these simple steps to retrieve a playlist: "),
    tags$ol
    (
      tags$li("Open Spotify"),
      tags$li("Copy and paste the link from your selected playlist (see image below)"),
      tags$li("Hit the Submit button and start guessing!")
    ),
    br(),
    p("Other tips for using this site:"),
    tags$ol(
      tags$li("Your playlist must be made public (right click the playlist and click 'Add to profile')"),
      tags$li("Some Spotify generated playlists are not eligible. To play those, select all the songs (ctrl+a) and copy and paste them into a new playlist"),
      tags$li("If there are multiple artists listed on the song, you only need to enter one of the names to get it correct."),
      tags$li("Unfortunately, not all tracks have listening previews which means they are not available for this game."),
      tags$li("There is a little bit of leniency, so don't worry if you mispell anything. We got you covered."),
      tags$li("Hit the Enter key to submit a song. It's faster than having to click the Submit button!"),
      tags$li("There is no time limit. Use as much as you need to dig into the depths of your brain.")
      ),
    br(),
    p("Example playlist links to use:"),
    p("Suo's Favorites: 5Ne9CRbMkDcICC6jVDVGB7"),
    p("Alt Megalist: 1x5H89xQejE46G1GOKqRo3"),
    p("00s Rock: 37i9dQZF1DX3oM43CtKnRV"),
    p("90s Rock: 37i9dQZF1DX1rVvRgjX59F"),
    p("Pop Hits: 37i9dQZF1DWTwnEm1IYyoj"),
    HTML("<img style='padding: 10px 0;' src=spot.png width=300px />"),
    width=5
  ),
  
  
  #
  #
  #
  
  #
  #
  #
  
  
  ###Right Side Panel
  mainPanel(
    p(id = "head1", style="font-size: 18px; font-weight: bold;", align="center", "Select Game Mode"),
    br(),
    
    #Select the game mode - side by side options
    fluidRow(column(8, align="center", offset=2, actionButton("gts_but_start", "Guess the Song"), actionButton("gta_but_start", "Guess the Artist"))),
    br(),
    
    #Allow ability to end round
    fluidRow(column(8, align="center", offset=2, hidden(actionButton("restart", "End Round")))),
    
    
    #After a game mode is chosen, enter a playlist link
    #GTS
    fluidRow(column(8, align="center", offset=2, hidden(textInput("gts_ui_playlist", label="Enter a Playlist Link",)))),
    fluidRow(column(8, align="center", offset=2, hidden(actionButton("gts_but_submit_playlist", "Submit")))),
    #GTA
    fluidRow(column(8, align="center", offset=2, hidden(textInput("gta_ui_playlist", label="Enter a Playlist Link",)))),
    fluidRow(column(8, align="center", offset=2, hidden(actionButton("gta_but_submit_playlist", "Submit")))),
    br(),
    br(),
    
    #Replay buttons
    fluidRow(column(8, align="center", offset=2, hidden(actionButton("gts_but_replay_playlist", "Replay Playlist")))),
    fluidRow(column(8, align="center", offset=2, hidden(actionButton("gta_but_replay_playlist", "Replay Playlist")))),
    
    #Both
    uiOutput("numsongsavailable"),
    #GTS
    fluidRow(column(8, align="center", offset=2, hidden(textInput("gts_ui_numsongs", "How many songs do you want to play?", value=25)))),
    fluidRow(column(8, align="center", offset=2, hidden(actionButton("gts_but_submit_numsongs", "Submit")))),
    #GTA
    fluidRow(column(8, align="center", offset=2, hidden(textInput("gta_ui_numsongs", "How many songs do you want to play?", value=25)))),
    fluidRow(column(8, align="center", offset=2, hidden(actionButton("gta_but_submit_numsongs", "Submit")))),
    br(),
    
    hidden(p(id = "loading", align="center", "Loading...This may take a moment")),
    
    #Errors
    uiOutput("error"),
    uiOutput("error2"),
    uiOutput("error3"),
    
    hr(),
    
    #Audio
    fluidRow(column(8, align="center", offset=2, hidden(uiOutput("my_audio")))),
    fluidRow(column(8, align="center", offset=2, hidden(tags$audio(id = "myaudio", controls = NA, autoplay = NA, src = "")))),
    fluidRow(column(8, align="center", offset=2, uiOutput("newplay"))),
    br(),
    
    #Submit guess buttons
    fluidRow(column(8, align="center", offset=2, hidden(textInput("gts_ui_guess", "Guess the Song")))),
    fluidRow(column(8, align="center", offset=2, hidden(actionButton("gts_but_submit_guess", "Submit")))),
    fluidRow(column(8, align="center", offset=2, hidden(textInput("gta_ui_guess", "Guess the Artist")))),
    fluidRow(column(8, align="center", offset=2, hidden(actionButton("gta_but_submit_guess", "Submit")))),
    br(),
    br(),
    
    #History table
    fluidRow(column(12, align="center", offset=0, verbatimTextOutput("value"))),
    DT::dataTableOutput("songtable"),
    br(),
    width=7
  )
)


#
#
#

#
#
#


#Functions to create HTML elements
get_numsongs_tag <- function(nrowtot_allsongs, nrowtest, totallplaylist) {
  xy <- paste0("Your playlist has ", totallplaylist, " songs. ", nrowtot_allsongs, " are playable.")
  tags$p(xy, align="center", style="font-weight: bold;")
}
get_error <- function() {
  tags$p("Invalid Playlist. Try Again!", align="center")
}
get_error2 <- function() {
  tags$p("All songs in the playlist do not have a preview!", align="center")
}
get_error3 <- function() {
  tags$p("Not a valid number! Entry must be positive and less than the total number of available songs.", align="center")
}
get_newplay <- function() {
  tags$p("Enter a new playlist above ",align="center")
}


#
#
#

#
#
#



#Starts when you enter the site
server <- function(input, output, session) {

  
  ###
  #Get my Spotify App ID and Secret to call the API
  Sys.setenv(SPOTIFY_CLIENT_ID = "182b5e445ae845498a60c3fa49b689a7")
  Sys.setenv(SPOTIFY_CLIENT_SECRET = "4b93c4ee1a2b4a0fb70eb0e56f7b7fa2")
  
  #Lowers the media volume from 1 to 0.2 (default is too loud)
  runjs("document.getElementById('myaudio').volume = 0.2;")
  
  
  #
  #
  #
  
  #
  #
  #
  
  
  ###Initialize variables
  #This is the history table
  r_values <- reactiveValues()
  r_values$df <- data.frame("Num" = character(0), "Song" = character(0),"Artist" = character(0),"Guess" = character(0),"Correct" = character(0), "Round" = character(0),stringsAsFactors = FALSE)
  
  score_total <- 0
  score_this_round <- 0
  guesses_total <- 0
  guesses_this_round <- 0
  round <- 0
  x <- 8
  test <- data.frame(char = character(), stringsAsFactors = FALSE)
  
  #This stores the playlist value so that the replay button works
  last_playlist <- NA
  
  
  #
  #
  #
  
  #
  #
  #
  
  
  ###
  #Choose which game mode is picked: song or artist
  #GTS
  observeEvent(input$gts_but_start, {
    shinyjs::show("gts_ui_playlist")
    shinyjs::show("gts_but_submit_playlist")
    shinyjs::hide("gts_but_start")
    shinyjs::hide("gta_but_start")
    shinyjs::hide("head1")
    
    #This works because round is 0 if you haven't started guessing yet 
    if(round > 0){
      shinyjs::show("gts_but_replay_playlist")
    }
  })
  
  #GTA
  observeEvent(input$gta_but_start, {
    shinyjs::show("gta_ui_playlist")
    shinyjs::show("gta_but_submit_playlist")
    shinyjs::hide("gts_but_start")
    shinyjs::hide("gta_but_start")
    shinyjs::hide("head1")
    
    #This works because round is 0 if you haven't started guessing yet 
    if(round > 0){
      shinyjs::show("gta_but_replay_playlist")
    }
  })
  
  
  #
  #
  #
  
  #
  #
  #
  
  
  ###Guess the Song
  #Submit a playlist
  observeEvent(input$gts_but_submit_playlist, {
    shinyjs::hide("error")
    shinyjs::hide("error2")
    shinyjs::hide("error3")
    shinyjs::hide("gts_but_replay_playlist")
    shinyjs::hide("gts_ui_numsongs")
    shinyjs::hide("gts_but_submit_numsongs")
    shinyjs::hide("gta_ui_numsongs")
    shinyjs::hide("gta_but_submit_numsongs")

    
    #Tracks the number of plays/score in the current sample
    guesses_this_round <<- 0
    score_this_round <<- 0
    round <<- round + 1
    
    #Initialize the list of songs in the playlist
    datalist = list()
    
    #Update the textbox to blank
    updateTextInput(session, "gts_ui_playlist", value = "")
    
    #Allows "Submit" == press Enter Key
    value <- eventReactive(input$gts_but_submit_playlist, {
      input$gts_ui_playlist
    })
    
    
    #Clean playlist name and get playlist data
    #Use the user input. Cut off the first part of the copy if the link is the URI, cut off the front and end if it is a playlist link
    current_playlist <- str_replace(input$gts_ui_playlist, "spotify:playlist:", "")
    current_playlist <- gsub("https://open.spotify.com/playlist/", "\\1",current_playlist)
    current_playlist <- gsub("\\?.*","\\1",current_playlist)
    
    
    #Check if the user's input is a valid playlist (has the correct length and is the correct format (tested by seeing if it links to a valid playlist))
    txtcheck <- TRUE
    playlist_url <- paste0("https://open.spotify.com/playlist/",current_playlist)
    if(str_length(current_playlist) != 22){
      txtcheck <- FALSE
    }
    if(http_error(playlist_url)){
      txtcheck <- FALSE
    }


    #If the playlist is valid
    if (txtcheck == TRUE) {
      
      #Stores the playlist link for the replay button
      last_playlist <<- current_playlist
      
      shinyjs::hide("gts_but_submit_playlist")
      shinyjs::hide("gts_ui_playlist")
      shinyjs::show("loading")

      #Spotify can only get 100 songs at a time, so calculate how many times we need to get playlists
      api_calls <- get_playlist(current_playlist)$tracks$total %/% 100 + 1
  
      #Get all the songs in the playlist and add them to a dataframe
      for (j in 1:api_calls) {
        allsongs <- data.frame(get_playlist_tracks(current_playlist, offset = 100 * (j - 1)))
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
      
      #If all the songs in the playlist are invalid, error
      if(nrow(tot_allsongs) == 0){
        shinyjs::show("gts_but_submit_playlist")
        shinyjs::show("gts_ui_playlist")
        shinyjs::hide("loading")
        
        
        output$error <- renderUI(get_error())
        shinyjs::show("error")
        output$error2 <- renderUI(get_error2())
        shinyjs::show("error2")
      } else{
        tot_allsongs_g <<- tot_allsongs
        totallplaylist_g <<- totallplaylist
        shinyjs::show("numsongsavailable")
        output$numsongsavailable <- renderUI(get_numsongs_tag(nrow(tot_allsongs), nrow(test), totallplaylist))
        shinyjs::hide("newplay")
        shinyjs::hide("loading")
        shinyjs::hide("mytable")
        shinyjs::show("gts_ui_numsongs")
        
        updateTextInput(session, "gts_ui_numsongs", value = min(nrow(tot_allsongs),25))
        
        shinyjs::show("gts_but_submit_numsongs")
      }
    } else{
      
      #If the playlist is invalid, show that there was an error
      output$error <- renderUI(get_error())
      shinyjs::show("error")
    }
    
  })
  
  #Submit number of songs
  observeEvent(input$gts_but_submit_numsongs, {

    
    #Allows "Submit" == press Enter Key
    value <- eventReactive(input$gts_but_submit_numsongs, {
      input$gts_ui_numsongs
    })

    
    numsongs <- as.numeric(input$gts_ui_numsongs)

      
    #Error statement if the input is invalid
    if((is.na(numsongs)) | (numsongs %% 1 != 0) | (numsongs > nrow(tot_allsongs_g)) | (numsongs<=0)){
      
      #Update the textbox to the default 25
      updateTextInput(session, "gts_ui_numsongs", value = 25)
      
      output$error3 <- renderUI(get_error3())
      shinyjs::show("error3")
        
    } else{
        
      shinyjs::hide("error3")
        
      #Sample number of desired songs
      samp <- sample(1:nrow(tot_allsongs_g), numsongs, replace = FALSE)
        
      #Store the valid songs in TEST
      test <- tot_allsongs_g[samp, ]
        
      
      shinyjs::show("restart")
      shinyjs::show("gts_ui_guess")
      shinyjs::show("gts_but_submit_guess")
      shinyjs::hide("gts_ui_numsongs")
      shinyjs::hide("gts_but_submit_numsongs")
      shinyjs::hide("numsongsavailable")
        
        
      #Choose the first song and play it
      shinyjs::show("myaudio")
      songlink <- test[guesses_this_round + 1, ]$track.preview_url
      runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
      
      #Update the textbox to blank
      updateTextInput(session, "gts_ui_numsongs", value = "")
      
      #Allow access outside of this function
      test <<- test
      }
    numsongs <<- numsongs  
  })
  
  #Submit song guess
  observeEvent(input$gts_but_submit_guess, {
    
    #Allows "Submit" == press Enter Key
    value <- eventReactive(input$gts_but_submit_guess, {
      input$gts_ui_guess
    })

    
    #Record the number of guesses
    guesses_this_round <<- guesses_this_round + 1
    guesses_total <<- guesses_total + 1
    
    
    #Use our dataset to get the artist and song info
    rv <- reactiveValues(t = test[guesses_this_round, ])
    updateTextInput(session, "gts_ui_guess", value = "")
    
    
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
    
    
    #Convert the user input and artist data to lowercase. Remove the word "the" from the fuzzy matching
    twicexx <- tolower(song2)
    twicexx <- str_replace(twicexx,"the ","")
    once2 <- tolower(trimws(input$gts_ui_guess, which=c("both")))
    once2 <- str_replace(once2, "the ","")
    
    
    #Check if the user input matches the song title. The text matching software is more lenient if the string is longer
    if (str_length(twicexx) < 6 | is.na(twicexx)) {
      txtcheckxx <- amatch(once2, twicexx, maxDist = 2) == 1
    } else{
      txtcheckxx <- amatch(once2, twicexx, maxDist = 3) == 1
    }
    
    
    #Convert NA values to FALSE
    if (is.na(txtcheckxx)) {
      txtcheckxx <- FALSE
    }
    
    
    #Update the score and other vars depending on if the user was correct
    if (txtcheckxx == TRUE) {
      score_total <<- score_total + 1
      score_this_round <<- score_this_round + 1
      y <- 0
      ret <- "Correct!"
      ret2 <- "Yes"
    } else{
      y <- 1
      ret <- "Wrong :("
      ret2 <- "No"
    }
    
    
    #Create a new row on the table
    c_song <- data.frame("Num" = character(0), "Song" = character(0),"Artist" = character(0),"Guess" = character(0),"Correct" = character(0), "Round" = character(0),stringsAsFactors = FALSE)
    c_song[1, ] <- c(guesses_total, song, artist[1, 1], input$gts_ui_guess, ret2, round)
    
    newEntry <- observe({
      isolate(r_values$df <- rbind(c_song,r_values$df))
    })
    
    
    #Print a message based on if the user was correct and how many artists there are
    output$value <- renderText({
      
      if(round < 2){
      
        if (nrow(artist) == 1) {
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist, "\n", "Score: ", score_total, "/", guesses_total, "\n")
        } else if (nrow(artist) == 2) {
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], " & ", artist[2, 1], "\n", "Score: ", score_total, "/", guesses_total, "\n")
        } else if (nrow(artist) == 3) {
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], " & ", artist[3, 1], "\n", "Score: ", score_total, "/", guesses_total, "\n")
        } else if (nrow(artist) == 4) {
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], " & ", artist[4, 1], "\n", "Score: ", score_total, "/", guesses_total, "\n")
        } else if (nrow(artist) == 5) {
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], ", ", artist[4, 1], " & ", artist[5, 1], "\n", "Score: ", score_total, "/", guesses_total, "\n")
        } else{
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], " and others", "\n", "Score: ", score_total, "/", guesses_total, "\n")
        }
          
      } else{
        if (nrow(artist) == 1) {
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist, "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        } else if (nrow(artist) == 2) {
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], " & ", artist[2, 1], "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        } else if (nrow(artist) == 3) {
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], " & ", artist[3, 1], "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        } else if (nrow(artist) == 4) {
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], " & ", artist[4, 1], "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        } else if (nrow(artist) == 5) {
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], ", ", artist[4, 1], " & ", artist[5, 1], "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        } else{
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1,1], " and others", "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        }
      }
    })
    
    #Create the datatable of previous songs tested
    output$songtable <- DT::renderDataTable({
        DT::datatable(r_values$df, options = list(pageLength = 100,lengthChange=FALSE), rownames = FALSE, colnames = c('#' = 1))# %>%  
        #formatStyle(columns = c(3:9), 'text-align' = 'center')
    })
    
    #Update to the next song in the sample
    songlink <- test[guesses_this_round + 1, ]$track.preview_url
    runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
    
    #What to do if the sample has run out
    if (guesses_this_round  == nrow(test)) {
      #Show/Hide appropriate HTML elements
      shinyjs::hide("restart")
      shinyjs::hide("gts_but_submit_guess")
      shinyjs::hide("gts_ui_guess")
      shinyjs::hide("numsongsavailable")
      shinyjs::hide("myaudio")
      
      shinyjs::show("head1")
      shinyjs::show("gts_but_start")
      shinyjs::show("gta_but_start")
    }
  })
  
  #Replay a playlist
  observeEvent(input$gts_but_replay_playlist, {
    
    shinyjs::hide("error")
    shinyjs::hide("error2")
    shinyjs::hide("error3")
    shinyjs::hide("gts_ui_numsongs")
    shinyjs::hide("gts_but_submit_numsongs")
    shinyjs::hide("gta_ui_numsongs")
    shinyjs::hide("gta_but_submit_numsongs")
    
    
    #Tracks the number of plays/score in the current sample
    guesses_this_round <<- 0
    score_this_round <<- 0
    round <<- round + 1
    
    #Update the textbox to blank
    updateTextInput(session, "gts_ui_playlist", value = "")
    


    #####
    #Remove songs that do not have a preview
    whichna <- which(is.na(tot_allsongs_g$track.preview_url))

    
    shinyjs::show("numsongsavailable")
    output$numsongsavailable <- renderUI(get_numsongs_tag(nrow(tot_allsongs_g), nrow(test), totallplaylist_g))
    shinyjs::hide("newplay")
    shinyjs::hide("gts_but_replay_playlist")
    shinyjs::hide("gts_but_submit_playlist")
    shinyjs::hide("gts_ui_playlist")
    shinyjs::hide("mytable")
    shinyjs::show("gts_ui_numsongs")
        
    updateTextInput(session, "gts_ui_numsongs", value = min(nrow(tot_allsongs_g),25))
        
    shinyjs::show("gts_but_submit_numsongs")
  })
  
  
  #
  #
  #
  
  #
  #
  # 

  
  ###Guess the Artist
  #Submit a playlist
  observeEvent(input$gta_but_submit_playlist, {
    
    shinyjs::hide("error")
    shinyjs::hide("error2")
    shinyjs::hide("error3")
    shinyjs::hide("gta_but_replay_playlist")
    shinyjs::hide("gts_ui_numsongs")
    shinyjs::hide("gts_but_submit_numsongs")
    shinyjs::hide("gta_ui_numsongs")
    shinyjs::hide("gta_but_submit_numsongs")
    
    
    #Tracks the number of plays/score in the current sample
    guesses_this_round <<- 0
    score_this_round <<- 0
    round <<- round + 1
    
    #Initialize the list of songs in the playlist
    datalist = list()
    
    #Updates the textbox to blank
    updateTextInput(session, "gta_ui_playlist", value = "")
    
    #Allows "Submit" == press Enter Key
    value <- eventReactive(input$gta_but_submit_playlist, {
      input$gta_ui_playlist
    })
    
    
    #Clean playlist name and get playlist data
    #Use the user input. Cut off the first part of the copy
    current_playlist <- str_replace(input$gta_ui_playlist, "spotify:playlist:", "")
    current_playlist <- gsub("https://open.spotify.com/playlist/", "\\1",current_playlist)
    current_playlist <- gsub("\\?.*","\\1",current_playlist)
    
    
    #Check if the user's input a valid playlist (has the correct length and is the correct format (tested by seeing if it links to a valid playlist))
    txtcheck <- TRUE
    playlist_url <- paste0("https://open.spotify.com/playlist/",current_playlist)
    if(str_length(current_playlist) != 22){
      txtcheck <- FALSE
    }
    if(http_error(playlist_url)){
      txtcheck <- FALSE
    }
    
    #If the playlist is valid
    if (txtcheck == TRUE) {
      
      #Stores the playlist link for the replay button
      last_playlist <<- current_playlist
      
      
      shinyjs::hide("gta_but_submit_playlist")
      shinyjs::hide("gta_ui_playlist")
      shinyjs::show("loading")
      
      
      #Spotify can only get 100 songs at a time, so calculate how many times we need to get playlists
      api_calls <- get_playlist(current_playlist)$tracks$total %/% 100 + 1
      
      #Get all the songs in the playlist and add them to a dataframe
      for (j in 1:api_calls) {
        allsongs <- data.frame(get_playlist_tracks(current_playlist, offset = 100 * (j - 1)))
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
        shinyjs::show("gta_but_submit_playlist")
        shinyjs::show("gta_ui_playlist")
        shinyjs::hide("loading")
        
        output$error <- renderUI(get_error())
        shinyjs::show("error")
        output$error2 <- renderUI(get_error2())
        shinyjs::show("error2")
      } else{     

        tot_allsongs_g <<- tot_allsongs
        totallplaylist_g <<- totallplaylist
        shinyjs::show("numsongsavailable")
        output$numsongsavailable <- renderUI(get_numsongs_tag(nrow(tot_allsongs), nrow(test), totallplaylist))
        
        shinyjs::hide("newplay")
        shinyjs::hide("loading")
        shinyjs::hide("mytable")
        shinyjs::show("gta_ui_numsongs")
        
        updateTextInput(session, "gta_ui_numsongs", value = min(nrow(tot_allsongs),25))
        
        shinyjs::show("gta_but_submit_numsongs")
      }
    } else{
        
      #If the playlist is not valid, show that there was an error
      output$error <- renderUI(get_error())
      shinyjs::show("error")
    }
  })

  #Submit number of songs
  observeEvent(input$gta_but_submit_numsongs, {
    
    #Allows "Submit" == press Enter Key
    value <- eventReactive(input$gta_but_submit_numsongs, {
      input$gta_ui_numsongs
    })
    
    
    numsongs <- as.numeric(input$gta_ui_numsongs)
    
    
    #Error statement if the input is invalid
    if((is.na(numsongs)) | (numsongs %% 1 != 0) | (numsongs > nrow(tot_allsongs_g)) | (numsongs <= 0)){
      
      #Update the textbox to the default 25
      updateTextInput(session, "gta_ui_numsongs", value = 25)
      
      output$error3 <- renderUI(get_error3())
      shinyjs::show("error3")
      
    } else{
      
      shinyjs::hide("error3")
      
      #Sample number of desired songs
      samp <- sample(1:nrow(tot_allsongs_g), numsongs, replace = FALSE)
      
      #Store the valid songs in TEST
      test <- tot_allsongs_g[samp, ]
      
      
      shinyjs::show("restart")
      shinyjs::show("gta_ui_guess")  
      shinyjs::show("gta_but_submit_guess")
      shinyjs::hide("gta_ui_numsongs")
      shinyjs::hide("gta_but_submit_numsongs")
      shinyjs::hide("numsongsavailable")
      
              
      #Choose the first song and play it
      shinyjs::show("myaudio")
      songlink <- test[guesses_this_round + 1, ]$track.preview_url
      runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
        
        
      #Update the textbox to blank
      updateTextInput(session, "gta_ui_numsongs", value = "")
      
      #Allow access outside of this function  
      test <<- test
      }
    numsongs <<- numsongs
  })

  #Submit song guess
  observeEvent(input$gta_but_submit_guess, {
    
    #Allows "Submit" == press Enter Key
    value <- eventReactive(input$gta_but_submit_guess, {
      input$gta_ui_guess
    })
    
    
    #Record a guess
    guesses_this_round <<- guesses_this_round + 1
    guesses_total <<- guesses_total + 1
    
    
    #Use our dataset to get artist, song info
    rv <- reactiveValues(t = test[guesses_this_round, ])
    updateTextInput(session, "gta_ui_guess", value = "")
    
    
    #Get the artist
    aj <- rv$t[9]
    artist <- aj[[1]][[1]][3]
    
    
    #Get the song
    song <- rv$t[18]
    
    
    #Convert the user input and artist data to lowercase. Remove 'the' from fuzzy matching.
    once2 <- tolower(trimws(input$gta_ui_guess, which=c("both")))
    
    once2 <- str_replace(once2, "&","and")
    once2 <- str_replace(once2, "\\+","and")
    once2 <- str_replace(once2, "the ","")
    
    twicex <- tolower(artist[1, 1])
    twicex <- str_replace(twicex, "the ","")
    twicex <- str_replace(twicex, "&","and")
    twicex <- str_replace(twicex, "\\+","and")
    
    twicex2 <- tolower(artist[2, 1])
    twicex2 <- str_replace(twicex2, "the ","")
    twicex2 <- str_replace(twicex2, "&","and")
    twicex2 <- str_replace(twicex2, "\\+","and")
    
    twicex3 <- tolower(artist[3, 1])
    twicex3 <- str_replace(twicex3, "the ","")
    twicex3 <- str_replace(twicex3, "&","and")
    twicex3 <- str_replace(twicex3, "\\+","and")
    
    twicex4 <- tolower(artist[4, 1])
    twicex4 <- str_replace(twicex4, "the ","")
    twicex4 <- str_replace(twicex4, "&","and")
    twicex4 <- str_replace(twicex4, "\\+","and")
    
    twicex5 <- tolower(artist[5, 1])
    twicex5 <- str_replace(twicex5, "the ","")
    twicex5 <- str_replace(twicex5, "&","and")
    twicex5 <- str_replace(twicex5, "\\+","and")
    
    
    #Check if the user input matches any of the artists on the track. The text matching software is more lenient if the string is longer
    if (str_length(twicex) < 6 | is.na(twicex)) {
      txtcheckx <- amatch(once2, twicex, maxDist = 2) == 1
    } else{
      txtcheckx <- amatch(once2, twicex, maxDist = 3) == 1
    }
    if (str_length(twicex2) < 6 | is.na(twicex2)) {
      txtcheckx2 <- amatch(once2, twicex2, maxDist = 2) == 1
    } else{
      txtcheckx2 <- amatch(once2, twicex2, maxDist = 3) == 1
    }
    if (str_length(twicex3) < 6 | is.na(twicex3)) {
      txtcheckx3 <- amatch(once2, twicex3, maxDist = 2) == 1
    } else{
      txtcheckx3 <- amatch(once2, twicex3, maxDist = 3) == 1
    }
    if (str_length(twicex4) < 6 | is.na(twicex4)) {
      txtcheckx4 <- amatch(once2, twicex4, maxDist = 2) == 1
    } else{
      txtcheckx4 <- amatch(once2, twicex4, maxDist = 3) == 1
    }
    if (str_length(twicex5) < 6 | is.na(twicex5)) {
      txtcheckx5 <- amatch(once2, twicex5, maxDist = 2) == 1
    } else{
      txtcheckx5 <- amatch(once2, twicex5, maxDist = 3) == 1
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
      score_total <<- score_total + 1
      score_this_round <<- score_this_round + 1
      y <- 0
      ret <- "Correct!"
      ret2 <- "Yes"
    } else{
      y <- 1
      ret <- "Wrong :("
      ret2 <- "No"
    }
    
    
    #Create a new row on the table
    c_song <- data.frame("Num" = character(0), "Song" = character(0),"Artist" = character(0),"Guess" = character(0),"Correct" = character(0), "Round" = character(0),stringsAsFactors = FALSE)
    c_song[1, ] <- c(guesses_total, song, artist[1, 1], input$gta_ui_guess, ret2, round)
    
    newEntry <- observe({
      isolate(r_values$df <- rbind(c_song,r_values$df))
    })
    
    
    #Print a message based on if the user was correct and how many artists there are
    output$value <- renderText({
      
      if(round < 2){
      
        if (nrow(artist) == 1) {
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist, "\n", "Score: ", score_total, "/", guesses_total, "\n")
        } else if (nrow(artist) == 2) {
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], " & ", artist[2, 1], "\n", "Score: ", score_total, "/", guesses_total, "\n")
        } else if (nrow(artist) == 3) {
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], " & ", artist[3, 1], "\n", "Score: ", score_total, "/", guesses_total, "\n")
        } else if (nrow(artist) == 4) {
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], " & ", artist[4, 1], "\n", "Score: ", score_total, "/", guesses_total, "\n")
        } else if (nrow(artist) == 5) {
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], ", ", artist[4, 1], " & ", artist[5, 1], "\n", "Score: ", score_total, "/", guesses_total, "\n")
        } else{
          paste0("Song ",guesses_total,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], " and others", "\n", "Score: ", score_total, "/", guesses_total, "\n")
        }
      } else{
        if (nrow(artist) == 1) {
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist, "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        } else if (nrow(artist) == 2) {
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], " & ", artist[2, 1], "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        } else if (nrow(artist) == 3) {
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], " & ", artist[3, 1], "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        } else if (nrow(artist) == 4) {
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], " & ", artist[4, 1], "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        } else if (nrow(artist) == 5) {
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], ", ", artist[2, 1], ", ", artist[3, 1], ", ", artist[4, 1], " & ", artist[5, 1], "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        } else{
          paste0("Song ",guesses_this_round,"/",numsongs,"\n",ret, " That was \"", song, "\" by ", artist[1, 1], " and others", "\n", "Score this Round: ", score_this_round, "/", guesses_this_round, "\n", "Total Score: ",score_total, "/",guesses_total)
        }
      }
    })
    
    
    #Create the datatable of previous songs tested
    output$songtable <- DT::renderDataTable({
      DT::datatable(r_values$df, options = list(pageLength = 100,lengthChange=FALSE), rownames = FALSE, colnames = c('#' = 1))# %>%  
      #formatStyle(columns = c(3:9), 'text-align' = 'center')
    })
    
    
    #Update to the next song in the sample
    songlink <- test[guesses_this_round + 1, ]$track.preview_url
    runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
    
    
    #What to do if the sample has run out
    if (guesses_this_round  == nrow(test)) {
      #Show/Hide appropriate HTML elements
      shinyjs::hide("restart")
      shinyjs::hide("gta_but_submit_guess")
      shinyjs::hide("gta_ui_guess")
      shinyjs::hide("numsongsavailable")
      shinyjs::hide("myaudio")

      shinyjs::show("head1")
      shinyjs::show("gts_but_start")
      shinyjs::show("gta_but_start")
    }
  })
  
  #Replay a playlist
  observeEvent(input$gta_but_replay_playlist, {
    
    shinyjs::hide("error")
    shinyjs::hide("error2")
    shinyjs::hide("error3")
    shinyjs::hide("gts_ui_numsongs")
    shinyjs::hide("gts_but_submit_numsongs")
    shinyjs::hide("gta_ui_numsongs")
    shinyjs::hide("gta_but_submit_numsongs")
    
    
    #Tracks the number of plays/score in the current sample
    guesses_this_round <<- 0
    score_this_round <<- 0
    round <<- round + 1
    
    #Update the textbox to blank
    updateTextInput(session, "gta_ui_playlist", value = "")
    
    
    
    #####
    #Remove songs that do not have a preview
    whichna <- which(is.na(tot_allsongs_g$track.preview_url))
    
    
    shinyjs::show("numsongsavailable")
    output$numsongsavailable <- renderUI(get_numsongs_tag(nrow(tot_allsongs_g), nrow(test), totallplaylist_g))
    shinyjs::hide("newplay")
    shinyjs::hide("gta_but_replay_playlist")
    shinyjs::hide("gta_but_submit_playlist")
    shinyjs::hide("gta_ui_playlist")
    shinyjs::hide("mytable")
    shinyjs::show("gta_ui_numsongs")
    
    updateTextInput(session, "gta_ui_numsongs", value = min(nrow(tot_allsongs_g),25))
    
    shinyjs::show("gta_but_submit_numsongs")
  })
  
  
  #
  #
  #
  
  #
  #
  #
  
  
  ###Replay Buttons
  #Restart during a current play through
  observeEvent(input$restart, {
    #Set the current song to NA so no sound is playing
    songlink <- NA
    runjs(sprintf("document.getElementById('myaudio').src = '%s';", songlink))
    
    #Show/Hide appropriate HTML elements
    shinyjs::hide("restart")
    shinyjs::hide("gts_but_submit_guess")
    shinyjs::hide("gts_ui_guess")
    shinyjs::hide("gta_but_submit_guess")
    shinyjs::hide("gta_ui_guess")
    shinyjs::hide("numsongsavailable")
    shinyjs::hide("myaudio")
    
    shinyjs::show("head1")
    shinyjs::show("gts_but_start")
    shinyjs::show("gta_but_start")
  })
}

shinyApp(ui = ui, server = server)