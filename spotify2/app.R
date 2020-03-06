#2
    
library(shiny)
    library(DT)
    library(spotifyr)
    library(dplyr)
    library(stringdist)
    library(shinyjs)
    library(stringr)
    
    ui <- fluidPage(
        tags$head(tags$script(src = "enter_button.js")),
        useShinyjs(),
        titlePanel("Spotify Guessing Game"),
        mainPanel(
            p("auth0 for shinyapps.io"),
            DT::dataTableOutput("mytable"),
            textInput("txt2","Select a Playlist"),
            actionButton("selectbutton","Submit"),
            hr(),
            br(),
            p(id="jaja" ,"~~Please wait if you hit the button~~"),
            p(id="jajaja","~~Larger playlists can take a few secs~~"),
            hr(),
            uiOutput("numsongs"),
            uiOutput("error"),
            br(),
            uiOutput("my_audio"),
            uiOutput("newplay"),
            br(),
            textInput("txt1","Guess the Artist"),
            actionButton("guessbutton","Submit"),
            br(),
            br(),
            verbatimTextOutput("value"),
            DT::dataTableOutput("songtable"),
            br()
        )
    )
    
    
    get_audio_tag <- function(filename){
        tags$audio(src = filename, type="audio/mpeg", controls = "controls", autoplay = "autoplay")
    }
    get_numsongs_tag <- function(nrowtot_allsongs,nrowtest){
        xy <- paste0(nrowtest, " out of ",nrowtot_allsongs, " available songs were sampled")
        tags$p(xy)
    }
    get_error <- function(){
        tags$p("Invalid Playlist. Try Again!")
    }
    get_newplay <- function(){
        tags$p("Select a new playlist to continue!")
    }
    
    
    server <- function(input, output, session) {
        Sys.setenv(SPOTIFY_CLIENT_ID = "182b5e445ae845498a60c3fa49b689a7")
        Sys.setenv(SPOTIFY_CLIENT_SECRET = "4b93c4ee1a2b4a0fb70eb0e56f7b7fa2")
        scope = spotifyr::scopes
        scope <- scope[-11]
        access_token <- get_spotify_access_token()
        auth_token <- get_spotify_authorization_code(scope = scope)
        
        shinyjs::hide("guessbutton")
        shinyjs::hide("txt1")
        n <- reactiveValues()
        n$df <- data.frame("Song" = character(0), "Artist" = character(0), "Guess" = character(0), "Correct" = character(0), stringsAsFactors = FALSE)
        score <- 0
        guesses <- 0
        guesses2 <- 0
        output$mytable <- DT::renderDataTable({
            play <- get_my_playlists(limit = 50, offset = 0, authorization = auth_token, include_meta_info = FALSE) 
            play2 <- cbind("Name" = play$name, "Song Count" = play$tracks.total)
            play2
        })
        test <- data.frame(char=character(),stringsAsFactors = FALSE)
        observeEvent(input$selectbutton, {
            guesses2 <<- 0
            shinyjs::hide("error")
            
            
            updateTextInput(session,"txt2",value = "")
            full <- get_my_playlists(limit = 50, offset = 0, authorization = auth_token, include_meta_info = FALSE)
            datalist = list()
            for(i in 1:nrow(full)){
                shinyjs::hide("error")
                once <- tolower((input$txt2))
                twice <- tolower((full$name[i]))
                txtcheck <- amatch(once, twice, maxDist = 1) == 1
                if(is.na(txtcheck)){
                    txtcheck <- FALSE
                }
                
                if(txtcheck == TRUE){
                    playl <- full[i,]
                    sdf <- as.character(full$id[i])
                    runs <- playl$tracks.total %/% 100 + 1
                    for(j in 1:runs){
                        allsongs <- data.frame(get_playlist_tracks(sdf, offset = 100*(j-1)))  
                        datalist[[j]] <- allsongs
                    }
                    tot_allsongs <- dplyr::bind_rows(datalist)
                    whichna <- which(is.na(tot_allsongs$track.preview_url))
                    tot_allsongs <- tot_allsongs[-c(whichna),]
                    samp <- sample(1:nrow(tot_allsongs),min(25,nrow(tot_allsongs)),replace=FALSE)
                    test <- tot_allsongs[samp,]
                    songlink <- test[guesses2+1,]$track.preview_url
                    output$my_audio <- renderUI(get_audio_tag(songlink))
                    shinyjs::show("my_audio")
                    output$numsongs <- renderUI(get_numsongs_tag(nrow(tot_allsongs),nrow(test)))
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
                    break
                } else{
                    output$error <- renderUI(get_error())
                    shinyjs::show("error")
                }
            }
            
            
            
            
            
            # output$table2 <- DT::renderDataTable({ test })
            
            # songlink <- test[guesses+1,]$track.preview_url
            # output$my_audio <- renderUI(get_audio_tag(songlink))
            # test <<- test
        })
        
        observeEvent(input$guessbutton, {
            guesses2 <<- guesses2 + 1
            guesses <<- guesses + 1
            rv <- reactiveValues(t = test[guesses2,])
            updateTextInput(session,"txt1",value = "")
            aj <- rv$t[9]
            artist <- aj[[1]][[1]][3]
            song <- rv$t[18]
            
            once2 <- tolower(input$txt1)
            twicex <- tolower(artist[1,1])
            twicex2 <- tolower(artist[2,1])
            twicex3 <- tolower(artist[3,1])
            twicex4 <- tolower(artist[4,1])
            twicex5 <- tolower(artist[5,1])
            
            
            
            
            
            if(str_length(twicex) < 6 | is.na(twicex)){
                txtcheckx <- amatch(once2, twicex, maxDist = 1) == 1
            } else{
                txtcheckx <- amatch(once2, twicex, maxDist = 4) == 1
            }
            if(str_length(twicex2) < 6 | is.na(twicex2)){
                txtcheckx2 <- amatch(once2, twicex2, maxDist = 1) == 1
            } else{
                txtcheckx2 <- amatch(once2, twicex2, maxDist = 4) == 1
            }
            if(str_length(twicex3) < 6 | is.na(twicex3)){
                txtcheckx3 <- amatch(once2, twicex3, maxDist = 1) == 1
            } else{
                txtcheckx3 <- amatch(once2, twicex3, maxDist = 4) == 1
            }
            if(str_length(twicex4) < 6 | is.na(twicex4)){
                txtcheckx4 <- amatch(once2, twicex4, maxDist = 1) == 1
            } else{
                txtcheckx4 <- amatch(once2, twicex4, maxDist = 4) == 1
            }
            if(str_length(twicex5) < 6 | is.na(twicex5)){
                txtcheckx5 <- amatch(once2, twicex5, maxDist = 1) == 1
            } else{
                txtcheckx5 <- amatch(once2, twicex5, maxDist = 4) == 1
            }
            

            if(is.na(txtcheckx)){
                txtcheckx <- FALSE
            }
            if(is.na(txtcheckx2)){
                txtcheckx2 <- FALSE
            }
            if(is.na(txtcheckx3)){
                txtcheckx3 <- FALSE
            }
            if(is.na(txtcheckx4)){
                txtcheckx4 <- FALSE
            }
            if(is.na(txtcheckx5)){
                txtcheckx5 <- FALSE
            }
            
            
            if(txtcheckx == TRUE){
                score <<- score + 1
                y <- 0
                ret <- "Correct!"
                ret2 <- "Yes"
            } else if(txtcheckx2 == TRUE){ 
                score <<- score + 1
                y <- 0
                ret <- "Correct!"
                ret2 <- "Yes"
            } else if(txtcheckx3 == TRUE){ 
                score <<- score + 1
                y <- 0
                ret <- "Correct!"
                ret2 <- "Yes"
            } else if(txtcheckx4 == TRUE){ 
                score <<- score + 1
                y <- 0
                ret <- "Correct!"
                ret2 <- "Yes"
            } else if(txtcheckx5 == TRUE){ 
                score <<- score + 1
                y <- 0
                ret <- "Correct!"
                ret2 <- "Yes"
            } else{
                y <- 1
                ret <- "Wrong :("
                ret2 <- "No"
            }
            newEntry <- observe({
                isolate(n$df[nrow(n$df) + 1,] <- c(song, artist[1,1], input$txt1, ret2))
            })
            output$value <- renderText({
                if(nrow(artist) == 1){
                    paste0(ret," That was \"", song, "\" by ", artist, "\n","Score: ", score, "/",guesses,"\n")
                } else if(nrow(artist) == 2){
                    paste0(ret," That was \"", song, "\" by ", artist[1,1], " & ", artist[2,1], "\n","Score: ", score, "/",guesses,"\n")
                } else if(nrow(artist) == 3){
                    paste0(ret," That was \"", song, "\" by ", artist[1,1], ", ", artist[2,1], " & ", artist[3,1],  "\n","Score: ", score, "/",guesses,"\n")
                } else if(nrow(artist) == 4){
                    paste0(ret," That was \"", song, "\" by ", artist[1,1], ", ", artist[2,1], ", ", artist[3,1], " & ", artist[4], "\n","Score: ", score, "/",guesses,"\n")
                } else if(nrow(artist) == 5){
                    paste0(ret," That was \"", song, "\" by ", artist[1,1], ", ", artist[2,1], ", ", artist[3,1], ", ", artist[4,1], " & ", artist[5,1], "\n","Score: ", score, "/",guesses,"\n")
                } else{
                    paste0(ret," That was \"", song, "\" by ", artist, "\n","Score: ", score, "/",guesses,"\n")
                }
            })
            
            
            
            
            
            
            
            output$songtable <- DT::renderDataTable({
                DT::datatable(n$df, options = list(pageLength = 100))
            })
            
            songlink <- test[guesses2+1,]$track.preview_url
            output$my_audio <- renderUI(get_audio_tag(songlink))
            
            if(guesses2  == nrow(test)){
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
