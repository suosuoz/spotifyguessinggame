if (interactive()) {
    library(shiny)
    library(DT)
    library(spotifyr)
    library(dplyr)
    
    Sys.setenv(SPOTIFY_CLIENT_ID = "182b5e445ae845498a60c3fa49b689a7")
    Sys.setenv(SPOTIFY_CLIENT_SECRET = "4b93c4ee1a2b4a0fb70eb0e56f7b7fa2")
    scope = spotifyr::scopes
    scope <- scope[-11]
    
    access_token <- get_spotify_access_token()
    auth_token <- get_spotify_authorization_code(scope = scope)
    
    
    ui <- fluidPage(
        titlePanel("Spotify Guessing Game"),
        
        mainPanel(
            p("MUST BE PLAYING A SONG IN SPOTIFY CURRENTLY"),
            
            textInput("txt1","Guess the Artist"),
            actionButton("guessbutton","Submit"),
            
            br(),
            br(),
            verbatimTextOutput("value"),
            DT::dataTableOutput("songtable"),
            br()
        )
    )
    
    server <- function(input, output, session) {
        n <- reactiveValues()
        n$df <- data.frame("Song" = character(0), "Artist" = character(0), "Guess" = character(0), "Correct" = character(0), "Time" = character(0), stringsAsFactors = FALSE)
        score <- 0
        guesses <- 0
        
        observeEvent(input$guessbutton, {
            rv <- reactiveValues(yy = get_my_currently_playing(market = NULL, authorization = auth_token))
            time1 <- reactiveValues(a = get_my_currently_playing(market = NULL, authorization = auth_token)$progress_ms,
                                    b = get_my_currently_playing(market = NULL, authorization = auth_token)$item$duration_ms)
            time2 <- (time1$a/1000)%/%60
            time3 <- round((time1$a/1000/60 - (time1$a/1000)%/%60)*60)
            if(time3 < 10){
                time3 <- paste0("0",time3)
            }
            timef <- paste0(time2,":" ,time3)
            
            guesses <<- guesses + 1
            if(tolower(input$txt1) == tolower(rv$yy$item$artists$name[1])){
                score <<- score + 1
                y <- 0
                ret <- "Correct!"
                ret2 <- "Yes"
            } else if((!is.na(rv$yy$item$artists$name[2])) & (tolower(input$txt1) == tolower(rv$yy$item$artists$name[2]))){ 
                score <<- score + 1
                y <- 0
                ret <- "Correct!"
                ret2 <- "Yes"
            } else if((!is.na(rv$yy$item$artists$name[3])) & (tolower(input$txt1) == tolower(rv$yy$item$artists$name[3]))){ 
                score <<- score + 1
                y <- 0
                ret <- "Correct!"
                ret2 <- "Yes"
            } else if((!is.na(rv$yy$item$artists$name[4])) & (tolower(input$txt1) == tolower(rv$yy$item$artists$name[4]))){ 
                score <<- score + 1
                y <- 0
                ret <- "Correct!"
                ret2 <- "Yes"
            } else if((!is.na(rv$yy$item$artists$name[5])) & (tolower(input$txt1) == tolower(rv$yy$item$artists$name[5]))){ 
                score <<- score + 1
                y <- 0
                ret <- "Correct!"
                ret2 <- "Yes"
            } else{
                y <- 1
                ret <- "Wrong :("
                ret2 <- "No"
            }
            updateTextInput(session,"txt1",value = "")
            artist <- rv$yy$item$artists$name
            song <- rv$yy$item$name
            art <- rv$yy$item$artists$name[1]
            
            newEntry <- observe({
                isolate(n$df[nrow(n$df) + 1,] <- c(song, art, input$txt1, ret2, timef))
            })
            
            output$value <- renderText({
                if(length(artist) == 1){
                    paste0(ret," That was \"", song, "\" by ", artist, "\n","Score: ", score, "/",guesses,"\n", "You got that in only ",timef,"\n")
                } else if(length(artist) == 2){
                    paste0(ret," That was \"", song, "\" by ", artist[1], " & ", artist[2], "\n","Score: ", score, "/",guesses,"\n", "You got that in only ",timef,"\n")
                } else if(length(artist) == 3){
                    paste0(ret," That was \"", song, "\" by ", artist[1], ", ", artist[2], " & ", artist[3],  "\n","Score: ", score, "/",guesses,"\n", "You got that in only ",timef,"\n")
                } else if(length(artist) == 4){
                    paste0(ret," That was \"", song, "\" by ", artist[1], ", ", artist[2], ", ", artist[3], " & ", artist[4], "\n","Score: ", score, "/",guesses,"\n", "You got that in only ",timef,"\n")
                } else if(length(artist) == 5){
                    paste0(ret," That was \"", song, "\" by ", artist[1], ", ", artist[2], ", ", artist[3], ", ", artist[4], " & ", artist[5], "\n","Score: ", score, "/",guesses,"\n", "You got that in only ",timef,"\n")
                } else{
                    paste0(ret," That was \"", song, "\" by ", artist, "\n","Score: ", score, "/",guesses,"\n", "You took ",timef,"\n")
                }
            })
            
            output$songtable <- DT::renderDataTable({ 
                DT::datatable(n$df, options = list(pageLength = 25))
            })
            
            skip_my_playback(device_id = NULL, authorization = auth_token)
            rv$yy <- get_my_currently_playing(market = NULL, authorization = auth_token)
        })
        
    }
    
    shinyApp(ui = ui, server = server)
}
