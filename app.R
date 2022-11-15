library(shinydashboard)
library(shiny)


load("mydict2.RData")
#mydict$pinyin=gsub("]/","",mydict[,"pinyin"])

words_all=rep(as.character(NA),5^4)
counter=1
for(i1 in 1:5){
  for(i2 in 1:5){
    for(i3 in 1:5){
      for(i4 in 1:5){
        words_all[counter]=paste0(i1,i2,i3,i4)
        counter=counter+1
      }
    }
  }
}

words_all_5=words_all
words_common_5=words_all

target=sample(1:dim(mydict)[1],1,prob=mydict$freq)
target=mydict[1,"sig"]

compare_words=function(target_str,guess_str){
  if ((nchar(target_str) != 4) | (nchar(guess_str)!=4)){
    stop("target and guess string must be length 4.")
  }

  target = strsplit(target_str,"")[[1]]
  guess = strsplit(guess_str,"")[[1]]
  result = character(nchar(guess_str))

  for(i in 1:4){
    if(guess[i]==target[i]){
      result[i]="correct"
      target[i]=""
      guess[i]=""
    }
  }
  for(i in 1:4){
    if(guess[i]!=""){
      if(is.element(guess[i],target)){
        result[i]="in-word"
        target[which(target==guess[i])[1]]=""
        next
      }
      result[i]="not-in-word"
    }
  }
  result
}



check_words = function(target_str,guess_str){
  compare_result = compare_words(target_str,guess_str)
  correct = FALSE
  if ( all(compare_result == "correct")){
    correct = TRUE
  }
  list(
    word = guess_str,
    letters=strsplit(guess_str,"")[[1]],
    result = compare_result,
    correct = correct,
    win = all(compare_result == "correct")
  )
}


format_result = function(r){
  out_str = ""
  out_divs <- tagList()
  for (i in 1:4){
    out_divs[[i]] <- div(r$letters[i], class = paste("guess-letter",r$result[i]))
  }
  out_divs
}


jscode <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#go").click();
}});'

########## UI

ui <- dashboardPage(
  dashboardHeader(title = "五音不全 （3142）"),

  #### SIDEBAR

  dashboardSidebar(),


  #### BODY


  dashboardBody(

    tags$head(
      tags$script(HTML(jscode)),
      tags$style(HTML("

      .content {
      //max-width: 500px;
      //outline: 1px solid salmon;
      }

    #result {
    display: grid;
    grid-template-columns: repeat(4, 50px);
    gap: 5px;
    padding: 15px;
    //justify-content: center;
    }

    #result > .guess-letter {
      border-radius: 10px;
      width: 50px;
      height: 50px;
      font-size: 20px;
      display: grid;
      place-content: center;
    }

    .guess-letter {
      border-radius: 5px;
      padding: 4px;
      color: white;
      font-weight: bold;
    }

    .in-word {
      background-color: #c8b458;
    }

    .not-in-word {
      background-color: #787c7e;
    }

    .correct {
    background-color: #6aa964;
    }

    .input-wrapper {
    //outline: 1px solid forestgreen;
    display: flex;
    align-items: flex-end;
    justify-content: left;
    }

    .input-wrapper > div {
    margin-bottom: 0;
    }

    .input-wrapper > button {
      margin-bottom: 0;
      margin-left: 10px;
    }
    
    .myclass {}

                    ")
      )
    ),

    uiOutput("result"),
    p(span("X", class="guess-letter correct"), "means that the letter is correct"),
    p(span("X", class="guess-letter in-word"), "means that the letter is in the word, but in the wrong place"),
    uiOutput("guess"),
    uiOutput("unsolved_go_button"),
    uiOutput("solved_new_game"),
    uiOutput("solved_chengyu"),
    uiOutput("solved_pinyin")
    )
)



########## SERVER

server <- function(input, output, session) {

  random_seed <- reactiveVal(set.seed(as.integer(Sys.time())))
  round <- reactiveVal(1)
  target_index <- reactiveVal(sample(1:dim(mydict)[1],1,prob=mydict$freq))
  all_guesses <- reactiveVal(character())
  solved <- reactiveVal(FALSE)
  finished <- reactiveVal(FALSE)
  target_word <- reactive(mydict[target_index(),"sig"])
  target_html3 <- reactive(
    paste0("https://baike.baidu.com/item/",
           mydict[target_index(),"simplified"]))
  target_html4 <- reactive(
    paste0("https://www.zerotohero.ca/en/zh/phrase/search/",
           mydict[target_index(),"simplified"]))
  
  
  

  output$answer <- renderValueBox({
      
    fluidRow(
      valueBox(target_word(),"",width=12)
      )
    
  })


  output$result <- renderUI({
    if (! input$guess %in% words_all_5){
      req(FALSE,cancelOutput=TRUE)
    }
    updateTextInput(session,"guess", value="")

    check_result <- check_words(target_word(),input$guess)

    if (isTRUE(check_result$win)) {
      solved(TRUE)
    }

    all_guesses_new <- c(all_guesses(),input$guess)
    all_guesses(all_guesses_new)
    out_str = lapply(all_guesses(),function(guess){
      result = check_words(target_word(),guess)
      format_result(result)
      

    })
    
    #browser()
    out_str
  }) |>
    bindEvent(input$go)
  
  
  output$solved_new_game <- renderUI({
    if (!solved() )
      return()
    
    actionButton("new_game", "New Round")
    
  })

  
  output$solved_chengyu <- renderUI({
    if (!solved())
      return()

    fluidRow(
    tags$a(href=target_html3(),
               valueBox(
                 mydict[target_index(),"simplified"],
                 mydict[target_index(),"english"],,width=12))
    )
    
  })

  output$solved_pinyin <- renderUI({
    if (!solved())
      return()
    
    fluidRow(
    tags$a(href=target_html4(),
    valueBox(
      mydict[target_index(),"pinyin"],"",
      icon = icon("fingerprint"),width=12)
    )
    )
    
  })

  output$solved_other <- renderUI({
    if (!solved())
      return()
    

    valueBox(
      mydict[target_index(),"freq"],
      target_index(),
      icon = icon("fingerprint"),
      width=12
    )
    
  })

  output$unsolved_go_button <- renderUI({
    if (solved() )
      return()

    actionButton("go", "Go")
  })

  
  output$guess <- renderUI({
    if (solved() )
      return()
    
  textInput("guess","",placeholder="1234",width='60px')
  })
  

  reset_game <- function() {
    target_index(sample(1:dim(mydict)[1],1,prob=mydict$freq))
    all_guesses(list())
    solved(FALSE)
  }


observeEvent(input$new_game, {
   reset_game()
})


}


shinyApp(ui, server)
