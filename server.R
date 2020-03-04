
load("textpred.RData")
library(wordcloud2)

shinyServer(
    function(input, output){
    
        
        
        phrase <- reactive({paste("",input$textid)})
        phrase.word <- reactive({unlist(strsplit(phrase(), " "))})
        phrase.char <- reactive({unlist(strsplit(phrase(), ""))})
        length.word <- reactive({length(phrase.word())})
        length.char <- reactive({length(phrase.char())})
        
        
        prediction <- reactive({
                                ifelse(phrase.char()[length.char()]!=rep(" ", 5), phrase.table1[startsWith(phrase.table1, tolower(phrase.word()[length.word()]))][1:5],
                                     ifelse(!is.na(output4[input4%in%paste(phrase.word()[length.word()-2], phrase.word()[length.word()-1], phrase.word()[length.word()])][1:5]),
                                            output4[input4%in%paste(phrase.word()[length.word()-2], phrase.word()[length.word()-1],phrase.word()[length.word()])][1:5],
                                            ifelse(!is.na(output3[input3%in%paste(phrase.word()[length.word()-1], phrase.word()[length.word()])][1:5]),
                                                   output3[input3%in%paste(phrase.word()[length.word()-1], phrase.word()[length.word()])][1:5],
                                                   ifelse(!is.na(output2[input2%in%paste(phrase.word()[length.word()])][1:5]),
                                                          output2[input2%in%paste(phrase.word()[length.word()])][1:5],
                                                          phrase.table1[1:5]))))
                            })
            
           output$predicted <- renderWordcloud2(wordcloud2(data.frame(word=prediction(), freq=c(15, 5,4,3,2)), size = 1,shape = 'diamond'))
           
           
           
           
           
        })
