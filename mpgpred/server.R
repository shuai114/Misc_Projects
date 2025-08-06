predictmpg<-function(wt,cyl,am) {
    round(2.0604201+30963.3035459/wt+42.8135255/cyl-0.3976339*am,1)
}

shinyServer(
    function(input,output){
        output$wt <- renderText({input$wt})
        output$ocyl <- renderText({input$cyl})
        output$oam <- renderText({ifelse(input$am=="1","Manual","Automatic")})
        output$Empg <- renderText({predictmpg(as.numeric(input$wt),input$cyl,as.numeric(input$am))})
    }
)