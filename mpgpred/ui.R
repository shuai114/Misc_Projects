shinyUI(pageWithSidebar(
    headerPanel("Automobile MPG Prediction Website"),
    sidebarPanel(h3('Please input here'),
                 textInput(inputId="wt",label="Car weight in lb"),
                 numericInput('cyl','Number of cylinders',4,min=4,max=8,step=2),
                 radioButtons("am","Transmission",c("Automatic"="0","Manual"="1"),inline=T)
                 #checkboxInput("am","Manual Transmission")
                 ),
    mainPanel(h2('Prediction result'),
              h4('Car weight in lb entered:'),
              verbatimTextOutput('wt'),
              h4('Number of cylinders entered:'),
              verbatimTextOutput("ocyl"),
              h4('Transmission entered:'),
              verbatimTextOutput('oam'),
              h4('The predicted miles per US gallon (MPG) for your car:'),
              verbatimTextOutput('Empg'),
              h2('Welcome to the MPG prediction website'),
              p('Are you going to buy a used car?
                Are you suspecting your car consuming too much gas?
                This website can help you figure out if the car you are
                concerned about is normal in terms of gas consumption.
                Just input the car weight in pounds, number of cylinders,
                and click on the transmission type in the left panel,
                and we will predict the expected miles per US gallon (MPG)
                for your car. Comparing this predicted number to your number,
                you will get the answer you need.'),
              p('One thing to note is that, for inputting the number of cylinders,
                you can use up or down arrow on your keyboard to adjust the number
                while your cursor is in the input line. Of course,
                it also works if you delete the old number and type in a new one.'),
              p('All your inputs will be printed back for you to check in the right main panel,
                and the predicted miles per US gallon (MPG) will also be shown after them.
                Thank you very much for using my website!')
              )
))