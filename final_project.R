library(shiny)
library(ggplot2)
data(state)

ui <- fluidPage(
    headerPanel("Evaluate US States by Selected Statistics"),
    selectInput(inputId = "var", 
                        label = "Choose a metric - X axis",
                        choices = colnames(state.x77[,-1])
                        #actionButton("update", "change")
                ),
    checkboxInput(inputId = "log1", "Set to log scale", value = FALSE),
    selectInput(inputId = "var2", 
                label = "Choose a metric - Y axis",
                choices = colnames(state.x77[,-1])
                #actionButton("update", "change")
    ),
    checkboxInput(inputId = "log2", "Set to log scale", value = FALSE),
    sliderInput(inputId = "num",
                    label = "Limit states by population",
                    value = 10000, min = 365, max = 21198
                    #actionButton("update", "change")
                ),
    mainPanel(
                plotOutput("plot"),
                tableOutput("summary")
                )
)

server <- function(input, output) {
            data <- reactive({
                #x <- state.x77[order(state.x77[,input$var], decreasing = T)[1:input$num],]
                #y <- state.x77[order(state.x77[,input$var2], decreasing = T)[1:input$num],]
                x <- data.frame(state.x77[,c(input$var, input$var2, "Population")])
                z <- x[x[,"Population"]>=input$num,]
                if(input$log1){
                    z[,1] = log(z[,1])
                }
                if(input$log2){
                    z[,2] = log(z[,2])
                }
                z
            })
            output$plot <- renderPlot({ggplot(data = data(), aes(data()[,1],data()[,2],label=rownames(data()))) + geom_point(shape = 21, colour = "black", fill = "white", size = 10, stroke = 1) + labs(x = input$var, y = input$var2) + geom_text(check_overlap = TRUE)
                })
            output$summary <- renderTable(data()[,c(1,2)], rownames = T)

}
    
shinyApp(ui = ui, server = server)

