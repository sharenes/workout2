library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investing Modalities"),
    
   fluidRow(
     column(4, 
            sliderInput("initial", 
                        h5("Initial amount"),
                        min = 0, 
                        max = 100000, 
                        value = 1000,
                        pre = "$",
                        step = 500)),
     column(4, 
            sliderInput("return",
                        h5("Return rate (in %)"),
                        min = 0,
                        max = 20,
                        value = 5,
                        step = 0.1)),
     column(4, 
            sliderInput("years",
                        h5("Years"),
                        min = 0,
                        max = 50,
                        value = 20,
                        step = 1))
   ),
   fluidRow(
     column(4, 
            sliderInput("annual", 
                        h5("Annual Contribution"),
                        min = 0, 
                        max = 50000,  
                        value = 2000,
                        pre = "$",
                        step = 500)),
     column(4,
            sliderInput("growth",
                        h5("Growth rate (in %)"),
                        min = 0,
                        max = 20,
                        value = 2,
                        step = 0.1)),
     column(4, 
            selectInput("facet", 
                        h5("Facet?"),
                        choices = c("No", "Yes"),
                        selected = "No"
                        )
            )
   ),

    
    h4("Timelines"),
    plotOutput("modal_plot", width = 1100),
      
    h4("Balances"),
    verbatimTextOutput("modal_table")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  modal <- reactive({
    future_value <- function(amount, rate, years){
      fv <- amount*((1+rate)^{years})
      return(fv)
    }
    
    annuity <- function(contrib, rate, years){
      fva <- contrib*((((1+rate)^{years}) - 1)/rate)
      return(fva)
    }
    
    growing_annuity <- function(contrib, rate, growth, years){
      fvga <- contrib*((((1+rate)^{years}) - ((1+growth)^{years}))/(rate- growth))
      return(fvga)
    }
    
    fv = rep(0,input$years)
    fva = rep(0,input$years)
    fvga = rep(0,input$years)
    
    for (years in 0:input$years){
      fv1 <- future_value(input$initial, input$return/100, years)
      fv[years+1] <- fv1
    }
    fv

    for (years in 0:input$years){
      fva1 <- future_value(input$initial, input$return/100, years) + annuity (input$annual, input$return/100, years)
      fva[years+1] <- fva1
    }
    fva

    for (years in 0:input$years){
    fvga1 <- future_value(input$initial, input$return/100, years) + growing_annuity(input$annual, input$return/100, input$growth/100, years)
    fvga[years+1] <- fvga1
    }
    fvga

    modalities <- data.frame("year" = c(0:input$years), "no_contrib" = fv, "fixed_contrib" = fva, "growing_contrib" = fvga)
    modalities
  })

  output$modal_plot <- renderPlot({
    if (input$facet == "No"){  
      ggplot(data = modal()) +
      labs(x = "Year", y = "Savings($)", title = "Three modes of investing") + 
      geom_line(aes(x = year, y = no_contrib, color = "No Contribution")) + 
      geom_line(aes(x = year, y = fixed_contrib, color = "Fixed Contribution")) + 
      geom_line(aes(x = year, y = growing_contrib, color = "Growing Contribution")) + 
      scale_color_manual(name = "Modality", values = c("forest green","blue", "red"))
    }
    else{
      output$modal_plot <- renderPlot({
        fv <- modal()$no_contrib
        fva <- modal()$fixed_contrib
        fvga <- modal()$growing_contrib
        modal_facet <- data.frame(modes = factor(rep(c("no_contrib", "fixed_contrib","growing_contrib"), each = input$years + 1),
                                  levels = c("no_contrib", "fixed_contrib","growing_contrib")),
                                  year = rep(0:input$years,3),value = c(fv, fva, fvga))
        ggplot(data = modal_facet, aes(x = year, y = value, color = modes)) +
        labs(x = "Year", y = "Savings($)", title = "Three modes of investing") +
        geom_line() + facet_grid(.~modes) + geom_area(aes(fill = modes), alpha = 0.5) + geom_point() +
        scale_color_manual(values = c("red","forest green", "blue"))
      })
    }
  })
  
  output$modal_table <- renderPrint({modal()}, width = 10000)
}


# Run the application 
shinyApp(ui = ui, server = server)

