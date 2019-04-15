#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Future Value of Investment Combinations"),
   
   # fluidrow with different input for number of bins 
       fluidRow(
         column(4,sliderInput(inputId="ini",
                              label="Initial Amount",
                              min=0,max=100000,step=500,
                              value=1000,pre="$")
           
         ),column(4,sliderInput(inputId = "rr",
                                label="Return Rate(in %)",
                                min=0,max=20,step=0.1,
                                value=5)
           
         ),column(4,sliderInput(inputId = "year",
                                label="Years",
                                min=0,max=50,step=1,
                                value=20)
                  
         
         )
      
      
      
   ),
   fluidRow(column(4,sliderInput(inputId = "ac",
                                 label="Annual Contribution",
                                 min=0,max=50000,step=500,
                                 value =2000,pre="$" )
   ),column(4,sliderInput(inputId = "gr",
                          label="Growth Rate(in %)",
                          min=0,max=20,step=0.1,
                          value=2)
            
   ),column(4,selectInput(inputId = "facet",
                          label="Facet",
                          choices=c("No","Yes"),
                          selected = "No")
   )
  ), 
   # Show a plot of the generated distribution
   h4("Timelines"),
   fluidRow( plotOutput("Timeline")),
   h4("Balances"),
   fluidRow(verbatimTextOutput("Balances"))
   
   
    
   
   
)

# Define server logic required to draw a plot
server <- function(input, output) {
dat <- reactive({
 future_value <-  function(amount,rate,years){
    future_value <- amount*(1+rate)^years
    return(future_value)
 }
 annuity <- function(contrib,rate,years){
      annuity <- contrib*((1+rate)^years-1)/rate
      return(annuity)
    }
    
    growing_annuity <- function(contrib,rate,growth,years){
      growing_annuity <- contrib*((1+rate)^years-(1+growth)^years)/(rate-growth)
      return(growing_annuity)
    }
    rrate <- input$rr/100
    grate <- input$gr/100
    no_contrib <- rep(0,(input$year+1))
    for(i in 0:input$year){
      no_contrib[i+1]=future_value(amount = input$ini, rate = rrate, years = i)
    }
    
    
    fixed_contrib <- rep(0,(input$year+1))
    for(i in 0:input$year){
      fixed_contrib[i+1]=annuity(contrib = input$ac,rate= rrate, years = i)+
        future_value(amount = input$ini, rate = rrate, years = i)
    }
    
    growing_contrib <- rep(0,(input$year+1))
    for(i in 0:input$year){
      growing_contrib[i+1]=growing_annuity(contrib = input$ac, rate=rrate, growth = grate, years = i)+
        future_value(amount = input$ini, rate = rrate, years = i)
    }
    dat <- data.frame(year=rep(seq(0,input$year,1),3),mix=c(no_contrib,fixed_contrib,growing_contrib),
                           modalities=rep(c("no_contrib","fixed_contrib","growing_contrib"),c(rep(input$year+1,3))))
    return(dat)
})

  
   output$Balances <- renderPrint({
     
     table <- data.frame(year=0:input$year,no_contrib=dat()$mix[dat()$modalities=="no_contrib"],
                         fixed_contrib=dat()$mix[dat()$modalities=="fixed_contrib"],
                         growing_contrib=dat()$mix[dat()$modalities=="growing_contrib"])
     table
   })
   output$Timeline <- renderPlot({
     
     timeline <- ggplot(data=dat(),aes(col=modalities))+geom_line(aes(x=year,y=mix),size=0.7)+
       geom_point(aes(x=year,y=mix),size=1.5)+labs(x="years",y="value",title="Three Models of Investment")
      
     if (input$facet=="Yes"){
       timeline <- timeline+geom_area(aes(x=year,y=mix,fill=modalities,alpha=0.3))+
         facet_grid(.~modalities)+theme_bw()
      }
     timeline
   })
   
}  


# Run the application 
shinyApp(ui = ui, server = server)

