library(shiny)
library(tidyverse)
library(dplyr)

library(ggplot2)


dat2015 <- read_csv("may data_clean_2015.csv",guess_max = 2000) %>%
  filter(complete.cases(data.frame(chla_ugl,phyco_ugl))) %>%
  arrange(desc(chla_ugl), desc(phyco_ugl)) %>%
  filter(fluorometer_type == "Beagle") %>%
  filter(sample_method==("Intergrated Sampler"))

server <- function(input,output) {
    
  selectedData <- reactive({
    xx <- dat2015 %>%
      filter(state == input$State)
    xx <- xx[,c("sample_date", input$Data, "state")]
    names(xx) <- c("x", "y","cols")
    xx})
 
    
  output$plot1 <-  renderPlot({
      
       p <-   ggplot(selectedData(),aes(x,y,color=cols))+       
        
        geom_point( pch = 20, 
                    cex = 5)
      
      p+labs(x="sample_date",y=input$Data, color="State(s)")
      
      
       }) 
    
    output$plot2 <-  renderPlot({
      p <-   ggplot(selectedData(),aes(x,y,color=cols))+       
        
        geom_point( pch = 20, 
                    cex = 5)
      p+labs(x="sample_date",y=input$Data, color="State(s)")}
    )}
