library(shiny)
library(tidyverse)

library(dplyr)
library(ggplot2)
#install.packages("devtools")
library("devtools")

dat2015 <- read_csv("may data_clean_2015.csv",guess_max = 2000) %>%
  filter(complete.cases(data.frame(chla_ugl,phyco_ugl))) %>%
  filter(chla_ugl > 0) %>% 
  filter(phyco_ugl > 0.1) %>%
  arrange(desc(chla_ugl), desc(phyco_ugl)) %>%
  filter(dilution == "1:1") %>%
  filter(analysis_rep == "Primary") %>%
  filter(fluorometer_type == "Beagle") %>%
  filter(sample_temp_c > 20 & sample_temp_c < 24)%>%
  filter(sample_method==("Intergrated Sampler"))#%>%
  #filter(state == input$State)%>%
  #select(input$Data)


##############################################
  library(dplyr)
  library(ggplot2)

  server <- function(input,output) {
      selectedData <- reactive({
        xx <- dat2015 %>%
          filter(state == input$State)
        xx <- xx[,c("sample_date", input$Data, "state")]
        names(xx) <- c("x", "y", "cols")
        xx})
      
        output$Plot1 <- renderPlot({
          
       p <-   ggplot(selectedData(),aes(x,y,color = cols))+       
        
               geom_point( pch = 20, 
                           cex = 5)+
         geom_smooth()
         
          p+labs(x="Sample Date",y=input$Data, color="State(s)")
           })
        }
  
