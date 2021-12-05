library(ggplot2)   # visualizing data
library(dplyr)     # transforming (joining, summarizing, etc.) data
library(DT)        # for printing nice HTML output tables
library(tidyr)     # tidying data
library(ggfittext) # for labelling treemap elements
library(treemapify)# for plotting treemaps
library(data.table)# for importing large datasets
library(knitr)  
library(shiny)
library(readr)
library(lubridate)
library(stringr)
library(tidyverse)
library(caret)
library(plotly)
library(ranger)
library(zoo)
library(arules)
library(arulesViz)
library(plyr)
library(shinycssloaders)

ui <- fluidPage(
  
  titlePanel("Retail Dashboard"),
  
  fluidRow(
    
    column(6, 
           plotOutput("plot1", click = "plot_click") %>% withSpinner(color="#0dc5c1"),
           verbatimTextOutput("info")      
    ),
    
    column(6,
           plotOutput("plot2", click = "plot_click2") %>% withSpinner(color="#0dc5c1"),
           verbatimTextOutput("info2")
    )
  ),
  
  fluidRow(
    
    column(6, 
           plotOutput("plot3", click = "plot_click3") %>% withSpinner(color="#0dc5c1"),
           verbatimTextOutput("info3")      
    ),
    
    column(6,
           plotOutput("plot4", click = "plot_click4") %>% withSpinner(color="#0dc5c1"),
           verbatimTextOutput("info4")
    )
  ),
  
  fluidRow(
    
    column(12,
           plotOutput("plot5", click = "plot_click5") %>% withSpinner(color="#0dc5c1"),
           verbatimTextOutput("info5")      
    )
  )
 
)

server <- function(input, output) {
  
  households <- fread("data/400_households.csv", strip.white = T) 
  
  products <- fread("data/400_products.csv", strip.white = T)
  
  transactions <- fread("data/400_transactions.csv", strip.white = T)
  
  # create tidy households data
  
  households[] <- lapply(households, as.character)
  
  # null values and all the data points where the data is not available are assigned a uniform 
  # value NA
  
  households[households[,] == "null"] <- NA
  households[households[,] == "NOT AVAILABLE"] <- NA
  households[households[,] == "Not Available"] <- NA
  households[households[,] == "Unknown"] <- NA
  
  households[] <- lapply(households, factor)
  households$HSHD_NUM <- as.integer(households$HSHD_NUM)
  colnames(households)[2] <- "LOYALTY_FLAG"
  colnames(households)[4] <- "MARITAL_STATUS"
  colnames(households)[6] <- "HOMEOWNER_DESCRIPTION"
  
  # create tidy products data
  
  colnames(products)[5] <- "NATURAL_ORGANIC_FLAG"
  colnames(products)[4] <- "BRAND_TYPE"
  
  # create tidy transactions data
  
  colnames(transactions)[3] <- "PURCHASE_DATE"
  colnames(transactions)[7] <- "STORE_REGION"
  
  # converting purchase date to a date column
  
  transactions$PURCHASE_DATE <- as.character(transactions$PURCHASE_DATE)
  transactions$PURCHASE_DATE <- as.Date(transactions$PURCHASE_DATE, format = "%d-%b-%y")
  
  # create tidy merged data frame
  
  hh_trans <- merge(households, transactions, by = "HSHD_NUM")
  final_dataset <- merge(hh_trans, products, by = "PRODUCT_NUM")
  
  #View(final_dataset)
  
  # slicing and dicing data for new visuals
  
  data_year_2018 <- final_dataset[which(final_dataset$YEAR == 2018) , ]
  #View(data_year_2018)
  
  
  data_year_2019 <- final_dataset[which(final_dataset$YEAR == 2019) , ]
  #View(data_year_2019)
  
  
  data_year_2020 <- final_dataset[which(final_dataset$YEAR == 2020) , ]
  #View(data_year_2020)
  
  data_line_chart <- final_dataset %>%
    group_by(WEEK_NUM) %>%
    #summarise(SPEND = sum(SPEND)) %>%
    arrange(final_dataset$WEEK_NUM) %>%
    na.omit()
  
  #View(data_line_chart)
  
  output$plot1 <- renderPlot({
    ggplot(data = data_line_chart, aes(x = WEEK_NUM, y = data_line_chart$SPEND)) +
      geom_line(color = 'purple4', size = 2) +
      labs(title = "Household Spend over Time", x = "Week Number", y = "Spend") +
      theme_minimal()
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
  #merged_data <- transactions %>% 
    #inner_join(products, by = "PRODUCT_NUM") %>%
    #inner_join(households, by = "HSHD_NUM")
  
  #View(merged_data)
  
  summar2 <- final_dataset %>% 
    dplyr::select(COMMODITY, SPEND, YEAR) %>%
    group_by(COMMODITY, YEAR) %>% 
    dplyr::summarise(tot_spend = sum(SPEND))
  
  summar2$YEAR <- as.factor(summar2$YEAR)
  
  output$plot2 <- renderPlot({
    ggplot(summar2, aes(x = tot_spend, y = COMMODITY, col = YEAR)) +
    geom_point(position = "jitter", alpha = 0.4) +
    labs(x = "Spending",
         y = "Commodity Type",
         title = "Yearly Spending commodity wise", 
         color = "Year") +
    theme(axis.text.y = element_text(size = 8))
  })
  
  output$info2 <- renderText({
    paste0("x=", input$plot_click2$x, "\ny=", input$plot_click2$y)
  })
  
  
  summar <- final_dataset %>% 
    dplyr::select(HSHD_NUM, SPEND, YEAR, STORE_REGION) %>%
    group_by(STORE_REGION, HSHD_NUM, YEAR) %>% 
    dplyr::summarise(tot_spend = sum(SPEND)) 
  
  
  output$plot3 <- renderPlot({
    ggplot(summar,aes(x = tot_spend, fill = STORE_REGION)) + 
    geom_density(col = NA, alpha = 0.35) +
    labs(x = "Household Spending",
         y = "Density",
         title = "Region Wise Spending", 
         color = "Store Region")
  })
  
  output$info3 <- renderText({
    paste0("x=", input$plot_click3$x, "\ny=", input$plot_click3$y)
  })
  
  purchase_yemo <- final_dataset$PURCHASE_DATE %>% str_sub(start = 1, end = 7)
  
  final_dataset <- final_dataset %>% mutate(YeMo = purchase_yemo)

  
  spending_timeseries_org <- final_dataset %>% 
    dplyr::select(SPEND, YeMo, YEAR, NATURAL_ORGANIC_FLAG) %>% 
    group_by(YeMo, NATURAL_ORGANIC_FLAG) %>% 
    dplyr::summarise(tot_spend = sum(SPEND), YE = unique(YEAR))
  
  output$plot4 <-  renderPlot({
    ggplot(spending_timeseries_org, aes(y = tot_spend, 
                                            x = as.Date(as.yearmon(YeMo)),
                                            color = as.factor(YE))) +
    geom_point(alpha = 0.5, position = "jitter") +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(. ~ NATURAL_ORGANIC_FLAG, scales = "free_y") +
    labs(x = "Monthly Index",
         y = "Monthwise Spending",
         title = "Monthly spending for orgainic products and non-organic products", 
         color = "Year")
  })
  
  output$info4 <- renderText({
    paste0("x=", input$plot_click4$x, "\ny=", input$plot_click4$y)
  })
  
  
  purchase_year <- as.numeric(final_dataset$PURCHASE_DATE %>% str_sub(start = 1, end = 4))
  final_dataset <- final_dataset %>% mutate(Year = purchase_year)
  
  spending <- final_dataset %>% 
    dplyr::select(HSHD_NUM, SPEND, INCOME_RANGE, UNITS, LOYALTY_FLAG, 
                  AGE_RANGE,MARITAL_STATUS, HOMEOWNER_DESCRIPTION,
                  HSHD_COMPOSITION, HH_SIZE, CHILDREN) %>%
    group_by(HSHD_NUM) %>% 
    dplyr::summarise(TOT_SPEND = sum(SPEND), INCOME_RANGE = unique(INCOME_RANGE), 
                     TOT_UNITS  = sum(UNITS), LOYALTY_FLAG = unique(LOYALTY_FLAG), 
                     AGE_RANGE = unique(AGE_RANGE), MARITAL_STATUS = unique(MARITAL_STATUS), 
                     HOMEOWNER_DESCRIPTION = unique(HOMEOWNER_DESCRIPTION), 
                     HSHD_COMPOSITION = unique(HSHD_COMPOSITION),
                     HH_SIZE = unique(HH_SIZE), CHILDREN = unique(CHILDREN))
  
  
  
  output$plot5 <- renderPlot({
    ggplot(spending, aes(y = TOT_SPEND, x = TOT_UNITS
                            , color = INCOME_RANGE)) +
    geom_point(alpha = 0.5, position = "jitter") +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(x = "Total Units",
         y = "Total Spending of individual households",
         title = "Income wise Household Spending - 2018, 2019 & 2020")
  })
  
  output$info5 <- renderText({
    paste0("x=", input$plot_click5$x, "\ny=", input$plot_click5$y)
  })
  
}

shinyApp(ui, server)

