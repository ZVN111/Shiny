# Load libraries
#install.packages("car")
library(car)   
#install.packages("psych")
library(psych)
#install.packages("dplyr")
library(dplyr)
#install.packages("zip")
library(zip)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("oaxaca")
library(oaxaca)
#install.packages("janitor")
library(janitor)
#install.packages("vtree")
library(vtree)
#install.packages("labeling")
library(labeling)
# install.packages("cli")
library(cli)
#install.packages("readxl")
library("readxl")

#Load data
data <- read_excel("data_anonymous.xlsx")

#Recode variables
names(data)[names(data) == "PD Gender"] <- "Gender"
names(data)[names(data) == "Average CompaRatio"] <- "Mean_CR"
names(data)[names(data) == "PD Age"] <- "Age"
names(data)[names(data) == "Grade"] <- "Personal_Pos_Grade"
names(data)[names(data) == "BP.Total.Target.Compensation..per.FTE..in.EUR."] <- "TTC"
data$Gender_num[data$Gender == "Male"]<-"0"
data$Gender_num[data$Gender == "Female"]<-"1"
data$Gender_num<-as.factor(data$Gender_num) #define variable as factor
summary(data$Gender_num)
data$Personal_Pos_Grade.r<-recode(data$Personal_Pos_Grade, 'B'=4, 'C'=3, 'D'=2, 'E'=1, 'F'=0)
data$Age<-as.numeric(data$Age)
class(data$Age)
data$Gender_oa <- as.numeric(as.character(data$Gender_num))
class(data$Gender_oa)
data$Gender_oa
class(data$Personal_Pos_Grade)

Oaxaca_TTC <- oaxaca(formula = TTC ~ Personal_Pos_Grade.r + Age | Gender_oa , data = data, R = 10)

summary.oaxaca(Oaxaca_TTC)

###################################
#Pay Equity Calculator
#Set Filters
CompaThres <- 90
Increase <- 1.05

equity <- subset(data, data$Mean_CR < CompaThres & data$Gender_num == 1)
equity_inv <- subset(data, data$Gender_num == 0 | (data$Mean_CR >= CompaThres & data$Gender_num == 1))


equity$TTC_new <- equity$TTC * Increase
equity_inv$TTC_new <- equity_inv$TTC

Costs <- sum(equity$TTC_new - equity$TTC)

Equity_all <- rbind(equity, equity_inv)

Oaxaca_TTC_new <- oaxaca(formula = TTC_new ~ Personal_Pos_Grade.r + Age | Gender_oa , data = Equity_all, R = 10)
Costs

#Gaps
#Unadjusted
Salary_Men <- Oaxaca_TTC_new$y$y.A
Total_Gap <- Oaxaca_TTC_new$y$y.A - Oaxaca_TTC_new$y$y.B
Unadjusted_Gap <- Total_Gap / Salary_Men * 100
Unadjusted_Gap

#Adjusted
#Grade
Grade_g <- c("Personal_Pos_Grade.r")
Column <- c("coef(explained)")
Grade_Effect <- (Oaxaca_TTC_new$twofold$variables[[5]][Grade_g, Column] / Total_Gap) * Unadjusted_Gap 
#Age
Age_g <- c("Age")
Age_Effect <- (Oaxaca_TTC_new$twofold$variables[[5]][Age_g, Column] / Total_Gap) * Unadjusted_Gap
#Adjusted Gap
Adjusted_Gap <- Unadjusted_Gap - Grade_Effect - Age_Effect
Adjusted_Gap

library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Pay Equity Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose your options"),
      
      checkboxGroupInput("checkGroup", 
                         h3(strong("Scope")), 
                         choices = list("Female" = 1, 
                                        "Male" = 0),
                         selected = 1),
      
      sliderInput("range", 
                  label = h3(strong("CompaRatio Threshold")),
                  min = 0, max = 100, value = c(80)),
      
      sliderInput("increase", 
                  label = h3(strong("Increase in %")),
                  min = 0, max = 10, value = c(2))
      
    ),
    mainPanel(print("Depending on user filter Male / Female / All, the CompaRatio
                 Threshold and the increase value, I would like to display costs here
                 as well as the unadjusted and adjusted gap")
    )
  )
)


# Define server logic ----
server <- function(input, output) {
  output$range <- renderText({
    paste(input$range)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
