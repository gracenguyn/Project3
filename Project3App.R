#Grace Nguyen
#SDS313
#PROJECT 3

library(shiny)
library(ggplot2)
turbines <- read.csv("Project1_wind_turbines.csv")

runGitHub("Project3", "gracenguyn", ref = "main")

ui <- fluidPage(
    
    # Application title
    titlePanel("Wind Turbines Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        selectInput("selectvar", label = h3("Choose a variable:"), 
                    choices = list("Capacity"=1, "Total Height"=2, "Rotor Diameter"=3, "Swept Area"=4, "State"=5, "Year"=6)),
        
        sliderInput("bins",
                     "Number of bins:",
                     min = 5,max = 50,step = 5,value = 20),
        p("Note: Bins slider only works on univariate histograms!"),
        
        radioButtons("color", label=h3("Select color:"),
                     choices=list("Pink" = "#f4cae4", "Orange" = "#fdcdac", "Yellow" = "#fff2ae", "Green"="#b2df8a","Blue"="#a6cee3", "Purple" = "#beaed4")),
        hr(),
        
        checkboxInput("bivar", label=strong("Convert to Bivariate Distribution"), value=FALSE), 
        p("Note: Please allow some time for bivariate plots to load!")
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot"),
        hr(),
        h3("Summary Statistics:"),
        fluidRow(verbatimTextOutput("mean")),
        fluidRow(verbatimTextOutput("sd")),
        fluidRow(verbatimTextOutput("fivenum")),
        h4("References:"),
        h5("Diffendorfer, James E. ScienceBase, US Geological Survey, 19 Apr. 2018, https://www.sciencebase.gov/catalog/item/57bdfd8fe4b03fd6b7df5ff9"),
        h5("Kaftura, Dennis. “Wind Turbines CSV File.” CORGIS Datasets Project, 17 Oct. 2021, https://think.cs.vt.edu/corgis/csv/wind_turbines/.")
        )
    ))
  

server <- function(input, output){
  
  output$distPlot <- renderPlot({
    
    if(input$selectvar == 1){
      if(input$bivar == TRUE){
        plot(turbines$Turbine.Capacity, turbines$Project.Capacity, pch = 20,col=input$color,xlab='Turbine Capacity (kilo-watts)',ylab="Turbine Capacity (mega-watts)", main="Distribution of Turbine Capacity (kw) and Turbine Capacity (mw)")}
      else if(input$bivar == FALSE){
      hist(turbines$Turbine.Capacity, col = input$color, main='Distribution of Turbine Capacity',xlab='Capacity (in xxx)', border = 'black', breaks = input$bins)}}
    
    if(input$selectvar == 2){
      if(input$bivar == TRUE){
        plot(turbines$Turbine.Total_Height, turbines$Project.Capacity, pch = 20,col=input$color,xlab='Total Height (meters))',ylab="Turbine Capacity (mega-watts)", main="Distribution of Turbine Height (meters) and Turbine Capacity (mw)")}
      else if(input$bivar == FALSE){
        hist(turbines$Turbine.Total_Height,col = input$color, main='Distribution of Total Turbine Height',xlab='Height (in xxx)', border = 'black',breaks = input$bins)}}
    
    if(input$selectvar == 3){
      if(input$bivar == FALSE){
        hist(turbines$Turbine.Rotor_Diameter,main='Distribution of Turbine Rotor Diameter',xlab='Diameter (in xxx)', col = input$color, border = 'black',breaks = input$bins)}
      else if(input$bivar == TRUE){
        plot(turbines$Turbine.Rotor_Diameter, turbines$Project.Capacity, pch = 20,col=input$color,xlab='Rotor Diameter (meters)',ylab="Turbine Capacity (mega-watts)", main="Distribution of Rotor Diameter (meters) and Turbine Capacity (mw)")}}
    
    
    if(input$selectvar == 4){
      if(input$bivar == TRUE){
        plot(turbines$Turbine.Swept_Area, turbines$Project.Capacity, pch = 20,col=input$color,xlab='Turbine Swept Area',ylab="Turbine Capacity (mega-watts)", main="Distribution of Turbine Swept Area and Turbine Capacity (mw)")}
      else if(input$bivar==FALSE){
        hist(turbines$Turbine.Swept_Area, main='Distribution of Turbine Swept Area', xlab='Area (in xxx)', col = input$color, border = 'black',breaks = input$bins)}}
    
    if(input$selectvar == 5){ #fix this!!!!!!
      if(input$bivar==FALSE){
        barplot(table(turbines$Site.State), main='Distribution of Turbines by State',xlab='State', ylab= "Number of Turbines", col = input$color, border = 'black')}
      else if(input$bivar==TRUE){
        barplot(table(turbines$Site.State), width=turbines$Project.Capacity, main='Distribution of Turbines by State',xlab='State', ylab= "Turbine Capacity (mw)", col = input$color, border = 'black')
        }}
    
    if(input$selectvar == 6){ 
      if(input$bivar == TRUE){
        ggplot(turbines) + aes(y=Project.Capacity, x=Year) +  geom_point(col =input$color) + xlab('Turbine Capacity (kilo-watts)') + ylab("Year First Made Operational") + ggtitle('Distribution of Turbine Capacity and Operational Year') + theme_bw()}
      else if(input$bivar==FALSE){
        hist((turbines$Year), main='Distribution of Turbines by Year',xlab='Year', ylab= "Number of Turbines", col = input$color, border = 'black',breaks = input$bins)}}
  })
  
  #Display mean if selected
  output$mean <- renderPrint({
    if(input$bivar==FALSE){
      if(input$selectvar == 1){
        print("Mean:")
      mean(turbines$Turbine.Capacity, na.rm=TRUE)}
      else if(input$selectvar == 2) {
        print("Mean:")
      mean(turbines$Turbine.Total_Height, na.rm=TRUE)}
      else if(input$selectvar == 3) {
        print("Mean:")
      mean(turbines$Turbine.Rotor_Diameter, na.rm=TRUE)}
      else if(input$selectvar == 4) {
        print("Mean:")
      mean(turbines$Turbine.Swept_Area, na.rm=TRUE)}
      else if(input$selectvar == 5) {
        print("Proportions Table:")
      prop.table(table(turbines$Site.State))}
      else if(input$selectvar==6){
        print("Mean:")
      mean(turbines$Year, na.rm=TRUE)}}
    else if(input$bivar==TRUE){
      if(input$selectvar == 1){
        print("Correlation Coefficient:")
        cor(turbines$Turbine.Capacity, turbines$Project.Capacity)}
      else if(input$selectvar == 2) {
        print("Correlation Coefficient:")
        cor(turbines$Turbine.Total_Height, turbines$Project.Capacity)}
      else if(input$selectvar == 3) {
        print("Correlation Coefficient:")
        cor(turbines$Turbine.Rotor_Diameter, turbines$Project.Capacity)}
      else if(input$selectvar == 4) {
        print("Correlation Coefficient:")
        cor(turbines$Turbine.Swept_Area, turbines$Project.Capacity)}
      else if(input$selectvar == 5) {
        print("Proportions Table:")
        prop.table(table(turbines$Site.State, turbines$Project.Capacity))}
      else if(input$selectvar==6){
        print("Correlation Coefficient:")
        cor(turbines$Year, turbines$Project.Capacity)}
    }
    })
  
  output$sd <- renderPrint({
    if(input$selectvar == 1 & (input$bivar==FALSE)){
      print("Standard Deviation:")
      sd(turbines$Turbine.Capacity, na.rm=TRUE)}
    else if(input$selectvar == 2 & (input$bivar==FALSE)) {
      print("Standard Deviation:")
      sd(turbines$Turbine.Total_Height, na.rm=TRUE)}
    else if(input$selectvar == 3 & (input$bivar==FALSE)) {
      print("Standard Deviation:")
      sd(turbines$Turbine.Rotor_Diameter, na.rm=TRUE)}
    else if(input$selectvar == 4 & (input$bivar==FALSE)) {
      print("Standard Deviation:")
      sd(turbines$Turbine.Rotor_Diameter, na.rm=TRUE)}
    else if(input$selectvar==6 & (input$bivar==FALSE)){
      print("Standard Deviation:")
      sd(turbines$Year, na.rm=TRUE)}
  })
  
  output$fivenum <- renderPrint({
    if(input$selectvar == 1 & (input$bivar==FALSE)){
      print("Five Number Summary:")
      fivenum(turbines$Turbine.Capacity, na.rm=TRUE)}
    else if(input$selectvar == 2 & (input$bivar==FALSE)) {
      print("Five Number Summary:")
      fivenum(turbines$Turbine.Total_Height, na.rm=TRUE)}
    else if(input$selectvar == 3 & (input$bivar==FALSE)) {
      print("Five Number Summary:")
      fivenum(turbines$Turbine.Rotor_Diameter, na.rm=TRUE)}
    else if(input$selectvar == 4 & (input$bivar==FALSE)) {
      print("Five Number Summary:")
      fivenum(turbines$Turbine.Rotor_Diameter, na.rm=TRUE)}
    else if(input$selectvar==6 & (input$bivar==FALSE)){
      print("Five Number Summary:")
      fivenum(turbines$Year, na.rm=TRUE)}
  })
}


# Run the application 
shinyApp(ui = ui, server=server)