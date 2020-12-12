# loading the required libraries
library(shiny)
library(ggplot2)
library(leaflet)

# loading the dataset
coral_data<-read.csv("assignment-02-data-formated.csv")
# removing the percentage sign and convert the data type to double
coral_data$value = as.double(gsub("%", "", coral_data$value))

# user-interface script
# creating fluid page to show the point plot in the server script
UI <- fluidPage(
  # title of the shiny plot and the leaflet map
  headerPanel("Bleaching percentage of different corals across differrent sites (2010-2017)"),
  # # Sidebar with a slider input for the different across of coral types
  sidebarLayout(
    sidebarPanel(
      selectInput("type","Coral type:",
                  c("Blue Corals" = "blue corals",
                    "Hard Corals" = "hard corals",
                    "Sea Fans" =  "sea fans",
                    "Sea Pens" = "sea pens",
                    "Soft Corals" = "soft corals")),
      # creating a checkbox that can be used to select smooth fit.
      # mentioning False, so it will be deselected
    checkboxInput("smoothening","Select to show Smooth Line",FALSE)),
    # showing a plot of the generated distribution
    mainPanel
    (
      # h4, size of the plot title
      h4(textOutput("plot_title")),
      # function to output the plot function in the server script
      plotOutput("bleaching_plot"))),
  # function to output the map in the leaflet function
  leafletOutput("coral_bleaching_map")
    )

# server script listening from UI script on where and what to display
SERVER <- function(input,output)
{
  # reactiveText is used to automatically re-execute when inputs change
  # function specifying the title of the shiny plot
  output$plot_title <- reactiveText(function(){
    # text displayed in the title of the shiny plot
    # toupper() function to show the coral type in capital
    paste("You have selected the coral type:",toupper(input$type))
  })
  # function for the shiny plot
  output$bleaching_plot <- reactivePlot(function(){
  # initializing to store the rows of the coral that will be 
  # selected through the shiny user interface
  coral <- coral_data[coral_data$coralType==input$type,]
  # creating the plot function i.e. the ggplot
  # two scenarios:
  # 1) when smoothening is deselected i.e. 
  # either by default when opening the plot or by manually, and
  # 2) when smoothening is selected manually
  # Case 1:
  if (input$smoothening == FALSE)
    {
    # ggplot function, same as the static visualization plot
    # however, in the case 1, geom_smooth is not used as 
    # case 1 deals with smoothening not selected
    plot <-ggplot(coral, aes(year,value)) + 
           geom_point() +
           # here we are ordering the bleaching sites by the values
           facet_grid(coralType~reorder(location, value)) +
           scale_x_continuous(breaks=c(2010,2013,2016)) +
           theme(axis.text.x = element_text(face = "bold", angle = 65)) +        
           labs(y = 'Bleaching Percentage(%)',x = 'Year (2010-2017)')
    print(plot)
    }
  
  # Case 2:
  else
    {
    # same function as the static visualization
    # with the geom_smooth function
    plot <-ggplot(coral, aes(year,value)) + 
           geom_point() +
           # here we are ordering the bleaching sites by the values
           facet_grid(coralType~reorder(location, value)) +
           scale_x_continuous(breaks=c(2010,2013,2016)) +
           theme(axis.text.x = element_text(face = "bold", angle = 65)) +
           geom_smooth(method="lm",formula=y~poly(x,2),se=FALSE) +
           labs(y = 'Bleaching Percentage(%)',x = 'Year (2010-2017)')
    print(plot)}
    }
  )
  # leaflet script for the map
  output$coral_bleaching_map <- renderLeaflet(
    {
    leaflet() %>%
    addTiles() %>%
    # this below function specifies which data needs to be used
    # popup function will show the label of the site when clicked
    addMarkers(data = coral_data, ~longitude,  ~latitude, 
               popup = toupper(coral_data$location))
    
    }
  )
}
# calling the UI and the server scripts
shinyApp(ui = UI, server = SERVER)
