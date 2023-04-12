library(shiny)
library(ggplot2)
library(dplyr)

#list.files("Resources")
# Load the "train" dataset
train <- read.csv("train.csv")

train_data <- dplyr:: filter(train, Neighborhood == "Edwards" | Neighborhood == "NAmes" | Neighborhood == "BrkSide")





# Define UI
ui <- fluidPage(
  
  titlePanel("Housing Price Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Select the neighborhood class
      selectInput("neighborhood", "Select Neighborhood Class", choices = c("All", unique(train_data$Neighborhood))),
      
      # Checkbox to display x-axis on a logarithmic scale
      checkboxInput("log_x", "Logarithmic x-axis", value = FALSE),
      
      # Checkbox to display y-axis on a logarithmic scale
      checkboxInput("log_y", "Logarithmic y-axis", value = FALSE)
      
    ),
    
    mainPanel(
      
      # Display the ggplot
      plotOutput("housingPlot")
      
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Define color dictionary
  color_dict <- c("NAmes" = "violet", "BrkSide" = "turquoise", "Edwards" = "coral")
  
  # Filter the dataset based on the selected neighborhood class
  filtered_data <- reactive({
    if(input$neighborhood == "All") {
      train_data
    } else {
      train_data %>% filter(Neighborhood == input$neighborhood)
    }
  })
  
  # Create the ggplot based on the filtered data
  output$housingPlot <- renderPlot({
    
    # Apply logarithmic scaling to x-axis if selected
    x_scale <- if(input$log_x) {scale_x_continuous(trans = "log")} else {NULL}
    
    # Apply logarithmic scaling to y-axis if selected
    y_scale <- if(input$log_y) {scale_y_continuous(trans = "log")} else {NULL}
    
    ggplot(filtered_data(), aes(x = GrLivArea, y = SalePrice, color = Neighborhood)) +
      geom_point() +
      scale_color_manual(values = color_dict) +
      x_scale + y_scale
  })
}


# Run the app
shinyApp(ui, server)
