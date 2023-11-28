# Install required packages if not already installed
# install.packages(c("shiny", "shinydashboard", "dplyr", "plotly", "scales"))

# Load libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(scales)

# Sample dataset
sample_data <- data.frame(
  dtcontrato = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01")),
  sales = c(100000, 150000, 120000, 200000, 180000),
  product = c("A", "A", "B", "B", "A")
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Monthly Total Sales"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Date Range",
        width = 3,
        dateRangeInput("dateRange", "Date Range", start = min(sample_data$dtcontrato), end = max(sample_data$dtcontrato))
      ),
      box(
        title = "Options",
        width = 4,
        checkboxInput("accumulatedCheckbox", "Accumulated", value = FALSE),
        checkboxInput("splitCheckbox", "Split", value = FALSE)
      )
    ),
    fluidRow(
      box(
        title = "Monthly Total Sales",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        plotlyOutput("linePlot")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$linePlot <- renderPlotly({
    # Filter data based on date range
    filtered_data <- sample_data %>%
      filter(dtcontrato >= input$dateRange[1] & dtcontrato <= input$dateRange[2])
    
    # Check if the split checkbox is checked
    if (input$splitCheckbox) {
      # Check if the accumulated checkbox is also checked
      if (input$accumulatedCheckbox) {
        # Compute accumulated sales for each product
        filtered_data <- filtered_data %>%
          group_by(product) %>%
          arrange(product, dtcontrato) %>%
          mutate(accumulated_sales = cumsum(sales))
        
        # Create a separate line for each product with accumulated sales
        line_plot <- plot_ly(data = filtered_data, x = ~dtcontrato, y = ~accumulated_sales, color = ~product, type = 'scatter', mode = 'lines+text', text = ~scales::comma(accumulated_sales / 1000, scale = 0.1), textposition = 'top center')
      } else {
        # Create a separate line for each product
        line_plot <- plot_ly(data = filtered_data, x = ~dtcontrato, y = ~sales, color = ~product, type = 'scatter', mode = 'lines+text', text = ~scales::comma(sales / 1000, scale = 0.1), textposition = 'top center')
      }
    } else {
      # Aggregate monthly total sales for all products
      monthly_total <- filtered_data %>%
        group_by(month = format(dtcontrato, "%Y-%m")) %>%
        summarise(total_sales = sum(sales))
      
      # Check if the accumulated checkbox is checked
      if (input$accumulatedCheckbox) {
        # Calculate accumulated sum of sales
        monthly_total$total_sales <- cumsum(monthly_total$total_sales)
      }
      
      # Create plotly line plot with formatted value labels
      line_plot <- plot_ly(x = ~as.Date(paste0(monthly_total$month, "-01")),
                           y = ~monthly_total$total_sales,
                           type = 'scatter',
                           mode = 'lines+text',
                           text = scales::comma(monthly_total$total_sales / 1000, scale = 0.1),
                           textposition = 'top center',
                           line = list(color = 'blue'))
    }
    
    # Layout for the plot
    layout <- list(title = if (input$accumulatedCheckbox) "Accumulated Monthly Total Sales" else if (input$splitCheckbox) "Monthly Total Sales by Product" else "Monthly Total Sales",
                   xaxis = list(title = "Date", tickvals = ~as.Date(paste0(filtered_data$dtcontrato, "-01")), tickformat = "%b %Y"),
                   yaxis = list(title = if (input$accumulatedCheckbox) "Accumulated Total Sales (in '000s)" else if (input$splitCheckbox) "Total Sales per Product (in '000s)" else "Total Sales (in '000s)", rangemode = "tozero"))
    
    # Combine the line plot and layout
    line_plot %>% layout(layout)
  })
}

# Run the application
shinyApp(ui, server)
