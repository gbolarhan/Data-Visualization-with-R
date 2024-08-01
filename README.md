## Retail Sales Forecasting Application

This R Shiny application provides a user interface for visualizing historical sales data and generating sales forecasts for a selected store. The application uses various R packages for data manipulation, visualization, and forecasting.

### Features

- **Historical Sales Data Visualization**: View historical weekly sales data for a selected store.
- **Sales Forecasting**: Generate and visualize sales forecasts using the ARIMA model.

### Setup Instructions

#### Prerequisites

Ensure you have R and RStudio installed on your machine. You can download them from the following links:

- [R](https://cran.r-project.org/)
- [RStudio](https://www.rstudio.com/products/rstudio/download/)

#### Installation

1. **Install Required Packages**

   Open R or RStudio and run the following command to install the necessary packages:

   ```r
        install.packages(c("fastmap", "shiny", "forecast", "ggplot2", "tidyverse", "DT"))
   ```

2. **Load the Libraries**
   Ensure the required libraries are loaded by running:

```r
    library(fastmap)
    library(shiny)
    library(forecast)
    library(ggplot2)
    library(tidyverse)
    library(DT)
```

### Running the Application

1. Download the Application Code

Save the provided R script (IndividualAssignment.R) to your local machine.

2. Run the Application

To run the application, use the following command in your R console:

```r
    shiny::runApp("path/to/IndividualAssignment.R")
```

Replace "path/to/IndividualAssignment.R" with the actual path to the script file.

### Application Usage

1. Select Store
   Use the dropdown menu to select the store for which you want to view historical sales data and generate forecasts.

2. Generate Forecast
   Click the "Generate Forecast" button to create a sales forecast for the selected store. The application will display the forecasted sales alongside the actual sales for the most recent quarter.

### Application Structure

- **UI (User Interface)**: Defined using fluidPage, sidebarLayout, and mainPanel to create a user-friendly interface.
- **Server**: Contains the logic for data processing, visualization, and forecasting.
  - **Reactive Data**: Filters sales data based on the selected store.
  - **Historical Sales Table and Plot**: Displays historical sales data in a table and plot.
  - **Sales Forecast**: Uses the ARIMA model to forecast sales and visualize the results.

### Data Source

The sales data is sourced from a CSV file hosted online:

```r
    fsales <- "https://raw.githubusercontent.com/multidis/hult-inter-bus-reports-r/main/forecasting/sales_weekly.csv"
    sales <- read_csv(fsales)
```

### Troubleshooting

If you encounter any issues, ensure all packages are correctly installed and loaded. If the problem persists, try reinstalling the problematic package(s).

### License

This project is licensed under the MIT License. See the LICENSE file for details.

#### Acknowledgments

- The _shiny_ package for creating interactive web applications.
- The _forecast_ package for time series forecasting.
- The _tidyverse_ and _ggplot2_ packages for data manipulation and visualization.
- The _DT_ package for rendering data tables.
  Feel free to contribute to this project by submitting issues or pull requests. Happy forecasting! ```
