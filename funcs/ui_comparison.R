
### Function to define the countries compasion UI

ui_countries_comparison = function(df_data,
                                   data_source,
                                   countries_names){
    
    fluidRow(style = "margin-left: 5px;
                      margin-right: 5px;",

        # General info:
        fluidRow(
            box(background = "aqua",
                solidHeader = FALSE,
                width = 12,
                collapsible = FALSE,
                fluidRow(
                    column(12, 
                        h2(style = "font-weight: 700;",
                            "Comparison between countries trajectories"
                        ),
                        h3(paste0("Updated in ", format(df_data$date[nrow(df_data)],
                                                            format = "%d/%m/%Y")))
                    ),
                    column(12,
                        tags$div(style = "font-size: large;",
                            HTML(paste0("Data source: ", 
                                        tags$span(a(href = data_source, data_source))))
                        )
                    )
                )
            )
        ),
        
        # Plots:
        fluidRow(
            
            # Cases trajectories:
            box(title = "Cases",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                fluidRow(
                    column(12,
                        column(6,
                           selectizeInput(inputId = "list_countries_traj_cases",
                                          label = "Select the countries:",
                                          choices = countries_names$name,
                                          selected = "Brazil", 
                                          multiple = TRUE,
                                          options = NULL),
                        ),
                        column(6,
                            selectInput(inputId = "moving_average_traj_cases",
                                        label = "Choose the period to average over:",
                                        choices = c("1 day", "3 days", "7 days", "14 days"),
                                        selected = "1 day",
                                        width = "100%")
                        ),
                        radioButtons(inputId = "scale_trajectories_cases",
                                     label = NULL,
                                     choices = c("linear", "log"),
                                     selected = "linear",
                                     width = "100%",
                                     inline = TRUE),
                        plotlyOutput("plot_trajectories_cases",
                                     height = "500") %>% 
                            withSpinner(type = 4,
                                        proxy.height = "400px")
                    )
                )
            ),
            
            # Deaths trajectories:
            box(title = "Deaths",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                fluidRow(
                    column(12,
                        column(6,
                           selectizeInput(inputId = "list_countries_traj_deaths",
                                          label = "Select the countries:",
                                          choices = countries_names$name,
                                          selected = "Brazil", 
                                          multiple = TRUE,
                                          options = NULL),
                        ),
                        column(6,
                            selectInput(inputId = "moving_average_traj_deaths",
                                        label = "Choose the period to average over:",
                                        choices = c("1 day", "3 days", "7 days", "14 days"),
                                        selected = "1 day",
                                        width = "100%")
                        ),
                        radioButtons(inputId = "scale_trajectories_deaths",
                                     label = NULL,
                                     choices = c("linear", "log"),
                                     selected = "linear",
                                     width = "100%",
                                     inline = TRUE),
                        plotlyOutput("plot_trajectories_deaths",
                                     height = "500") %>% 
                            withSpinner(type = 4,
                                        proxy.height = "400px")
                    )
                )
            ),
            
            # Recovered trajectories:
            box(title = "Recovered",
                status = "danger",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                fluidRow(
                    column(12,
                        column(6,
                           selectizeInput(inputId = "list_countries_traj_recovered",
                                          label = "Select the countries:",
                                          choices = countries_names$name,
                                          selected = "Brazil", 
                                          multiple = TRUE,
                                          options = NULL),
                        ),
                        column(6,
                            selectInput(inputId = "moving_average_traj_recovered",
                                        label = "Choose the period to average over:",
                                        choices = c("1 day", "3 days", "7 days", "14 days"),
                                        selected = "1 day",
                                        width = "100%")
                        ),
                        radioButtons(inputId = "scale_trajectories_recovered",
                                     label = NULL,
                                     choices = c("linear", "log"),
                                     selected = "linear",
                                     width = "100%",
                                     inline = TRUE),
                        plotlyOutput("plot_trajectories_recovered",
                                     height = "500") %>% 
                            withSpinner(type = 4,
                                        proxy.height = "400px")
                    )
                )
            )
        )
    )
}



