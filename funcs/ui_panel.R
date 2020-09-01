
### Function to define data panel UI

ui_data_panel = function(country_name_id,
                         country_name,
                         df_data, 
                         data_source){

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
                            paste0("Covid-19 pandemic evolution in ", country_name)
                        ),
                        h3(paste0("Updated in ", format(df_data$date[nrow(df_data)],
                                                        format = "%d/%m/%Y"))),
                        br()
                    ),
                    
                    column(4, valueBoxOutput(paste0("card_total_cases_", country_name_id),
                                             width = 12) %>% withSpinner(type = 4,
                                                                         proxy.height = "100px")),
                    column(4, valueBoxOutput(paste0("card_total_deaths_", country_name_id),
                                             width = 12) %>% withSpinner(type = 4,
                                                                         proxy.height = "100px")),
                    column(4, valueBoxOutput(paste0("card_total_recovered_", country_name_id),
                                             width = 12) %>% withSpinner(type = 4,
                                                                         proxy.height = "100px")),
                    
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
            # Cumulative plots:
            box(title = "Cumulative",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                background = "purple",
                fluidRow(
                    box(title = "Cases",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 4,
                        collapsible = FALSE,
                        fluidRow(
                            column(12,
                                radioButtons(inputId = ifelse(country_name_id == "World",
                                                              "scale_cum_cases_world",
                                                              "scale_cum_cases"),
                                             label = NULL,
                                             choices = c("linear", "log"),
                                             selected = "linear",
                                             width = "100%",
                                             inline = TRUE),
                                plotlyOutput(paste0("plot_cumulative_cases_", country_name_id)) %>% 
                                    withSpinner(type = 4,
                                                proxy.height = "400px")
                            )
                        )
                    ),
                    box(title = "Deaths",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 4,
                        collapsible = FALSE,
                        fluidRow(
                            column(12,
                                radioButtons(inputId = ifelse(country_name_id == "World",
                                                              "scale_cum_deaths_world",
                                                              "scale_cum_deaths"),
                                             label = NULL,
                                             choices = c("linear", "log"),
                                             selected = "linear",
                                             width = "100%",
                                             inline = TRUE),
                                plotlyOutput(paste0("plot_cumulative_deaths_", country_name_id)) %>% 
                                    withSpinner(type = 4,
                                                proxy.height = "400px")
                            )
                        )
                    ),
                    box(title = "Recovered",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 4,
                        collapsible = FALSE,
                        fluidRow(
                            column(12,
                                radioButtons(inputId = ifelse(country_name_id == "World",
                                                              "scale_cum_recovered_world",
                                                              "scale_cum_recovered"),
                                             label = NULL,
                                             choices = c("linear", "log"),
                                             selected = "linear",
                                             width = "100%",
                                             inline = TRUE),
                                plotlyOutput(paste0("plot_cumulative_recovered_", country_name_id)) %>% 
                                    withSpinner(type = 4,
                                                proxy.height = "400px")
                            )
                        )
                    )
                )
            ),
            
            # Daily plots:
            box(title = "Daily",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                background = "light-blue",
                fluidRow(
                    box(title = "Cases",
                        status = "success",
                        solidHeader = TRUE,
                        width = 4,
                        collapsible = FALSE,
                        fluidRow(
                            column(12,
                                radioButtons(inputId = ifelse(country_name_id == "World",
                                                              "scale_day_cases_world",
                                                              "scale_day_cases"),
                                             label = NULL,
                                             choices = c("linear", "log"),
                                             selected = "linear",
                                             width = "100%",
                                             inline = TRUE),
                                plotlyOutput(paste0("plot_perday_cases_", country_name_id)) %>% 
                                    withSpinner(type = 4,
                                                proxy.height = "400px")
                            )
                        )
                    ),
                    box(title = "Deaths",
                        status = "success",
                        solidHeader = TRUE,
                        width = 4,
                        collapsible = FALSE,
                        fluidRow(
                            column(12,
                                radioButtons(inputId = ifelse(country_name_id == "World",
                                                              "scale_day_deaths_world",
                                                              "scale_day_deaths"),
                                             label = NULL,
                                             choices = c("linear", "log"),
                                             selected = "linear",
                                             width = "100%",
                                             inline = TRUE),
                                plotlyOutput(paste0("plot_perday_deaths_", country_name_id)) %>% 
                                    withSpinner(type = 4,
                                                proxy.height = "400px")
                            )
                        )
                    ),
                    box(title = "Recovered",
                        status = "success",
                        solidHeader = TRUE,
                        width = 4,
                        collapsible = FALSE,
                        fluidRow(
                            column(12,
                                radioButtons(inputId = ifelse(country_name_id == "World",
                                                              "scale_day_recovered_world",
                                                              "scale_day_recovered"),
                                             label = NULL,
                                             choices = c("linear", "log"),
                                             selected = "linear",
                                             width = "100%",
                                             inline = TRUE),
                                plotlyOutput(paste0("plot_perday_recovered_", country_name_id)) %>% 
                                    withSpinner(type = 4,
                                                proxy.height = "400px")
                            )
                        )
                    )
                )
            )
        )
    
    )

}

