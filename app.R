
require(coronavirus)
require(shiny)
require(shinydashboard)
require(dplyr)
require(tidyr)
require(plotly)
require(stats)
require(shinycssloaders)
require(leaflet)
require(jsonlite)
require(stringr)
require(geojsonio)


# Paths:
path_funcs = "funcs/"
path_data =  "data_offline/"

# Load the geojson for the map:
# Source: https://datahub.io/core/geo-countries/datapackage.json
countries_pol = geojsonio::geojson_read(paste0(path_data, "countries.geojson"),
                                        what = "sp")

# Functions:
source(paste0(path_funcs, "prepare_dataset.R"))
source(paste0(path_funcs, "plots.R"))
source(paste0(path_funcs, "cards.R"))
source(paste0(path_funcs, "maps.R"))
source(paste0(path_funcs, "ui_panel.R"))
source(paste0(path_funcs, "ui_comparison.R"))
source(paste0(path_funcs, "ui_maps.R"))
source(paste0(path_funcs, "multigsub.R"))

# Default values:
data_source = "https://ramikrispin.github.io/coronavirus/"
cases_color = "#f49a17"
deaths_color = "#fe4934"
recovered_color = "#2a7a16"
options(spinner.size = 0.5)
options(spinner.color = "#0dc5c1")

# Update the dataset:
update_dataset(silence = TRUE)

# Countries list:
df_countries = data.frame("name" = coronavirus$country %>% 
                                       unique(),
                          stringsAsFactors = FALSE)
df_countries$name_id = df_countries$name %>%
                           toupper() %>%
                           multigsub(x = .,
                                     patterns = c(" ", "(", ")", "*", "-", "'", ","),
                                     subs = c("_", "", "", "", "_", "_", "_"))

# World data:
df_world = prepare_dataset(country_chosen = "World")



### Backend

server = function(input, output, session){

    # Restart the R session to refresh the data from the API:
    observeEvent(input$button_reload_session, {
        aggg_result = -1
        if(aggg_result == -1){
            .rs.restartR()
            session$reload()
            return()
        }
    })    
    
    ### Dataset choosen from input ###
    
    values = reactiveValues(
        df_contry = prepare_dataset(country_chosen = "Brazil")
    )

    observe({
        values$df_contry = prepare_dataset(country_chosen = input$country_id)
        values$country_name = (df_countries %>%
                                   dplyr::filter(name == input$country_id))$name[1]
    })

    ### Plots ###

    # By country:
    output$plot_cumulative_cases_country_ID = renderPlotly({
        plot_cumulative_cases(df = values$df_contry,
                              type_color = cases_color,
                              type_scale = input$scale_cum_cases)
    })
    output$plot_cumulative_deaths_country_ID = renderPlotly({
        plot_cumulative_deaths(df = values$df_contry,
                               type_color = deaths_color,
                               type_scale = input$scale_cum_deaths)
    })
    output$plot_cumulative_recovered_country_ID = renderPlotly({
        plot_cumulative_recovered(df = values$df_contry,
                                  type_color = recovered_color,
                                  type_scale = input$scale_cum_recovered)
    })
    output$plot_perday_cases_country_ID = renderPlotly({
        plot_perday_cases(df = values$df_contry,
                          type_color = cases_color,
                          type_scale = input$scale_day_cases)
    })
    output$plot_perday_deaths_country_ID = renderPlotly({
        plot_perday_deaths(df = values$df_contry,
                           type_color = deaths_color,
                           type_scale = input$scale_day_deaths)
    })
    output$plot_perday_recovered_country_ID = renderPlotly({
        plot_perday_recovered(df = values$df_contry,
                              type_color = recovered_color,
                              type_scale = input$scale_day_recovered)
    })

    # World:
    output$plot_cumulative_cases_World = renderPlotly({
        plot_cumulative_cases(df = df_world,
                              type_color = cases_color,
                              type_scale = input$scale_cum_cases_world)
    })
    output$plot_cumulative_deaths_World = renderPlotly({
        plot_cumulative_deaths(df = df_world,
                               type_color = deaths_color,
                               type_scale = input$scale_cum_deaths_world)
    })
    output$plot_cumulative_recovered_World = renderPlotly({
        plot_cumulative_recovered(df = df_world,
                                  type_color = recovered_color,
                                  type_scale = input$scale_cum_recovered_world)
    })
    output$plot_perday_cases_World = renderPlotly({
        plot_perday_cases(df = df_world,
                          type_color = cases_color,
                          type_scale = input$scale_day_cases_world)
    })
    output$plot_perday_deaths_World = renderPlotly({
        plot_perday_deaths(df = df_world,
                           type_color = deaths_color,
                           type_scale = input$scale_day_deaths_world)
    })
    output$plot_perday_recovered_World = renderPlotly({
        plot_perday_recovered(df = df_world,
                              type_color = recovered_color,
                              type_scale = input$scale_day_recovered_world)
    })

    # Trajectories:
    output$plot_trajectories_cases = renderPlotly({
        plot_countries_trajectories_cases(countries = input$list_countries_traj_cases,
                                          type_scale = input$scale_trajectories_cases,
                                          mov_avg = input$moving_average_traj_cases)
    })
    output$plot_trajectories_deaths = renderPlotly({
        plot_countries_trajectories_deaths(countries = input$list_countries_traj_deaths,
                                           type_scale = input$scale_trajectories_deaths,
                                           mov_avg = input$moving_average_traj_deaths)
    })
    output$plot_trajectories_recovered = renderPlotly({
        plot_countries_trajectories_recovered(countries = input$list_countries_traj_recovered,
                                              type_scale = input$scale_trajectories_recovered,
                                              mov_avg = input$moving_average_traj_recovered)
    })

    ### Cards ###

    # By country:
    output$card_total_cases_country_ID = renderValueBox({card_total_cases(values$df_contry)})
    output$card_total_deaths_country_ID = renderValueBox({card_total_deaths(values$df_contry)})
    output$card_total_recovered_country_ID = renderValueBox({card_total_recovered(values$df_contry)})

    # World:
    output$card_total_cases_World = renderValueBox({card_total_cases(df_world)})
    output$card_total_deaths_World = renderValueBox({card_total_deaths(df_world)})
    output$card_total_recovered_World = renderValueBox({card_total_recovered(df_world)})

    ### Interactively render the data panel for a country ###

    output$ui_panel_country = renderUI({
        ui_data_panel(country_name_id = "country_ID",
                      country_name = values$country_name,
                      df_data = values$df_contry,
                      data_source = data_source)

    })

    ### Maps ###
    output$map_generalized = renderLeaflet({
        create_map(covid_var = input$cdr_cdrpop,
                   countries_pol = countries_pol)
    })
    
}


### Frontend

sidebar = dashboardSidebar(
    sidebarMenu(
        div(class = "sidemenubox",
            menuItem(text = "Country panel", 
                     tabName = "plots_country", 
                     icon = icon("chart-bar")),
            selectInput(inputId = "country_id",
                        label = "Choose the country",
                        choices = df_countries$name,
                        selected = "Brazil",
                        width = "100%")
        ),
        div(class = "sidemenubox",
            menuItem(text = "World panel",
                     tabName = "plots_world",
                     icon = icon("chart-bar"))
        ),
        div(class = "sidemenubox",
            menuItem(text = "Countries comparison",
                     tabName = "plots_comparison",
                     icon = icon("chart-line"))
        ),
        div(class = "sidemenubox",
            menuItem(text = "Maps",
                     tabName = "maps_world",
                     icon = icon("globe-africa"))
        )
    ),
    actionButton(inputId = "button_reload_session",
                 label = "Update Data",
                 class = "btn-primary")
)

body = dashboardBody(
    tags$head(tags$style(HTML(paste0(
                                     "
                                     .box-header {
                                         text-align: center;
                                     }
                                     .box-title {
                                         font-weight: 800;
                                     }
                                     .box {
                                        border-top: 3px solid #0073b7;
                                     }
                                     .box.box-solid.box-primary>.box-header{
                                         background-color: #413c9b;
                                     }
                                     .box.box-solid.box-success>.box-header{
                                         background-color: #075e91;
                                     }
                                     .box.box-solid.box-info>.box-header{
                                         background-color: ", cases_color, ";
                                     }
                                     .box.box-solid.box-warning>.box-header{
                                         background-color: ", deaths_color, ";
                                     }
                                     .box.box-solid.box-danger>.box-header{
                                         background-color: ", recovered_color, ";
                                     }
                                     .box.box-solid.box-info{
                                         background-color: #f9c67d;
                                         border: solid #f9c67d 1px;
                                         padding-bottom: 5px;
                                     }
                                     .box.box-solid.box-warning{
                                         background-color: #f79388;
                                         border: solid #f79388 1px;
                                         padding-bottom: 5px;
                                     }
                                     .box.box-solid.box-danger{
                                         background-color: #7da274;
                                         border: solid #7da274 1px;
                                         padding-bottom: 5px;
                                     }
                                     .box.box-solid.bg-aqua{
                                         background-color: #036cdd !important;
                                     }
                                     .small-box>.inner{
                                         border: #d2d2d2 1px solid;
                                         border-radius: 4px;
                                     }
                                     a:link, a:visited {
                                         color: #f6fc8c;
                                     }
                                     a:hover, a:active {
                                         color: #e7b3ea;
                                     }
                                     .shiny-input-radiogroup {
                                         color: black;
                                         text-align: center;
                                         font-size: 16px;
                                     }
                                     .sidemenubox {
                                          border: solid rgba(222, 214, 222, 0.3) 2px;
                                          border-radius: 5px;
                                          margin: 6px;
                                          padding: 6px;
                                          font-weight: 800;
                                     }
                                     #cdr_cdrpop.shiny-input-radiogroup {
                                         text-align: left;
                                     }
                                     .small-box h3 {
                                         font-size: 2vw;
                                     }
                                     .btn-primary {
                                         color: white !important;
                                     }
                                     #card_total_cases_", df_countries$name_id, " .small-box.bg-orange {
                                                 background-color: ", cases_color, " !important;
                                     }
                                     #card_total_deaths_", df_countries$name_id, " .small-box.bg-red {
                                                 background-color: ", deaths_color, " !important;
                                     }
                                     #card_total_recovered_", df_countries$name_id, " .small-box.bg-green {
                                                 background-color: ", recovered_color, " !important;
                                     }"
                                    )))),

    tabItems(
        
        # Choosen country:
        tabItem(tabName = "plots_country",
            uiOutput("ui_panel_country")
        ),
        
        # World:
        tabItem(tabName = "plots_world",
            ui_data_panel(country_name_id = "World",
                          country_name = "the World",
                          df_data = df_world,
                          data_source = data_source)
        ),

        # Comparision of countries:
        tabItem(tabName = "plots_comparison",
            ui_countries_comparison(df_data = df_world,
                                    data_source = data_source,
                                    countries_names = df_countries)

        ),

        # Maps:
        tabItem(tabName = "maps_world",
            ui_world_maps(df_data = df_world,
                          data_source = data_source,
                          countries_names = df_countries)
        )
        
    )
)

ui = dashboardPage(
    dashboardHeader(title = "COVID-19 Dashboard"),
    sidebar,
    body
)


# Run the application 
shinyApp(ui = ui, server = server)



