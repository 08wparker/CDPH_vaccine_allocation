#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(shinythemes)
library(tidyverse)
library(ggpubr)
library(sf)

load("zip_code_data.R")

zips_sf <- zip_codes %>%
    select(zip = ZCTA5CE10, geometry)



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("sandstone"),

    # Application title
    titlePanel("Vaccination Rates and COVID mortality"),

    # panel for inputs ----
    fluidRow(
        # Input: Selector for choosing dataset ----
        column(3, selectInput(inputId = "city",
                              label = "Choose a city:",
                              choices = c("Chicago"))),
        column(9, actionButton("do", "Load Map"))
    ),
        
        # Main panel for displaying outputs ----
    splitLayout(plotOutput("main_plot") %>% withSpinner(color="#0dc5c1"),plotOutput("cor_plot")),
    hr(),
    #verbatimTextOutput("caption_text"),
    h5("Mortality rates are cumulative COVID-19 deaths to date per 100,000 residents of the zip code."),
    uiOutput("death_data_source"),
    h5("Vaccination rates are percentage of population in the zip code receiving the first dose of either mRNA vaccine."),
    uiOutput("vax_data_source"),
    renderText("corr_text"),
    hr(),
    h4(tags$a(href = "https://github.com/08wparker/CDPH_vaccine_allocation.git", 
              "https://github.com/08wparker/CDPH_vaccine_allocation.git"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    plot_out <- eventReactive(input$do, {
        vax_data_by_zip <- read_csv("https://data.cityofchicago.org/api/views/553k-3xzc/rows.csv?accessType=DOWNLOAD") %>%
            mutate(zip = `Zip Code`) %>% 
            select(zip, `1st Dose - Percent Population`) %>%
            group_by(zip) %>%
            summarise(first_dose_rate = max(`1st Dose - Percent Population`)) %>%
            mutate(pct_vax = first_dose_rate)
        
        covid_by_zip <- read_csv("https://data.cityofchicago.org/api/views/yhhz-zm2v/rows.csv?accessType=DOWNLOAD") %>%   
            mutate(zip = `ZIP Code`) %>%
            group_by(zip) %>% 
            summarise(total_cases = sum(`Cases - Weekly`, na.rm =  TRUE ),
                      case_rate = max(`Case Rate - Cumulative`,  na.rm =  TRUE),
                      death_rate = max(`Death Rate - Cumulative`),
                      total_deaths = sum(`Deaths - Weekly`),
                      population = mean(Population)
            ) %>% mutate(death_rate_check = 100000*total_deaths/population)
        
        to_map <- vax_data_by_zip %>% 
            left_join(covid_by_zip %>% select(zip, death_rate)) %>%
            select(zip, pct_vax, death_rate) %>% 
            filter(is.infinite(pct_vax) == FALSE)
        
        
        plot_A <- zips_sf %>%
            left_join(to_map) %>% 
            filter(is.na(pct_vax) == FALSE) %>%
            ggplot(aes(geometry = geometry, fill = 100*pct_vax)) + 
            geom_sf() + 
            theme_void() + coord_sf() + 
            scale_fill_distiller(direction = 1, limits = c(0, 100*max(to_map$pct_vax))) + 
            #scale_fill_viridis_c(direction = -1) +
            labs(fill = "Residents Vaccinated (%)", title = "Vaccination") +
            theme(legend.position = "bottom", 
                  plot.title = element_text(hjust = 0.5))
        
        plot_B <- zips_sf %>%
            left_join(to_map) %>% 
            filter(is.na(pct_vax) == FALSE) %>%
            ggplot(aes(geometry = geometry, fill = death_rate)) + 
            geom_sf() + 
            theme_void() + coord_sf() + 
            scale_fill_distiller(direction = 1, limits = c(0, max(to_map$death_rate)), palette = "Reds") + 
            labs(fill = "Mortality rate\n(deaths/100,000)", title = "Deaths")+
            theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
        
        plot_C <-  to_map %>%
            ggplot(aes(x = death_rate, y = 100*pct_vax))+
            geom_point() + labs(x = "Deaths per 100,000 residents", y = "Residents vaccinated (%)")

        ggarrange(plot_B, plot_A)
    })
    
    
    cplot <- eventReactive(input$do, {
        vax_data_by_zip <- read_csv("https://data.cityofchicago.org/api/views/553k-3xzc/rows.csv?accessType=DOWNLOAD") %>%
            mutate(zip = `Zip Code`) %>% 
            select(zip, `1st Dose - Percent Population`) %>%
            group_by(zip) %>%
            summarise(first_dose_rate = max(`1st Dose - Percent Population`)) %>%
            mutate(pct_vax = first_dose_rate)
        
        covid_by_zip <- read_csv("https://data.cityofchicago.org/api/views/yhhz-zm2v/rows.csv?accessType=DOWNLOAD") %>%   
            mutate(zip = `ZIP Code`) %>%
            group_by(zip) %>% 
            summarise(total_cases = sum(`Cases - Weekly`, na.rm =  TRUE ),
                      case_rate = max(`Case Rate - Cumulative`,  na.rm =  TRUE),
                      death_rate = max(`Death Rate - Cumulative`),
                      total_deaths = sum(`Deaths - Weekly`),
                      population = mean(Population)
            ) %>% mutate(death_rate_check = 100000*total_deaths/population)
        
        to_map <- vax_data_by_zip %>% 
            left_join(covid_by_zip %>% select(zip, death_rate)) %>%
            select(zip, pct_vax, death_rate) %>% 
            filter(is.infinite(pct_vax) == FALSE)
        
        to_map %>%
            ggplot(aes(x = death_rate, y = 100*pct_vax))+
            geom_point() + labs(x = "Deaths per 100,000 residents", y = "Residents vaccinated (%)")
        
    })
    
    
    output$main_plot <- renderPlot({
        plot_out()
    })
    
    output$cor_plot <- renderPlot({
        cplot()
    })
    
    disp_text <- reactiveValues(show_source = FALSE)
    
    
    
    
    observeEvent(input$do, {
        disp_text$show_source  <- TRUE
    })
    output$vax_data_source <- renderUI({
        if (disp_text$show_source == TRUE) {
            tagList("", 
                    a(href =  "https://data.cityofchicago.org/Health-Human-Services/COVID-19-Vaccine-Doses-by-ZIP-Code-1st-Dose/c28u-q29v", "Vaccination source data"))
        }
    })
    
    output$death_data_source <- renderUI({
        if (disp_text$show_source == TRUE) {
        tagList("", a(href =  "https://data.cityofchicago.org/Health-Human-Services/COVID-19-Cases-Tests-and-Deaths-by-ZIP-Code/yhhz-zm2v", "Death source data"))
        }
    })
    
    # output$caption_text <- renderText({
    #     if (disp_text$show_source == TRUE) {
    #         paste("Mortality rates are cumulative COVID-19 deaths to date per 100,000 residents of the zip code", 
    #               "Vaccination rates are percentage of population in the zip code receiving the first dose of either mRNA vaccine.",
    #               sep = "\n"
    #         )
    #     }
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
