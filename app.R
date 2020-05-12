
# load packages
library(shiny)
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(maps)
library(tidyverse)

# read in data
map.world <- map_data("world")
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

eruptions_volcanos <- eruptions %>%
    left_join(volcano,
               by = c("volcano_number", "latitude", "longitude", "volcano_name"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Volcanic Eruptions Over Time"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            strong("Explore volcanic eruptions over time with this interactive map."),

            em("Data from https://www.himalayandatabase.com/"),

            sliderInput("year",
                        label = "Filter by Date Range (year eruption started):",
                        min = -11345,
                        max = 2020,
                        value = c(-11345,2020)
                        ),

            sliderInput("elevation",
                        label = "Filter by Elevation Range:",
                        min = -2500,
                        max = 6879,
                        value = c(-2500,6879)
                        ),

            sliderInput("count",
                        label = "Filter to only see volcanos that have erupted a specific number of times within this date range:",
                        min = 1,
                        max = 241,
                        value = c(1, 241)
                        ),

            sliderInput("population",
                        label = "Filter by population within 100km:",
                        min = 0,
                        max = 40640105,
                        value = c(0, 40640105)
                        ),

            checkboxGroupInput("category",
                               label = "Filter by Eruption Category:",
                               choices = c("Confirmed Eruption",
                                           "Uncertain Eruption",
                                           "Discredited Eruption"),
                               selected = c("Confirmed Eruption",
                                            "Uncertain Eruption",
                                            "Discredited Eruption")
                               ),

            checkboxGroupInput("type",
                               label = "Filter by Primary Volcano Type",
                               choices = c( "Shield(s)", "Stratovolcano", "Stratovolcano(es)",
                                            "Caldera", "Submarine", "Shield", "Volcanic field", "Fissure vent(s)",
                                            "Compound", "Complex", "Pyroclastic shield", "Pyroclastic cone(s)",
                                            "Pyroclastic cone", "Caldera(s)", "Lava dome(s)",
                                            "Lava cone", "Crater rows", "Maar(s)",
                                            "Tuff cone(s)", "Complex(es)","Subglacial",
                                           "Lava dome","Lava cone(s)","Tuff cone",
                                           "Lava cone(es)","Stratovolcano?"
                                           ),
                               selected = c( "Shield(s)", "Stratovolcano", "Stratovolcano(es)",
                                             "Caldera", "Submarine", "Shield", "Volcanic field", "Fissure vent(s)",
                                             "Compound", "Complex", "Pyroclastic shield", "Pyroclastic cone(s)",
                                             "Pyroclastic cone", "Caldera(s)", "Lava dome(s)",
                                             "Lava cone", "Crater rows", "Maar(s)",
                                             "Tuff cone(s)", "Complex(es)","Subglacial",
                                             "Lava dome","Lava cone(s)","Tuff cone",
                                             "Lava cone(es)","Stratovolcano?"
                                             )
                               ),

            checkboxGroupInput("evidence_category",
                               label = "Filter by Evidence Category:",
                               choices = c("Eruption Observed",
                                           "Evidence Credible",
                                           "Evidence Uncertain",
                                           "Eruption Dated",
                                           "Unrest / Holocene",
                                           "NA"
                                           ),
                               selected = c("Eruption Observed",
                                            "Evidence Credible",
                                            "Evidence Uncertain",
                                            "Eruption Dated",
                                            "Unrest / Holocene",
                                            "NA"
                                            )
                               ),

            checkboxGroupInput("rock_type",
                               label = "Filter by Major Rock Type:",
                               choices = c("Andesite / Basaltic Andesite",
                                           "Basalt / Picro-Basalt",
                                           "Dacite",
                                           "Foidite",
                                           "Phonolite",
                                           "Phono-tephrite / Tephri-phonolite",
                                           "Rhyolite",
                                           "Trachybasalt / Tephrite Basanite",
                                           "Trachyandesite / Basaltic Trachyandesite",
                                           "Trachyte / Trachydacite",
                                           "NA"
                                           ),
                               selected = c("Andesite / Basaltic Andesite",
                                            "Basalt / Picro-Basalt",
                                            "Dacite",
                                            "Foidite",
                                            "Phonolite",
                                            "Phono-tephrite / Tephri-phonolite",
                                            "Rhyolite",
                                            "Trachybasalt / Tephrite Basanite",
                                            "Trachyandesite / Basaltic Trachyandesite",
                                            "Trachyte / Trachydacite",
                                            "NA"
                                            )

                               ),
            checkboxGroupInput("region",
                               label = "Filter by Region:",
                               choices = c("Indonesia",
                                           "Middle East and Indian Ocean",
                                           "México and Central America" ,
                                           "Philippines and SE Asia",
                                           "Japan, Taiwan, Marianas",
                                           "New Zealand to Fiji",
                                           "Alaska",
                                           "Kamchatka and Mainland Asia",
                                           "South America",
                                           "Kuril Islands",
                                           "Melanesia and Australia",
                                           "Africa and Red Sea",
                                           "West Indies",
                                           "Antarctica",
                                           "Hawaii and Pacific Ocean",
                                           "Atlantic Ocean",
                                           "Iceland and Arctic Ocean",
                                           "Mediterranean and Western Asia",
                                           "Canada and Western USA",
                                           "NA"
                                           ),
                               selected = c("Indonesia",
                                           "Middle East and Indian Ocean",
                                           "México and Central America" ,
                                           "Philippines and SE Asia",
                                           "Japan, Taiwan, Marianas",
                                           "New Zealand to Fiji",
                                           "Alaska",
                                           "Kamchatka and Mainland Asia",
                                           "South America",
                                           "Kuril Islands",
                                           "Melanesia and Australia",
                                           "Africa and Red Sea",
                                           "West Indies",
                                           "Antarctica",
                                           "Hawaii and Pacific Ocean",
                                           "Atlantic Ocean",
                                           "Iceland and Arctic Ocean",
                                           "Mediterranean and Western Asia",
                                           "Canada and Western USA",
                                           "NA"
                                           )
                               ),
            textInput("name",
                      label = "Filter by Specific Volcano Name (e.g. 'Etna')",
                      value = "")
            ),

        # Show a map of erruptions
        mainPanel(
           plotOutput("map")
        )
)
)

# Define server logic required to draw map
server <- function(input, output) {

    output$map <- renderPlot({
        eruptions_filtered <- eruptions_volcanos %>%
            filter(start_year >= input$year[1] & start_year <= input$year[2]) %>%
            filter(elevation >= input$elevation[1] & elevation <= input$elevation[2]) %>%
            filter(population_within_100_km >= input$population[1] & elevation <= input$population[2]) %>%
            filter(eruption_category %in% input$category) %>%
            filter(primary_volcano_type %in% input$type) %>%
            filter(evidence_category %in% input$evidence_category) %>%
            filter(region %in% input$region) %>%
            filter((major_rock_1 %in% input$rock_type)
                   | (major_rock_2 %in% input$rock_type)
                   | (major_rock_3 %in% input$rock_type)
                   | (major_rock_4 %in% input$rock_type)
                   | (major_rock_5 %in% input$rock_type)) %>%
            group_by(volcano_number) %>%
            mutate(count = n()) %>%
            filter(count >= input$count[1] & count <= input$count[2])

        if(input$name != ""){eruptions_filtered <- eruptions_filtered %>%
            filter(volcano_name == input$name)}

        ggplot() +
            geom_polygon(data = map.world,
                         aes(x = long, y = lat, group = group)) +
            geom_point(data = eruptions_filtered,
                       aes(x = latitude,
                           y = longitude,
                           color = eruption_category,
                           size = count,
                           alpha = elevation)
                       )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
