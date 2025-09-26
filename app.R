library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(readr)
library(rnaturalearth)
library(sf)
library(bslib)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  fluidRow(
    column(12, tags$h2("Science Engagement Initiatives in Africa", class = "map-title")),
    column(12, tags$h4("Click on a green country to display initiatives", class = "subtitle")),
    column(7, leafletOutput("map", height = "650px")),
    column(
      5,
      DTOutput("initiative_table"),
      # disclaimer actionLink ----
      div(
        style = "text-align: center; margin-top: 10px;", # Basic styling
        actionLink("show_disclaimer", "Disclaimer for external links")
      )
    )
  ),
)

# Define Server
server <- function(input, output, session) {
  
  # Load African country boundaries
  africa <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf") %>%
    mutate(name = trimws(tolower(name)))

  # Group initiatives by country and count the number of initiatives
  country_links <- initiatives %>%
    mutate(
      name_of_initiative = coalesce(name_of_initiative, "Unknown Initiative"),
      website = coalesce(website, "#")
    ) %>%
    group_by(country) %>%
    summarize(
      links = paste0(
        "<a href='", website, "' target='_blank'>", name_of_initiative, "</a>",
        collapse = "<br>"
      ),
      initiative_count = n(),  # Add initiative count
      .groups = "drop"
    )

  # Merge initiatives with map data
  africa <- left_join(africa, country_links, by = c("name" = "country"))
  
  # Ensure initiative_count exists and replace NA with 0
  africa$initiative_count <- ifelse(is.na(africa$initiative_count), 0, africa$initiative_count)
  
  
  # Define color scale
  color_pal <- colorFactor(
    palette = colorRampPalette(c("#ACE1AF", "darkgreen"))(5),  # Light to dark green
    domain = unique(africa$initiative_count),  # Ensures we map exact values, not ranges
  )

  # Set colors based on presence of initiatives
  #africa$color <- ifelse(!is.na(africa$links), "#2aa145", "white")
  
  # Assign colors based on initiative count
  africa$color <- color_pal(africa$initiative_count)

  # Reactive variable for selected country
  selected_country <- reactiveVal(NULL)

  # Update selected country when clicking on map
  observeEvent(input$map_shape_click, {
    clicked_country <- trimws(tolower(input$map_shape_click$id))
    selected_country(ifelse(clicked_country == "", NULL, clicked_country))
    print(paste("Selected country:", selected_country()))
  })
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet(africa) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom = 3, maxZoom = 7)) %>%
      addPolygons(
        layerId = ~name,
        fillColor = ~color_pal(initiative_count),  
        fillOpacity = ~ifelse(initiative_count == 0, 0, 0.7),  # Lower opacity for no initiatives
        color = "darkgreen",  
        weight = 0.5,  
        stroke = TRUE,  
        highlightOptions = highlightOptions(color = "darkgreen", weight = 1.3, bringToFront = TRUE),
        popup = ~ifelse(
          initiative_count == 0,
          paste0(name, ": ", 0, " found"),
          paste0(name, ": ", initiative_count, " found"))
      ) %>%
      setView(lng = 20, lat = 0, zoom = 3) %>%
      setMaxBounds(lng1 = -25, lat1 = -40, lng2 = 60, lat2 = 40)
  })
  

  # Render initiative table with filtered results
  output$initiative_table <- renderDT({
    if (is.null(selected_country())) {
      return(datatable(
        data.frame(Message = "Initiatives will appear here."),
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE,
        escape = FALSE
      ))
    }

    df <- initiatives %>%
      filter(country == selected_country()) %>%
      mutate(Website = paste0("<a href='", website, "' target='_blank'>", website, "</a>"))

    if (nrow(df) == 0) {
      return(datatable(
        data.frame(Message = "No initiatives found for this country yet.<br>Would you like to suggest an initiative? <a href='mailto:marvin@underthemicroscope.net?subject=Initiative%20Suggestion&body=Please%20provide%20the%20following%20details%20for%20the%20suggested%20initiative:%0D%0A%0D%0AInitiative%20Name:%20%0D%0AWebsite%20URL:%20%0D%0ACountry/Countries:%20'>click here</a>"),
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE,
        escape = FALSE
      ))
    }

    datatable(
      df %>% select("Name of Initiative" = name_of_initiative, Website),
      escape = FALSE,
      rownames = TRUE,
      class = "compact stripe",
      options = list(
        dom = "tip",
        pageLength = 5,
        scrollX = TRUE
      )
    )
  })


  # --- OBSERVE EVENT FOR DISCLAIMER ---
  observeEvent(input$show_disclaimer, {
    showModal(modalDialog(
      title = "Disclaimer",
      p("This table contains external links provided by the initiatives listed. We do not control and cannot guarantee the accuracy, relevance, timeliness, or completeness of this external information. Clicking these links will take you to websites outside of this application."),
      footer = modalButton("Close"), # Standard close button
      easyClose = TRUE # Allows closing by clicking outside the modal
    ))
  })
  # --- End of observeEvent ---
  
}

# Run the App
shinyApp(ui = ui, server = server)

# rsconnect::deployApp(forceUpdate = TRUE)
# rsconnect::applications()

# .rs.restartR()
