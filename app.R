library(tidyverse)
library(sf)
library(tidygeocoder)
library(shiny)
library(leaflet)
library(bslib)

ui <- fillPage(
    tags$script(src = "search_on_enter.js"),
    padding = 10,
    theme = bs_theme(
        version = 4,
        bootswatch = "flatly"
    ),
    title = "NKCPS Elementary School Zone Finder",
    div(
        class = "outer",
        tags$head(
            includeCSS("styles.css"),
        )
    ),
    leafletOutput(
        outputId = "map",
        width = "100%",
        height = "100%"
    ),
    absolutePanel(
        id = "input",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 30,
        left = "auto",
        right = 30,
        bottom = "auto",
        width = 500,
        height = "auto",
        h2("Elementary Zone Finder"),
        textInput(
            inputId = "addr",
            label = "Enter an address",
            placeholder = "P.O. Box 110, New Kent, VA 23124"
        ),
        actionButton(
            style = "margin-bottom: 10px;",
            inputId = "submit",
            label = "Search",
            icon = icon(
              name = "search"
            )
        ),
        uiOutput(
            outputId = "zone"
        ),
        a(
            href = "https://icons8.com/icon/mr5uztlBCWtw/school",
            target = "_blank",
            "School Icons by Icons8"
        )
    )
)

server <- function(input, output, server) {
    boundaries <- st_read("shapefiles/New Kent  2022_23.shp") %>%
        st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
        left_join(
          read_csv("schools.csv")
        ) %>%
        bind_cols(Principal = c("Tammy Krejcarek", "Dr. Amy Stamm", "Kyle Moser"))

    color_palette <- colorFactor("plasma", boundaries$Name)

    labels <- sprintf(
        "<strong style=\"font-size: 18px;\">%s</strong><br/>
         <strong>Principal:  </strong>%s<br/>
         <strong><u>Address</u></strong><br/>
         %s<br/>
         %s",
        boundaries$Name,
        boundaries$Principal,
        boundaries$Street,
        paste0(boundaries$City, ", ", boundaries$State, " ", boundaries$ZIP)
    ) %>%
        lapply(htmltools::HTML)

    school_icons <- makeIcon(
        iconUrl = "./www/img/school.png",
        iconWidth = 35,
        iconHeight = 35
    )

    output$map <- renderLeaflet({
        x <- leaflet(boundaries) %>%
            addTiles() %>%
            addPolygons(
                fillColor = ~ color_palette(Name),
                color = "#333",
                dashArray = "3",
                weight = 2,
                smoothFactor = 0.5,
                opacity = 1,
                fillOpacity = 0.5,
                group = "2022 - 23 Elementary Boundaries",
                highlightOptions = highlightOptions(
                    weight = 4,
                    color = "#333",
                    fillOpacity = 0.8,
                    bringToFront = TRUE
                ),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addMarkers(
                group = "init",
                lng = ~ Longitude,
                lat = ~ Latitude,
                label = labels,
                icon = school_icons
            )
    })

    observeEvent(
        eventExpr = input$submit,
        handlerExpr = {
            address <- input$addr

            geod <- address %>%
                geo(
                    method = "census",
                    lat = latitude,
                    long = longitude
                )

            if (is.na(geod$latitude) | is.na(geod$longitude)) {
                output$zone <- renderUI({
                    p(
                        style = "margin-top: 25px; color: red;",
                        "Address not found. Please ensure you enter the full
                         street address, the city/county, and the zip code."
                    )
                })

                updateTextInput(
                  inputId = "addr",
                  value = "",
                  placeholder = "P.O. Box 110, New Kent, VA 23124"
                )
            } else {
                addr_label <- sprintf("<p style=\"font-size: 12px;\">%s</p>", geod$address) %>%
                    lapply(htmltools::HTML)

                leafletProxy(mapId = "map") %>%
                    removeMarker(layerId = "addr") %>%
                    addMarkers(
                        layerId = "addr",
                        lng = geod$longitude,
                        lat = geod$latitude,
                        label = addr_label
                    )

                geod_sf <- geod %>%
                    st_as_sf(
                        coords = c("longitude", "latitude"),
                        crs = st_crs(boundaries)
                    )

                zone <- geod_sf %>%
                    mutate(
                      intersection = as.integer(st_intersects(geometry, boundaries)),
                      name = if_else(
                        condition = is.na(intersection),
                        true = "Not found in NKCPS",
                        false = boundaries$Name[intersection])
                    )

                output$zone <- renderUI({
                    tagList(
                        p(
                          style = "margin-top: 25px;",
                          paste0("Student Address:  ", address)
                        ),
                        p(
                            style = "font-size: 24px; color: #1F2B5D;",
                            strong(paste0("Register for:  ", zone$name)))
                    )
                })

                updateTextInput(
                  inputId = "addr",
                  value = "",
                  placeholder = "P.O. Box 110, New Kent, VA 23124"
                )
            }
        }
    )
}

shinyApp(ui, server)
