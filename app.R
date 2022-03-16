library(dash)
library(dashHtmlComponents)
library(dplyr)
library(plotly) 
library(jsonlite)

app <- Dash$new(external_stylesheets = dbcThemes$CYBORG)

imdb <- read.csv("data/imdb_2011-2020.csv")

selected_genres <- as.list(unique(imdb$genres))

# Set the layout of the app
app %>% set_layout(
  dccStore(id = "filtered_data"),
  dbcContainer(
    list(
      htmlDiv(
        dbcRow(
          list(
            dbcCol(
              list(
                htmlImg(
                  src = "assets/projector.gif",
                  id = "r",
                  width = "100%"
                )
              ),
              width = 1
            ),
            dbcCol(
              htmlStrong(
                children=list(
                  htmlDiv(
                    "IMDb Dashboard",
                    style = list(fontSize = 50, textAlign = "right", display="inline", color = "#DBA506")
                  ),
                  htmlDiv(
                    "-------------------------------------------------------------------------------",
                    style = list(fontSize = 20, textAlign = "right", display="inline", color = "#000000")
                  ),
                  htmlDiv(
                    "Plan your next movie.",
                    style = list(fontSize = 20, textAlign = "right", display="inline", color = "#F2DB83")
                  )
                ),
                style = list(width = "100%", verticalAlign = "bottom", borderBottom = "2px solid gold")
                # children=list(
                #     htmlH1(
                #       "IMDb Dashboard",
                #       style = list(textAlign = "left", color = "#DBA506")
                #       ),
                #     htmlH5(
                #       "Plan your next movie.",
                #       style = list(textAlign = "right", color = "#F2DB83", borderBottom = "2px solid gold")
                #     )
              ),
              width = 11
            )
          )
        )
      ),
      htmlImg(
        src = "assets/reel.png",
        id = "image",
        width = "100%",
        style = list(width = "100%", height = "200px", background = "#DBA506")
      ),
      htmlDiv(
        dbcRow(
          list(
            dbcCol(
              list(
                htmlStrong(
                  htmlDiv(
                    "Select Genre(s):",
                    style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000")
                  )
                ),
                dbcChecklist(
                  id = "genre_list",
                  options = levels(factor(imdb$genres))%>%
                    purrr::map(function(col) list(label = col, value = col)),
                  value = list("Action", "Horror", "Romance"),
                  style = list(width = "100%", "color" = "#DBA506")
                ),
                htmlBr(),
                htmlStrong(
                  htmlDiv(
                    "Select Region(s):",
                    style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000")
                  )
                ),
                dccDropdown(
                  id = "region_list",
                  options = levels(factor(na.omit(imdb$region)))%>%
                    purrr::map(function(col) list(label = col, value = col)),
                  value = list("US", "IN"),
                  multi = TRUE,
                  placeholder = "All Regions",
                  clearable = FALSE,
                  style = list(width = "100%", background = "#000000", backgroundColor = "#000000", "color" = "#DBA506")
                ),
                htmlBr(),
                htmlStrong(
                  htmlDiv(
                    "Top N (actors)",
                    style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000")
                  )
                ),
                dccSlider(
                  id = "top_n",
                  min = 1,
                  max = 15,
                  step = 1,
                  marks = list(
                    "1" = "1",
                    "5" = "5",
                    "10" = "10",
                    "15" = "15"
                  ),
                  value = 10,
                  tooltip = list(placement = "bottom")
                ),
                htmlBr(),
                htmlStrong(
                  htmlDiv(
                    "Year Range:",
                    style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000")
                  )
                ),
                dccRangeSlider(
                  id = "year_range",
                  min = 2011,
                  max = 2020,
                  step = 1,
                  marks = list("2011" = "2011", "2020" = "2020"),
                  value = list("2011", "2020"),
                  tooltip = list(placement = "bottom")
                )
              ),
              width = 2
            ),
            # Charts
            dbcCol(
              list(
                # First row of charts
                dbcRow(
                  list(
                    # KPI
                    dbcCol(
                      list(
                        dbcRow(
                          list(
                            htmlStrong(
                              htmlDiv(
                                "Total Movies",
                                style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000")
                              )
                            ),
                            htmlDiv(
                              htmlH2(
                                children = list(htmlDiv(id = "total_movies", style = list(display="inline"))),
                                style = list(width = "100%", textAlign = "center", background = "#000000", color = "#F2DB83", border = "1px solid gold")
                              )
                            ),
                            htmlStrong(
                              htmlDiv(
                                "Total Actors",
                                style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000")
                              )
                            ),
                            htmlDiv(
                              htmlH2(
                                children = list(htmlDiv(id = "total_actors", style = list(display="inline"))),
                                style = list(width = "100%", textAlign = "center", background = "#000000", color = "#F2DB83", border = "1px solid gold")
                              )
                            ),
                            htmlStrong(
                              htmlDiv(
                                "Average Runtime",
                                style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000")
                              )
                            ),
                            htmlDiv(
                              htmlH2(
                                children = list(
                                  htmlDiv(id = "avg_runtime", style = list(display="inline")),
                                  htmlDiv("mins", style = list(fontSize = 20, display="inline"))
                                  ),
                                style = list(width = "100%", textAlign = "center", background = "#000000", color = "#F2DB83", border = "1px solid gold")
                              )
                            ),
                            htmlStrong(
                              htmlDiv(
                                "Average Rating",
                                style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000")
                              )
                            ),
                            htmlDiv(
                              htmlH2(
                                children = list(htmlDiv(id = "avg_rating", style = list(display="inline"))),
                                style = list(width = "100%", textAlign = "center", background = "#000000", color = "#F2DB83", border = "1px solid gold")
                              )
                            )
                          )
                        )
                      ),
                      width = 2
                    ),
                    dbcCol(
                      list(
                        htmlStrong(
                          htmlDiv(
                            "Distribution of Movies by Genre",
                            style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000")
                          )
                        ),
                        # boxplot
                            dccLoading(
                                id = "loading-2",
                                children = list(
                                    dccGraph(
                                        id = "boxplot"
                                    )
                                ),
                                type = "circle",
                                style = list(width = "100%")
                            ),####
                        htmlDiv(
                          htmlH2(
                            children = list(htmlDiv(id = "box_plot", style = list(display="inline"))),
                            style = list(width = "100%", background = "#DBA506")
                          )
                        )
                      ),
                      width = 5
                    ),
                    dbcCol(
                      list(
                        dbcRow(
                            list(
                            htmlStrong(
                              htmlDiv(
                                "Average rating by Genre over Time",
                                style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000")
                              ),
                            ),
                            # Line chart
                            dccLoading(
                                id = "loading-line",
                                children = list(
                                    dccGraph(
                                        id = "line"
                                    )
                                ),
                                type = "circle",
                                style = list(width = "100%")
                            ),
                            dbcRow(list(
                                htmlH6("Select y-axis: ",
                                       style = list(
                                           width = "120px", color = "#000000", font_weight = "bold", background = "#DBA506"
                                       )),
                                dccRadioItems(
                                    id = 'ycol',
                                    # BUG: none of this styling actually works
                                    style = list(width = "300px", height = "20px"),
                                    inputStyle = list(margin_right = "10px", margin_left = "10px"),
                                    inline = TRUE,
                                    options = list(
                                        list(label = "Average Rating", value = "averageRating"),
                                        list(label = "Average Runtime", value = "runtimeMinutes")), 
                                    value='averageRating')
                            )),
                            htmlDiv(
                              htmlH2(
                                children = list(htmlDiv(id = "total_movies3", style = list(display="inline"))),
                                style = list(width = "100%", background = "#DBA506")
                              )
                            )
                        )
                        )
                      ),
                      width = 5
                    ) 
                  )
                ),
                # Second row of charts
                dbcRow(
                  list(
                    # Bar Chart
                    dbcCol(
                      list(
                        dbcRow(
                          list(
                            # Bar Chart Title
                            htmlStrong(
                              htmlDiv(
                                children=list(
                                  "Top ",
                                  htmlDiv(id = "top_n_value",
                                          style=list(display = "inline")),  # BUG: this doesn't work on initial load
                                  " Actors from the best rated movies"
                                ),
                                style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000") 
                              )
                            ),
                            # Bar Chart Plot
                            dbcCol(
                              htmlDiv(
                                dccLoading(
                                  id = "loading-1",
                                  children = list(
                                    dccGraph(
                                      id = "plot-area"
                                    )
                                  ),
                                  type = "circle",
                                  style = list(width = "100%")
                                ),
                                style = list(width = "100%", border = "1px solid gold")
                              ) 
                            )
                          )
                        )  
                      ),
                      width = 4
                    ),
                    # Map
                    dbcCol(
                      list(
                        dbcRow(
                          list(
                            # Map Title
                            htmlStrong(
                              htmlDiv(
                                "Top rated movie in each Region",
                                style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000") 
                              )
                            ),
                            # Map Plot
                            dbcCol(
                              htmlDiv(
                                dccLoading(
                                  id = "loading-map",
                                  children = list(
                                    dccGraph(
                                      id = "map",
                                      figure = list(layout = list(height = 400))
                                    )
                                  ),
                                  type = "circle",
                                  style = list(width = "100%")
                                ),
                                style = list(width = "100%", border = "1px solid gold")
                              ) 
                            )
                          )
                        )
                      ),
                      width = 8
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# Filter data based on inputs
app$callback(
  output("filtered_data", "data"),
  list(input("genre_list", "value"),
       input("region_list", "value"),
       input("year_range", "value")),
  function(genre_list, region_list, year_range){
    filtered_data <- imdb %>%
      filter(genres %in% genre_list,
             region %in% region_list,
             between(startYear, year_range[1], year_range[2]))
    jsonlite::toJSON(filtered_data)
  }  
)

# Top N Slider value for Bar Chart Title
app$callback(
  output("top_n_value", "children"),
  list(input("top_n", "value")),
  function(top_n=10){
    top_n
  }  
)

# Bar Chart Callback
app$callback(
  output("plot-area", "figure"),
  list(input("filtered_data", "data"),
       input("top_n", "value")),
  function(data, top_n) {
    data <- jsonlite::fromJSON(data)
    actors <- data %>%
      select(averageRating, primaryName) %>% 
      group_by(primaryName) %>% 
      summarise(rating = mean(averageRating)) %>% 
      arrange(desc(rating)) %>% 
      head(top_n)
      
    p <- ggplot(
        data = actors,
        aes(x = rating,
            y = reorder(primaryName, rating))) +
      geom_col(fill = "#DBA506") +
      geom_text(aes(label = rating),
                nudge_x = -1,
                colour = "black") +
      labs(x = "Average Movie Rating",
           y = "") +
      ggthemes::scale_color_tableau()
      
    p <- p + theme(panel.background = element_rect(fill = "black"),
                   plot.background = element_rect(fill = "black"),
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(size = 0.5,
                                            linetype = "solid",
                                            colour = "white"),
                   axis.text = element_text(colour = "#DBA506"),
                   axis.title = element_text(colour = "#DBA506") 
                  )
      ggplotly(p)
  }
)

# KPI: Total Movies
app$callback(
  output("total_movies", "children"),
  list(input("filtered_data", "data")),
  function(data) {
    df <- jsonlite::fromJSON(data)
    n_distinct(df$primaryTitle, na.rm = TRUE)
  }
)

# KPI: Total Actors
app$callback(
  output("total_actors", "children"),
  list(input("filtered_data", "data")),
  function(data) {
    df <- jsonlite::fromJSON(data)
    n_distinct(df$primaryName, na.rm = FALSE)
  }
)

# KPI: Average Runtime
app$callback(
  output("avg_runtime", "children"),
  list(input("filtered_data", "data")),
  function(data) {
    df <- jsonlite::fromJSON(data)
    round(mean(df$runtimeMinutes, na.rm = TRUE), 0)
  }
)

# KPI: Average Rating
app$callback(
  output("avg_rating", "children"),
  list(input("filtered_data", "data")),
  function(data) {
    df <- jsonlite::fromJSON(data)
    round(mean(df$averageRating, na.rm = TRUE), 1)
  }
)
      
# Boxplot Callback
app$callback(
  output("boxplot", "figure"),
  list(input("filtered_data", "data")),
  function(data) {
    df <- jsonlite::fromJSON(data)
    df <- df %>%
      select(primaryTitle, averageRating, genres) %>%
      distinct()
 
    p <- ggplot(df) +
        aes(x = genres,
            y = averageRating,
            color = genres) +
        geom_boxplot() +
        labs(x = "Genre", y = "IMDb rating")
    
    p <- p + theme(panel.background = element_rect(fill = "black"),
                   plot.background = element_rect(fill = "black"),
                   legend.background = element_rect(fill = "black"),
                   legend.text = element_text(colour = "#DBA506"),
                   panel.border = element_blank(),
                   panel.grid.major = element_line(color = "#444444"),
                   panel.grid.minor = element_line(color = "#444444"),
                   axis.line = element_line(size = 0.5,
                                            linetype = "solid",
                                            colour = "white"),
                   axis.text = element_text(colour = "#DBA506"),
                   axis.title = element_text(colour = "#DBA506")
                  )
    ggplotly(p)
}
)

# Line plot
app$callback(
    output('line', 'figure'),
    list(input("filtered_data", "data"),
        input('ycol', 'value')),
    function(data, ycol) {
        df <- jsonlite::fromJSON(data)
        ylab <- ""
        if(ycol == "averageRating") {
            ylab <- "Average Rating (/10)"
        } else if(ycol == "runtimeMinutes") {
            ylab <- "Average Runtime (minutes)"
        }
            
        df$startYear <- as.Date(paste(df$startYear, 1, 1, sep = "-"))
        # Calculate the mean Y per genre per year
        # this has to be done outside ggplot for the tooltip to work
        df <- df %>%
            group_by(genres, startYear) %>%
            summarize(meanY = mean(!!sym(ycol), na.rm = TRUE)) %>%
            merge(df)
    
        p <- ggplot(df) +
            aes(x = startYear,
                y = meanY,
                color = genres,
                text = meanY) +  # TODO: improve me but there's ggplot bugs
            geom_line() +
            labs(x = "Year", y = ylab) +
            scale_x_date(date_labels = "%Y")
    
        p <- p + theme(panel.background = element_rect(fill = "black"),
                       plot.background = element_rect(fill = "black"),
                       legend.background = element_rect(fill = "black"),
                       legend.text = element_text(colour = "#DBA506"),
                       panel.border = element_blank(),
                       panel.grid.major = element_line(color = "#444444"),
                       panel.grid.minor = element_line(color = "#444444"),
                       axis.line = element_line(size = 0.5,
                                                linetype = "solid",
                                                colour = "white"),
                       axis.text = element_text(colour = "#DBA506"),
                       axis.title = element_text(colour = "#DBA506"),
                       legend.position = c(0.5, 0.5))
    
        ggplotly(p, tooltip = "text") %>%
            layout(legend = list(orientation = "h", y = -0.15))  # Return
}
)

# Map
app$callback(
  output("map", "figure"),
  list(input("filtered_data", "data")),
  function(data){
    # Read JSON data and country code information, merge to form consolidated data
    map_data <- jsonlite::fromJSON(data)
    country_codes <- read.csv("data/country_codes.csv")
    map_data <- merge(map_data, country_codes, by.x = "region", by.y = "alpha_2", all.x = TRUE)
    
    # Data wrangling
    map_data <- map_data %>%
      subset(region != "") %>%
      group_by(alpha_3) %>%
      arrange(desc(averageRating)) %>%
      select(primaryTitle, alpha_3, averageRating, name) %>%
      distinct()
    
    # Add custom tooltip to show title
    map_data$hover <- with(map_data, paste("Country:", name, "<br>", "Title:", primaryTitle))
    
    # Fill same color for all countries
    map_data$fill <- 1
    
    # Remove duplicate data to have 1 row by country
    map_data <- map_data[!duplicated(map_data$alpha_3),]
    
    # Map layout list
    g <- list(scope = "world",
              # projection = list(type = 'natural earth'),
              showcountries = TRUE,
              countrycolor = toRGB("#F2DB83"),
              showland = TRUE,
              landcolor = toRGB("grey22"),
              showocean = TRUE,
              oceancolor = toRGB("black"),
              showscale = FALSE
              )
    
    # Plotly map object
    p <- plot_geo(map_data) %>%
         add_trace(z = ~averageRating,
                   text = ~hover,
                   locations = ~alpha_3,
                   colors = toRGB("#DBA506"),
                   color = ~fill,
                   type = 'choropleth',
                   showscale = FALSE
                   ) %>%
         layout(geo = g,
                paper_bgcolor = toRGB("black"))
    
    return(p)
  }
)

app$run_server(Debug=T)
