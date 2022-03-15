library(dash)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(jsonlite)

source("bar_chart.R")
source("boxplot.R")
source("line_chart.R")

app <- Dash$new(external_stylesheets = dbcThemes$CYBORG)

imdb <- read.csv("../data/imdb_2011-2020.csv")

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
    df <- jsonlite::fromJSON(data)
    figure <- generate_bar_chart(df, top_n)
    figure
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
    figure <- generate_boxplot(df)
    figure
  }
)

# Line plot
app$callback(
    output('line', 'figure'),
    list(input("filtered_data", "data"),
        input('ycol', 'value')),
    function(data, ycol) {
        df <- jsonlite::fromJSON(data)
        figure <- generate_line_chart(df, ycol)
        figure
    }
)

app$run_server(Debug=T)