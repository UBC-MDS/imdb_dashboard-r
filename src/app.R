library(dash)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(purrr)

source("src/bar_chart.R")

app <- Dash$new(external_stylesheets = dbcThemes$CYBORG)

imdb <- read.csv("data/imdb_2011-2020.csv")

selected_genres <- as.list(unique(imdb$genres))

# Set the layout of the app
app %>% set_layout(
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
                    style = list(width = "100%", background = "#DBA506", color = "#000000")
                  )
                ),
                dbcChecklist(
                  id = "genre_list",
                  options = levels(factor(imdb$genres))%>%
                    purrr::map(function(col) list(label = col, value = col)),
                  value = list("Action", "Horror", "Romance"),
                  style = list(width = "100%", "color" = "#DBA506")
                ),
                htmlStrong(
                  htmlDiv(
                    "Top N (actors)",
                    style = list(width = "100%", background = "#DBA506", color = "#000000")
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
                  value = 10
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
                    dbcCol(
                      list(
                        dbcRow(
                          list(
                            htmlStrong(
                              htmlDiv(
                                "Total Movies",
                                style = list(width = "100%", background = "#DBA506", color = "#000000")
                              )
                            ),
                            htmlDiv(
                              htmlH2(
                                children = list(htmlDiv(id = "total_movies", style = list(display="inline"))),
                                style = list(width = "100%", background = "#DBA506")
                              )
                            ),
                            htmlStrong(
                              htmlDiv(
                                "Total Actors",
                                style = list(width = "100%", background = "#DBA506", color = "#000000")
                              )
                            ),
                            htmlDiv(
                              htmlH2(
                                children = list(htmlDiv(id = "total_actors", style = list(display="inline"))),
                                style = list(width = "100%", background = "#DBA506")
                              )
                            ),
                            htmlStrong(
                              htmlDiv(
                                "Average Rating",
                                style = list(width = "100%", background = "#DBA506", color = "#000000")
                              )
                            ),
                            htmlDiv(
                              htmlH2(
                                children = list(htmlDiv(id = "avg_rating", style = list(display="inline"))),
                                style = list(width = "100%", background = "#DBA506")
                              )
                            ),
                            htmlStrong(
                              htmlDiv(
                                "Average Runtime",
                                style = list(width = "100%", background = "#DBA506", color = "#000000")
                              )
                            ),
                            htmlDiv(
                              htmlH2(
                                children = list(htmlDiv(id = "avg_runtime", style = list(display="inline"))),
                                style = list(width = "100%", background = "#DBA506")
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
                            style = list(width = "100%", background = "#DBA506", color = "#000000")
                          )
                        ),
                        htmlDiv(
                          htmlH2(
                            children = list(htmlDiv(id = "total_movies2", style = list(display="inline"))),
                            style = list(width = "100%", background = "#DBA506")
                          )
                        )
                      ),
                      width = 5
                    ),
                    dbcCol(
                      list(
                        htmlStrong(
                          htmlDiv(
                            "Average rating by Genre over Time",
                            style = list(width = "100%", background = "#DBA506", color = "#000000")
                          )
                        ),
                        htmlDiv(
                          htmlH2(
                            children = list(htmlDiv(id = "total_movies3", style = list(display="inline"))),
                            style = list(width = "100%", background = "#DBA506")
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
                            htmlStrong(
                              htmlDiv(
                                children=list(
                                  "Top ",
                                  htmlDiv(id = "top_n_value",
                                          style=list(display = "inline")),
                                  " Actors from the best rated movies"
                                ),
                                style = list(width = "100%", textAlign = "center", background = "#DBA506", color = "#000000") 
                              )
                            ),
                            dccLoading(
                              id = "loading-1",
                              children = list(
                                dccGraph(
                                  id = "plot-area"
                                )
                              ),
                              type = "circle",
                              style = list(width = "100%")
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


app$callback(
  output("top_n_value", "children"),
  list(input("top_n", "value")),
  function(top_n){
    top_n
  }  
)

app$callback(
  output("plot-area", "figure"),
  list(input("top_n", "value"),
       input("genre_list", "value")),
  function(top_n, genre_list) {
    actors <- imdb %>%
      filter(genres %in% genre_list,
             region %in% c("US", "IN")) %>%
      select(averageRating, primaryName) %>% 
      group_by(primaryName) %>% 
      summarise(rating = mean(averageRating)) %>% 
      arrange(desc(rating)) %>% 
      head(top_n)
    
    generate_bar_chart(actors, top_n) 
  }
)

app$run_server(Debug=T)