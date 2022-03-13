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
  htmlH1("IMDb Dashboard",
     style = list(
       textAlign = "center",
       color = "#DBA506"
     )),
  htmlH4("Plan your next movie.",
     style = list(
       textAlign = "center",
       color = "#DBA506"
     )),
  htmlDiv(
    dbcRow(
      list(
        dbcCol(
          list(
            htmlStrong(
              htmlDiv(
                "Select Genre(s):",
                style = list(
                  width = "100%",
                  background = "#DBA506",
                  color = "#000000"
                  ) 
                )
              ),
            dbcChecklist(
              id = "genre_list",
              options = levels(factor(imdb$genres))%>%
                          purrr::map(function(col) list(label = col, value = col)),
              value = list("Action", "Horror", "Romance"),
              style = list(
                width = "100%",
                "color" = "#DBA506"
                )
            ),
            htmlStrong(
              htmlDiv(
                "Top N (actors)",
                style = list(
                  width = "100%",
                  background = "#DBA506",
                  color = "#000000"
                  )
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
          width = 1
          ),
        dbcCol(
          list(
            dbcRow(
              list(
                htmlStrong(
                  children=list(
                    "Top ",
                    htmlDiv(id="top_n_value",
                            style=list(display="inline")),
                    " Actors from the best rated movies"
                    ),
                  style = list(
                    width = "100%",
                    font_weight = "bold",
                    textAlign = "center",
                    background = "#DBA506",
                    color = "#000000"
                    )
                  )
                )
              ),
            dbcRow(
              list(
                dccGraph(
                  id="plot-area",
                  style = list(
                    width = "100%",
                    height = "100%", 
                    border = "1px solid gold"
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

app$callback(
  output("top_n_value", "children"),
  list(input("top_n", "value")),
  function(top_n){
    top_n
  }  
)

app$callback(
  output("plot-area", "figure"),
  list(input("top_n", "value")),
  function(top_n) {
    actors <- imdb %>%
      filter(genres %in% c("Action", "Horror", "Thriller"),
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