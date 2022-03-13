library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)
library(dashHtmlComponents)

imdb <- read.csv("data/imdb_2011-2020.csv")

app <- Dash$new(external_stylesheets = dbcThemes$CYBORG)

# Set the layout of the app
app %>% set_layout(
  h1("IMDb Dashboard"),
  h4("Plan your next movie."),
  div(
    dbcRow(
      list(
        dbcCol(
          list(
            htmlLabel(
              "Top N (actors)",
              style = list(
                width = "100%",
                textAlign = "center",
                background = "#DBA506",
                color = "#000000"
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
                htmlLabel(
                  children=list(
                    "Top N Actors from the best rated movies"
                    ),
                  style = list(
                    width = "100%",
                    textAlign = "center",
                    background = "#DBA506",
                    color = "#000000"
                  )
                )
              )
            ),
            dbcRow(
              list(
                dccGraph(id="plot-area")   
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
    
    p <- ggplot(
      data = actors,
      aes(x = rating,
          y = reorder(primaryName, rating))) +
      geom_col(fill = "#DBA506") +
      geom_text(aes(label = rating),
                nudge_x = -0.3,
                colour = "white") +
      labs(x = "Average Movie Rating",
           y = "") +
      ggthemes::scale_color_tableau()

    p <- p + theme(panel.background = element_rect(fill = "black"),
                   plot.background = element_rect(fill = "black"),
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(size = 0.5, linetype = "solid",
                                            colour = "white"),
                   axis.text = element_text(colour = "white"),
                   axis.title = element_text(colour = "white") 
                   )
  ggplotly(p)
  }
)

app$run_server(Debug=T)