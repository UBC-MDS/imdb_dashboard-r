library(ggplot2)
library(plotly)

generate_boxplot <- function(df) {
    
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