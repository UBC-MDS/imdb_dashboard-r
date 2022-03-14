library(ggplot2)
library(plotly)

generate_line_chart <- function(df, ycol) {
    ylab <- ""
    if(ycol == "averageRating") {
        ylab <- "Average Rating (/10)"
    } else if(ycol == "runtimeMinutes") {
        ylab <- "Average Runtime (minutes)"
    }
    
    p <- ggplot(df) +
        aes(x = startYear,
            y = !!sym(ycol),
            color = genres) +
        geom_line(stat = "summary", fun = mean) +
        labs(x = "Year", y = ylab)
    
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
                   axis.title = element_text(colour = "#DBA506"))
    
    ggplotly(p)  # Return
}