library(ggplot2)
library(plotly)

generate_line_chart <- function(df, ycol) {
    ylab <- ""
    if(ycol == "averageRating") {
        ylab <- "Average Rating (/10)"
    } else if(ycol == "runtimeMinutes") {
        ylab <- "Average Runtime (minutes)"
    }
    
    df$startYear <- as.Date(paste(df$startYear, 1, 1, sep = "-"))
    
    p <- ggplot(df) +
        aes(x = startYear,
            y = !!sym(ycol),
            color = genres,
            text = startYear) +  # TODO: improve me but there's ggplot bugs
        geom_line(stat = "summary", fun.y = mean) +
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
    
    ggplotly(p, tooltip = "text") |>
        layout(legend = list(orientation = "h", y = -0.15))  # Return
}