library(ggplot2)
library(plotly)

generate_line_chart <- function(df, ycol) {
    p <- ggplot(df) +
        aes(x = startYear,
            y = !!sym(ycol),
            color = genres) +
        geom_line(stat = "summary", fun = mean)
    ggplotly(p)  # Return
}