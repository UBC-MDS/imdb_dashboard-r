library(ggplot2)
library(plotly)

generate_bar_chart <- function(data, top_n) {

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