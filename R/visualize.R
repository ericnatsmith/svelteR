#' Create a Pie Chart Based on a Continuous Variable Categorized into Levels
#'
#' This function creates a pie chart from a dataframe by categorizing a continuous dependent variable into 
#' different levels (e.g., Disagree, Slightly Agree, Agree) using custom bins, then visualizes the distribution 
#' of the data in those levels.
#'
#' @param df A dataframe containing the dependent variable.
#' @param DV_input A string representing the name of the dependent variable column in the dataframe.
#' @param my_title A string for the title of the pie chart.
#' @param bins A numeric vector of breakpoints used to categorize the dependent variable into levels. Default is c(0, 3.6, 4.6, 6.5).
#' @param mylevels A character vector representing the names of the levels for the categorical dependent variable. Default is c("Disagree", "Slightly Agree", "Agree").
#'
#' @return A ggplot object displaying a pie chart with the specified title.
#' @examples
#' df <- data.frame(score = c(2.5, 4.0, 5.5, NA, 3.9))
#' make_piechart(df, "score", "Sample Pie Chart")
make_piechart <- function(df, DV_input, my_title, bins = c(0, 3.501, 4.501, 6.5), mylevels = c("Disagree", "Slightly Agree", "Agree")) {
  
  # Rename the dependent variable (DV) to a uniform name 'DV' and filter out missing values
  df <- df %>% 
    rename(DV = all_of(DV_input)) %>%
    filter(!is.na(DV)) %>%
    
    # Categorize the continuous variable into factors using the provided bins and levels
    mutate(DV = factor(cut(DV, bins, mylevels), levels = mylevels)) %>%
    
    # Group by the categorized levels and count occurrences
    group_by(DV) %>% 
    count() %>% 
    ungroup()
  
  # Ensure that all levels from 'mylevels' are present, even if they have 0 counts
  for (lvl in mylevels) {
    if (!lvl %in% df$DV) {
      df <- rbind(df, data.frame(DV = lvl, n = 0))
    }
  }
  
  # Calculate the percentage for each level and format it for labeling
  df <- df %>%
    mutate(perc = `n` / sum(`n`)) %>%  # Compute percentages
    arrange(DV) %>%
    mutate(labels = scales::percent(perc, accuracy = 1))  # Format labels as percentages
  
  # Create the pie chart using ggplot2
  ggplot(df, aes(x = "", y = perc, fill = DV)) +
    geom_col() +  # Create bar segments
    coord_polar(theta = "y") +  # Convert bar chart to pie chart
    scale_fill_manual(values=c("#f15b40","#fff450", "#a3cf62"), name="Response") +  # Set custom colors
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.55)) +  # Add percentage labels
    horzi_theme +  # Custom theme (assumed predefined)
    
    # Modify theme to remove unnecessary axis and strip elements for a cleaner pie chart
    theme( 
      axis.title.x     = element_blank(),
      axis.text.x      = element_blank(),
      axis.text.y      = element_blank(),
      axis.title.y     = element_blank(),
      strip.text.x     = element_blank(),
      strip.text.y     = element_blank(),
      strip.background = element_blank(),
      plot.title       = element_text(size = text_size * 1.5, hjust = 0.5)  # Title formatting
    ) + 
    labs(title = my_title)  # Add the specified title
}
