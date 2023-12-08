#' Make a heatmap of CoV prevalence by bat family and quarter of the year
#'
#'
#' @title plot_quarterly_heatmap
#' @param heat_data summary table with CoV prevalence by bat family and quarter of the year
#' @return 
#' @author Cecilia Sanchez

plot_quarterly_heatmap <- function(heat_data){
  
  ggplot(data = heat_data, mapping = aes(x = quarter, y = family,
                                         fill = prop)) +
    geom_tile(color = "black") +
    scale_fill_viridis_c(name = "Prop. \nCoV +", option = "magma",
                         direction = -1) +
    # have text be labeled white if background fill is dark
    geom_text(aes(label = paste0(n, "/", totN), color = prop > 0.15), size = 5) +
    scale_color_manual(guide = "none", values = c("black", "white")) +
    scale_x_discrete(labels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")) +
    ylab("") + xlab("") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(color = "black", size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          legend.box.spacing = unit(0, "pt"),
          legend.margin = margin(0,5,0,5),
          plot.margin = margin(t = 2, r = 0, b = -10, l = -10))

  ggsave("outputs/quarterly_prevalence_heatmap.png", width = 6, height = 4,
         units = "in", dpi = 600)  
}
