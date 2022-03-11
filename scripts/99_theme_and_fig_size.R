single_col_mm <- 80
half_col_mm <- 125
double_col_mm <- 166

mm_to_in <- 0.0393701

single_col_in <- single_col_mm * mm_to_in
half_col_in <- half_col_mm * mm_to_in
double_col_in <- double_col_mm * mm_to_in

theme_custom <- function () { 
    theme_bw(base_size=9) %+replace% 
      theme(
        text = element_text(colour = "black",
                            size = 9),
        axis.text = element_text(colour = "black",
                                 size = 8),
        axis.title = element_text(colour = "black",
                                  size = 9),
        panel.border = element_rect(fill = NA, colour = "black"),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key        = element_rect(fill="transparent", colour=NA),
        strip.background = element_blank()
      )
}
