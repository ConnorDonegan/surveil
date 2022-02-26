#' Default surveil plot theme
#' @noRd
#' @import ggplot2
theme_surveil <- function(base_size, ...) {
    theme_classic(base_size = base_size) +            
        theme(
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(),
        panel.grid.major.y = element_line(),
        ...
        )
}
