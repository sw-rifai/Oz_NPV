


scale_fill_div <- function(..., low = scales::muted("red"), 
  mid = "white", 
  high = scales::muted("blue"), 
    midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar", 
    aesthetics = "colour") 
{
    continuous_scale(aesthetics, "gradient2", 
      scales::div_gradient_pal(low, 
        mid, high, space), na.value = na.value, guide = guide, 
        ..., rescaler = ggplot2:::mid_rescaler(mid = midpoint))
}


