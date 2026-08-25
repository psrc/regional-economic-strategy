library(psrcplot)
# creates a basic count plot
plot_counts <- function(dt, xcol = "year", ycol = "count", ...) {
    
    dt[, (xcol) := as.character(get(xcol))] # category axis needs to be character/factor
    plot_data <- as_tibble(dt)  

    pop_chart <- static_column_chart(
        t     = plot_data,
        x     =  xcol,       # category (x) axis
        y     =  ycol,       # numeric value to plot
        fill  =  xcol, ... )
    return(pop_chart)
}