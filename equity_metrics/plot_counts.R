library(psrcplot)
# creates a basic count plot
plot_counts <- function(dt, xcol = "year", ycol = "count", ...) {
    
    dt[, (xcol) := as.character(get(xcol))] # category axis needs to be character/factor
    plot_data <- as_tibble(dt)  

    time_levels <- unique(dt[[xcol]])
    col_palette <- stats::setNames(
        grDevices::hcl.colors(length(time_levels), palette = "Dynamic"),
        time_levels
    )
    
    pop_chart <- static_column_chart(
        t     = plot_data,
        x     =  xcol,       # category (x) axis
        y     =  ycol,       # numeric value to plot
        fill  =  xcol, 
        #color = col_palette, 
        ... )
    return(pop_chart)
}

plot_line_counts <- function(dt, xcol = "year", ycol = "count", fill = NULL, legend = "counts", ...) {
    
    dt[, category := legend]
    plot_data <- as_tibble(dt)  
    
    pop_chart <- static_line_chart(
        t     = plot_data,
        x     =  xcol,       # category (x) axis
        y     =  ycol,       # numeric value to plot
        fill  =  if(is.null(fill)) "category" else fill,
        ... )
    return(pop_chart)
}