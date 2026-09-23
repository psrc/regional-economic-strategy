library(DT)

# Render a data.frame/data.table as an interactive table with "Copy" and
# "Export to CSV" buttons.
#
# @param dt A data.frame or data.table to display.
# @param caption Optional table caption; also used to derive the CSV filename.
# @param digits Number of decimal places to round numeric columns to.
#
# @return A DT::datatable htmlwidget.
render_data_table <- function(dt, caption = NULL, digits = 3) {
    df <- as.data.frame(dt)
    num_cols <- which(vapply(df, is.numeric, logical(1)))

    filename <- if (!is.null(caption)) gsub("[^A-Za-z0-9_-]+", "_", caption) else "data"

    tbl <- DT::datatable(
        df,
        caption = caption,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
            dom = "Bfrtip",
            buttons = list(
                "copy",
                list(extend = "csv", filename = filename)
            ),
            scrollX = TRUE,
            pageLength = 10
        )
    )

    if (length(num_cols) > 0) {
        tbl <- DT::formatRound(tbl, columns = num_cols, digits = digits)
    }

    return(tbl)
}
