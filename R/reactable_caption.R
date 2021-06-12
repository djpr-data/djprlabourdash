
caption_reactable <- function(extra = "",
                              base_caption = "Source: ABS Labour Force. Note: ") {
  core_caption <- paste(
    base_caption,
    "Shading of cells is based on how the indicator relates to historical trends. If the indicator grew by around its typical amount, the cell will be white. If growth was very strong relative to historical levels, it will be dark green. If it was weak relative to historical growth, the cell will be dark red."
  )

  caption <- paste0(core_caption, extra)

  djprshiny::djpr_plot_caption(caption)
}
