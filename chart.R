theme_profiles <- function() {
  fontStyle <- "sans"
  gridLineColor <- grDevices::rgb(190 / 255, 190 / 255, 190 / 255)
  fontSize <- 11
  
  ggplot2::theme(
    
    # Text format:
    # This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(
      family = fontStyle,
      size = fontSize,
      face = "bold"
    ),
    
    # Legend format
    # This sets the position and alignment of the legend, removes a title and
    # background for it and sets the requirements for any text within the legend.
    # The legend may often need some more manual tweaking when it comes to its
    # exact position based on the plot coordinates.
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = fontStyle,
      size = fontSize
    ),
    
    # Axis format
    # This sets the text font, size and colour for the axis test, as well as
    # setting the margins and removes lines and ticks.
    # In some cases, axis lines and axis ticks are things we would want to have
    # in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_text(
      family = fontStyle,
      size = fontSize
    ),
    axis.text = ggplot2::element_text(
      family = fontStyle,
      size = fontSize
    ),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    # Grid Lines
    # This removes all minor gridlines and adds major vertical gridlines.
    # In many cases you will want to change this to remove vertical gridlines
    # and add horizontal gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color = gridLineColor),
    panel.grid.major.y = ggplot2::element_blank(),
    
    # Blank Background
    # This sets the panel background as blank, removing the standard grey ggplot
    # background colour from the plot
    panel.background = ggplot2::element_blank()
  )
}



## PHS colour palette from phsstyles
palette <- phsstyles::phs_colours(c(
  "phs-purple", "phs-magenta", "phs-blue", "phs-green",
  "phs-graphite", "phs-teal", "phs-liberty", "phs-rust", "phs-blue-50", "phs-purple-50"
))

# filter and reorder data
age_band_deaths_5y %>%
  mutate(period = paste0(year-4, "-", year)) %>%
  # plot
  ggplot(aes(
    x = str_wrap(period, width = 120), y = deaths_5y_rate,
    group = age_band, fill = age_band, linetype = age_band
  )) +
  geom_line(aes(colour = age_band), linewidth = 1) +
  geom_point(aes(colour = age_band), size = 2) +
  geom_ribbon(
    aes(
      x = str_wrap(period, width = 120),
      ymin = lowci,
      ymax = upci
    ),
    alpha = 0.1
  ) +
  scale_fill_manual(values = palette) +
  scale_colour_manual(values = palette) +
  theme_profiles() +
  theme(axis.text.x = element_text(angle = 45)) +
  expand_limits(y = 0) +
  labs(
    title = "Crude rate of deaths by age in West Dunbartonshire",
    x = "Year (5-year average)",
    y = "Crude rate of deaths (per 100,000)",
    caption = "Source: NRS Deaths"
  ) +
  guides(
    linetype = "none", shape = "none", fill = "none",
    colour = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  facet_wrap("age_band", scales = "free_y")



