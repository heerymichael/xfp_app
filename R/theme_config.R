# R/theme_config.R - Theme configuration

# Color Palette
etr_colors <- list(
  primary = "#37AF4A",
  primary_dark = "#2D9440",
  primary_light = "#90EE90",
  primary_pale = "#D4F4D7",
  
  success = "#37AF4A",
  danger = "#C53160",
  warning = "#F88B01",
  info = "#2196F3",
  
  black = "#000000",
  gray_900 = "#212529",
  gray_800 = "#333333",
  gray_700 = "#495057",
  gray_600 = "#666666",
  gray_500 = "#999999",
  gray_400 = "#CCCCCC",
  gray_300 = "#D1D1D1",
  gray_200 = "#E0E0E0",
  gray_100 = "#F0F0F0",
  gray_50 = "#F8F9FA",
  white = "#FFFFFF",
  
  bg_primary = "#F7F7F7",
  bg_secondary = "#F0F5F1",
  bg_input = "#F6F6F6"
)

# Typography
etr_fonts <- list(
  family_primary = "Inter",
  family_display = "Fjalla One",
  
  weight_regular = 400,
  weight_medium = 500,
  weight_semibold = 600,
  weight_bold = 700,
  
  ggplot_title = 32,
  ggplot_subtitle = 24,
  ggplot_axis_text = 12,
  ggplot_axis_title = 14,
  ggplot_strip_text = 24,
  ggplot_legend_text = 14
)

# Spacing
etr_spacing <- list(
  xs = 4,
  sm = 8,
  md = 16,
  lg = 24,
  xl = 32
)

# Borders
etr_borders <- list(
  width_thin = "1px",
  width_medium = "1.5px",
  width_thick = "2px",
  
  radius_sm = "4px",
  radius_md = "6px",
  radius_lg = "8px",
  radius_xl = "10px",
  
  color_default = "#D1D1D1",
  shadow_lg = "0 2px 8px rgba(0,0,0,0.1)"
)

# Dimensions
etr_dimensions <- list(
  content_max_width = "1200px",
  logo_selector = "40px",
  plot_width_default = 12,
  plot_height_default = 8,
  plot_dpi = 300
)

# Theme function for ggplot
theme_etr <- function(base_size = 14) {
  showtext_auto(enable = TRUE)
  
  theme_minimal(base_size = base_size, base_family = "Inter") %+replace%
    theme(
      plot.background = element_rect(fill = etr_colors$white, color = NA),
      panel.background = element_rect(fill = etr_colors$white, color = NA),
      panel.grid.major = element_line(color = etr_colors$gray_200, size = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text = element_text(
        color = etr_colors$gray_500,
        size = etr_fonts$ggplot_axis_text,
        family = etr_fonts$family_display
      ),
      axis.title = element_text(
        color = etr_colors$gray_500,
        size = etr_fonts$ggplot_axis_title,
        face = "bold",
        family = etr_fonts$family_primary
      ),
      
      plot.title = element_text(
        size = etr_fonts$ggplot_title,
        face = "bold",
        family = etr_fonts$family_primary,
        lineheight = 1.1
      ),
      plot.subtitle = element_text(
        size = etr_fonts$ggplot_subtitle,
        family = etr_fonts$family_primary,
        lineheight = 1.3
      ),
      
      strip.text = element_text(
        size = etr_fonts$ggplot_strip_text,
        face = "bold",
        family = etr_fonts$family_primary
      ),
      strip.background = element_blank(),
      
      legend.position = "bottom",
      plot.margin = margin(20, 10, 10, 10)
    )
}

# Reactable theme function
etr_reactable <- function(font_size = 16,
                          header_font_size = 12,
                          cell_padding = 5,
                          centered = FALSE) {
  
  reactableTheme(
    color = etr_colors$gray_800,
    backgroundColor = etr_colors$white,
    borderWidth = "0px",  # Remove default borders
    borderColor = etr_colors$gray_200,
    stripedColor = etr_colors$white,  # No striping
    highlightColor = etr_colors$white,  # No highlight
    cellPadding = cell_padding,
    style = list(
      fontFamily = "'Inter', sans-serif"  # Force Inter everywhere
    ),
    tableStyle = list(
      fontSize = font_size,
      fontFamily = "'Inter', sans-serif"
    ),
    headerStyle = list(
      borderWidth = etr_borders$width_medium,
      borderColor = etr_colors$gray_500,
      background = etr_colors$white,
      color = etr_colors$gray_500,  # Match main headers
      fontWeight = etr_fonts$weight_medium,
      textTransform = "uppercase",
      fontSize = header_font_size,
      fontFamily = "'Inter', sans-serif"  # Force Inter
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(
        borderWidth = "0px",
        backgroundColor = etr_colors$white,
        textTransform = "uppercase",
        fontSize = header_font_size,
        borderColor = etr_colors$white,
        color = etr_colors$gray_500,  # Match main headers
        fontFamily = "'Inter', sans-serif"
      )
    ),
    rowStyle = list(
      borderBottom = "1px solid #E0E0E0"  # Thin grey lines between rows
    )
  )
}