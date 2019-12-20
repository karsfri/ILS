library(tidyverse)

# Load fonts
windowsFonts(Setimo = windowsFont("Setimo"))
windowsFonts(SetimoLight = windowsFont("Setimo Light"))

blue <- "#11223a"

# Colors for monthly report -----------------------------------------------

palette_light <- c(
  yellow = "#F3E074",
  blue = "#82C2D3",
  red = "#E48865",
  gray = "#B1BFC2",
  green = "#94BB75",
  orange = "#EC8865",
  purple = "#C56BA4"
)

palette_medium <- c(
  yellow = "#E4CC26",
  blue = "#5990AE",
  red =  "#CD4F3C",
  dark_green = "#2D3737",
  green = "#5DA666",
  orange = "#DD9222",
  purple = "#A55884"
)

palette_dark <- c(
  yellow = "#DBB419",
  blue = "#2F769C",
  red =  "#B9202D",
  blackish = "#0C191D",
  green = "#3E884E",
  orange = "#BE7117",
  purple = "#8C426B"
)

# Palette for the montly reports - use for areas and columns
palette_hms <- c(
  palette_light,
  palette_dark,
  palette_medium
)

# Palette for the montly reports - use for lines and dots
palette_hms_darker <- c(
  palette_dark,
  palette_medium,
  palette_light
)

palette_light <- palette_light %>% unname()
palette_medium <- palette_medium %>% unname()
palette_dark <- palette_dark %>% unname()
palette_ils <- palette_hms %>% unname()
palette_ils_darker <- palette_hms_darker %>% unname()


# theme for montly report -------------------------------------------------

tsm <- 1 # 4 / 3 # text size multiplier

theme_hms <- function(tms = 1){
  theme_gray() +
    theme(
      text = element_text(family = "SetimoLight"),
      line = element_line(size = 0.6),
      rect = element_rect(size = 0.6),
      
      
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.y = element_line(colour = "gray90", size = 0.2),
      panel.grid.major.x = element_blank(),
      
      strip.text = element_text(family = "SetimoLight", size = 7 * tsm, color = "black", face = "bold"),
      strip.text.x = element_text(family = "SetimoLight", size = 7 * tsm, color = "black", face = "bold"),
      strip.background = element_blank(),
      
      plot.title = element_text(family = "Setimo", size = 12 * tsm, face = "bold", color = blue),
      plot.subtitle = element_text(family = "Setimo", size = 7 * tsm, color = blue),
      plot.caption = element_text(family = "SetimoLight", size = 7 * tsm, color = blue, face = "italic"),
      axis.title = element_text(size = 7 * tsm),
      axis.text = element_text(size = 7 * tsm),
      legend.text = element_text(size = 8 * tsm),
      plot.title.position = "plot",
      # axis.text.x = element_blank(),
      
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_blank(),
      
      legend.position = "bottom",
      legend.key = element_rect(fill = "transparent"),
      legend.key.height = ggplot2::unit(0.4, "cm"),
      legend.key.width = ggplot2::unit(0.4, "cm")
      # aspect.ratio = 16 / 9
      
    )
}



# Plot helper functions ---------------------------------------------------

theme_flip <- function(color = gray, ...){
  theme(
    panel.grid.major.x = element_line(color = color, ...),
    panel.grid.major.y = element_blank()
  )
}

theme_vertical_x <- theme(
  axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)
)

guides_off  <- function(){
  guides(fill = FALSE, color = FALSE, alpha = FALSE)
  
} 

flip <- function(...){
  theme_flip(...) + 
    theme_vertical_x
  
}

geom_zero_line <- function(..., color = palette_dark[4]){
  geom_hline(yintercept = 0, color = color, ...)
}

legend_right <- theme(
  legend.position = "right",
  legend.direction = "vertical"
)

label_isk <- function(...) scales::label_dollar(prefix = "", suffix = " kr.", decimal.mark = ",", big.mark = ".", ...)
label_point <- function(...) scales::label_dollar(decimal.mark = ",", big.mark = ".", ...)
label_percent <- function(accuracy = 1, ...) scales::label_percent(accuracy = accuracy, decimal.mark = ",", big.mark = ".", ...)


# Save plot functions ----------------------------------------------------------

# common size
width_wide_report <- (16.2 + 3)
width_narrow_report <- (8 + 3)

width_narrow <- 8.5
width_wide <- 18
height_full <- 20
height_regular <- 9
dpi_reg <- 300
dpi_high <- 700


# Depreciating
ggsave_word <- function(filename, plot = last_plot(), device = ragg::agg_png, width = c(width_wide, width_narrow),
                        height = height_regular, units = "cm", dpi = dpi_reg, bg = "transparent", ...){
  ggsave(filename = filename, plot = plot, device = device, width = width, height = height, units = units,
         res = dpi, background = bg, ...)
} 

ggsave_png <- function(filename, plot = last_plot(), device = ragg::agg_png, width = width_wide,
                       height = height_regular, units = "cm", dpi = dpi_reg, bg = "white", ...){
  ggsave(filename = filename, plot = plot, device = device, width = width, height = height, units = units,
         res = dpi, background = bg, ...)
}

ggsave_svg <- function(filename, plot = last_plot(), device = "svg", width = width_wide,
                       height = height_regular, units = "cm", dpi = dpi_reg, ...){
  ggsave(filename = filename, plot = plot, device = device, width = width, height = height, units = units,
         dpi = dpi, ...)
} 

ggsave_both <- function(filename, plot = last_plot(), width = width_wide,
                        height = height_regular, units = "cm", dpi = dpi_reg, bg = "white", ...){
  
  # save as png
  ggsave(filename = paste0(filename, ".png"), plot = plot, device = "png", width = width, height = height, units = units,
         dpi = dpi, ...)
  
  # save as svg
  ggsave(filename = paste0(filename, ".svg"), plot = plot, device = "svg", width = width, height = height, units = units,
         dpi = dpi, bg = bg, ...)
} 


# Set theme  --------------------------------------------------------------


theme_set_hms <- function(theme = theme_hms(), change_palettes = TRUE){
  # Note that this function has external effects!
  # Wierd implimentation because the palettes cannot be named 
  yellow <- palette_light[1]
  yellow_dark <- palette_dark[1]
  red <- palette_dark [3]
  
  names(yellow) <- NULL
  names(yellow_dark) <- NULL
  names(red) <- NULL
  
  ggplot2::theme_set(theme)
  update_geom_defaults(GeomBoxplot, list(fill = yellow))
  # update_geom_defaults(GeomBoxplot, list(outlier.color = red))
  update_geom_defaults("col", list(fill = yellow, color = NA))
  update_geom_defaults("bar", list(fill = yellow, color = NA))
  update_geom_defaults("area", list(fill = yellow, color = NA))
  update_geom_defaults("line", list(color = yellow_dark))
  update_geom_defaults("point", list(color = yellow_dark, size = 3))
  
  if(change_palettes){
    
    scale_colour_discrete <<- function(...) {
      scale_colour_manual(..., values = palette_ils_darker %>% unname())
    }
    
    scale_fill_discrete <<- function(...) {
      scale_fill_manual(..., values = palette_ils)
    }
    
  }
  
}


# Common caption names ----------------------------------------------------

cap_thjodskra <- "Heimild: Þjóðskrá Íslands"
cap_ils_thjodskra <- "Heimild: Þjóðskrá Íslands og hagdeild Íbúðalánasjóðs"
cap_hagstofa <- "Heimild: Hagstofa Íslands"
cap_ils_hagstofa <- "Heimild: Hagstofa Íslands og hagdeild Íbúðalánasjóðs"


# read functions ----------------------------------------------------------

read_hagstofan <- function(
  file, 
  na = "..",
  encoding = "WINDOWS-1252",
  decimal_mark = ",",
  grouping_mark = ".",
  delim = ";",
  ...
){
  read_delim(
    file = file,
    delim = delim,
    na = na,
    locale = locale(
      encoding = encoding, 
      decimal_mark = decimal_mark, 
      grouping_mark = grouping_mark
    ), 
    ...
  )
}


# clean_sql_data ----------------------------------------------------------


