library(tidyverse)

# Load fonts
windowsFonts(Setimo = windowsFont("Setimo"))
windowsFonts(SetimoLight = windowsFont("Setimo Light"))


# Colors for monthly report -----------------------------------------------

blue <- "#11223a"
red <- "#ff5028"
color_main <- "#11223a"
color_extra <- "#11223a"


# we really need more colors. This is mixture of the new and the old palette
palette_light <- c(
 "#11223a", # blue - main color
  "#d8e6e8", # hms2
  "#ded1c4", # hms3
  "#efbcbb", # hms extra 1
  "#c5e6e6", # hms extra 2
  "#92b1b0", # old
  "#b39271"  # old
)

palette_medium <- c(
  "#11223a", # blue - main color
  "#8cc0c6", # hms2
  "#b3cfd1", # hms3
  "#b39271", # hms extra 1
  "#00aec7", # hms extra 2
  "#EC8865", # old
  "#C56BA4"  # old
)

palette_dark <- c(
  "#11223a", # blue - main color
  "#92b1b0", # hms2
  "#b39271", # hms3
  "#e25e5c", # hms extra 1
  "#7cc3c2", # hms extra 2
  "#d8e6e8", # old
  "#ded1c4"  # old
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
palette_hms <- palette_hms %>% unname()
palette_hms_darker <- palette_hms_darker %>% unname()


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
      panel.grid.major.y = element_line(colour = color_extra, size = 0.04),
      panel.grid.major.x = element_blank(),
      
      strip.text = element_text(family = "SetimoLight", size = 7 * tsm, color = "black", face = "bold"),
      strip.text.x = element_text(family = "SetimoLight", size = 7 * tsm, color = "black", face = "bold"),
      strip.background = element_blank(),
      
      plot.title = element_text(family = "Setimo", size = 12 * tsm, face = "bold", color = blue),
      plot.subtitle = element_text(family = "Setimo", size = 10 * tsm, color = blue),
      plot.caption = element_text(family = "SetimoLight", size = 8 * tsm, color = blue, face = "italic"),
      axis.title = element_text(size = 7 * tsm),
      axis.text = element_text(colour = color_extra, size = 8 * tsm),
      legend.text = element_text(colour = color_extra, size = 9 * tsm),
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

theme_vertical_x <- theme(
  axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)
)


theme_flip <- function(color = gray, ...){
  theme(
    panel.grid.major.x = element_line(color = color, ...),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)
  )
}





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

label_isl <- function(accuracy = 1, scale = 1, ...){
  scales::label_comma(accuracy = accuracy, scale = scale, decimal.mark = ",", big.mark = ".", ...)
} 

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
    
    scale_colour_discrete <- function(...) {
      scale_colour_manual(..., values = palette_hms_darker %>% unname())
    }
    
    scale_fill_discrete <- function(...) {
      scale_fill_manual(..., values = palette_hms)
    }
    
    assign("scale_colour_discrete", scale_colour_discrete, envir = .GlobalEnv)
    assign("scale_fill_discrete", scale_fill_discrete, envir = .GlobalEnv)
  }
  
}


# Common caption names ----------------------------------------------------

cap_hms <- "Heimild: Hagdeild HMS"
cap_thjodskra <- "Heimild: Þjóðskrá Íslands"
cap_hms_thjodskra <- "Heimild: Þjóðskrá Íslands og hagdeild HMS"
cap_hagstofa <- "Heimild: Hagstofa Íslands"
cap_hms_hagstofa <- "Heimild: Hagstofa Íslands og hagdeild HMS"


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

string_to_date <- function(df){
  df %>% 
    collect() %>% 
    mutate_at(
      vars(contains("timi"), contains("DATE")),
      ~ ymd(.)
      )
}

int_to_double <- function(df){
  df %>% 
    collect() %>% 
    mutate_if(is.integer, "as.double")
}

sql_clean <- function(df){
  require(tidyverse)
  require(lubridate)
  
  df %>% 
    string_to_date %>% 
    int_to_double
}


# seasonal ----------------------------------------------------------------

tidy_seas <- function(x, date, frequency = 12, ...){
  require(lubridate)
  require(broom)
  start <- min(date)
  start <- year(start) + month(start) / 12
  x <- ts(x, start = start, frequency = frequency)
  
  x %>% 
    seas(...) %>% 
    pluck("data") %>% 
    as_tibble() 
    
}

nested_seas <- function(data, x, date, frequency = 12, ...){
  x <- enquo(x)
  date <- enquo(date)
  
  tx <- data %>% select(!!x) %>%  pull(1)
  date_series <- data %>% select(!!date) %>% pull(1)
  
  tidy_seas(tx, date_series, frequency = frequency, ...) %>% 
    bind_cols(data %>% select(-!!date))
  
}

map_seas <- function(data, var, date){
  require(seasonal)
  var <- enquo(var)
  date <- enquo(date)
  data %>% 
    nest() %>% 
    mutate(seas = map(data, ~nested_seas(., !!var, !!date))) %>% 
    select(-data) %>% 
    unnest(c(seas))
}

test_line<- function(){
  data("economics")
  
  economics %>% 
    gather(var, val, -date) %>% 
    ggplot(aes(date, val, color = var)) +
    geom_line() +
    theme_hms() +
    scale_color_manual(values = palette_dark)
}

test_graphs <- function(){
  require(here)
  data("economics")
  
  economics %>% 
    gather(var, val, -date) %>% 
    ggplot(aes(date, val, color = var)) +
    geom_line() +
    theme_hms() +
    scale_color_manual(values = palette_dark) +
    scale_y_continuous(labels = label_isl()) +
    labs(
      title = "Lines and stuff",
      subtitle = "With HMS colors",
      x = NULL, 
      y = NULL,
      caption = "Source: None of your business :P"
    )
  
  ggsave_both(here("test_graphs", "lines"))
  
  economics %>% 
    filter(date == floor_date(date, "year"), year(date) > 2000) %>% 
    gather(var, val, -date) %>% 
    ggplot(aes(date, val, color = var)) +
    geom_col(width = 0.3) +
    geom_point() +
    theme_hms() +
    scale_color_manual(values = palette_dark) +
    scale_y_continuous(labels = label_isl()) +
    labs(
      title = "Lollipops and stuff",
      subtitle = "With HMS colors",
      x = NULL, 
      y = NULL,
      caption = "Source: None of your business :P"
    ) +
    facet_wrap(~var, scales = "free_y")
  
  ggsave_both(here("test_graphs", "lollipop"))
  
  economics %>% 
    filter(date == floor_date(date, "year"), year(date) > 2000) %>% 
    gather(var, val, -date) %>% 
    ggplot(aes(date, val, color = var)) +
    geom_col(width = 0.3) +
    geom_point() +
    theme_hms() +
    scale_color_manual(values = palette_dark) +
    scale_y_continuous(labels = label_isl()) +
    labs(
      title = "Lollipops and stuff",
      subtitle = "With HMS colors",
      x = NULL, 
      y = NULL,
      caption = "Source: None of your business :P"
    ) +
    facet_wrap(~var, scales = "free_y", ncol = 2)
  
  ggsave_both(here("test_graphs", "lollipop_narrow_high"), width = width_narrow, height =  height_full)
  
  economics %>% 
    filter(date == floor_date(date, "year"), year(date) > 2000) %>% 
    gather(var, val, -date) %>% 
    ggplot(aes(date, val, fill = var)) +
    geom_col() +
    theme_hms() +
    scale_color_manual(values = palette_dark) +
    scale_y_continuous(labels = label_isl()) +
    labs(
      title = "Bars and stuff",
      subtitle = "With HMS colors",
      x = NULL, 
      y = NULL,
      caption = "Source: None of your business :P"
    ) +
    facet_wrap(~var, scales = "free_y")
  
  ggsave_both(here("test_graphs", "col"))
  
  economics %>% 
    filter(date == floor_date(date, "year"), year(date) > 2000) %>% 
    gather(var, val, -date) %>% 
    filter(var == "pop") %>% 
    ggplot(aes(date, val)) +
    geom_col() +
    geom_text(aes(label = val), angle = 90, size = 3, color = "white", hjust = 1) +
    theme_hms() +
    scale_color_manual(values = palette_dark) +
    scale_y_continuous(labels = label_isl()) +
    labs(
      title = "Bars and stuff",
      subtitle = "With HMS colors",
      x = NULL, 
      y = NULL,
      caption = "Source: None of your business :P"
    ) +
    # facet_wrap(~var, scales = "free_y") + 
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_blank()
    )
  
  ggsave_both(here("test_graphs", "col_minimal"))
  
  economics %>% 
    filter(date == floor_date(date, "year"), year(date) > 2000) %>% 
    gather(var, val, -date) %>% 
    ggplot(aes(date, val, fill = var, color = var)) +
    geom_area(alpha = 0.5, color = NA) +
    geom_line() +
    theme_hms() +
    scale_color_manual(values = palette_dark) +
    scale_y_continuous(labels = label_isl()) +
    labs(
      title = "Area and stuff",
      subtitle = "With HMS colors",
      x = NULL, 
      y = NULL,
      caption = "Source: None of your business :P"
    ) +
    facet_wrap(~var, scales = "free_y")
  
  ggsave_both(here("test_graphs", "area"))
  
}


# gögn --------------------------------------------------------------------

visitala_launa <- function(){
  require(lubridate)
  require(tidyverse)
  
  read_hagstofan(
    "https://px.hagstofa.is:443/pxis/sq/ca030daf-96e0-4e07-bbab-586a9beb0d0e",
    decimal_mark = ".",
    grouping_mark = ","
    ) %>% 
    rename(Gildi = "Launavísitala frá 1989") %>% 
    mutate(
      timi = ymd(paste(Ár, Mánuður, "01")),
      Vísitala = "Launavísitala"
      ) %>% 
    select(Gildi, timi, Vísitala)
}

visitala_leiguverds <- function(file){
  require(readxl)
  require(lubridate)
  require(tidyverse)
  
  readxl::read_excel(
    file,
    skip = 2
  ) %>% 
    mutate(
      mánuður = mánuður %>% str_replace("julí", "júlí"),
      timi = ymd(paste(Ár, mánuður, "01")),
      Vísitala = "Vísitala leiguverðs"
      ) %>% 
    rename(Gildi = "Vísitala leiguverðs íbúðarhúsnæðis á höfuðborgarsvæðinu") %>% 
    select(timi, Gildi, Vísitala)
}

visitala_ibudaverds <- function(file){
  require(readxl)
  require(lubridate)
  require(tidyverse)
  
  read_excel(
    file,
    skip = 2
  ) %>% 
    rename(ar = 1, man = 2) %>% 
    fill(ar, .direction = "up") %>% 
    mutate(timi = ymd(paste(ar, man, "01"))) %>% 
    select(-ar, -man) %>% 
    gather(`Tegund`, `Gildi`, -timi) %>% 
    mutate(Vísitala = "Vísitala íbúðaverðs")
}

lubriyear <- function(year, month = "01"){
  require(lubridate)
  require(tidyverse)
  ymd(paste(year, month, "01", sep = "-"))
}

shadow_years <- function(start = 1900, end = 2100, color = red){
  shadings <- tibble(
    lower = ymd(paste(seq(start, end - 1, by = 2), 01, 01)),
    upper = ymd(paste(seq(start + 1, end, by = 2), 01, 01))
  )
  
  geom_rect(
    aes(xmin = lower , xmax = upper, ymin = -Inf, ymax = Inf), 
    data = shadings, 
    inherit.aes=FALSE, 
    alpha = 0.2, 
    fill = red) 
  
}
  