library(plotly)
library(tibble)
library(tidyverse)

data <- tribble(
  ~x , ~y   , ~z   , ~group       , ~frame ,
   0 ,  0   ,  0   , "y"          ,      3 ,
  15 ,  6   ,  9   , "y"          ,      3 ,
   0 ,  0   ,  0   , "x"          ,      1 ,
  12 , 12   , 12   , "x"          ,      1 ,
   0 ,  0   ,  0   , "x"          ,      2 ,
  25 ,  5   ,  5   , "x"          ,      2 ,
   0 ,  0   ,  0   , "projection" ,      4 ,
  15 ,  7.5 ,  7.5 , "projection" ,      4 ,
  15 ,  6   ,  9   , "residual"   ,      5 ,
  15 ,  7.5 ,  7.5 , "residual"   ,      5 ,
)
fig <- data |>
  mutate(row = row_number()) |>
  group_by(frame, x, y, z) |>
  mutate(frame = list(seq(unique(frame), max(data$frame)))) |>
  unnest_longer(frame) |>
  ungroup() |>
  arrange(frame, row) |>

  # Create 3D scatter line plot
  plot_ly(
    x = ~x,
    y = ~y,
    z = ~z,
    type = 'scatter3d',
    mode = 'lines',
    # color = ~group,
    line = list(width = 4),
    frame = ~frame
  ) |>
  layout(
    scene = list(
      xaxis = list(title = "X-axis", range = c(0, max(data$x))),
      yaxis = list(title = "Y-axis", range = c(0, max(data$y))),
      zaxis = list(title = "Z-axis", range = c(0, max(data$z))),
      aspectmode = "cube",
      camera = list(
        eye = list(x = 1.5, y = -1.5, z = 1.5)
      )
    )
  ) |>
  animation_button(
    label = NULL # remove Play button
  )
fig
