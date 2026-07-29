geom_strikezone = function(color = "black", linewidth = 0.25, sz_top = 3.8, sz_bot = 1.1) {
  sz_left = -0.85
  sz_right = 0.85
  strikezone = data.frame(
    x = c(sz_left, sz_left, sz_right, sz_right, sz_left),
    y = c(sz_bot, sz_top, sz_top, sz_bot, sz_bot)
  )
  geom_path(aes(.data$x, .data$y), data = strikezone, linewidth = linewidth, col = color)
}