
source('shiny_module_rr_display.R')

library(isoband)
library(sf)

exp_ret<- seq(0, max(base_assets_rr$exp_ret), length.out = 100)
exp_sd_ret <- seq(0, max(base_assets_rr$exp_sd_ret), length.out = 100)

df <- expand.grid(exp_sd_ret = exp_sd_ret, exp_ret = exp_ret)
utility <- matrix(
  nrow = 100, ncol = 100, 
  data = 1 - exp(-df$exp_ret +10*df$exp_sd_ret^2), 
  byrow = TRUE
)
l <- isolines(exp_sd_ret, exp_ret, utility, seq(0.035, 0.07, length.out = 4))

lines <- iso_to_sfg(l)
data_lines <- st_sf(
  level = 2:(length(lines)+1),
  geometry = st_sfc(lines)
)

static_rr_display(nassets = 3, nportfolios = 1000) +
  geom_sf(data = data_lines,color = "black") +
  coord_sf(expand = FALSE) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_point(aes(x = 0.057, y = 0.068), color = "orange", size = 2)

