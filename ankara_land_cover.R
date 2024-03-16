# 1. PACKAGES

libs <- c(
  "terra",
  "giscoR",
  "sf",
  "tidyverse",
  "ggtern",
  "elevatr",
  "png",
  "rayshader",
  "magick"
)

installed_libraries <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libraries == F)){
  install.packages(
    libs[!installed_libraries]
  )
}

invisible(
  lapply(
    libs, library, character.only = T
  )
)

# 2. COUNTRY BORDERS

get_country_borders_tr <- function() {
  main_path <- getwd()
  country_borders <- geodata::gadm(
    country = "TR",
    level = 1,
    path = main_path
  ) |>
    sf::st_as_sf()
  
  return(country_borders)
}

country_borders <- get_country_borders_tr()
unique(
  country_borders$NAME_1
)

ankara_data <- subset(country_borders, NAME_1 == "Ankara")

plot(sf::st_geometry(
  ankara_data
))

ankara_data <- st_transform(ankara_data, crs = 4326)

# 3 DOWNLOAD ESRI LAND COVER TILES

urls <- c(
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/36T_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/36S_20220101-20230101.tif"
)

for(url in urls){
  download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
  )
}

# 4 LOAD TILES

raster_files <- list.files(
  path = getwd(),
  pattern = "20230101.tif$",
  full.names = T
)

crs <- "EPSG:4326"

for(raster in raster_files){
  rasters <- terra::rast(raster)
  
  country <- ankara_data |>
    sf::st_transform(
      crs = terra::crs(
        rasters
      )
    )
  
  land_cover <- terra::crop(
    rasters,
    terra::vect(
      country
    ),
    snap = "in",
    mask = T
  ) |>
    terra::aggregate(
      fact = 30,
      fun = "modal"
    ) |>
    terra::project(crs)
  
  terra::writeRaster(
    land_cover,
    paste0(
      raster,
      "_ankara",
      ".tif"
    ),
    overwrite = TRUE  
  )
} 

# 5 LOAD VIRTUAL LAYER

r_list <- list.files(
  path = getwd(),
  pattern = "_ankara",
  full.names = T
)

land_cover_vrt <- terra::vrt(
  r_list,
  "ankara_land_cover_vrt.vrt",
  overwrite = T
)

# 6 FETCH ORIGINAL COLORS

ras <- terra::rast(
  raster_files[[1]]
)

raster_color_table <- do.call(
  data.frame,
  terra::coltab(ras)
)

head(raster_color_table)

hex_code <- ggtern::rgb2hex(
  r = raster_color_table[,2],
  g = raster_color_table[,3],
  b = raster_color_table[,4]
)

# 7 ASSIGN COLORS TO RASTER

cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))
land_cover_vrt <- na.omit(land_cover_vrt)

land_cover_ankara <- terra::subst(
  land_cover_vrt,
  from = from,
  to = to,
  names = cols
)

terra::plotRGB(land_cover_ankara)

# 8 DIGITAL ELEVATION MODEL

elev <- elevatr::get_elev_raster(
  locations = ankara_data,
  z = 10, clip = "locations"
)

crs_lambert <-
  "+proj=utm +zone=35 +datum=WGS84 +units=m +no_defs"

land_cover_ankara_resampled <- terra::resample(
  x = land_cover_ankara,
  y = terra::rast(elev),
  method = "bilinear"
) |>
  terra::project(crs_lambert)

terra::plotRGB(land_cover_ankara_resampled)

img_file <- "land_cover_ankara.png"

terra::writeRaster(
  land_cover_ankara_resampled,
  img_file,
  overwrite = T,
  NAflag = 255
)

img <- png::readPNG(img_file)

# 9. RENDER SCENE
#----------------

elev_lambert <- elev |>
  terra::rast() |>
  terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
  elev_lambert
)

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

try(rgl::close3d())

elmat |>
  rayshader::height_shade(
    texture = colorRampPalette(
      cols[9]
    )(256)
  ) |>
  rayshader::add_overlay(
    img,
    alphalayer = .9
  ) |>
  rayshader::plot_3d(
    elmat,
    zscale = 5,
    solid = F,
    shadow = T,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      w / 6, h / 6
    ),
    zoom = .515,
    phi = 85,
    theta = 0
  )

rayshader::render_camera(
  zoom = .58
)


# 10. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"
hdri_file <- basename(u)

download.file(
  url = u,
  destfile = hdri_file,
  mode = "wb"
)

filename <- "3d_land_cover_ankara-dark.png"

rayshader::render_highquality(
  filename = filename,
  preview = T,
  light = F,
  environment_light = hdri_file,
  intensity_env = 1,
  rotate_env = 90,
  interactive = F,
  parallel = T,
  width = w * 1.5,
  height = h * 1.5
)

# 11. PUT EVERYTHING TOGETHER

c(
  "#419bdf", "#397d49", "#7a87c6", 
  "#e49635", "#c4281b", "#a59b8f", 
  "#a8ebff", "#616161", "#e3e2c3"
)

legend_name <- "land_cover_legend.png"
png(legend_name)
par(family = "sans")

plot(
  NULL,
  xaxt = "n",
  yaxt = "n",
  bty = "n",
  ylab = "",
  xlab = "",
  xlim = 0:1,
  ylim = 0:1,
  xaxs = "i",
  yaxs = "i"
)
legend(
  "center",
  legend = c(
    "Water",
    "Trees",
    "Crops",
    "Built area",
    "Rangeland"
  ),
  pch = 15,
  cex = 2,
  pt.cex = 1,
  bty = "n",
  col = c(cols[c(1:2, 4:5, 9)]),
  fill = c(cols[c(1:2, 4:5, 9)]),
  border = "grey20"
)
dev.off()

# filename <- "land-cover-bih-3d-b.png"

lc_img <- magick::image_read(
  filename
)

my_legend <- magick::image_read(
  legend_name
)

my_legend_scaled <- magick::image_scale(
  magick::image_background(
    my_legend, "none"
  ), 1500
)

p <- magick::image_composite(
  magick::image_scale(
    lc_img, "x5000" 
  ),
  my_legend_scaled,
  gravity = "southwest",
  offset = "+100+0"
)

# Add the title "ANKARA" on the image
p <- magick::image_annotate(
  p, 
  text = "ANKARA",
  font = "sans",
  size = 450, # Text size, you can adjust according to need
  color = "grey20", # Text color, you can change it according to your need
  location = "+150+150", # Adjust the position of the text on the image as needed
  gravity = "northwest" # Where the text will be positioned, the upper left corner is selected here
)

p <- image_annotate(
  p,
  text = "Data: Sentinel-2 10m Land Use/Land Cover - Esri", 
  size = 50, 
  color = "grey20", 
  location = "+100+0", 
  gravity = "east", # Place text on the right side
  degrees = -90, # Rotate text 90 degrees to make it vertical
  font = "sans" 
)


magick::image_write(
  p, "3d_ankara_land_cover_final.png"
)




