#############################################
# 3D forest height with R
# Jorge Villa 2024/11/14
#############################################

libs <- c(
    "tidyverse", "sf", "geodata",
    "terra", "classInt", "rayshader","ggspatial"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

invisible(lapply(
    libs,
    library,
    character.only = T
))

# 1. DOWNLOAD ETH DATA
#---------------------

urls <- c(
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_S15W075_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_S15W078_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_S18W075_Map.tif"
)

for (url in urls) {
    download.file(
        url,
        destfile = basename(gsub(".*ETH_","", url)),
        mode = "wb"
    )
}

setwd("C:/Users/Jorge/Desktop/Portofolio/3Dforestheightmaps")

raster_files <-
    list.files(
        path = getwd(),
        pattern = "GlobalCanopyHeight",
        full.names = T
    )

# 2. PORTUGAL POLYGON
#--------------------

get_country_borders <- function() {
    main_path <- getwd()
    country_borders <- geodata::gadm(
        country = "PER",
        level = 1,
        path = main_path
    ) |>
        sf::st_as_sf()

    return(country_borders)
}

country_borders <- get_country_borders()
unique(
    country_borders$NAME_1
)

Ayacucho_sf <- country_borders |>
    dplyr::filter(
        NAME_1 == "Ayacucho"
    ) #|>
    #sf::st_union()

plot(sf::st_geometry(
  Ayacucho_sf
))

# 3. LOAD FOREST HEIGHT
#----------------------

forest_height_list <- lapply(
    raster_files,
    terra::rast
)

forest_height_rasters <- lapply(
    forest_height_list,
    function(x) {
        terra::crop(
            x,
            terra::vect(
              Ayacucho_sf
            ),
            snap = "in",
            mask = T
        )
    }
)

forest_height_mosaic <- do.call(
    terra::mosaic,
    forest_height_rasters
)

forest_height_ayacucho <- forest_height_mosaic |>
    terra::aggregate(
        fact = 10
    )

writeRaster(forest_height_ayacucho, "forest_height_ayacucho.tif", overwrite=TRUE)

forest_height_ayacucho<-rast("C:/Users/Jorge/Desktop/Portofolio/3Dforestheightmaps/forest_height_ayacucho.tif")
# 4. RASTER TO DATAFRAME
#-----------------------

forest_height_ayacucho_df <- forest_height_ayacucho |>
    as.data.frame(
        xy = T
    )

head(forest_height_ayacucho_df)
names(forest_height_ayacucho_df)[3] <- "height"

write.table(forest_height_ayacucho_df, "fh.csv")
forest_height_ayacucho_df <- read.csv("C:/Users/Jorge/Desktop/Portofolio/3Dforestheightmaps/fh.csv", sep="")

# 5. BREAKS
#----------

breaks <- classInt::classIntervals(
    forest_height_ayacucho_df$height,
    n = 5,
    style = "fisher"
)$brks

# 6. COLORS
#----------

cols <-
    c(
        "white", "#ffd3af", "#fbe06e",
        "#6daa55", "#205544"
    )

texture <- colorRampPalette(
    cols,
    bias = 2
)(6)

# 7. GGPLOT2
#-----------
#proj <- "EPSG:4668"

p <- ggplot(forest_height_ayacucho_df) +
  geom_raster(aes(x = x, y = y, fill = height)) +
  scale_fill_gradientn(
    name = "height (m)",
    colors = texture,
    breaks = round(breaks, 0)) +
  coord_sf(crs = 4326) +
  guides(
    fill = guide_legend(
        direction = "vertical",
        keyheight = unit(5, "mm"),
        keywidth = unit(5, "mm"),
        title.position = "top",
        label.position = "right",
        title.hjust = .5,
        label.hjust = .5,
        ncol = 1,
        byrow = F)) +
  annotation_scale(
    #location = "bl", 
    #height = .15,
    pad_x = unit(.5, "cm"),
    pad_y = unit(.5, "cm")) +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(.5, "cm"),
    pad_y = unit(.5, "cm")) +
  #coord_sf(crs = proj) +
  labs(
    title = "Forest Cover | Ayacucho (Péru)",
    caption = "©2024 Jorge Villa\nData: ©ETH Global Canopy Height dataset at 10 m"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(
        size = 11, color = "grey10"),
    legend.text = element_text(
        size = 10, color = "grey10"),
    panel.grid.major = element_line(
        color = "white"),
     panel.grid.minor = element_line(
        color = "white"),
    plot.background = element_rect(
        fill = "white", color = NA),
    legend.background = element_rect(
        fill = "white", color = NA),
    panel.border = element_rect(
        fill = NA, color = "white"),
    plot.margin = unit(
        c(
            t = 0, r = 0,
            b = 0, l = 0
        ), "lines"))

p

# 8. RENDER SCENE
#----------------

h <- nrow(forest_height_ayacucho)
w <- ncol(forest_height_ayacucho)

rayshader::plot_gg(
    ggobj = p,
    width = w / 500,
    height = h / 500,
    scale = 75,
    solid = F,
    soliddepth = 0,
    shadow = T,
    shadow_intensity = .99,
    offset_edges = F,
    sunangle = 315,
    #window.size = c(800, 800),
    zoom = .4,
    phi = 30,
    theta = -30,
    multicore = T
)

rayshader::render_camera(
    phi = 50,
    zoom = .7,
    theta = 45
)

# 9. RENDER OBJECT
#-----------------

rayshader::render_highquality(
    samples=100,
    filename = "ayacucho-forest-height-2020_V2.png",
    preview = T,
    interactive = F,
    light = T,
    lightdirection = c(
        315, 310, 315, 310
    ),
    lightintensity = c(
        1000, 1500, 150, 100
    ),
    lightaltitude = c(
        15, 15, 80, 80
    ),
    ground_material = 
    rayrender::microfacet(
        roughness = .6
    ),
    width = 2000,
    height = 2000
)
