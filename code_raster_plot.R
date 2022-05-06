
bio<- raster("C:\\Users\\Luca Danese\\Downloads\\PLS_Aedes\\datasets\\WORLDCLIM\\wc2.1_30s_bio_1.tif")

gplot_data <- function(x, maxpixels = 5000000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x)))
  names(dat) <- c('value', 'variable')

  dat <- dplyr::as.tbl(data.frame(coords, dat))

  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]],
                            by = c("value" = "ID"))
  }
  dat
}

gplot_wrld_r <- gplot_data(bio)

saveRDS(gplot_wrld_r,"bio_1.RDS")
write.csv(gplot_wrld_r, "bio_1.csv",row.names = FALSE)
save(gplot_wrld_r, file = "bio_1.RData")


a <- read.csv("bio_1.csv")

library(data.table)
a <- fread("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_1.RData")


library(ggplot2)
ggplot() +
  geom_tile(data = dplyr::filter(a, !is.na(value)),
            aes(x = x, y = y), fill = "coral3") +
  geom_tile(data = a,
            aes(x = x, y = y, fill = value)) +
  scale_fill_gradient(paste0("ciao"),
                      low = 'yellow', high = 'blue',
                      na.value = NA) +
  coord_quickmap()

githubURL <- "https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_1.RDS"
load(url(githubURL))

readRDS(githubURL)

a <- readRDS(url(githubURL))


prova <- fread("https://raw.githubusercontent.com/lucadanese/PLS---Summer-School-2022/main/aedes_albopictus_GBIF.csv")
rasterDF_2 <- read_excel(url(githubURL))

