
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


library(curl)

download.file("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_1.RDS", "bio_1.RDS", method="curl")

library(data.table)
a <- fread("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_1.RDS")

BestMyyu <- readRDS("bio_1.RDS")



githubURL <- paste0("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_",1,".RDS")


##

bio_number = 1
rasterDF <- readRDS("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\bio_1.RDS")



##
