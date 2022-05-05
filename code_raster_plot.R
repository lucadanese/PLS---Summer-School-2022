bio<- raster("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\gpw_v4_population_density_rev11_2020_30_sec.tif")

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

saveRDS(gplot_wrld_r,"bio_pop.RDS")
