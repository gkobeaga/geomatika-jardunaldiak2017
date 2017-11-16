eskualdeak.ftx <- "../datuak/euskalgeo/eskualdeak/Eskualdeak.shp"
if (!file.exists(eskualdeak.ftx))
{ eskualdeak.url <- "http://euskalgeo.net/sites/euskalgeo.net/files/fitxategi-eranskin/Eskualdeak_0.zip"
  if (!dir.exists("datuak/euskalgeo"))
    dir.create("../datuak/euskalgeo")
  download.file(eskualdeak.url, "../datuak/euskalgeo/eskualdeak.zip")
  unzip("../datuak/euskalgeo/eskualdeak.zip", exdir="../datuak/euskalgeo/eskualdeak")
}

udalerriak.ftx  <- "../datuak/euskalgeo/udalerriak/udalerriak.shp"
if (!file.exists(udalerriak.ftx))
{ udalerriak.url <- "http://euskalgeo.net/sites/euskalgeo.net/files/fitxategi-eranskin/udalerriak.zip"
  if (!dir.exists("../datuak/euskalgeo"))
    dir.create("../datuak/euskalgeo")
  download.file(udalerriak.url, "../datuak/euskalgeo/udalerriak.zip")
  unzip("../datuak/euskalgeo/udalerriak.zip", exdir="../datuak/euskalgeo/udalerriak")
}

library(rgdal)
eskualdeak.shp <- readOGR(eskualdeak.ftx)
udalerriak.shp <- readOGR(udalerriak.ftx)

library(broom)
eskualdeak <- tidy(eskualdeak.shp,region="es_kod_2")
udalerriak <- tidy(udalerriak.shp,region="ud_kodea")

library(ggplot2)
g <- ggplot()
g <- g + geom_map(data=eskualdeak, map=eskualdeak,
                    aes(x=long, y=lat, group=group, map_id=id),
                color="white")
ggsave("../aurkezpena/irudiak/mapa-hutsik.png", plot=g , width = 6, height = 4.5)


g <- ggplot()
g <- g + geom_map(data=udalerriak, map=udalerriak,
                    aes(x=long, y=lat, group=group, map_id=id),
                    color="white")
