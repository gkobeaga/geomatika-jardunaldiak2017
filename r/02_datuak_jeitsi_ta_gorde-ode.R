library(dplyr)    # awesome data manipulation
library(tidyr)    # reshaping data

library(stringi)  # string manipulation
library(stringr)  # string manipulation

library(rgdal) # readShapePoly

datu_mota <- function(izena, ezaug) {
    if (length(grep("%",izena)) + length(grep("‰",izena)) +
        length(grep("%",ezaug)) + length(grep("‰",ezaug)))
    { return("ehunekoa")
    } else if (length(grep("€",izena))+length(grep("€",ezaug)))
    { return("diru-kopurua")
    } else if (length(grep("indizea",izena)) + length(grep("indizea",ezaug)))
    { return("indizea")
    } else if (length(grep("azalera",izena)) + length(grep("azalera",ezaug)))
    { return("azalera")
    }
    return("ezezaguna")
}


ode <- "../datuak/opendataeuskadi/"

dir.create(paste0(ode, "jatorrizkoak"),recursive=T)

adierazleak <- seq(1,208)

for (adierazlea in adierazleak) {
  if (!adierazlea %in% c(54,107,128,133, 147,155,156,171)){
    print(adierazlea)
    adierazlea.helb <- paste0('http://opendata.euskadi.eus/contenidos/estadistica/udalmap_indicador_',adierazlea,'/eu_def/adjuntos/indicator.csv')
    adierazlea.fitx <- paste0(ode, 'jatorrizkoak/', adierazlea,'.csv')
    if ( !file.exists(adierazlea.fitx)) {
      download.file(adierazlea.helb, adierazlea.fitx)
    }
  }
}


dir.create(paste0(ode, "eskualdeka"),recursive=T)
dir.create(paste0(ode, "herrialdeka"),recursive=T)
dir.create(paste0(ode, "udalerrika"),recursive=T)

eskualdeak.shp <- readOGR("../../udalmap-r-maps/data/shp/eskualdeak/Eskualdeak.shp")

adierazleak.ezaugarriak <- data.frame( adi_id=adierazleak, adi_izena="", urteak="", adi_ezaug=""
                                     , adi_mota="", stringsAsFactors=F )

for (adierazlea in adierazleak)
{ if (!adierazlea %in% c(54,107,128,133, 147,155,156,171))
  { print(adierazlea)
    adierazlea.fitx <- paste0(ode, 'jatorrizkoak/',adierazlea,'.csv')
    adierazlea.csv <- read.csv(adierazlea.fitx,sep=";",header=F, stringsAsFactors=F,
                              encoding="latin1")


    for (i in 1:nrow(adierazlea.csv))
    { for (j in 1:ncol(adierazlea.csv))
      { adierazlea.csv[i,j] <- gsub("\\.","",adierazlea.csv[i,j])
        adierazlea.csv[i,j] <- gsub(",",".",adierazlea.csv[i,j])
        adierazlea.csv[i,j] <- gsub("-1234.56","NA",adierazlea.csv[i,j])
      }
    }

    adierazlea.csv[(adierazlea.csv=="")] <- NA
    stops <- which(apply(is.na(adierazlea.csv),1,sum) == ncol(adierazlea.csv))

    adierazlea.id <- adierazlea
    adierazlea.izenatmp <- gsub("Udalerrietako iraunkortasun adierazleak: ", "",
                           adierazlea.csv[1,1])

    adierazlea.izena <- str_trim(strsplit(adierazlea.izenatmp,"\\(")[[1]][1])
    adierazlea.ezaug <- ifelse(!is.na(strsplit(adierazlea.izenatmp,"\\(")[[1]][2]), str_trim(paste0("(",strsplit(adierazlea.izenatmp,"\\(")[[1]][2])),"")

    # Herrialdeka
    if ( "Entitatea" %in% adierazlea.csv[,1] )
    { herrialdeak <- as.data.frame(adierazlea.csv[c((stops[1]+3):(stops[2]-1)),-2])
      colnames(herrialdeak) <- adierazlea.csv[(stops[1]+2),-2]
      rownames(herrialdeak) <- 1:nrow(herrialdeak)

      herrialdeak$adm_id <- ""
      herrialdeak$adm_izena <- ""
      herrialdeak$adm_mota <- "herrialdea"

      for (row in 1:nrow(herrialdeak))
      { if (herrialdeak[row, "Entitatea"]         == "Araba/Álava" )
        { herrialdeak[row,]$adm_id    <- '01'
          herrialdeak[row,]$adm_izena <- "Araba"
        } else if (herrialdeak[row, "Entitatea"]  == "Bizkaia" )
        { herrialdeak[row,]$adm_id    <- '48'
          herrialdeak[row,]$adm_izena <- 'Bizkaia'
        } else if (herrialdeak[row, "Entitatea"]  == "Gipuzkoa" )
        { herrialdeak[row,]$adm_id    <- '20'
          herrialdeak[row,]$adm_izena <- 'Gipuzkoa'
        }
      }

      herrialdeak$adi_id    <- sprintf("A%03d", adierazlea)
      herrialdeak$adi_izena <- iconv(adierazlea.izena, "latin1", "UTF-8")
      herrialdeak$adi_ezaug <- iconv(adierazlea.ezaug, "latin1", "UTF-8")
      herrialdeak$adi_mota  <- datu_mota(herrialdeak$adi_izena, herrialdeak$adi_ezaug)


      herrialdeak.fitx <- paste0(ode, "herrialdeka/udalmap_herrialdeak_"
                                 , sprintf("A%03d", adierazlea),".csv")
      write.csv(herrialdeak, herrialdeak.fitx, row.names=F )
    }

    # Eskualdeka
    if ("Lurralde" %in% adierazlea.csv[,2] )
    { eskualdeak <- as.data.frame(adierazlea.csv[c((stops[2]+3):(stops[3]-1)),])
      colnames(eskualdeak) <- adierazlea.csv[(stops[2]+2),]
      rownames(eskualdeak) <- 1:nrow(eskualdeak)

      eskualdeak$adm_id <- NA
      for (i in 1:nrow(eskualdeak.shp@data))
      { if (sum(grepl(eskualdeak.shp$es_iz_e_2[i], eskualdeak$Lurralde)))
        { eskualdeak[grepl(eskualdeak.shp$es_iz_e_2[i], eskualdeak$Lurralde),]$adm_id <- as.character(eskualdeak.shp$es_kod_2[i])
        }
      }

      eskualdeak[grepl("Markina-Ondarroa", eskualdeak$Lurralde),]$adm_id <- "E33" # Lea Artibai
      eskualdeak[grepl("Arratia Nerbioi", eskualdeak$Lurralde),]$adm_id <- "E07" # Arrati Nerbioi
      eskualdeak[grepl("Deba Beherea", eskualdeak$Lurralde),]$adm_id <- "E13" # Debabarrena


      eskualdeak$adm_izena <- iconv(eskualdeak[,"Lurralde"], "latin3", "UTF-8")# Eus/Es
      eskualdeak$adm_mota <- "eskualdea"

      eskualdeak$adi_id <- sprintf("A%03d", adierazlea)
      eskualdeak$adi_izena <- iconv(adierazlea.izena, "latin1", "UTF-8")
      eskualdeak$adi_ezaug <- iconv(adierazlea.ezaug, "latin3", "UTF-8")
      eskualdeak$adi_mota  <- datu_mota(eskualdeak$adi_izena,
                               eskualdeak$adi_ezaug)

      eskualdeak <- eskualdeak[,-1]

      eskualdeak.fitx <- paste0(ode,"eskualdeka/udalmap_eskualdeak_",sprintf("A%03d", adierazlea),".csv")
      write.csv(eskualdeak, eskualdeak.fitx, row.names=F )

      stops3 <- stops[3]
    } else
      stops3 <- stops[2]


    ## Municipalities
    udalerriak           <- as.data.frame(adierazlea.csv[c((stops3+3):nrow(adierazlea.csv)),])
    colnames(udalerriak) <- adierazlea.csv[(stops3+2),]
    rownames(udalerriak) <- 1:nrow(udalerriak)

    udalerriak$adm_id    <- udalerriak[,"Udalerri kodea"]
    udalerriak$adm_izena <- iconv(udalerriak[,"Udalerria"], "Windows-1252", "UTF-8")# Eus/Es
    udalerriak$adm_mota  <- "Udalerria"

    udalerriak$adi_izena <- iconv(adierazlea.izena, "Windows-1252", "UTF-8")
    udalerriak$adi_ezaug <- iconv(adierazlea.ezaug, "Windows-1252", "UTF-8")
    udalerriak$adi_id    <- sprintf("A%03d", adierazlea)
    udalerriak$adi_mota  <- datu_mota(udalerriak$adi_izena,
                                udalerriak$adi_ezaug)

    udalerriak <- udalerriak[,-1]

    udalerriak.fitx <- paste0(ode, "udalerrika/udalmap_udalerriak_"
                                 , sprintf("A%03d", adierazlea),".csv")
    write.csv(udalerriak, udalerriak.fitx, row.names=F )

  }

  urteak1 <- names(udalerriak)[startsWith(names(udalerriak),"1") | startsWith(names(udalerriak),"2")]
  urteak2 <- names(eskualdeak)[startsWith(names(udalerriak),"1") | startsWith(names(udalerriak),"2")]
  urteak3 <- names(herrialdeak)[startsWith(names(udalerriak),"1") | startsWith(names(udalerriak),"2")]
  urteak  <- sort(as.numeric(unique(c(urteak1, urteak2, urteak3))))
  
  adierazleak.ezaugarriak[adierazlea,"urteak"] <- paste(urteak, collapse="-" )

  adierazleak.ezaugarriak[adierazlea,"adi_izena"] <- adierazlea.izena
  adierazleak.ezaugarriak[adierazlea,"adi_ezaug"] <- adierazlea.ezaug
  adierazleak.ezaugarriak[adierazlea,"adi_mota"] <- datu_mota(adierazlea.izena, adierazlea.ezaug)

}

library(gridExtra)
pdf("adierazleak-udalmap.pdf", height=60, width=30)
grid.table(adierazleak.ezaugarriak, theme = ttheme_default(core = list(fg_params=list(hjust=1, x=0.95))))
graphics.off()
