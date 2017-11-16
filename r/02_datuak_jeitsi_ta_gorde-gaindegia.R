library(dplyr)    # awesome data manipulation
library(tidyr)    # reshaping data

library(stringi)  # string manipulation
library(stringr)  # string manipulation

library(ckanr)
library(readxl)

ckanr_setup(url = "http://datuak.eus")

datu_multzoak <- package_list(as = "table")

gaindegia <- "../datuak/gaindegia/"
dir.create(paste0(gaindegia, "jatorrizkoak/udalerrika"),recursive=T)
dir.create(paste0(gaindegia, "jatorrizkoak/eskualdeka"),recursive=T)
dir.create(paste0(gaindegia, "jatorrizkoak/herrialdeka"),recursive=T)

adierazleak <- data.frame()

for (datu_multzoa in datu_multzoak)
{ print(datu_multzoa)

  multzoa <- package_show(datu_multzoa)

  multzoa.bl2dk <- multzoa$resources

  baliabideak <- c()
  zonifikazioa <- c()
  for (baliabidea in multzoa.bl2dk )
  { if ( ( !baliabidea$name %in% baliabideak)                              &
         ( (baliabidea$format == "XLS") | (baliabidea$format == "xlsx") ) )
    { if ( length( grep("Eskualdeak",baliabidea$name) ) +
           length( grep("ES-EH",baliabidea$name)      ) )
      { baliabidea.helb <- baliabidea$url
        adierazlea.fitx <- paste0(gaindegia, 'jatorrizkoak/eskualdeka/',baliabidea$name,'.',tolower(baliabidea$format))

        download.file(baliabidea.helb, adierazlea.fitx)
        print(baliabidea$name)
        baliabideak <- c(baliabideak,baliabidea$name)
        zonifikazioa <- c(zonifikazioa,"eskualdeka")
      } else if ( length( grep("Herrialdeak",baliabidea$name) ) +
                  length( grep("HE-EH",baliabidea$name)       ) )
      { baliabidea.helb <- baliabidea$url
        adierazlea.fitx <- paste0(gaindegia, 'jatorrizkoak/herrialdeka/',baliabidea$name,'.',tolower(baliabidea$format))

        download.file(baliabidea.helb, adierazlea.fitx)
        print(baliabidea$name)
        baliabideak <- c(baliabideak,baliabidea$name)
        zonifikazioa <- c(zonifikazioa,"herrialdeka")
      } else if ( length( grep("Udalerriak",baliabidea$name) ) +
                  length( grep("UD-EH",baliabidea$name)      ) )
      { baliabidea.helb <- baliabidea$url
        adierazlea.fitx <- paste0(gaindegia, 'jatorrizkoak/udalerrika/',baliabidea$name,'.',tolower(baliabidea$format))

        download.file(baliabidea.helb, adierazlea.fitx)
        print(baliabidea$name)
        baliabideak <- c(baliabideak,baliabidea$name)
        zonifikazioa <- c(zonifikazioa,"udalerrika")
      }
    }
  }

  zutabe.kop <- length(unlist(multzoa.bl2dk))/multzoa$num_resources
  zutabe.iz    <- names(unlist(multzoa.bl2dk)[1:zutabe.kop])

  multzoa.bl2dk <- matrix(unlist(multzoa.bl2dk),nrow= multzoa$num_resources,byrow=T) %>% as.data.frame()
  colnames(multzoa.bl2dk) <- zutabe.iz

  multzoa.bl2dk <- multzoa.bl2dk %>%
    filter(format=="XLS" | format=="xlsx" ) %>%
    filter( grepl("Eskualdeak|Herrialdeak|Udalerriak|ES-EH|HE-EH|UD-EH", name) ) %>%
    mutate(format=tolower(format)) %>%
    select(id, name, description, resource_group_id, url, format, created, revision_timestamp)

  if (nrow(multzoa.bl2dk))
  { multzoa.bl2dk$multzoa     <-  multzoa$title
    multzoa.bl2dk$multzoa.id  <-  multzoa$name
    multzoa.bl2dk$gaia        <-  multzoa$organization$name
    multzoa.bl2dk$azpitaldea  <-  ifelse ( length(multzoa$groups)
                                       , multzoa$groups[[1]]$name
                                     #, multzoa$name )
                                       , "ezezaguna" )
    multzoa.bl2dk$zonifikazioa  <- zonifikazioa

    tags <- c()
    for (tag in 1:multzoa$num_tags)
      tags <- c(tags, multzoa$tags[[tag]]$name)

    multzoa.bl2dk$tags <- paste(tags,collapse=",")

    if (length(adierazleak))
    { adierazleak <- bind_rows(adierazleak, multzoa.bl2dk)
    } else
    { adierazleak <- multzoa.bl2dk
    }
  }

}

adierazleak.fitx  <- paste0(gaindegia, "jatorrizkoak/adierazleak.csv")
write.csv(adierazleak, adierazleak.fitx, row.names=F )
#adierazleak <- read.csv( adierazleak.fitx )

dir.create(paste0(gaindegia, "udalerrika"),recursive=T)
dir.create(paste0(gaindegia, "eskualdeka"),recursive=T)
dir.create(paste0(gaindegia, "herrialdeka"),recursive=T)



# 6 Gizonak?
# 11 Hezkuntza maila
# 41 arazoa
#37 zutabeak txarto
for (ilara in 1:nrow(adierazleak))
{ print(ilara)
  if (!ilara %in% c(38,41) )
  {
  adierazlea <- adierazleak[ilara,]

  adierazlea.jatorrizkoa.fitx <- paste0( gaindegia, 'jatorrizkoak/', adierazlea$zonifikazioa
                                        ,"/",adierazlea$name, ".", adierazlea$format)

  print(adierazlea.jatorrizkoa.fitx)

  adierazlea.izena <- colnames(read_excel(adierazlea.jatorrizkoa.fitx,n_max=0))[1]

  urtea <- ifelse(grepl("\\/",adierazlea.izena),
                  sub("\\/","-",sub(".*?\\b([1-2][0-9][0-9][0-9]\\/[1-2][0-9][0-9][0-9])\\b.*", "\\1", adierazlea.izena)),
                  sub(".*?\\b([1-2][0-9][0-9][0-9])\\b.*", "\\1", adierazlea.izena))

  adierazlea.moldatua.fitx <- ""

  errhasi  <- 1
  ncols <- 1
  while (ncols==1)
  { ncols <- length( colnames(read_excel(adierazlea.jatorrizkoa.fitx,skip=errhasi,n_max=0)))
    if (ncols==1)
    errhasi <- errhasi +1
  }

  adierazlea.taul <- read_excel(adierazlea.jatorrizkoa.fitx, skip=errhasi,col_types="text")

  print(adierazlea.izena)

  adinka  <- grepl("adin", adierazlea.izena, ignore.case = TRUE)
  sexuka  <- grepl("sexua", adierazlea.izena, ignore.case = TRUE)
  kopuruka <- grepl("kopuruaren", adierazlea.izena, ignore.case = TRUE)
  jardueraka <- grepl("jarduera-adar", adierazlea.izena, ignore.case = TRUE)
  arabera <- grepl("arabera", adierazlea.izena, ignore.case = TRUE) # jarduera-adarrareren arabera

  adierazlea.izena <- str_trim(gsub("[0-9]*|\\.|,", "",  adierazlea.izena))
  adierazlea.izena <- str_trim(strsplit(adierazlea.izena,"Euskal")[[1]][1])

  if (sexuka)
  { print("sexuka")
    zuthasi <- ifelse (adierazlea$zonifikazioa == "udalerrika", 5, 4)
    adierazlea.taul <- adierazlea.taul %>% gather(zutabea, balioa, zuthasi:ncol(adierazlea.taul)) %>% as.data.frame()


    adierazlea.taul$zutabea <- gsub("2010__1","2011",adierazlea.taul$zutabea)

    if (adinka)
    {print(" + adinka")
      adierazlea.taul$aldagaia  <- str_trim(sub("Gizon.|Emakum.", "", adierazlea.taul$zutabea))
      adierazlea.taul$sexua <- ifelse(grepl("Gizon.",  adierazlea.taul$zutabea),"Gizonezkoak","Emakumezkoak")
      adierazlea.taul$mota <- ""
    } else {
      adierazlea.taul$sexua <- ifelse(grepl("Gizon.",  adierazlea.taul$zutabea),"Gizonezkoak","Emakumezkoak")
      adierazlea.taul$aldagaia  <- sub(".*?\\b([1-2][0-9][0-9][0-9])\\b.*", "\\1", adierazlea.taul$zutabea)
      adierazlea.taul$mota <- ifelse(grepl("%",  adierazlea.taul$zutabea),"ehunekoa","kopurua")
    }

    sexuak <- unique(adierazlea.taul$sexua)
    motak <- unique(adierazlea.taul$mota)


    for (sexua in sexuak)
    { for (mota in motak)
    {
      sexuka.taul              <- adierazlea.taul[adierazlea.taul$sexua==sexua & adierazlea.taul$mota==mota,]
      sexuka.taul <- sexuka.taul[!duplicated(sexuka.taul[c("Lurralde kodea",  "aldagaia")]),]
      sexuka.taul              <- sexuka.taul %>% select(-zutabea, -sexua, -mota) %>% spread(aldagaia, balioa)
      sexuka.taul$izena        <- adierazlea.izena
      sexuka.taul$multzoa.id   <- adierazlea$multzoa.id
      sexuka.taul$gaia         <- adierazlea$gaia
      sexuka.taul$azpitaldea   <- adierazlea$azpitaldea
      sexuka.taul$etiketak     <- adierazlea$tags
      sexuka.taul$zonifikazioa <- sexuka.taul$zonifikazioa
      sexuka.taul$urtea        <- urtea

      adierazlea.moldatua.fitx  <- paste0( gaindegia, adierazlea$zonifikazioa
                                         , "/","gaindegia_", adierazlea$zonifikazioa,"_"
                                         , gsub("\\/| ", "_", tolower(adierazlea.izena)),"-"
                                         , sexua,"-",mota,"-",urtea, ".", "csv")

      write.csv(sexuka.taul, adierazlea.moldatua.fitx, row.names=F )
      print(adierazlea.moldatua.fitx)

    }
    }


  } else if (adinka)
  { print("adinka")
    zuthasi <- ifelse (adierazlea$zonifikazioa == "udalerrika", 5, 4)
    adierazlea.taul <- adierazlea.taul %>% gather(adina, balioa, zuthasi:ncol(adierazlea.taul)) %>% as.data.frame()

    if (sum(grepl("%",adierazlea.taul$adina)))
    { adinka.taul.ehun  <- adierazlea.taul %>% filter(grepl("%",adina)) %>% spread(adina, balioa)
      adinka.taul.ehun$izena        <- adierazlea.izena
      adinka.taul.ehun$multzoa.id   <- adierazlea$multzoa.id
      adinka.taul.ehun$gaia         <- adierazlea$gaia
      adinka.taul.ehun$azpitaldea   <- adierazlea$azpitaldea
      adinka.taul.ehun$etiketak     <- adierazlea$tags
      adinka.taul.ehun$zonifikazioa <- adierazlea$zonifikazioa
      adinka.taul.ehun$urtea        <- urtea

      adierazlea.moldatua.fitx  <- paste0( gaindegia, adierazlea$zonifikazioa
                                         , "/","gaindegia_", adierazlea$zonifikazioa,"_"
                                         , gsub("\\/| ", "_", tolower(adierazlea.izena)),"-"
                                         ,"-ehunekoak-",urtea, ".", "csv")
      write.csv(adinka.taul.ehun, adierazlea.moldatua.fitx, row.names=F )
      print(adierazlea.moldatua.fitx)

      adinka.taul.kop  <- adierazlea.taul %>% filter(!grepl("%",adina)) %>% spread(adina, balioa)
    }
    else
    { adinka.taul.kop  <- adierazlea.taul %>% spread(adina, balioa)
    }

    adinka.taul.kop$izena        <- adierazlea.izena
    adinka.taul.kop$multzoa.id   <- adierazlea$multzoa.id
    adinka.taul.kop$gaia         <- adierazlea$gaia
    adinka.taul.kop$azpitaldea   <- adierazlea$azpitaldea
    adinka.taul.kop$etiketak     <- adierazlea$tags
    adinka.taul.kop$zonifikazioa <- adierazlea$zonifikazioa
    adinka.taul.kop$urtea        <- urtea

    adierazlea.moldatua.fitx  <- paste0( gaindegia, adierazlea$zonifikazioa
                                        , "/","gaindegia_", adierazlea$zonifikazioa,"_"
                                         , gsub("\\/| ", "_", tolower(adierazlea.izena)),"-"
                                        ,"-kopurua-",urtea, ".", "csv")
    write.csv(adinka.taul.kop, adierazlea.moldatua.fitx, row.names=F )
    print(adierazlea.moldatua.fitx)

  } else if (jardueraka)
  { zuthasi <- ifelse (adierazlea$zonifikazioa == "udalerrika", 5, 4)
    adierazlea.taul <- adierazlea.taul %>% gather(zutabea, balioa, zuthasi:ncol(adierazlea.taul)) %>% as.data.frame()

    adierazlea.taul$jarduera  <- str_trim(sub("\\(%\\)", "", adierazlea.taul$zutabea))


    if (sum(grepl("%",adierazlea.taul$zutabea)))
    { jardueraka.taul.ehun  <- adierazlea.taul %>% filter(grepl("%",zutabea)) %>%
                                select(-zutabea) %>% spread(jarduera, balioa)
      jardueraka.taul.ehun$izena        <- adierazlea.izena
      jardueraka.taul.ehun$multzoa.id   <- adierazlea$multzoa.id
      jardueraka.taul.ehun$gaia         <- adierazlea$gaia
      jardueraka.taul.ehun$azpitaldea   <- adierazlea$azpitaldea
      jardueraka.taul.ehun$etiketak     <- adierazlea$tags
      jardueraka.taul.ehun$zonifikazioa <- adierazlea$zonifikazioa
      jardueraka.taul.ehun$urtea        <- urtea

      adierazlea.moldatua.fitx  <- paste0( gaindegia, adierazlea$zonifikazioa
                                         , "/","gaindegia_", adierazlea$zonifikazioa,"_"
                                         , gsub("\\/| ", "_", tolower(adierazlea.izena)),"-"
                                         ,"-ehunekoak-",urtea, ".", "csv")
      write.csv(jardueraka.taul.ehun, adierazlea.moldatua.fitx, row.names=F )
      print(adierazlea.moldatua.fitx)

      jardueraka.taul.kop  <- adierazlea.taul %>% filter(!grepl("%",zutabea)) %>%
                                select(-zutabea) %>% spread(jarduera, balioa)
    }
    else
    { jardueraka.taul.kop  <- adierazlea.taul %>% select(-zutabea) %>% spread(jarduera, balioa)
    }

    jardueraka.taul.kop$izena        <- adierazlea.izena
    jardueraka.taul.kop$multzoa.id   <- adierazlea$multzoa.id
    jardueraka.taul.kop$gaia         <- adierazlea$gaia
    jardueraka.taul.kop$azpitaldea   <- adierazlea$azpitaldea
    jardueraka.taul.kop$etiketak     <- adierazlea$tags
    jardueraka.taul.kop$zonifikazioa <- adierazlea$zonifikazioa
    jardueraka.taul.kop$urtea        <- urtea

    adierazlea.moldatua.fitx  <- paste0( gaindegia, adierazlea$zonifikazioa
                                        , "/","gaindegia_", adierazlea$zonifikazioa,"_"
                                         , gsub("\\/| ", "_", tolower(adierazlea.izena)),"-"
                                        ,"-kopurua-",urtea, ".", "csv")
    write.csv(jardueraka.taul.kop, adierazlea.moldatua.fitx, row.names=F )
    print(adierazlea.moldatua.fitx)

  }
  else
  { zuthasi <- ifelse (adierazlea$zonifikazioa == "udalerrika", 5, 4)
    if (ilara==9) zuthasi <- 5
    adierazlea.taul <- adierazlea.taul %>% gather(urtea, balioa, zuthasi:ncol(adierazlea.taul)) %>% as.data.frame()
    adierazlea.taul$balioa <- as.numeric(adierazlea.taul$balioa)

    mota.bai <- F
    if (sum(str_trim(gsub("[0-9]*", "",  adierazlea.taul$urtea))=="") != nrow(adierazlea.taul))
    { mota.bai <- T
      adierazlea.taul$mota <- str_trim(gsub("[0-9]*", "",  adierazlea.taul$urtea))
    }

    if (sum(grepl(".*?\\b([1-2][0-9][0-9][0-9])\\b.*", adierazlea.taul$urtea)))
    { adierazlea.taul$urtea  <- sub(".*?\\b([1-2][0-9][0-9][0-9])\\b.*", "\\1", adierazlea.taul$urtea)
    } else
    { adierazlea.taul$urtea  <- urtea
    }

    if (mota.bai)
    { motak <- unique(adierazlea.taul$mota)
      { taldeka.taul.ehun  <- adierazlea.taul %>% spread(urtea, balioa) #%>% select(-mota)
        taldeka.taul.ehun$izena        <- adierazlea.izena
        taldeka.taul.ehun$multzoa.id   <- adierazlea$multzoa.id
        taldeka.taul.ehun$gaia         <- adierazlea$gaia
        taldeka.taul.ehun$azpitaldea   <- adierazlea$azpitaldea
        taldeka.taul.ehun$etiketak     <- adierazlea$tags
        taldeka.taul.ehun$zonifikazioa <- adierazlea$zonifikazioa
        taldeka.taul.ehun$urtea        <- urtea

        adierazlea.moldatua.fitx  <- paste0( gaindegia, adierazlea$zonifikazioa
                                         , "/","gaindegia_", adierazlea$zonifikazioa,"_"
                                         , gsub("\\/| ", "_", tolower(adierazlea.izena)),"-"
                                         ,"-",urtea, ".", "csv")
        write.csv(taldeka.taul.ehun, adierazlea.moldatua.fitx, row.names=F )
        print(adierazlea.moldatua.fitx)
      }

    }
    else
    { taldeka.taul.kop  <- adierazlea.taul %>% spread(urtea, balioa)
      taldeka.taul.kop$izena        <- adierazlea.izena
      taldeka.taul.kop$multzoa.id   <- adierazlea$multzoa.id
      taldeka.taul.kop$gaia         <- adierazlea$gaia
      taldeka.taul.kop$azpitaldea   <- adierazlea$azpitaldea
      taldeka.taul.kop$etiketak     <- adierazlea$tags
      taldeka.taul.kop$zonifikazioa <- adierazlea$zonifikazioa
      taldeka.taul.kop$urtea        <- urtea

      adierazlea.moldatua.fitx  <- paste0( gaindegia, adierazlea$zonifikazioa
                                         , "/","gaindegia_", adierazlea$zonifikazioa,"_"
                                         , gsub("\\/| ", "_", tolower(adierazlea.izena)),"-"
                                         ,"-kopurua-",urtea, ".", "csv")
      write.csv(taldeka.taul.kop, adierazlea.moldatua.fitx, row.names=F )
      print(adierazlea.moldatua.fitx)
    }

  }

  }
}


#adierazleak

#length(package$resources)
#adierazle.helb <- package$resources[[1]]$url

#user_list()
#group_list(as = 'table')
#tag_list(as = 'table')
#organization_list()
#resource_search(q = 'berrikuntza', limit = 2)

#x <- package_search(q = 'eskualde', rows = 5)
