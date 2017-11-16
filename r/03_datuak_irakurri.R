library(readr)

gaindegia <- "../datuak/gaindegia/"
adierazlea.fitx  <- paste0(gaindegia, "udalerrika/","gaindegia_udalerrika_biztanleria_bost_urteko_adin-taldeen_arabera--ehunekoak-2016.csv")

adierazlea.taula <- read_csv( adierazlea.fitx )

adinka  <- adierazlea.taula[2:4,] %>%
  gather(adina, balioa, which(grepl("%",names(.))))

g <- g + geom_map(data=adinka, map=udalerriak,
                    aes(map_id=`Lurralde kodea`, color=factor(adina), fill=balioa),
                    color="#7f7f7f", size=0.01)

g  <-  g + facet_wrap(~ adina)
