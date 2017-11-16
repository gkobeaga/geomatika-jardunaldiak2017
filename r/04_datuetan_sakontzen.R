library(dplyr)
library(tidyr)

biztanleria.tald <- adierazlea.taula %>%
  select(which(grepl("%",names(.)))) %>%
  scale() %>%
  kmeans(2)

adierazlea.taula$taldea <- biztanleria.tald$cluster

adierazlea.taula    %>%
  filter(taldea==1) %>%
  summary()

g <- g + geom_map(data=adierazlea.taula, map=udalerriak,
                    aes(map_id=`Lurralde kodea`,  fill=factor(taldea)),
                    color="black", size=0.05)
ggsave("../aurkezpena/irudiak/mapa-taldeak.png",height=4.5, width=6)

adierazlea.taula %>%
  gather(adina, balioa, which(grepl("%",names(.)))) %>%
  ggplot(aes(x=adina, y=balioa, colour=factor(taldea), group=taldea)) +
    geom_point(alpha=.3) +
    geom_smooth(alpha=.2, size=1)


########
library(fpc)

adierazlea.esk <-  adierazlea.taula %>%
  select(which(grepl("%",names(.)))) %>%
  scale()

taldeak2 <- pamk(adierazlea.esk,krange=1:5,criterion="asw",critout=TRUE)
taldeak3 <- pamk(adierazlea.esk,krange=1:5,criterion="multiasw",ns=2,critout=TRUE)
plotcluster(adierazlea.esk, taldeak2$pamobject$clustering) 

cluster.stats(dist
              (adierazlea.esk), taldeak2$pamobject$clustering, taldeak3$pamobject$clustering) 
