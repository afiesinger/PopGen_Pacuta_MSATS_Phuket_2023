---
### R Script for the creation of a map of Thailand and Phuket Island created by Dr. Marlene Wall ### 
### maps downloaded from GADM.org; a freely available dataset of the world's maps for academic use ###
### R Script was used to create all maps in Fiesinger et al. (2023): "Population genetic differentiation of the ubiquitous brooding coral Pocillopora acuta along Phuket Island reefs in the Andaman Sea, Thailand" ###

title: "Map of sampling sites"
output: pdf_document

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, warning =FALSE, echo = TRUE}
library(raster)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(ggspatial)
library(ggrepel)
library(stringr)
library(maps)
library(mapdata)
library(kableExtra)
library(officer)

#install.packages('officer')
#install.packages('rvg')
#install.packages('flextable')
#install.packages('rgl')
#install.packages('stargazer')

setwd("")

#install.packages('export_0.2.2.tar.gz')
#require(export)

thailand <- getData("GADM", country = "thailand", level = 1)
th <- fortify(thailand)
myanmar<-getData("GADM", country ="myanmar",level=1)
my<- fortify(myanmar)

world_map <- map_data("world")

## search for coordinates of your required area

x<-c(98.35,98.55,98.55,98.35,98.35) # creates a rectangular of the required area
y<-c(7.75,7.75,7.9,7.9,7.75)

## data = th searches for thailand as specified above

g0 <-ggplot() +
    geom_cartogram(data = th, aes(x = long, y = lat, map_id = id), map = th) +
    geom_cartogram(data = my, aes(x = long, y = lat, map_id = id), map = my) +
    scale_x_continuous(limits=c(97,100),breaks = seq(97, 100, 1)) +
     scale_y_continuous(limits = c(7, 12)) +
  geom_polygon(aes(x=c(98.1, 99.1, 99.1, 98.1, 98.1), y=c(7.6, 7.6, 8.2, 8.2, 7.6)), color="red", alpha=0)+
  annotate("text",x= 99, y= 8.9, label="Thailand", color ="white",size=2.8) +
  annotate("text",x= 99, y= 11.65, label="Myanmar", color ="white",size=2.8,angle=90) +
  theme(axis.title=element_blank(),axis.text=element_text(size=7,family="Helvetica"),panel.background = element_rect(colour = 'black', fill = 'white', size = 0.15, linetype=1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
geom_sf() 
  g0

ggsave("Fig_MapSites.eps", plot = g0, path = NULL,
       scale = 1, dpi = 300) #to save it as different file format
```

```{r, warning =FALSE, echo = TRUE}
g <- ggplot() +
     geom_cartogram(data = th, aes(x = long, y = lat, map_id = id), map = th) +
    scale_x_continuous(limits = c(98.2, 99.1),1) +
     scale_y_continuous(limits = c(7.6, 8.2)) +
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', size = 0.15, linetype=1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
geom_sf()  
g

```


```{r, warning =FALSE, echo = TRUE}

setwd("")

Sites  <- read_excel("x", sheet = "") #list of sites that should be shown and their coordinates
head(Sites)
#knitr::kable(Sites[c(1:11),c(1:5)], booktabs = T,align = "c", caption= "Sample Sites", digits = 3)%>%kable_styling(font_size=10)

```
## create a map without the site names

```{r, warning =FALSE, echo = TRUE}
# g1 <- g +
#     geom_text_repel(data=Sites,aes(y = longitude, x = latitude,               # make sure that the excel file has lat and long named as in this command
#          label = SitePdam),box.padding = 0.4,point.padding = 0.25,size=3,show.legend=FALSE)
# g1

# add points last so they are on top
my_col = c("skyblue", "brown1", "firebrick1", "red3", "darkred", "royalblue", "navy", "darkseagreen", "darkgreen")

g2 <- g + geom_point(data = Sites, aes(y = longitude, x = latitude), colour = my_col, size = 3) + 
  xlab("Longitude") + ylab("Latitude")
g2

ggsave("Fig_MapSites_nonames.eps", plot = g2, path = "",
       scale = 1, dpi = 300)#to save it as different file format

```


```{r, warning =FALSE, echo = TRUE}

# export as (with export package)
graph2ppt(g2,file="Fig_MapSites.pptx")

# export with officer package
ph_with(x = "Fig_MapSites.pptx", value = g2, location = ph_location_fullsize())

doc <- ppt()
# Add a new slide into the ppt document
doc <- addSlide(doc, "Thailand" )
# Add an editable box plot
doc <- addPlot(doc, function() print(g2), vector.graphic = TRUE )
# write the document to a file
writeDoc(doc, file = "Fig1d_mapSites.pptx")

```

```{r, warning =FALSE, echo = TRUE}
graph2ppt(g2,file="Fig_MapSites.pptx")# to save plot as pptx
ggsave("Fig_MapSitesp.eps", plot = g2, path = NULL,
       scale = 1, dpi = 300) #to save it as different file format (.eps can be used in Illustrator)

```

```{r, warning =FALSE, echo = TRUE}
library(cowplot)
gg_inset_map1 = ggdraw() +
  draw_plot(g2) +
  draw_plot(g0, x = 0.595, y = 0.485, width = 0.5, height = 0.5)

gg_inset_map1

```

```{r, warning =FALSE, echo = TRUE}

graph2ppt(gg_inset_map1,file="Fig_MapSites.pptx")
ggsave("Fig_MapSites_a.eps", plot = gg_inset_map1, path = NULL,
       scale = 1, dpi = 300)

```
