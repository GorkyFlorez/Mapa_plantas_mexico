#------------------------------------------------------------------------
require(pacman)
pacman::p_load(raster, sf, ggplot2, ggspatial, tidyverse, ggrepel, openxlsx,RColorBrewer, ggpubr)
#------------------------------------------------------------------------
Mexico    <- getData('GADM', country='Mexico', level=1) %>% st_as_sf()       # Extracion del paiz
Data      <- read.xlsx("Data excel/Data_mexico.xlsx", sheet="Hoja1") 
Mexico_dat<- cbind(Mexico, Data) 
#------------------------------------------------------------------------
cortes <- c(1402,2765,4195,5013,6900)
A<-ggplot()+
  geom_sf(data= Mexico_dat, aes(fill= Plantas.vasculares), alpha=0.5)+
  scale_fill_distiller(palette   = "YlOrRd", direction = 1,
                     na.value = 'white', breaks = cortes ,
                     labels = c("[1402 - 2764] ","[2765 - 4194]", "[4195 - 5012]", "[5013 - 6899]", "[6900 - 10863]"), 
                     name="Plantas vasculares\n(numero)")+
  guides(fill = guide_legend(title.position = "right",direction = "vertical",
                             title.theme = element_text(angle = 90, size = 12, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Plantas vasculares\n(numero)'))+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position = c(.70, .80))
  
cortes1 <- c(100,200,400,600,800)
summary(Mexico_dat$Hongos)
B <-ggplot()+
  geom_sf(data= Mexico_dat, aes(fill=Hongos), alpha=0.5)+
  scale_fill_distiller(palette   = "YlOrRd", direction = 1,
                       na.value = 'white', breaks = cortes1 ,
                       labels = c("[100- 199] ","[200 - 399]", "[400 - 599]", "[600 - 799]", "[800 - 987]"), 
                       name="Angiospermas\n(numero)")+
  guides(fill = guide_legend(title.position = "right",direction = "vertical",
                             title.theme = element_text(angle = 90, size = 12, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Angiospermas\n(numero)'))+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position = c(.70, .80))
    
F1<- ggarrange(A,B, labels = c("A)","B)"), ncol = 1)
#------------------------------------------------------------------------
ggsave("Mapas exportados/Mapa de plantas de mexico.png", plot = F1, units = "cm", width = 21,height = 29, dpi = 900)



