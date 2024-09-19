library(ggplot2)
library(tidytext)
library(magick)
library(dplyr)
library(stringr)
library(viridis)      

Datos = read.csv("seleccion.csv")

questions = names(Datos)
df = Datos[c(3,4,5,7,8)]

################################################################################
############################ Map Countries #####################################
################################################################################

world = map_data("world")
df_countries = df %>% count(pais = pais)
df_genero = df %>% count(n_genero = genero)
df_educacion = df %>% count(n_educacion = educacion)
df$area_profesional[df$area_profesional == 'E learning'] = 'Ciencia de datos'
df_area = df %>% count(n_area = area_profesional)
df_edad = df %>% count(n_edad = edad)

# change names to english to merge with maps

df_countries$pais[df_countries$pais == 'Reino Unido'] = 'UK' 
df_countries$pais[df_countries$pais == 'Brasil'] = 'Brazil'
df_countries$pais[df_countries$pais == 'Espana'] = 'Spain'
df_countries$pais[df_countries$pais == 'Estados Unidos'] = 'USA'
df_countries$pais[df_countries$pais == 'Filipinas'] = 'Philippines'
df_countries$pais[df_countries$pais == 'Francia'] = 'France'
df_countries$pais[df_countries$pais == 'Suiza'] = 'Switzerland'

df_countries$n = as.factor(df_countries$n)

map = world %>%
  merge(df_countries, by.x = "region", by.y = "pais", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) + geom_polygon() +
  theme_void() + theme(legend.position = 'bottom') +
  scale_fill_brewer(palette="Set2", na.value="grey") + 
  labs( fill = "",labels = "" )

################## Edad, genero, educacion y area profesional ##################

colorsC = c('#6D2D75','#990099','#B266ff','#C090B0')

genero_plot =  ggplot(df_genero, aes(x = "", y = n, fill = n_genero)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    geom_text(aes(label = paste0(n)), color ='white', 
            position = position_stack(vjust = 0.5))  +
    coord_polar("y", start = 0)+
    ggpubr::fill_palette("jco")+
    theme_void() + 
    scale_fill_manual(values = colorsC) +labs( fill = "Género",labels = "" )


educacion_plot =  ggplot(df_educacion, aes(x = "", y = n, fill = n_educacion)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(label = paste0(n)), color ='white', 
            position = position_stack(vjust = 0.5))  +
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("jco")+
  theme_void() + 
  scale_fill_manual(values = colorsC) +labs( fill = "Educación",labels = "" )

colorsC = 
  c('#6666FF','#99FFCC','#990099','#B266ff','#C090B0','#6D2D75')

area_plot =  ggplot(df_area, aes(x = "", y = n, fill = n_area)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(label = paste0(n)), color ='white', 
            position = position_stack(vjust = 0.5))  +
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("jco")+
  theme_void() + 
  scale_fill_manual(values = colorsC) +
  labs( fill = "Área profesional",labels = "" )


edad_plot = ggplot(data = df_edad, aes(x=n_edad, y=n)) +
  geom_bar(stat="identity",fill='#B266ff') +theme_classic() + 
  theme(legend.position = 'none') +
  scale_fill_viridis(option='magma',direction=-1)  + labs(x = "Edad", y = '')  
  

###############################################################################
#################################### Final Plot ###############################
###############################################################################

void_p = ggplot()+ theme_void() 


plot_figura1 = gridExtra::grid.arrange(edad_plot,educacion_plot,area_plot,genero_plot,map,
                                     ncol = 2, nrow = 6, 
                                     layout_matrix = rbind(c(1,1,2,2,3,3), 
                                                           c(4,4,5,5,5,5)))



ggsave("plot_figura1.pdf", plot_figura1, width = 10, height = 7)

