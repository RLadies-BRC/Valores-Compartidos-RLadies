library(ggplot2)
library(magick)
library(dplyr)
library(stringr)
library(png)

setwd('/home/sofia/Rladies/datos_encuesta/')

Datos = readxl::read_excel("Datos_base.xlsx")
Datos_texto = read.csv("resp_textos.csv")

questions = names(Datos)
df = Datos[c(11,25)]
df_pq = Datos_texto[,4]

names(df) = paste('Q',seq(1,2),sep='')

## Q1
df$Q1[df$Q1=='Ns'] = "No sabe/ No contesta"
df$Q1 = as.factor(df$Q1)
df$Q1 = str_wrap(df$Q1, width = 10)
df$Q1 = as.factor(df$Q1)
df$Q1 = factor(df$Q1, levels=c('Si', 'No', 'No\nsabe/ No\ncontesta'))

df_lid1 = df %>%
  count(Q1) %>% mutate(Percentage = n / sum(n) * 100)  

## Q2
df$Q2[df$Q2=='Yes'] = 'Sí'
df$Q2[df$Q2=='No sabe'] = 'No sabe/ No contesta'
df$Q2[df$Q2=='I do not know'] = 'No sabe/ No contesta'
df$Q2[df$Q2=='I do not know/No answer'] = 'No sabe/ No contesta'
df$Q2[is.na(df$Q2)] = 'No sabe/ No contesta'

df$Q2 = as.factor(df$Q2)
df$Q2 = str_wrap(df$Q2, width = 10)
df$Q2 = factor(df$Q2, levels=c('Sí', 'No', 'No\nsabe/ No\ncontesta'))

df_lid2 = df %>%
  count(Q2) %>% mutate(Percentage = n / sum(n) * 100)  


################################# Bar plots ####################################

title1 = str_wrap(questions[11], width = 30)

colorsC = c('#990099','#6666FF','#A0A0A0')

q_lid1 = ggplot(df_lid1, aes(x = 1, y = Percentage, fill = Q1)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  scale_x_continuous(breaks = NULL) +  
  scale_fill_manual(values = colorsC)+
  labs( x = NULL, y = "Percentage") +
  theme_minimal() + ylab('')+ggtitle(title1)+
  theme(axis.ticks.x = element_blank(),legend.position = 'right',
        legend.title = element_blank(),plot.title = element_text(size = 12.5),
        axis.text = element_text(size = 12.5))  


title2 = str_wrap(questions[25], width = 30)

q_lid2 = ggplot(df_lid2, aes(x = 1, y = Percentage, fill = Q2)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  scale_x_continuous(breaks = NULL) +  
  scale_fill_manual(values = colorsC)+
  labs( x = NULL, y = "Percentage") +
  theme_minimal() + ylab('')+ggtitle(title2)+
  theme(axis.ticks.x = element_blank(),legend.position = 'right',
        legend.title = element_blank(),plot.title = element_text(size = 12.5),
        axis.text = element_text(size = 12.5))  

################################# Tabla grafica ################################

table = readPNG("./subfigura3.png")
table_grob = grid::rasterGrob(table, interpolate = TRUE)

################################# Combine plots ################################

void_p = ggplot()+ theme_void() 

join_plots = ggpubr::ggarrange(q_lid1,q_lid2, ncol = 2, 
                       common.legend = TRUE,legend = 'bottom')



ggsave("subfigura3b.pdf", join_plots, width = 10, height = 7)


# gridExtra::grid.arrange(
#   table_grob, join_plots,
#   ncol = 2, nrow = 5,
#   layout_matrix = rbind(c(1,1,1,1),
#                         c(1,1,1,1),
#                         c(1,1,1,1),
#                         c(2,2,2,2))  
# )




