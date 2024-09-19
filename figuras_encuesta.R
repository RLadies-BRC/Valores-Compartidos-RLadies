library(likert)
library(wordcloud2)
library(ggplot2)
library(tidytext)
library(magick)
library(dplyr)
library(stringr)

Datos = readxl::read_excel("Datos_base.xlsx")

questions = names(Datos)
names(Datos) = paste('Q',seq(1,31),sep='')

df = Datos[c(18,19,22,25,27)]

#### Unify responses ####

# Q19
much = paste0('A lot (I feel confident programming, I help,',
              ' and I can teach what I know)')

little = paste0('A little (I can help someone on the topics I know about,',
                ' but I would not give a talk about it)')

same = 'No, everything stayed the same'

q_well = paste0('Quite well (I can solve other people\'s requests, ',
         'and explain different R tools)')

df$Q19[df$Q19==much] = 'mucho'
df$Q19[df$Q19==little] = 'Un poco'
df$Q19[df$Q19==same] = 'igual'
df$Q19[df$Q19=='No, todo sigue igual'] = 'bastante'
df$Q19[df$Q19==q_well] = 'bastante'

df$Q19[df$Q19=='bastante'] = 'Bastante'
df$Q19[df$Q19=='igual'] = 'Igual'
df$Q19[df$Q19=='mucho'] = 'Mucho'

# Q22
df$Q22[df$Q22=='Yes'] = 'Sí'

# Q25
df$Q25[df$Q25=='Yes'] = 'Sí'
df$Q25[df$Q25=='I do not know'] = "No sabe/ No contesta"
df$Q25[df$Q25=='I do not know/No answer'] = "No sabe/ No contesta"
df$Q25[df$Q25=='No sabe'] = "No sabe/ No contesta"
df$Q25[is.na(df$Q25)] = "No sabe/ No contesta"

# Q27
df$Q27[df$Q27=='Yes'] = 'Sí'
df$Q27[is.na(df$Q27)] = "No sabe/ No contesta"

df = data.frame(df)
df$Q18 = as.factor(df$Q18)
df$Q19 = as.factor(df$Q19)
df$Q22 = as.factor(df$Q22)
df$Q25 = as.factor(df$Q25)
df$Q27 = as.factor(df$Q27)

############################## Using ggplot ####################################

# Get percentage

df_q18 = df %>%
  count(Q18) %>% mutate(Percentage = n / sum(n) * 100)  

df_q19 = df %>%
  count(Q19) %>% mutate(Percentage = n / sum(n) * 100)  

df_q22 = df %>%
  count(Q22) %>% mutate(Percentage = n / sum(n) * 100)  

df_q25 = df %>%
  count(Q25) %>% mutate(Percentage = n / sum(n) * 100)  

df_q27 = df %>%
  count(Q27) %>% mutate(Percentage = n / sum(n) * 100)  

# Q18
title18 = str_wrap(questions[18], width = 30)

colorsC = c('#990099','#B266ff','#A0A0A0')

# to avoid long labels 
df_q18$Q18 = str_wrap(df_q18$Q18, width = 10)

q_18 = ggplot(df_q18, aes(x = 1, y = Percentage, fill = Q18)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  scale_x_continuous(breaks = NULL) +  
  scale_fill_manual(values = colorsC)+
  labs( x = NULL, y = "Percentage") +
  theme_minimal() + ylab('')+ggtitle(title18)+
  theme(axis.ticks.x = element_blank(),legend.position = 'right',
        legend.title = element_blank(),plot.title = element_text(size = 12.5),
        axis.text = element_text(size = 12.5))  
  

# Q19

title_q19 = str_wrap(questions[19], width = 40)

colorsC = c('#6D2D75','#990099','#B266ff','#C090B0')

df_q19$Q19 = str_wrap(df_q19$Q19, width = 10)

df_q19$Q19 = factor(df_q19$Q19, levels=c('Igual', 'Un poco', 'Bastante', 'Mucho'))

q_19 = ggplot(df_q19, aes(x = 1, y = Percentage, fill = Q19)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))+
  scale_x_continuous(breaks = NULL) +  
  scale_fill_manual(values = colorsC)+
  labs( x = NULL, y = "Percentage") +
  theme_minimal() + ylab('')+ggtitle(title_q19)+
  theme(axis.ticks.x = element_blank(),legend.position = 'right',
        legend.title = element_blank(),plot.title = element_text(size = 12.5),
        axis.text = element_text(size = 12.5))  


# Q22 

title_q22 = str_wrap(questions[22], width = 40)

colorsC = c('#990099','#6666FF','#A0A0A0')
df_q22$Q22 = str_wrap(df_q22$Q22, width = 10)
df_q22$Q22 = factor(df_q22$Q22, levels = c("Sí", "No", "No sabe/ No contesta"))


q_22 = ggplot(df_q22, aes(x = 1, y = Percentage, fill = Q22)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))+
  scale_x_continuous(breaks = NULL) +  
  scale_fill_manual(values = colorsC)+
  labs( x = NULL, y = "Percentage") +
  theme_minimal() + ylab('')+ggtitle(title_q22)+
  theme(axis.ticks.x = element_blank(),legend.position = 'right',
        legend.title = element_blank(),plot.title = element_text(size = 12.5),
        axis.text = element_text(size = 12.5))  


# Q25 

title3 = paste(substr(questions[25],1,32),'\n',
               substr(questions[25],34,52),'\n',
               substr(questions[25],53,86),'\n',
               substr(questions[25],88,106) )

colorsC = c('#990099','#6666FF','#A0A0A0')

df_q25$Q25 = factor(df_q25$Q25, levels = c("Sí", "No", "No sabe/ No contesta"))



q_25 = ggplot(df_q25, aes(x = 1, y = Percentage, fill = Q25)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))+
  scale_x_continuous(breaks = NULL) +  
  scale_fill_manual(values = colorsC)+
  labs( x = NULL, y = "Percentage") +
  theme_minimal() + ylab('')+ggtitle(title3)+
  theme(axis.ticks.x = element_blank(),legend.position = 'right',
        legend.title = element_blank(),plot.title = element_text(size = 10))  

# Q27 

title4 = paste(substr(questions[27],1,21),'\n',
               substr(questions[27],23,45),'\n',
               substr(questions[27],47,60),'\n',
               substr(questions[27],62,75))

colorsC = c('#990099','#6666FF','#A0A0A0')

df_q27$Q27 = factor(df_q27$Q27, levels = c("Sí", "No", "No sabe/ No contesta"))

q_27 = ggplot(df_q27, aes(x = 1, y = Percentage, fill = Q27)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))+
  scale_x_continuous(breaks = NULL) +  
  scale_fill_manual(values = colorsC)+
  labs( x = NULL, y = "Percentage") +
  theme_minimal() + ylab('')+ggtitle(title4)+
  theme(axis.ticks.x = element_blank(),legend.position = 'bottom',
        legend.title = element_blank(),plot.title = element_text(size = 10))  


final_plot = ggpubr::ggarrange(q_18, q_22,q_25,q_27,
                               labels = c("", "", "", ""), heights = rep(2.3,4),
                               ncol = 2, nrow = 2)


################################################################################
################################ Word cloud ####################################
################################################################################

df.2 = read.csv("resp_textos.csv",sep=",",encoding = "UTF-8")

# 4.8 ¿Qué significa para vos  “R-Ladies” en 3 palabras? ----------

rladies = df.2 %>% select("significado_rladies") %>% slice(1:44)

# convert to upper
rladies = toupper(rladies$significado_rladies)
rladies = as.data.frame(rladies)

# Tokens
rladies = rladies %>% 
  tidytext::unnest_tokens(output = "word",input = "rladies")

# remove trivial words
df.stopwords = tibble(
    word = c("de","del","una","en","se","es", "mi", "no", "a","la","y","lo",
             "largo","volverse","toda","el",'misma','estoy','para'))

rladies = rladies %>% anti_join(df.stopwords)

#count word frequencies 
freq.rladies = sort(table(unlist(strsplit(rladies$word, " "))),      
                    decreasing = TRUE)
freq.rladies = as.data.frame(freq.rladies)

# Using ggwordcloud and ggplot
library(ggwordcloud)

names(freq.rladies) = c('word','freq')

set.seed(1222)
freq.rladies = freq.rladies %>%
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, 
                             prob = c(1, 1, 4, 1, 1)))

set.seed(16)
q_48 = ggplot(freq.rladies, aes(label = word,size = freq, angle = angle, 
                         color = factor(sample.int(6, nrow(freq.rladies),
                                                   replace = TRUE)))) +
  geom_text_wordcloud(eccentricity = 1) +
  scale_radius(range = c(2, 17), limits = c(0, NA)) +
  theme_minimal() +  scale_fill_brewer(palette = "Set1")  

library(grid)
grid.text("Grupa del bien", x = unit(0.5, "npc"), y = unit(0.95, "npc"), 
         gp = gpar(col = "black", fontsize = 16, fontface = "bold"))

###############################################################################
################################ Final Plot ###################################
###############################################################################

void_p = ggplot()+ theme_void() 

#### Plot Rladies

plot_rladies = gridExtra::grid.arrange(q_48,q_22 ,
                                       q_19,void_p,q_18,
                                       ncol = 2, nrow = 7, 
                                       layout_matrix = rbind(c(1,1,1,1,2,2,2), 
                                                             c(3,3,3,4,5,5,5))) 



ggsave("plot_rladies.pdf", plot_rladies, width = 10, height = 10)





