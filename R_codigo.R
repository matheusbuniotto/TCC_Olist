library('RColorBrewer')
library('tidyverse')
if (!require(scales)) install.packages("scales")
library(lubridate)
library(wordcloud)
library(wordcloud2)
library("tm")
library(patchwork)
library(formattable)
install.packages('corrplot')
library(corrplot)
library(RColorBrewer)
devtools::install_github('bbc/bbplot')
library(ggridges)


pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')


#carregar os dados
produtos <- read_csv("~/TCC/Olist/db_original/olist_products_dataset.csv")
itens <- read_csv("~/TCC/Olist/db_original/olist_order_items_dataset.csv")
pedidos <- read_csv("~/TCC/Olist/db_original/olist_orders_dataset.csv")
clientes <- read.csv("~/TCC/Olist/db_original/olist_customers_dataset.csv")
review <- read_csv("TCC/Olist/db_original/olist_order_reviews_dataset.csv")
sellers <- read_csv("TCC/Olist/db_original/olist_sellers_dataset.csv")



#criar regiões
sudeste <- c('SP', 'MG', 'RJ', 'ES')
sul <- c('RS', 'PR', 'SC')
centro_oeste <- c('GO', 'MT', 'MS', 'DF')
nordeste <- c('MA', 'CE', 'PI', 'BA', 'AL', 'SE', 'RN', 'PB', 'PE')
norte <- c('AC', 'AM', 'RR', 'PA', 'AP', 'TO', 'RO')

# classificar estados em regiões
clientes <- clientes %>% mutate(Regiao = case_when(
  (customer_state %in% sudeste ~ 'Sudeste'),
  (customer_state %in% sul ~ 'Sul'),
  (customer_state %in% norte ~ 'Norte'),
  (customer_state %in% nordeste ~ 'Nordeste'),
  (customer_state %in% centro_oeste ~ 'Centro-Oeste')))

#mesma coisa pra sellers
sellers <- sellers %>% 
  mutate(Regiao = case_when(
  (seller_state %in% sudeste ~ 'Sudeste'),
  (seller_state %in% sul ~ 'Sul'),
  (seller_state %in% norte ~ 'Norte'),
  (seller_state %in% nordeste ~ 'Nordeste'),
  (seller_state %in% centro_oeste ~ 'Centro-Oeste')))


#combinar os datasets (joins)
df_a <- merge(itens, produtos, by = 'product_id')
df_b <- merge(clientes, pedidos, by = 'customer_id')
df <- merge(df_a, df_b)


ggplot(data = df_a ) +
  geom_point(mapping = aes( x = order_id, y = product_name_lenght))


#categorias com maior numero de receia  
save <- df%>% #(12)
  group_by(product_category_name)%>%
  summarise(vol=sum(price))%>%
  mutate(rank=rank(-vol))%>%
  arrange(rank)%>%
  mutate(cum=cumsum(vol))%>%
  mutate(cumper=cum/max(cum))%>%
  ggplot(aes(x=as.factor(rank),y=cumper,fill=rank))+
  geom_col(color="white",size=0.001, alpha=0.9)+
  theme_light()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=7))+
        geom_vline(xintercept = 15,color="red")+
        labs(x="Ranking",y="% Cumulada da Receita")
  


### top 15 categorias
df %>%
  group_by(product_category_name)%>%
  summarise(vol=sum(price))%>%
  mutate(ShareVol=vol/sum(vol))%>%
  filter(vol>80000/0.3)%>%
  ggplot(aes(x=reorder(product_category_name,vol),y=ShareVol))+
  geom_col(aes(fill=vol))+
  theme_minimal()+
  labs(x="",y="% da Receita Total")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="none")+
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
  coord_flip()



#fretes 
frete_produtos <- df %>%
  group_by(product_category_name, Regiao)%>%
  summarise(porcentagem=sum(freight_value)/sum(price),vol=sum(price))%>%
  arrange(desc(porcentagem))%>%
  filter(porcentagem>0.24, Regiao == 'Sudeste')%>%
  ggplot(aes(x=reorder(product_category_name,porcentagem),y=porcentagem))+
  geom_col(aes(fill=vol<80000/0.3))+
  geom_text(aes(label = scales::percent(round((porcentagem),2), colour = "black"), hjust=1.2))+
  coord_flip()+
  bbc_style() + 
  labs(title="Proporção Frete x Valor do Produto", subtitle = 'Região Sudeste', x="",y="Valor do Frete / Valor do produto")+
  theme(legend.position = "none")+  
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
  scale_fill_brewer(name="Produto entre os 15 melhores?",labels=c("Sim","Não"))

  frete_produtos
  

###

cut_quantiles <- function(x, q) {
  x <- sort(x)
  i <- order(x)
  x[i[i >= quantile(i, min(q)) & i <= quantile(i, max(q))]]
}

p3 <- itens %>%
  left_join(produtos) %>%
  filter(!is.na(product_category_name), !is.na(freight_value), freight_value > 0) %>%
  group_by(product_category_name) %>%
  summarise(mean_frete = mean(freight_value), 
            min = first(cut_quantiles(freight_value, c(0.05, 0.95))), 
            max = last(cut_quantiles(freight_value, c(0.05, 0.95))), 
            q = q()) %>%
  ungroup() %>%
  mutate(product_category_name = fct_reorder(product_category_name, mean_frete)) %>%
  ggplot(aes(x = mean_frete, y = product_category_name)) + 
  geom_point(aes(size = q), alpha = 0.8) + 
  geom_errorbarh(aes(xmin = min, xmax = max), alpha = 0.3)
#  labs(x = "Preço médio dos produtos\n(e os limites de 95% dos preços)", y = "Categoria do produto", 
#     title = "Quais os preços médios dos produtos de cada categoria à venda na Olist?", 
#    subtitle = "Dados de 100 mil pedidos entre 2017 e 2018") 
 


#facet dos estados
ggplot(data = df) +
  geom_boxplot(mapping = aes(x = freight_value , fill = Regiao)) +
  facet_wrap(~ Regiao)

#facet review por região vs valor do frete
save <- 
  df %>%
  left_join(review) %>%
  ggplot(aes(x = freight_value, y = Regiao, fill = Regiao)) + 
  geom_density_ridges()+
  coord_cartesian(xlim = c(0, 120))+
  geom_vline(xintercept = mean((na.omit(df$freight_value))),color="red")

mean((na.omit(df$freight_value)))

  



## peso
df %>%
  filter(!is.na(product_weight_g), product_weight_g > 0, product_weight_g < 30000) %>%
  ggplot(aes(y = product_weight_g, color = Regiao)) + 
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
  labs(y = "Peso do produto em gramas")+ 
  facet_grid(. ~ Regiao)
  
## peso x frete
df %>%
  filter(!is.na(product_weight_g), product_weight_g > 0, product_weight_g < 30000,(!is.na(freight_value))) %>%
  ggplot(aes(x = freight_value, y = product_weight_g, color = Regiao)) + 
  geom_point()+
  bbc_style()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(y = "Peso do produto em gramas", x= "Valor do Frete")+ 
  facet_grid( Regiao ~. )



#sellers
p1 <- ggplot(data = sellers, aes(x=Regiao, fill =Regiao))+
  geom_bar()+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, colour = "black")+
  theme(axis.title.y=element_blank(),
       axis.text.y=element_blank(),
       axis.ticks.y=element_blank(),
       plot.title = element_text(hjust = 0.5),
       legend.position ="none")+
  labs(y = "Vendedores", x = "")+
  ggtitle('Vendedores por Região')


#compradores
p2 <- ggplot(data = clientes, aes(x=Regiao, fill =Regiao))+  geom_bar()+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, colour = "black")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position ="none")+
        labs(y = "Vendedores", x = "")+
        ggtitle('Clientes por Região')


ggplot()+
  geom_bar(data = clientes, aes(x=Regiao), fill = 'lightgreen')+
  geom_bar(data = sellers, aes(x=Regiao), width = 0.5, fill = 'lightblue')+
  facet_grid(Regiao ~.)

  
  
  
  
p1 + p2
  
#clientes %
plot_clientes <-ggplot(data = clientes, aes(x=Regiao, fill =Regiao))+  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),3)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25)+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position ="none")+
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0, 1))+
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
  ggtitle('Clientes por Região')

#sellers %
plot_seller <- ggplot(data = sellers, aes(x=Regiao, fill =Regiao))+  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),3)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25)+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position ="none")+
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0, 1))+
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
    ggtitle('Vendedores por Região')

 plot_seller+ plot_clientes 




###frete categoria

itens %>%
  left_join(review) %>%
  filter(!is.na(freight_value), !is.na(review_score)) %>%
  ggplot(aes(x= freight_value))+
  geom_histogram()+
  facet_wrap(review_score ~ .)+
  coord_cartesian(xlim = c(0, 100))
    


#ticket médio por região
df %>%
  filter(!is.na(freight_value), freight_value > 0, price > 0) %>%
  group_by(order_id) %>%
  group_by(Regiao) %>%
  summarise(tm=mean(price), fv=mean(freight_value)) %>%
  ggplot(aes(y=tm, x=fv, color =Regiao))+
  geom_point(size = 5, shape = 'circle')+
  bbc_style()+
  geom_text(aes(label = Regiao, vjust = -1))+
  geom_text(aes(label = round(tm,1), vjust = 2))+
  stat_smooth(method = "lm", col = "red")+
  coord_cartesian(ylim = c(100, 200), xlim = c(17,37))+
  labs(title = 'Ticket Médio por Região') + 
  labs(x = 'Média do Frete', y = "Ticket Médio")+
  theme(legend.position ="none")

#clientes cidade nordeste
clientes_nordeste <- clientes %>%
  filter(Regiao == 'Nordeste') %>%
  group_by(customer_city) %>%
  count() 
  
ggplot(aes(x = customer_city))+  
  geom_bar()+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, colour = "black")+
  labs(y = "Vendedores", x = "")+
  ggtitle('Clientes por Cidade no Nordeste')


clientes_nordeste <-as.data.frame(clientes_nordeste)
clientes_nordeste
  
#clientes da cidades do nordeste
clientes_nordeste %>% 
  filter(n > 150) %>%
ggplot(aes(x= reorder(customer_city, n), y=n, fill = customer_city)) +
  geom_bar(stat='identity')+
  coord_flip()+
  geom_text(aes(label = n), hjust = 1.5, colour = "black")+
  labs(y = "Vendedores", x = "")+
  ggtitle('Clientes por Cidade no Nordeste')



wc_baixo <- review %>%
  filter(review_score < 4) 

wc_baixo <-  na.omit(wc_baixo$review_comment_message)



wc_baixo

docs <- Corpus(VectorSource(wc_baixo))
tm_map(docs, function(x) iconv(enc2utf8(x), sub = "byte"))
tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))

 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

##lower
docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeWords, c("de", "para", "eu", "a", "e", "da", "do", 'que', 'com')) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

dtm

memory.limit(size=8000000)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#dias frete
pedidos$tempo_frete <- time_length(pedidos$order_delivered_customer_date-pedidos$order_delivered_carrier_date, unit="days")

pedidos <- pedidos %>%
  mutate(tempo_frete_ = time_length(pedidos$order_delivered_customer_date-pedidos$order_delivered_carrier_date, unit="days"))

#regiao x dias frete
install.packages('ggridges')
library(ggridges)

pedidos %>%
  left_join(clientes) %>%
  left_join(review) %>%
  left_join(itens) %>%
  ggplot(aes(y=tempo_frete, x= freight_value))+
  geom_density_ridges(alpha = 0.8)
 # facet_wrap(review_score ~ .)

#ridge
f1<- pedidos %>%
  left_join(clientes) %>%
  left_join(review) %>%
  left_join(itens) %>%
ggplot(aes(x = tempo_frete, y = Regiao, fill = Regiao)) +
  geom_density_ridges() +
  theme_ridges() + 
  bbc_style() + 
  labs(title="Tempo médio das entregas",
       subtitle="A região nordeste espera mais")+
  theme(legend.position = "none")+
  coord_cartesian(xlim = c(0, 50))+
  geom_vline(xintercept = mean((na.omit(pedidos$tempo_frete))),color="red")



f2 <- pedidos %>%
  left_join(clientes) %>%
  left_join(review) %>%
  left_join(itens) %>%
  ggplot(aes(x = freight_value, y = Regiao, fill = Regiao)) +
  geom_density_ridges() +
  bbc_style() + 
  labs(title="Tempo médio das entregas",
       subtitle="A região nordeste paga mais")+
  theme(legend.position = "none")+
  coord_cartesian(xlim = c(0, 100))+
  geom_vline(xintercept = mean((na.omit(itens$freight_value))),color="red")

f2

pedidos %>%
  left_join(clientes) %>%
  left_join(review) %>%
  left_join(itens) %>%
  group_by(Regiao) %>%
  summarise(med_review=mean(review_score), med_frete = mean(freight_value))


#tabela frete x review
tab<- pedidos %>%
  left_join(clientes) %>%
  left_join(review) %>%
  left_join(itens) %>%
  group_by(Regiao) %>%
  summarise(med_frete = mean(na.omit(freight_value)),
            med_review=format(round(mean(review_score),2)),
            entrega=format(round(mean(na.omit(tempo_frete))),2))
                                                
            
install.packages("formattable")
customRed = "#ff7f7f"


formattable(tab, list(
  med_frete = color_tile("transparent", "lightpink"),
  med_review =color_tile("lightpink", "transparent"),
  entrega = color_tile("transparent", "lightpink")))



ggsave('save.pdf')
?formattable
            

ggplot(data = clientes)+
  geom_bar(mapping = aes(x=customer_unique_id))


#produtos com frete mais caro
df %>%
  filter(Regiao == "Nordeste") %>%
  group_by(product_category_name)%>%
  summarise(volume=sum(price),avg=mean(freight_value))%>%
  filter(avg>(15/0.3))%>%
  ggplot(aes(x=reorder(product_category_name,avg),y=avg*0.3))+
  geom_col(aes(fill=volume<80000/0.3),color="black",alpha=0.8)+
  theme_minimal()+
  labs(x="",y="Average $ Delivery Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="none")+
  coord_flip()+
  scale_fill_brewer(palette="Set1")



#proporção frete x preço
df %>%
  group_by(product_category_name)%>%
  summarise(ratio=sum(freight_value)/sum(price),vol=sum(price))%>%
  arrange(desc(ratio))%>%
  filter(ratio>0.22)%>%
  ggplot(aes(x=reorder(product_category_name,ratio),y=ratio))+
  geom_col(aes(fill=vol<80000/0.3),color="black")+
  coord_flip()+
  bbc_style() + 
  labs(x="",y="DeliveryFee/OverallValue")+
  scale_y_continuous(labels = percent)+
  scale_fill_brewer(palette="Set1", name="Top 15 Product",labels=c("TRUE","FALSE"))



#teste correlação frete x review
cor<- pedidos %>%
  left_join(clientes) %>%
  left_join(review) %>%
  left_join(itens) %>%
  mutate(fator_frete = freight_value*tempo_frete) %>%
  select(fator_frete, review_score,freight_value,tempo_frete)

cor(cor, use = "complete.obs")

library("ggalt")
library("tidyr")

dumbbell_df <- gapminder %>%
  filter(year == 1967 | year == 2007) %>%
  select(country, year, lifeExp) %>%
  spread(year, lifeExp) %>%
  mutate(gap = `2007` - `1967`) %>%
  arrange(desc(gap)) %>%
  head(10)

#Make plot
ggplot(df, aes(x = ''0'', xend = `200`, y = reorder(freight_value, product_category_name), group = product_category_name)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  bbc_style() + 
  labs(title="We're living longer",
       subtitle="Biggest life expectancy rise, 1967-2007")

f1
