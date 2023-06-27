# sumary para tdas as variaveis 

grafico_geral <- function(var, tt = '', subt= '', lg_x = '', lg_y = '') {
  dados %>% 
    ggplot(aes_string(x = as.factor(reorder(as.factor(var),as.factor(var),function(x)-length(x))), fill = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) + 
    scale_y_continuous(labels=scales::percent) +
    geom_text(aes(y = ((..count..)/sum(..count..)),
                  label =  scales::percent((..count..)/sum(..count..))),
              size = 3,
              stat = "count", vjust = -0.1) +
    labs(x = lg_x,
         y = lg_y,
         title = tt,
         subtitle = subt) +
    theme(axis.title = element_text(size = 10),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(face = "italic"),
          legend.position = "none") +
    scale_fill_grey()
}
grafico_geral(dados$diagnosis)

#################################
# TABELA DE MEDIDAS DESCRITIVAS
#################################

#função para colocar em uma tabela as principais medidas descritivas
medidas.teste = function(v1, vari){
  #dados = tibble(v1, v2) #colocando os dados em um tibble
  teste = dados %>% 
    #group_by(v2) %>% 
    summarise( Mínimo = min(v1), Máximo = max(v1), 
               Média = mean(v1), 
               Mediana = round(median(v1), digits = 4), 
               Variância = var(v1), 
               Desvio_padrão = sd(v1)) %>% 
    mutate(Característica = vari) %>% #para criar a variável "Característica"
    #rename(Grupo = v2) %>% #renomear coluna "v2" para "Grupo"
    dplyr::select(Característica, everything()) #para colocar em primeiro a coluna da variável "Característica"
  #e depois todas as demais colunas
  
  return(teste)
}


tabela = bind_rows(medidas.teste(na.omit(radius_mean), names(dados[2])),
                   medidas.teste(na.omit(texture_mean), names(dados[3])))
for (cl in 2:length(dados)) {
  t2 =  bind_rows(medidas.teste(na.omit(as.numeric(unlist(dados[cl]))), names(dados[cl])))
  tabela = full_join(tabela, t2)
  
}

options(knitr.table.format = "latex")

kbl(tabela, caption = "Medidas descritivas para variáveis antes do tratamento", booktabs = TRUE) %>%
  kable_paper("striped", "hover",full_width = T) %>%
  row_spec(0, bold = T, background = '#EDE2DB')%>%
  add_header_above(c("Antes do tratamento" = 7), bold =  T) %>%
  kable_styling(latex_options = "hold_position")
dados$diagnosis = factor(x=dados$diagnosis, labels= c('Maligno', 'Benigno'))
table(dados$diagnosis)
??as.factor


##################################################################################




 numericas = c('price', 'city_fuel_economy','listed_date',
  'owner_count', 'mileage', 'savings_amount',
  'seller_rating', 'horsepower', 'highway_fuel_economy',
  'maximum_seating',  'year', 'combine_fuel_economy',
  'back_legroom','bed_height','front_legroom',
  'height', 'wheelbase', 'width', 'length',
  'engine_displacement', 'fuel_tank_volume',
  
  # extras
  'daysonmarket', 'engine_type','latitude','longitude')
  
  # char
char = c('city', 'bed', 'cabin','engine_cylinders',
  
  #dummy
  'body_type', #'city', já contei
  'exterior_color', 'franchise_make',
  'fuel_type', 'interior_color', 'listing_color', 'major_options', 'make_name',
  'model_name', 'power', 'torque', 'transmission', 'transmission_display',
  'wheel_system', 'wheel_system_display')

binaria = c(
  # binaria
  'franchise_dealer', 'fleet', 'frame_damaged',
  'has_accidents','isCab','is_certified', 
  'is_cpo','is_new','is_oemcpo', 'salvage', 'theft_title')
  
 insuficiente = c( # para excluir
  # TALVEZ: cabin
  'vin', #'bed', já contei
  #'bed_height', já contei
  'bed_length',# 'combine_fuel_economy',já contei
  'dealer_zip', 'sp_id', 'sp_name', 'listing_id',
  'trimId', 'trim_name', 'vehicle_damage_category',
  # SÓ PARA TESTAR
  'interior_color', 'listing_color', 'major_options', 'make_name',
  ' model_name', ' power', 'torque', 'transmission', 'transmission_display', 
  ' wheel_system', ' wheel_system_display')

 
 variaveis_dados = tibble( 'Numericas' = numericas, 'Categóricas' = char)
 
 ?tibble
 
 2
 3
 
teste = base_nova %>%
   mutate(price = ifelse(or(price > quantile(price, 0.9999), price<= 0), mean(price), price))
 
min(teste$price)
 boxplot(base_nova$price, 
         main="Boxplot of Used Car Mileage", 
         ylab="Preço($)")
mean(base_nova$price)
mean(teste$price)

median(base_nova$price)

median(teste$price)
hist(teste$price, 
         main="Boxplot of Used Car Mileage", 
         ylab="Preço($)")
 
plot(base_nova$price, base_nova$seller_rating)

quantile(base_nova$price, 0.9999)

#install.packages('corr')
library(corrr)

sp <- 
  dados %>% 
  corrr::correlate(method = "spearman") %>% 
  stretch()

# Like Figure B
sp %>% 
  ggplot(aes(x, y, fill = r)) +
  geom_tile()

library(ggcorrplot)
#install.packages('ggcorrplot')
ggcorrplot::ggcorrplot(cor(dados))


var = names(dados)
length(table(dados$is_cpo))
for (i in 2:ncol(dados)) {
  if(length(table(dados[var[i]])) >2){
    print(var[i])
    plot(dados['price'], dados[var[i]])
    
  }
}

plot(dados$price, dados$year)

a = names(dados)
b = names(dados2)
setdiff(a, b)
