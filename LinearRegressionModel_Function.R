#
# TRABALHO FINAL
#
##############
# Nome: Caroline Ferreira da Cruz Gomes
# Matrícula: 119054035

preco_carros_usados <- function(data) {
  ############################
  # TRATAMENTO DE DADOS
  ############################

  if (!require("expss")) install.packages("expss")
  if (!require("readr")) install.packages("readr")
  if (!require("dplyr")) install.packages("dplyr")
  if (!require("stringr")) install.packages("stringr")
  if (!require("gbm")) install.packages("gbm")
  
  tratamento_numericas_1 = function(x){
    
    x = ifelse(nchar(x <7), parse_number(x, locale = locale(decimal_mark = ".")), 0)
    x = ifelse(is.na(x), 0, x) 
  }
  
  tipo_char = function(x){
    x= ifelse(or(nchar(x) > 16, # grande demais
                 nchar(x) < 1, # pequeno demais
                 str_sub(x, 1, 1) == " ",# começando com espaço
                 str_sub(x, 1, 1) == "'",# começando com '
                 str_sub(x, 1, 1) == "*",# começando com '
                 x == 'False',
                 x == 'True',
                 x == 'WHITE',
                 x == 'Gray',
                 x == 'GRAY',
                 x == 'Gasoline',
                 x == 'PT Cruiser',
                 #grepl('*',x),
                 !is.na(parse_number(x, locale = locale(decimal_mark = ".")))), # é numerico
              "", x)
  }
  
  tipo_boolean = function(x){
    x = ifelse(and(nchar(x) <= 5, or( x == "True", x == "False")
    ), x, 0)
    x = ifelse(x == 'True', 1, 0)
  }

  base_nova = data %>%
    # Numericas
    mutate_at(c('price', 'city_fuel_economy','listed_date',
                'owner_count', 'mileage', 'savings_amount',
                'seller_rating', 'horsepower', 'highway_fuel_economy',
                'maximum_seating', 'combine_fuel_economy',
                'back_legroom','bed_height','front_legroom',
                'height', 'wheelbase', 'width', 'length',
                'engine_displacement', 'fuel_tank_volume'), tratamento_numericas_1)%>%
    
    mutate_at(c('city', 'body_type', 'franchise_make'), tipo_char)%>%
    
    mutate_at(c('franchise_dealer', 'fleet', 'frame_damaged',
                'has_accidents','isCab','is_certified', 
                'is_cpo','is_new','is_oemcpo', 'salvage',
                'theft_title'),tipo_boolean )%>%
    
    mutate(bed = ifelse(and((bed != 'Short'),(bed != 'Regular'),(bed != 'Long')),"", bed),
           cabin = ifelse(and(nchar(cabin)<15, 
                              str_sub(cabin, -3, -1) == "Cab"), cabin, ""),
           daysonmarket = ifelse(and(nchar(daysonmarket)<4),
                                 parse_number(daysonmarket, locale = locale(decimal_mark = ".")), 0),
           engine_cylinders = ifelse(nchar(engine_cylinders)<3, engine_cylinders, ""),
           engine_type = ifelse(and(nchar(engine_type)<3, nchar(engine_type)>1), engine_type, ""),
           latitude = ifelse(and(nchar(latitude)<15, nchar(latitude)>2, is.na(latitude)),
                             parse_number(latitude, locale = locale(decimal_mark = ".")), 0),
           longitude = ifelse(and(nchar(longitude)<15, nchar(longitude)>2, is.na(longitude)),
                              parse_number(longitude, locale = locale(decimal_mark = ".")), 0),
           exterior_color = ifelse(and(nchar(exterior_color)<18,
                                       nchar(exterior_color)>4,
                                       is.na(parse_number(exterior_color, locale = locale(decimal_mark = ".")))), 
                                   exterior_color, ""),
           fuel_type = ifelse(or(fuel_type == 'Gasoline', fuel_type == 'Hybrid',
                                 fuel_type == 'Electric', 
                                 fuel_type == 'Diesel'), fuel_type, ""),
           year = ifelse( ifelse(nchar(year) == 4, 
                                 parse_number(year, locale = locale(decimal_mark = ".")), NA)>1900,
                          parse_number(year, locale = locale(decimal_mark = ".")),
                          NA))%>%
    mutate(price = ifelse(or(price > quantile(price, 0.999), price<= 0), mean(price), price))
  
  # Removendo colunas
  
  base_nova = base_nova %>%
    filter(!is.na(year))
  
  base_nova = base_nova[, !(names(base_nova) %in% c('vin', 
                                               'bed_length',
                                               'dealer_zip', 'sp_id', 'sp_name', 'listing_id',
                                               'trimId', 'trim_name', 'vehicle_damage_category',
                                               'interior_color', 'listing_color', 'major_options', 'make_name',
                                               ' model_name', ' power', 'torque', 'transmission', 'transmission_display', 
                                               ' wheel_system', ' wheel_system_display', 'franchise_make', 'city'))]
  
  #############################
  # MODELO
  #############################
  
  dados = base_nova %>%
    dplyr::select(# numericas
      price, year, latitude, longitude, 
      city_fuel_economy,daysonmarket,
      owner_count,
      savings_amount,
      seller_rating, horsepower, highway_fuel_economy,
      maximum_seating, combine_fuel_economy,
      back_legroom,bed_height,front_legroom,
      height, wheelbase, width, length,
      engine_displacement, fuel_tank_volume,
      
      # binarias
      'franchise_dealer', 'fleet', 'frame_damaged',
      'has_accidents','isCab','is_certified', 
      'is_cpo','is_new','is_oemcpo', 'salvage',
      'theft_title')
  
  
  ###################################
  # FINAL
  ###################################
  
  
  set.seed(119054035)
  ## Carregando o pre-processamento e o classificador
  load("Caroline_Ferreira_trabalho_final.RData")

  ## Aplicando o pre-processamento
  data_pp <- kernlab::predict(funcao_preProc3, dados) 
  
  ## aplicando o classificador nos dados
  pred <- kernlab::predict(modelFit6, data_pp) 
  
  ## construindo o postResample
  caret::postResample(pred, data_pp$price)
  
  # Retorna os valores estimados para a variavel price
  return(pred)

}

# Insira sua base de dados na função
preco_carros_usados()





