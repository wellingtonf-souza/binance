library(dplyr)
library(magrittr)
library(httr)
library(jsonlite)
library(purrr)
library(stringr)
library(openssl)
library(lubridate)
library(glue)


KEY = ''
SECRET = ''
BASE_URL = 'https://testnet.binance.vision'


hashing = function(query_string){
  crypto = openssl::sha256(query_string, key = SECRET)
  return(crypto)
}

get_timestamp = function(dt=Sys.time()){
    time_time = as_datetime(dt, tz = "UTC") %>% 
      as.numeric() %>% 
      map_dbl(~.x*1000) %>% 
      round() %>% as.character()
    return(time_time)
}

send_signed_request = function(http_method, url_path, payload=NULL, simplify = TRUE){
    url = modify_url(url = paste(BASE_URL,url_path,sep=''), query = c(payload,list(timestamp = get_timestamp())))
    
    query_string = str_extract(url,pattern = '\\?.*') %>% str_remove('[\\?]')
    
    url = modify_url(url = url, query = list(signature = hashing(query_string)))
    
    headers_default = add_headers('Content-Type'= 'application/json;charset=utf-8', 'X-MBX-APIKEY'= KEY)
    
    if(http_method == 'GET'){
      response = GET(url,    headers_default)
    }else if(http_method == 'POST'){
      response = POST(url,   headers_default,encode = 'json')
    }else if(http_method == 'DELETE'){
      response = DELETE(url, headers_default,encode = 'json')
    }else{
      response = PUT(url,    headers_default,encode = 'json')
    }
    
    if(status_code(response)!=200){
      code = status_code(response)
      stop(glue('API request failed with status code {code}'))
    }
    
    data = jsonlite::fromJSON(content(response,"text"),simplifyVector = simplify)
    return(data)

}

send_public_request = function(url_path, payload = NULL, simplify = TRUE){
    url = paste(BASE_URL,url_path,sep='')
    url = modify_url(url = url, query = payload)
    response = GET(url)
    
    if(status_code(response)!=200){
      code = status_code(response)
      stop(glue('API request failed with status code {code}'))
    }
    
    data = jsonlite::fromJSON(content(response,"text"), simplifyVector = simplify)
    return(data)
}


get_exchangeInfo = function(){
  response = send_public_request('/api/v3/exchangeInfo')
  filters = response$symbols$filters
  names(filters) = response$symbols$symbol
  filters = bind_rows(filters, .id = "symbol")
  response$symbols = response$symbols %>% select(-filters)
  
  response$PRICE_FILTER = filters %>% 
    filter(filterType == 'PRICE_FILTER') %>% 
    select(symbol, filterType, minPrice, maxPrice, tickSize) %>% 
    mutate_at(vars(-filterType,-symbol),as.numeric)
  
  response$PERCENT_PRICE = filters %>% 
    filter(filterType == 'PERCENT_PRICE') %>% 
    select(symbol, filterType, multiplierUp, multiplierDown, avgPriceMins) %>% 
    mutate_at(vars(-filterType,-symbol),as.numeric)
  
  response$LOT_SIZE = filters %>% 
    filter(filterType == 'LOT_SIZE') %>% 
    select(symbol, filterType, minQty, maxQty, stepSize) %>% 
    mutate_at(vars(-filterType,-symbol),as.numeric)
  
  
  response$MIN_NOTIONAL = filters %>% 
    filter(filterType == 'MIN_NOTIONAL') %>% 
    select(symbol, filterType, minNotional, applyToMarket, avgPriceMins)%>% 
    mutate(
      minNotional = as.numeric(minNotional),
      avgPriceMins = as.numeric(avgPriceMins)
    )
  
  response$ICEBERG_PARTS = filters %>% 
    filter(filterType == 'ICEBERG_PARTS') %>% 
    select(symbol, filterType, limit) %>% 
    mutate(limit = as.numeric(limit))
  
  response$MARKET_LOT_SIZE = filters %>% 
    filter(filterType == 'MARKET_LOT_SIZE') %>% 
    select(symbol, filterType, minQty, maxQty, stepSize) %>% 
    mutate_at(vars(-filterType,-symbol),as.numeric)
  
  
  response$MAX_NUM_ORDERS = filters %>% 
    filter(filterType == 'MAX_NUM_ORDERS') %>% 
    select(symbol, filterType, maxNumOrders) %>% 
    mutate(maxNumOrders = as.numeric(maxNumOrders))
  
  
  response$MAX_NUM_ALGO_ORDERS = filters %>% 
    filter(filterType == 'MAX_NUM_ALGO_ORDERS') %>% 
    select(symbol, filterType, maxNumAlgoOrders) %>% 
    mutate(maxNumAlgoOrders = as.numeric(maxNumAlgoOrders))
  
  return(response)
}

get_klines = function(symbol, interval_obj, start, end){
  
  startTime = get_timestamp(start)
  endTime   = get_timestamp(end)
  params = list(
    "symbol"= symbol,
    "interval"= interval_obj,
    "startTime"= startTime,
    "endTime"= endTime,
    "limit"= 1000
  )
  kline = send_public_request('/api/v3/klines', params, simplify = FALSE)
  
  open_time = c()
  open = c()
  high = c()
  low = c()
  close = c()
  volume = c()
  close_time = c()
  quote_asset_volume = c()
  number_of_trades = c()
  taker_buy_base_asset_volume = c()
  taker_buy_quote_asset_volume = c()
  
  for(i in kline){
    open_time = c(open_time, i[[1]])
    open = c(open, i[[2]])
    high = c(high, i[[3]])
    low = c(low, i[[4]])
    close = c(close, i[[5]])
    volume = c(volume, i[[6]])
    close_time = c(close_time, i[[7]])
    quote_asset_volume = c(quote_asset_volume, i[[8]])
    number_of_trades = c(number_of_trades, i[[9]])
    taker_buy_base_asset_volume = c(taker_buy_base_asset_volume, i[[10]])
    taker_buy_quote_asset_volume = c(taker_buy_quote_asset_volume, i[[11]])
  }
  
  df = tibble(
    'open_time' = open_time,
    'open_time_utc' = open_time %>% as.numeric() %>% map_dbl(~.x/1000) %>% lubridate::as_datetime(),
    'open' = open,
    'high' = high,
    'low' = low,
    'close' = close,
    'volume' = volume,
    'close_time' = close_time,
    'close_time_utc' = close_time %>% as.numeric() %>% map_dbl(~.x/1000) %>% lubridate::as_datetime(),
    'quote_asset_volume' = quote_asset_volume,
    'number_of_trades' = number_of_trades,
    'taker_buy_base_asset_volume' = taker_buy_base_asset_volume,
    'taker_buy_quote_asset_volume' = taker_buy_quote_asset_volume
  )
  
  df = df %>% mutate_if(is.character,as.numeric)
  return(df)
}

get_history_klines = function(symbol, interval_obj, start, end){
  unit = list('1h'='hours','12h'='hours','1d'='days')
  dif = interval(as_datetime(start),as_datetime(end)) %>% 
    as.period() %>% 
    as.numeric(unit = unit[[interval_obj]])
  dif = if_else(interval_obj == '12h', dif/12, dif)
  if(dif/990 > 1){
    div = as.integer(dif/990)
    second = interval(as_datetime(start),as_datetime(end)) %>% 
      divide_by(div) %>% 
      int_end()
    interval_between = interval(as_datetime(start),second) %>% 
      as.period() %>% 
      as.numeric(unit = 'seconds')
    vetor_dates = c(as_datetime(start), second)
    while(vetor_dates[length(vetor_dates)]<as_datetime(end)){
      vetor_dates = c(vetor_dates, vetor_dates[length(vetor_dates)] + seconds(interval_between))
    }
    vetor_dates[length(vetor_dates)] = as_datetime(end)
    
    for(i in 1:(length(vetor_dates)-1)){
      if(i==1){
        result = get_klines(symbol, interval_obj, vetor_dates[i], vetor_dates[i+1])
      }else{
        result = bind_rows(result, get_klines(symbol, interval_obj, vetor_dates[i], vetor_dates[i+1]))
      }
    }
    
  }else{
    result = get_klines(symbol, interval_obj, start, end)
  }
  return(result)
}


account = send_signed_request('GET', '/api/v3/account')
account
account$balances

ticker = send_public_request('/api/v3/ticker/24hr')
ticker

ticker = send_public_request('/api/v3/ticker/24hr',list('symbol'='BTCBUSD'))
ticker

send_public_request('/api/v3/ticker/price', list('symbol'='BTCUSDT')) # last price
send_public_request('/api/v3/avgPrice', list('symbol'='BTCUSDT')) # price mean current

exchangeInfo = get_exchangeInfo()
exchangeInfo

# create new order type LIMIT
params = list(
    "symbol"= "BTCUSDT",
    "side"= "SELL",
    "type"= "LIMIT",
    "timeInForce"= "GTC",
    "quantity"= 0.01,
    "price"= 30000
)
order = send_signed_request('POST', '/api/v3/order', params)
order

# view open orders
openOrders = send_signed_request('GET', '/api/v3/openOrders', list('symbol' = 'BTCUSDT'))
openOrders

# cancel open order by orderId
send_signed_request('DELETE', '/api/v3/order', list('symbol' = 'BTCUSDT', orderId = 273814))

# create new order type MARKET
params = list(
  "symbol"= "BTCUSDT",
  "side"= "SELL",
  "type"= "MARKET",
  "quantity"= 0.01
)

send_signed_request('POST', '/api/v3/order', params)

# view trade
params = list(
  "symbol"= "BTCUSDT"
)
response = send_signed_request('GET', '/api/v3/myTrades', params)
response

BASE_URL = 'https://api.binance.com' 

# klines
get_klines(symbol ='BTCUSDT', interval_obj = '1h', start = '2020-01-01 00:00:00',end = '2020-05-01 00:10:00')
get_history_klines(symbol ='BTCUSDT', interval_obj = '1h', start = '2020-01-01 00:00:00',end = '2020-12-01 00:10:00')
