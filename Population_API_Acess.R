# ============================================================================================================ #
# === ACCESSING BRAZIL POPULATION DATA THROUGH BRAZILIAN INSTITUTE OF GEOGRAPHIC AND STATISTICS (IBGE) API === #
# ============================================================================================================ #

# --- Script by Paulo Icaro --- #


# ----------------- #
# --- Libraries --- #
# ----------------- #
library(httr2)                # API Connection (see https://httr2.r-lib.org/)
library(jsonlite)             # Convert Json data to an object
library(svDialogs)            # Library for displaying message boxes



# ----------------------------------------- #
# --- Funcao de Coleta de Ddados da API --- #
# ----------------------------------------- #
ibge_population_api = function(url, message = TRUE){
  if(message == TRUE){
    message('Iniciando a conexao com a API')
    Sys.sleep(1)
  }
  
  # --- Conexao com a API --- #
  api_connection = try(expr = request(base_url = url) %>% req_perform(), silent = TRUE)
  tries = 1
  
  if(class(api_connection) == 'try-error'){
    while(class(api_connection) == 'try-error' & tries <= 5){
      if(message == TRUE){
        message('Problemas na conexao. Tentando acessar a API novamente ...\n')
        Sys.sleep(0.7)
      }
      api_connection = try(expr = request(base_url = url) %>% req_perform(), silent = TRUE)
      tries = tries + 1
      if(tries > 5){dlg_message(message = 'Conexao mal sucedida ! \nTente conectar com a API mais tarde.', type = 'ok')}
    }
  } else {
    if(message == TRUE){
      message(message = 'Conexao bem sucedida ! \nDados sendo coletados ...\n')
      Sys.sleep(0.7)
    }
  }
  
  api_connection = rawToChar(api_connection$body)                 # Raw to Json
  api_connection = fromJSON(api_connection, flatten = TRUE)       # Json to Data Frame
  
  # --- Output --- #
  return(api_connection)
}