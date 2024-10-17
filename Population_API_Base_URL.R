# ====================== #
# === URL GENERATION === #
# ====================== #

# --- Script by Paulo Icaro --- #

# Base URL - Version 3.0.0: https://servicodados.ibge.gov.br/api/v3/agregados
# See this API website: https://servicodados.ibge.gov.br/api/docs/agregados?versao=3
# Example query to acess Resident Population: ibge_url(4709, 2022, 93)


# ----------------- #
# --- Libraries --- #
# ----------------- #
library(devtools)             # Import scripts from github
library(svDialogs)            # Library for displaying message boxes



# ------------------------------- #
# --- URL Generation Function --- #
# ------------------------------- #
ibge_url_agreggated = function(series, period, variables, type = 'manual', locality = NULL){
  
  if(type == 'form'){
    # Script to insert the geographic level of the query
    source('https://raw.githubusercontent.com/paulo-icaro/Brazil_Population_Census/main/Population_API_Specific_URL.R')
  } else if (type == 'ask') {
    message('Voce optou por informar manualmente a URL. Seguem alguns exemplos:\n* Pais: ?localidades=N1[all]\n* Regiao: ?localidades=N2[1,2]\n* Estado: ?localidades=N3[11, 12]\n* Municipios: ?localidades=N6[2300101,2300150]\n* Obs: caso deseje selecionar todas as opcoes disponiveis digite all no lugar dos itens')
    Sys.sleep(1.5)
    locality_url = readline(prompt = 'Insira manualmente o trecho referente a localidade:')
  } else if (type == 'manual' || is.null(locality)){
    message('Voce optou por informar manualmente a URL. Seguem alguns exemplos:\n* Pais: ?localidades=N1[all]\n* Regiao: ?localidades=N2[1,2]\n* Estado: ?localidades=N3[11, 12]\n* Municipios: ?localidades=N6[2300101,2300150]\n* Obs: caso deseje selecionar todas as opcoes disponiveis digite all no lugar dos itens')
    Sys.sleep(1.5)
    locality_url = locality
  } else {
    message('Você não informou como deseja informar a localidade. Tente novamente !')
  }
  
  # URL creation
  url = paste0('https://servicodados.ibge.gov.br/api/v3/agregados', '/', series, '/periodos/', period, '/variaveis/', variables, locality_url) 
  
  # Return output
  return(url)
  
  # Cleasing
  rm(locality_url)
}