# =========================================================== #
# === DATA COLLECTION OF POPULATION DATA THROUGH IBGE API === #
# =========================================================== #

# --- Script by Paulo Icaro --- #


# ----------------- #
# --- Libraries --- #
# ----------------- #
library(devtools)             # Import scripts from github
source('https://raw.githubusercontent.com/paulo-icaro/Brazil_Population_Census/main/Population_API_Base_URL.R')
source('https://raw.githubusercontent.com/paulo-icaro/Brazil_Population_Census/main/Population_API_Acess.R')




# ----------------------------- #
# --- Query Population Data --- #
# ----------------------------- #
population = ibge_population_api(url = ibge_url_agreggated(series = 4709,period = 2022, variables = 93, locality = 'form'), message = TRUE)[[4]][[1]][[2]][[1]]