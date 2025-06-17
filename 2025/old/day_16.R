library(owidapi)
library(data.table)

# life expectancy
le <- owid_get("life-expectancy")
setDT(le)
le_fr <- le[entity_name == "France"]

lit <- owid_get("inequality-adjusted-human-development-index")
setDT(lit)
lit_fr <- lit[entity_name == "France"]
