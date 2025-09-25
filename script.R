
library(googlesheets4)

sheet_url <- 'https://docs.google.com/spreadsheets/d/1A59ElYxr4Iq2v2M-PVhhusW3VcBF4bchDlGzzVSLgfg/edit?usp=sharing'
initiatives <- read_sheet(sheet_url)

# Standardizing country names ----

standardize_country_names <- function(country) {
  mapping <- list(
    "tanzania" = "tanzania",
    "republic of tanzania" = "tanzania",
    "western sahara" = "w. sahara",
    "democratic republic of congo" = "dem. rep. congo",
    "somalia" = "somalia",
    "somaliland" = "somaliland",
    "kenya" = "kenya",
    "sudan" = "sudan",
    "chad" = "chad",
    "south africa" = "south africa",
    "lesotho" = "lesotho",
    "zimbabwe" = "zimbabwe",
    "botswana" = "botswana",
    "namibia" = "namibia",
    "senegal" = "senegal",
    "mali" = "mali",
    "mauritania" = "mauritania",
    "benin" = "benin",
    "niger" = "niger",
    "nigeria" = "nigeria",
    "cameroon" = "cameroon",
    "togo" = "togo",
    "ghana" = "ghana",
    "côte d'ivoire" = "côte d'ivoire",
    "ivory coast" = "côte d'ivoire",
    "guinea" = "guinea",
    "guinea bissau" = "guinea-bissau",
    "liberia" = "liberia",
    "sierra leone" = "sierra leone",
    "burkina faso" = "burkina faso",
    "central african republic" = "central african rep.",
    "republic of the congo" = "congo",
    "gabon" = "gabon",
    "equatorial guinea" = "eq. guinea",
    "zambia" = "zambia",
    "malawi" = "malawi",
    "mozambique" = "mozambique",
    "swaziland" = "eswatini",
    "eswatini" = "eswatini",
    "angola" = "angola",
    "burundi" = "burundi",
    "madagascar" = "madagascar",
    "gambia" = "gambia",
    "the gambia" = "gambia",
    "tunisia" = "tunisia",
    "algeria" = "algeria",
    "eritrea" = "eritrea",
    "morocco" = "morocco",
    "egypt" = "egypt",
    "libya" = "libya",
    "ethiopia" = "ethiopia",
    "djibouti" = "djibouti",
    "uganda" = "uganda",
    "rwanda" = "rwanda",
    "south sudan" = "s. sudan"
  )

  country <- tolower(country) # Convert to lowercase
  country <- trimws(country) # Remove leading & trailing spaces
  country <- gsub("\\s+", " ", country) # Remove extra spaces between words
  return(ifelse(country %in% names(mapping), mapping[[country]], country))
}

# Apply standardization to initiatives dataset
initiatives$country <- sapply(initiatives$country, standardize_country_names)

# Check for any remaining mismatches
print(setdiff(initiatives$country, africa$name))
# If it prints "character(0)" you're good, else, update the mapping list with the differences
# the object 'africa' can be loaded from the first line of the server function in app.R






