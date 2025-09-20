library(tidyverse)
initiatives <- read_csv("initiatives.csv")
# library(janitor)

# Redundant ----

# initiatives <- initiatives %>%
#  slice(9:n()) %>%
#  setNames(as.character(initiatives[9,])) %>%
#  janitor::clean_names()
# initiatives <- initiatives[-1,]

# initiatives <- initiatives %>%
#  rename("name_of_initiative" = "name_of_initiative_organization")

# initiatives_simple <- initiatives %>%
#  select(-c("contact_name", "contact_email"))

# initiatives_clean <- initiatives_simple %>%
#  separate_rows(country, sep = "&|,")

# initiatives <- initiatives %>%
#  mutate(country = case_when(
#    country == "democratic republic of congo" ~ "dem. rep. congo",
#    country == "central african republic" ~ "central african rep.",
#    country == "south sudan" ~ "s. sudan",
#    country == "cote d'ivoire" ~ "côte d'ivoire",
#    TRUE ~ country  # Keep other names unchanged
#  ))

# initiatives <- read_csv("initiatives_clean.csv")
# colnames(initiatives)  # Check available column names

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

initiatives2 <- read_csv("more_initiatives.csv")

initiatives <- bind_rows(initiatives, initiatives2) %>% distinct()






