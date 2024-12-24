get_bond_data <- function() {
  # Load necessary libraries
  library(rvest)
  library(dplyr)

  url <- "https://www.worldgovernmentbonds.com/?utm_content=cmp-true"

  # Read the webpage content
  webpage <- read_html(url)

  # Extract the bond table
  bond_table <- webpage %>%
    html_node("table") %>% # Replace "table" with the specific selector for the bond data table
    html_table(fill = TRUE)

  # Remove unnecessary columns
  bond_table <- bond_table[, -c(1, 5)]

  # Rename columns based on the second row and remove the row
  current_colnames <- colnames(bond_table)
  second_row <- as.character(bond_table[1, ])
  new_colnames <- paste(current_colnames, second_row, sep = " ")
  colnames(bond_table) <- new_colnames
  bond_table <- bond_table[-1, ]

  colnames(bond_table)[3] <- "10Y Bond Yield"
  colnames(bond_table)[1] <- "Country"

  # Clean and convert columns to numeric
  bond_table$`10Y Bond Yield` <- as.numeric(gsub("%", "", bond_table$`10Y Bond Yield`)) / 100
  bond_table$`Bank Rate` <- as.numeric(gsub("%", "", bond_table$`Bank Rate`)) / 100
  bond_table$`Spread vs Bund` <- as.numeric(gsub(" bp", "", bond_table$`Spread vs Bund`))
  bond_table$`Spread vs T-Note` <- as.numeric(gsub(" bp", "", bond_table$`Spread vs T-Note`))
  bond_table$`Spread vs Bank Rate` <- as.numeric(gsub(" bp", "", bond_table$`Spread vs Bank Rate`))

  # Return the cleaned data frame
  return(bond_table)
}
