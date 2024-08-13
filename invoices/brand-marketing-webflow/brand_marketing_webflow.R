library(tidyverse)
library(stringr)
library(pdftools)

# Specify the path to the folder containing PDF files
pdf_folder <- "C:/Users/skt/Documents/Marketing-Operation/Invoices/brand_marketing_webflow"


pdf_files <- list.files(path = pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

# Initialize an empty data frame to store the results
results <- data.frame(InvoiceNumber = character(),
                      Date = character(),
                      AmountDue = character(),
                      stringsAsFactors = FALSE)

# Function to extract information from a single PDF file
extract_info <- function(pdf_file) {
  pdf_text <- pdf_text(pdf_file)
  page_text <- pdf_text[1]
  
  # Print the raw text for debugging
  print(paste("Raw text from PDF file:", pdf_file))
  print(page_text)
  
  # Preprocess the text to remove extra whitespace
  cleaned_text <- stringr::str_squish(page_text)
  
  # Debugging print to show cleaned text
  print(paste("Cleaned text from PDF file:", pdf_file))
  print(cleaned_text)
  
  # Extract Invoice Number
  invoice_number <- stringr::str_extract(cleaned_text, "(?<=Invoice Number\\s)\\d+")
  
  # Check if the invoice number extraction worked
  print(paste("Extracted Invoice Number:", invoice_number))
  
  # Extract Date
  date <- stringr::str_extract(cleaned_text, "(?<=Date\\s)[A-Za-z]+ [0-9]{1,2}(st|nd|rd|th)?,? \\d{4}")
  
  # Check if the date extraction worked
  print(paste("Extracted Date:", date))
  
  # Extract Amount Due
  amount_due <- stringr::str_extract(cleaned_text, "(?<=Amount Due\\s)USD [0-9,]+\\.\\d{2}")
  
  # Clean extracted values
  amount_due <- stringr::str_remove(amount_due, "USD\\s")
  
  # Check if the amount extraction worked
  print(paste("Extracted Amount Due:", amount_due))
  
  return(c(invoice_number, date, amount_due))
}

# Loop through each PDF file and extract the information
for (pdf_file in pdf_files) {
  info <- extract_info(pdf_file)
  results <- rbind(results, data.frame(InvoiceNumber = info[1],
                                       Date = info[2],
                                       AmountDue = info[3],
                                       stringsAsFactors = FALSE))
}

# Print the results
print(results)

# Save the results to a CSV file if needed
write_csv(results, "C:/Users/skt/Documents/Marketing-Operation/Invoices/extracted_webflow_invoices.csv")


results |> 
  mutate(`Company Name` = "Webflow")

library(googlesheets4)
library(keyring)

client_secret_path <- keyring::key_get(service = "googlesheets4", username = "client_secret_path")
email <- keyring::key_get(service = "googlesheets4", username = "email")

# Configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)
googlesheets4::sheet_append(results, ss="1SCjAPtbzyUwDjsiPwy5kovKy71ixie0ZasCF-H7NTgk", sheet = "Invoices")
