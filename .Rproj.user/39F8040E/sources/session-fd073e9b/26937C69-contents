library(tidyverse)
library(stringr)
library(pdftools)

# Specify the path to the folder containing PDF files
pdf_folder <- "C:/Users/skt/Documents/Marketing-Operation/Invoices/growth_click_send"

# Get a list of all PDF files in the folder
pdf_files <- list.files(path = pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

# Initialize an empty data frame to store the results
results <- data.frame(InvoiceDate = character(),
                      InvoiceNumber = character(),
                      TotalUSD = character(),
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
  
  # Extract Invoice Date using a more robust regex to find the date after "accounts@clicksend.com"
  invoice_date <- stringr::str_extract(cleaned_text, "(?<=accounts@clicksend.com\\s)[0-9]{1,2} [A-Za-z]+ \\d{4}")
  
  # Check if the date extraction worked
  print(paste("Extracted Invoice Date:", invoice_date))
  
  # Extract Invoice Number
  invoice_number <- stringr::str_extract(cleaned_text, "(?<=Invoice Number\\s)\\d+")
  
  # Check if the invoice number extraction worked
  print(paste("Extracted Invoice Number:", invoice_number))
  
  # Extract TOTAL USD using a more robust regex
  total_usd <- stringr::str_extract(cleaned_text, "(?<=TOTAL USD\\s)[0-9,]+\\.\\d{2}")
  
  # Check if the total amount extraction worked
  print(paste("Extracted Total USD:", total_usd))
  
  return(c(invoice_date, invoice_number, total_usd))
}

# Loop through each PDF file and extract the information
for (pdf_file in pdf_files) {
  info <- extract_info(pdf_file)
  results <- rbind(results, data.frame(InvoiceDate = info[1],
                                       InvoiceNumber = info[2],
                                       TotalUSD = info[3],
                                       stringsAsFactors = FALSE))
}

# Print the results
print(results)




write_csv(results, "growth_click_send/invoices.csv")

library(googlesheets4)
library(keyring)

client_secret_path <- keyring::key_get(service = "googlesheets4", username = "client_secret_path")
email <- keyring::key_get(service = "googlesheets4", username = "email")

# Configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)
googlesheets4::sheet_append(results, ss="1SCjAPtbzyUwDjsiPwy5kovKy71ixie0ZasCF-H7NTgk", sheet = "Invoices")
