import pdfplumber
import os
import csv
import re
import pandas as pd

# specify the folder containing the PDF files and output CSV file path
folder_path = "/Users/yukachen/marketing-operation/invoices/nov-invoices/SMS Braze"
csv_file_path = "/Users/yukachen/marketing-operation/invoices/nov-invoices/braze_costs.csv"

# open the CSV file in write mode
with open(csv_file_path, mode='w', newline='', encoding='utf-8') as csv_file:
    writer = csv.writer(csv_file)
    # write the header
    writer.writerow(["Invoice", "Date", "Item", "Amount"])
    
    # loop through each PDF file in the folder
    for filename in os.listdir(folder_path):
        if filename.endswith(".pdf"):
            file_path = os.path.join(folder_path, filename)
            
            # open and read each PDF
            with pdfplumber.open(file_path) as pdf:
                text = ""
                for page in pdf.pages:
                    text += page.extract_text() + "\n"

            # extract invoice, date, item, and amount using regex
            invoice = re.search(r"Invoice\s*#(\S+)", text)
            date = re.search(r"Date:\s*(\d{1,2}/\d{1,2}/\d{4})", text)
            item = re.search(r"Item Description\s*(.+?)\s*\d", text, re.DOTALL)  # captures the item description
            amount = re.search(r"Amount Due\s*\$([\d,]+\.\d{2})", text)

            # get the extracted values or set to 'N/A' if not found
            invoice = invoice.group(1) if invoice else "N/A"
            date = date.group(1) if date else "N/A"
            item = item.group(1).strip() if item else "N/A"
            amount = amount.group(1) if amount else "N/A"
            
            # write extracted data to CSV
            writer.writerow([invoice, date, item, amount])

print("Data has been saved to", csv_file_path)




data = pd.read_csv("/Users/yukachen/marketing-operation/invoices/nov-invoices/braze_costs.csv")


