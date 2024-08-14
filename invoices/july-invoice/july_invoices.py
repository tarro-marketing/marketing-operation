import pytesseract
from pdf2image import convert_from_path
import pandas as pd
import os
import re

pdf_dir = '/Users/yukachen/marketing-operation/invoices/july-invoice'


dates, invoice_numbers, amounts, company_names = [], [], [], []

def extract_info(text, file_name):
    # Regex patterns for various date formats
    date_pattern = r'\b(\d{1,2}[/-]\d{1,2}[/-]\d{4}|\d{4}[/-]\d{2}[/-]\d{2}|\d{1,2}\s+\w+\s+\d{4})\b'
    
    # Regex for different invoice identifiers, case insensitive
    invoice_pattern = r'(?i)(invoice\s*[:#]?\s*\S+|receipt\s*#?\s*\S+)'
    
    # Updated regex pattern for amounts with optional currency (e.g., USD)
    amount_pattern = r'(?i)(amount due|balance due|\$\(USD\)|net amount \(usd\)|total \(usd\)|total amount|amount due \(usd\)\:|total at checkout|amount due \(usd\)|grand total|total|total cost|payment currency/payment amount|amount paid)[:\s]*[\$]?\(?([\d,]+\.\d{2})\)?\s*(usd)?'

    # Vendor names to look for (case-insensitive)
    vendor_names = [
        "Braze, Inc.", "braza","Ling Tong Media Corporation", "Zach Technology Inc", 
        "Semrush Inc.", "Loom Business", "Adobe Inc.", "Jotform inc.", 
        "Craft&Crew Corp.", "Upwork Global Inc.", "Hotjar Ltd", "Chihuo Inc", 
        "North America Asian Food Industry Association \\(NAAFIA\\)", "TAO Marketing Inc."
    ]
    
    # Company names to exclude (case-insensitive)
    company_names_to_exclude = [
        "Wonders Technologies Corp.", "SKT Technologies Inc.", "SKT TECHNOLOGIES, INC."
    ]
    
    # Compile regex pattern for vendor names with case insensitivity
    vendor_pattern = r'(?i)' + r'|'.join([re.escape(name) for name in vendor_names])

    # Compile regex pattern to exclude company names with case insensitivity
    exclude_pattern = r'(?i)' + r'|'.join([re.escape(name) for name in company_names_to_exclude])

    # Search for patterns
    date_match = re.search(date_pattern, text)
    invoice_match = re.search(invoice_pattern, text)
    amount_match = re.search(amount_pattern, text)
    vendor_match = re.search(vendor_pattern, text)
    exclude_match = re.search(exclude_pattern, text)
    
    # Extract data if found
    date = date_match.group(1) if date_match else 'Not Found'
    invoice_number = invoice_match.group(0) if invoice_match else 'Not Found'
    amount = amount_match.group(2) if amount_match else 'Not Found'
    
    # Vendor name extraction: ensure it's not an excluded company name
    vendor_name = vendor_match.group(0) if vendor_match and not exclude_match else 'Not Found'
    
    return {
        "File Name": file_name,
        "Date": date,
        "Invoice Number": invoice_number,
        "Amount/Total Cost": amount,
        "Vendor Name": vendor_name,
        "Open Command": f'open "{os.path.join(pdf_dir, file_name)}"'

    }

def ocr_extract_text_from_pdf(pdf_path):
    # Convert the PDF to images
    images = convert_from_path(pdf_path)
    
    # Extract text from each image
    text = ''
    for image in images:
        text += pytesseract.image_to_string(image)
    
    return text

extracted_data = []

# Process each PDF
for pdf_file in os.listdir(pdf_dir):
    if pdf_file.endswith('.pdf'):
        pdf_path = os.path.join(pdf_dir, pdf_file)
        text = ocr_extract_text_from_pdf(pdf_path)
        extracted_info = extract_info(text, pdf_file)
        
        # Append the dictionary to the list
        extracted_data.append(extracted_info)

# Create a DataFrame with the extracted data
df = pd.DataFrame(extracted_data)

# Save to an Excel file
output_excel = '/Users/yukachen/marketing-operation/invoices/july_invoice_summary.xlsx'
df.to_excel(output_excel, index=False)

print(f"Data extracted and saved to {output_excel}")