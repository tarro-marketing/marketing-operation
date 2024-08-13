import gspread
import numpy as np
import pandas as pd
import janitor

gc = gspread.oauth()

sht1 = gc.open_by_key('1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg')

worksheet = sht1.worksheet("[all time] Marketing MEL/MQL Report")

all_time_lead = worksheet.get("A2:AL")

df_all_time_lead = pd.DataFrame(all_time_lead[1:], columns=all_time_lead[0])

df_all_time_lead = df_all_time_lead.clean_names()

df_all_time_lead.replace('', np.nan, inplace=True)


keep_vars = ['df_all_time_lead','gc']

for var in list(globals().keys()):
    if var not in keep_vars:
        del globals()[var]

# Print variables after deletion
print("After deletion:", globals().keys())

import re
import pandas as pd

def clean_phone_number(phone):

    if pd.isna(phone):

        return 'NA'  

    return re.sub(r'[^\d]', '', str(phone))

df_all_time_lead["mobile_primary"] = df_all_time_lead["mobile_primary"].apply(clean_phone_number)
df_all_time_lead["business_phone"] = df_all_time_lead["business_phone"].apply(clean_phone_number)


sht2 = gc.open_by_key('1-WkE7rGsIxYMLjVCZNEKYYliqXau71wNwwCXBjq_b0c')

sms_sam_list_sheet = sht2.worksheet("SAM List")

sms_sam_list_gsheet = sms_sam_list_sheet.get("A1:AK")

sms_sam_list = pd.DataFrame(sms_sam_list_gsheet[1:], columns=sms_sam_list_gsheet[0]).clean_names()

phone_list_source = sms_sam_list[["phone_clean","source"]]


def determine_metrics(row):
    if pd.notna(row['first_mel_timestamp']) and row['unqualified_reason'] not in ['Current Client', 'Duplicate']:
        return 'MEL'
    elif pd.notna(row['latest_mql_timestamp']) and row['unqualified_reason'] not in ['Current Client', 'Duplicate', 'Not a Restaurant', 'Incorrect Phone Number']:
        return 'MQL'
    elif pd.notna(row['opportunity_id']):
        return 'SQL'
    elif row['stage'] == 'Onboarded':
        return 'Onboarded'
    else:
        return 'Dead Lead'  

df_all_time_lead['metrics'] = df_all_time_lead.apply(determine_metrics, axis=1)

df_merged = df_all_time_lead.merge(
    phone_list_source,
    how='left',
    left_on='mobile_primary',
    right_on='phone_clean',
    suffixes=('', '_mobile')
)

df_merged.rename(columns={'source': 'source_mobile', 'phone_clean':'primary_sms'}, inplace=True)

df_merged = df_merged.merge(
    phone_list_source,
    how='left',
    left_on='business_phone',
    right_on='phone_clean',
    suffixes=('', '_business')
)

df_merged.rename(columns={'source': 'source_business', 'phone_clean':'business_sms'}, inplace=True)


df_merged['final_source'] = df_merged['source_mobile'].combine_first(df_merged['source_business'])

df_merged['final_source'] = df_merged['final_source'].fillna('No Match')

df_result = df_merged[['mobile_primary', 'business_phone', 'final_source','metrics','latest_campaign','unqualified_reason','rejected_reason']]


import numpy as np

df_result.replace('', np.nan, inplace=True)
df_result = df_result.fillna("NA")
