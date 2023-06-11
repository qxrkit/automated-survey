import os
import json
import pandas as pd
from google.oauth2 import service_account
from googleapiclient.discovery import build
from datetime import datetime

# Get credentials
info = json.loads(os.environ['SERVICE_ACCOUNT'])
credentials = service_account.Credentials.from_service_account_info(info)
service_sheets = build('sheets', 'v4', credentials=credentials)

# Get the sheet ID from the environment variable
SHEET_ID = os.getenv('SPREADSHEET_ID')

# Get the names of the sheets
sheet_metadata = service_sheets.spreadsheets().get(spreadsheetId=SHEET_ID).execute()
sheets = sheet_metadata.get('sheets', '')

# Define the directory for saving the files
timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
directory = f"data/device-testing/{timestamp}"

# Ensure the directory exists
os.makedirs(directory, exist_ok=True)

# For each sheet, get all the data and write it to a CSV file
for sheet in sheets:
    sheet_name = sheet.get('properties', {}).get('title')
    if sheet_name in ['Responses', 'Labels']:  # Replace with your sheet names
        result = service_sheets.spreadsheets().values().get(
            spreadsheetId=SHEET_ID,
            range=sheet_name).execute()
        data = result.get('values', [])

        # If there is data, write it to a CSV file
        if data:
            df = pd.DataFrame(data)
            df.columns = df.iloc[0]
            df = df[1:]
            df.to_csv(f"{directory}/{sheet_name}.csv", index=False)
