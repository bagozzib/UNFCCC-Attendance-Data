from googletrans import Translator
import pandas as pd
from tqdm import tqdm

# Create a Translator object
translator = Translator()

# Load the CSV file
ip_df = pd.read_csv("C:\\Users\\emuru\\Downloads\\precops.cleaned.csv")

# Define the batch size
batch_size = 100  # Adjust this based on your needs and system capabilities

# Initialize an empty list to store the processed job titles
processed_job_titles = []

# Excel file path
excel_file_path = "C:\\Users\\emuru\\Downloads\\Job_Title_Translated.xlsx"

# Process in batches
for i in tqdm(range(0, len(ip_df), batch_size)):
    batch = ip_df["Job_Title"][i:i+batch_size]

    for text in batch:
        try:
            # Detect the language of the text
            detected_language = translator.detect(text).lang

            # Check if the detected language is Spanish or French
            if detected_language in ['es', 'fr']:
                text = translator.translate(text, dest='en').text

            if pd.isnull(text):
                text = 'NA'

        except Exception as e:
            print(f"Error translating text: {e}")
            text = 'Translation Error'

        processed_job_titles.append(text)

    # After processing a batch, append it to the Excel file
    df = pd.DataFrame(processed_job_titles, columns=['Job_Title_Translated'])
    with pd.ExcelWriter(excel_file_path, engine='openpyxl', mode='a', if_sheet_exists='overlay') as writer:
        df.to_excel(writer, sheet_name='Sheet1', startrow=writer.sheets['Sheet1'].max_row, index=False, header=False)

    # Clear the list for the next batch
    processed_job_titles.clear()
