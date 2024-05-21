# UNFCCC Attendee Data Extraction and Analysis

## Table of Contents
- [Description](#description)
- [Installation](#installation)
- [Usage](#usage)
- [File Structure](#folder-structure)
- [Contributors](#contributors)
- [License](#license)


## Description:
This project involves extracting, classifying and analyzing attendee data from the United Nations Framework Convention on Climate Change (UNFCCC) meetings, covering over 30 years of international climate change negotiations. The data includes detailed information about individuals attending these meetings, such as **Group Type, Delegation, Honorific, Person Name, Job Title, Division, and Affiliation.**


## Installation:
To set up the project environment, follow these steps:

1. **Clone the repository:**
   ```
   git clone https://github.com/bagozzib/UNFCCC-Attendance-Data.git
   ```
   
2. **Install the required packages:**
   ```
   pip install -r requirements.txt
   ```

## Usage:
   For detailed usage instructions, please refer to the project [Wiki](https://github.com/bagozzib/UNFCCC-Attendance-Data/wiki).

 ## Folder Structure:
   - **master_data**: This directory holds the definitive CSV files, both with and without translated data.
        - Files:
           -  cops.cleaned.csv
           -  precops.cleaned.csv
           -  cops.cleaned.translated.csv
           -  precops.cleaned.translated.csv
        
        - Translated files contains data in Spanish and French languages, translated to English.

   - **python_files**: These scripts are designed for PDF text extraction and classification, facilitating the generation of CSV files.
        - Files:
            - extract_data :  directory, contains code to extract the data from any of 1, 2, 3 columns - text and image pdf files.
            - classify_data :  directory, contains code to classify the extracted data from extract_data folder.
            - manual text extracted and classified files :  directory, contains Manually Extracted and classifed pdf, csv files, served as Ground Truth, to calculate Accuracies for our final CSV file.
            - inputs_file.py : contains the input data dictionaries and other inputs necessary in the extraction and classification code.
            - post_processing_of_teseract_file.py : clean, and format data extracted from image csv files.
            - spanish_french_translate_to_english.py : Translate the final CSV files, Spanish and French Text to English.      

   - **r_code**: This directory contains R scripts designed for functions such as data cleaning, validation, visualization, and analysis.
       - Files:
           - data validation files: directory, includes files for data validation using Participation statistics to compare the retained information in the final CSV files against the original PDF documents.
           - data visualization files: directory, contains code for visualizing data from the final CSV files, such as cumulative temporal plots.
           - FinalDataCleaning_COP.R and FinalDataCleaning_PreCOP.R are scripts used for data cleaning.

     
   - **requirements.txt**: This file enumerates the Python dependencies necessary for the project.
     
 ## Contributors:
   - Benjamin E. Bagozzi (Corresponding author: bagozzib@udel.edu)
   - Daria Blinova
   - Rakesh Emuru
     
## License

#### This project is licensed under the Creative Commons Attribution 4.0 International License (CC-BY-4.0).
   - This license allows reusers to distribute, remix, adapt, and build upon the material in any medium or format, so long as attribution is given to the creator. The license allows for commercial use.

For more details about this license, please visit the [Creative Commons Attribution 4.0 International License webpage](https://creativecommons.org/licenses/by/4.0/).



   
