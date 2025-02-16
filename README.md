# UNFCCC Attendee Data Extraction and Analysis

## Table of Contents
- [Description](#description)
- [Installation](#installation)
- [Usage](#usage)
- [File Structure](#folder-structure)
- [Contributors](#contributors)
- [License](#license)


## Description:
The code associated with this project extracts, classifies and formats attendee data from the United Nations Framework Convention on Climate Change (UNFCCC) meetings (and the meetings leading up to the formation of the UNFCCC), covering over 30 years of international climate change negotiations. Several final attendee-level datasets are then provided. The datasets include detailed information about individuals attending these meetings, such as **Group Type, Delegation, Honorific, Person Name, Job Title, Division, and Affiliation.**


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
   For detailed usage instructions, please refer to the project [UNFCCC Project Code Execution Steps](https://github.com/bagozzib/UNFCCC-Attendance-Data/wiki/UNFCCC-Project-Code--Execution-Steps) WIKI.

 ## Folder Structure:
   - **master_data**: This directory holds the final datasets as definitive CSV files, both with and without translated data.
        - Files:
           -  cops.cleaned.csv
           -  precops.cleaned.csv
           -  cops.cleaned.translated.csv
           -  precops.cleaned.translated.csv
        
        - Translated files contains data that was originaly in Spanish and French that has been translated to English.

   - **python_files**: These scripts are designed for PDF text extraction and classification, facilitating the generation of CSV files.
        - Files:
            - extract_data :  directory, contains code to extract the data from any of 1, 2, 3 columns - text and image pdf files.
            - classify_data :  directory, contains code to classify the extracted data from extract_data folder.
            - manual text extracted and classified files :  directory, contains Manually Extracted and classifed pdf, csv files, served as Ground Truth, to calculate Accuracies for our final CSV file.
            - inputs_file.py : contains the input data dictionaries and other inputs necessary in the extraction and classification code.
            - post_processing_of_teseract_file.py : clean, and format data extracted from image csv files.
            - spanish_french_translate_to_english.py : Translate the final CSV files, Spanish and French text to English.      

   - **r_code**: This directory contains R scripts designed for functions such as data cleaning and validation.
       - Files:
           - data validation files: directory, includes files for data validation using Participation statistics to compare the retained information in the final CSV files against the original PDF documents.
           - FinalDataCleaning_COP.R, FinalDataCleaning_PreCOP.R and FinalDatasetFormatting.R are scripts used for data cleaning.

     
   - **requirements.txt**: This file enumerates the Python dependencies necessary for the project.
     
## Conclusion
- This project effectively addressed the challenges of extracting and categorizing attendee information from (pre-)COP PDF documents. By leveraging PDF Text Extraction Libraries, OCR technology, NLP packages, heuristics, and manual verification processes, the project achieved accurate classification of attendee details into predefined categories.

## Contributors:
   - Benjamin E. Bagozzi (Corresponding author: bagozzib@udel.edu)
   - Daria Blinova
   - Rakesh Emuru
     
## License

#### This project is licensed under the Creative Commons Attribution 4.0 International License (CC-BY-4.0).
   - This license allows reusers to distribute, remix, adapt, and build upon the material in any medium or format, so long as attribution is given to the creator. The license allows for commercial use.

For more details about this license, please visit the [Creative Commons Attribution 4.0 International License webpage](https://creativecommons.org/licenses/by/4.0/).



   
