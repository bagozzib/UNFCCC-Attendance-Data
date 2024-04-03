# IEA Attendence, Extract and Classify All the Attendence data from UNFCCC Source files
The project is aimed at extracting and categorizing attendee text entries from (Pre-)COP (Conference of the Parties) PDF documents into specific variables.

## Installation
To install the necessary dependencies, run:
requirements.txt

## Usage
Steps to use the code:
1. Extract the data from the Text or Image PDF files using the given project files except classify_data.py
2. After extracting the pdf data using step1, we will have a list or of data, we will input that data to the classify_data.py file, to classify the data.

below are the detailed steps:
select the uploaded project files, based on the PDF type(image/text) and column format, the output of the files will be a list or array of data.
 
- Steps for Text PDF Single Column Text PDF Extraction:
Uncomment the method ExtractOneColumnPdfData(pdf_path).run() to extract data from single-column text PDF files.

- Double or Triple Column Text PDF Extraction:
Uncomment the method for extracting data from double or triple-column text PDF files. Adjust the observed_pdf_line_width parameter based on the gap between each person's details.

- Single Column Image PDF Extraction:
Use the SingleColumnImagePDFTextExtraction(pdf_path).run() method to extract data from single-column image PDF files.

- Manual Correction:
After running the extraction scripts, manually verify the extracted data for entity names. If corrections are needed, update the data in the manually_corrected_ip_data list in the specified format.

- Image Single Column Correction:
Run the ImageSingleColumnCorrectExtractedContents(manually_corrected_ip_data, title_match_pattern).run() method to correct extracted contents from single-column image PDF files.

Once we have the data in a list or Array format, we will use the classify_data to segreagte the data to these Group Type, Delegation, Honorific, Person Name, Job Title, Division, Affiliation columns and generate an excel file.


## Data
The project utilizes COP and Pre PDF documents as its primary data source. These documents contain attendee information in various formats, including text and image-based PDFs.
- Data source: UNFCCC COP and Pre-COP PDF documents
- Preprocessing steps: Text extraction, OCR conversion (for image-based PDFs)
- Data format: PDF

## Code Structure
- extract_pdf_data: This script handles the extraction of data from PDF files with three columns and two columns.
- single_column_extract_text_data: This script manages the extraction of data from PDF files with a single column.
- image_pdf_extract_pdf_data: This script is responsible for extracting data from PDF files containing images.
- inputs_file: This file contains input lists, file paths, data dictionaries, and all necessary inputs for the functions in other files.
- post_processing_of_tesseract_file: This script contains code for preprocessing the data before classifying the data extracted from image files.
- classify_data: This script classifies the input list of codes into the required output variables and generates an output Excel file.



## Results
The project successfully extracted and categorized 310,202 attendee entries from 43 COP and 12 pre-COP PDFs.
- Group Type
- Delegation
- Honorific
- Person Name
- Job Title
- Division
- Affiliation

## Conclusion
In conclusion, the project effectively addressed the challenges associated with extracting and categorizing attendee information from COP PDF documents. By leveraging a combination of OCR technology, NLP packages, Heuristics, and manual verification processes, the project achieved accurate classification of attendee details into predefined categories.
