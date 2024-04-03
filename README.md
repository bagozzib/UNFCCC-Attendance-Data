# IEA Attendence, Extract and Classify All the Attendence data from UNFCCC Source files
The project is aimed at extracting and categorizing attendee text entries from (Pre-)COP (Conference of the Parties) PDF documents into specific variables.

## Installation
To install the necessary dependencies, run:

Steps to use the code:
1. Extract the data from the PDF or Image files using extarct

Steps for Text PDF Single Column Text PDF Extraction:
Uncomment the method ExtractOneColumnPdfData(pdf_path).run() to extract data from single-column text PDF files.

Image PDF to Text PDF Conversion:
For image PDF files, convert them to text PDF files using the online "I love PDF" website before extraction.

Double or Triple Column Text PDF Extraction:
Uncomment the method for extracting data from double or triple-column text PDF files. Adjust the observed_pdf_line_width parameter based on the gap between each person's details.

Single Column Image PDF Extraction:
Use the SingleColumnImagePDFTextExtraction(pdf_path).run() method to extract data from single-column image PDF files.

Manual Correction:
After running the extraction scripts, manually verify the extracted data for entity names. If corrections are needed, update the data in the manually_corrected_ip_data list in the specified format.

Image Single Column Correction:
Run the ImageSingleColumnCorrectExtractedContents(manually_corrected_ip_data, title_match_pattern).run() method to correct extracted contents from single-column image PDF files.


## Usage
To use the project, follow these steps:
1. Extract individual attendee entries from COP PDF documents.
2. Segment and categorize the extracted text into separate variable fields.
3. Export the categorized data into Excel format for further analysis.

## Data
The project utilizes COP PDF documents as its primary data source. These documents contain attendee information in various formats, including text and image-based PDFs.
- Data source: COP PDF documents
- Preprocessing steps: Text extraction, OCR conversion (for image-based PDFs)
- Data format: PDF

## Code Structure
- `src/`: Main source code directory
- `data/`: Directory for storing data files
- `scripts/`: Directory for executable scripts

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
