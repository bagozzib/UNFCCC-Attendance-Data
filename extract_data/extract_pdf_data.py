import pdfplumber
import re
from inputs_file import title_match_pattern
from pdf2image import convert_from_path
import pytesseract
from PIL import Image
import os

# Set the path to the Tesseract OCR executable file
pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract.exe'
pdf_path = "C:\\Users\\emuru\\Music\\Music\\A COPS STUFF\\pdf files\\2019_COP_25_Madrid.pdf"
# if the data is present in single column format or two-columns format or three columns format.


class ExtractOneColumnPdfData:
    """Class for extracting and processing text from single-column PDF documents."""

    def __init__(self, pdf_path):
        """Initialize the ExtractOneColumnPdfData object with the PDF file path."""

        self.pdf_path = pdf_path

    def run(self):
        """ Run the extraction and processing pipeline. """

        new_processed_words_list = []  # Initialize list to store processed text lines

        processed_extracted_lines = self.extract_text_and_process(self.pdf_path)  # Extract and process text

        # Iterate over each person's data
        for each_person_data in processed_extracted_lines:
            new_list = []  # Initialize list to store processed data for each person
            # Check if the first line does not match the title pattern
            if not title_match_pattern.match(each_person_data[0]):
                new_processed_words_list.append(each_person_data)  # Append to the processed list if it doesn't match
                continue

            # Iterate over each string item in the person's data
            for each_str_item in each_person_data:
                if title_match_pattern.match(each_str_item):
                    new_list.append([each_str_item])  # Start a new sublist for titles
                else:
                    new_list[-1].append(each_str_item)  # Append to the last sublist

            new_processed_words_list.extend(new_list)  # Extend the processed list with the new sublist

        pattern = re.compile(r"^\d+$")  # Define pattern to match numeric strings

        # Display the processed internal list
        for sublist in new_processed_words_list:
            if not pattern.match(sublist[0]) and sublist[0] and '(continued)' not in sublist[0]:
                print(sublist)

    def extract_text_and_process(self, pdf_path, space_threshold=1.5):
        """Extract text from a PDF, group lines based on vertical spacing, remove specific characters,
            and process uppercase text by prepending 'Entity:'. """

        nested_lines = []  # Initialize list to store extracted lines
        current_group = []  # Initialize list to store current line group
        last_y = None  # Initialize variable to store last y-coordinate
        last_char_height = None  # Initialize variable to store last character height

        # Open the PDF file
        with pdfplumber.open(pdf_path) as pdf:
            # Iterate through each page of the PDF
            for page in pdf.pages:
                chars = page.chars  # Get characters on the page

                # Iterate through each character on the page
                for ch in chars:
                    # Start a new line group if the vertical space exceeds the threshold
                    if last_y is not None and (ch["top"] > last_y + space_threshold * last_char_height):
                        if current_group:
                            nested_lines.append(current_group)
                            current_group = []  # Reset current line group

                    # Append the current character to the last line in the group, or start a new line
                    if not current_group or (last_y is not None and ch["top"] > last_y):
                        current_group.append(ch["text"])
                    else:
                        current_group[-1] += ch["text"]

                    # Update the last known position and height of a character
                    last_y = ch["top"]
                    last_char_height = ch["height"]

                # Add the last group if it's not empty
                if current_group:
                    nested_lines.append(current_group)
                    current_group = []  # Reset current line group

        # Clean, filter, and process the text
        processed_nested_lines = []
        for group in nested_lines:
            # Remove '*' and '%' from each line
            cleaned_group = [re.sub(r'[*%]', '', line).strip() for line in group]

            # Skip the group if it contains only one item and that item is all numeric
            if len(cleaned_group) == 1 and cleaned_group[0].isdigit():
                continue

            # Check if all items in the group are uppercase
            if all(line.isupper() for line in cleaned_group):
                entity_line = 'entity: ' + ' '.join(cleaned_group)
                processed_nested_lines.append([entity_line])
            else:
                # Filter out short, non-meaningful strings
                meaningful_lines = [line for line in cleaned_group if len(line) >= 1]
                if meaningful_lines:
                    processed_nested_lines.append(meaningful_lines)

        return processed_nested_lines

class ExtractTwoColumnsPDF():
    def __init__(self, pdf_path, x0_threshold):
        """ Initialize the ExtractMultipleColumnsPDF object with the PDF file path and x0 threshold. """

        self.pdf_path = pdf_path
        self.x0_threshold = x0_threshold

    def run(self):
        """Execute the extraction process for multiple columns PDF. """

        all_pages_data = self.extract_page_wise_column_data()
        persons_data = self.extract_each_person_details(all_pages_data)
        persons_data = self.separate_entity_lines(persons_data)
        persons_data = [dt for dt in persons_data if dt]

        persons_data = self.filter_strings(persons_data)

        for each_person_data in persons_data:
            print(each_person_data)

        return

    def extract_page_wise_column_data(self):
        """ Extract data from each page of the PDF file and organize it into columns. """

        with pdfplumber.open(self.pdf_path) as pdf:
            all_pages_data = []

            for page in pdf.pages:
                page_data = []

                words = page.extract_words()
                # Extract x0 values for 'Mr.' in each column
                x0_first_column_mr = [word['x0'] for word in words if word['text'] == 'Mr.' and word['x0'] <= self.x0_threshold]
                x0_second_column_mr = [word['x0'] for word in words if word['text'] == 'Mr.' and word['x0'] > self.x0_threshold]

                # Process the first column
                first_column_words = [word for word in words if word['x0'] <= self.x0_threshold]
                self.process_column_words(first_column_words, page_data, x0_first_column_mr, x0_second_column_mr)

                # Process the second column
                second_column_words = [word for word in words if word['x0'] > self.x0_threshold]
                self.process_column_words(second_column_words, page_data, x0_first_column_mr, x0_second_column_mr)

                all_pages_data.append(page_data)

            return all_pages_data


    def process_column_words(self, column_words, column_data, x0_first_column_mr, x0_second_column_mr):
        """ Processes sorted words of a column and appends them line-wise to the column data. """

        current_line_top = None
        line_text = ''
        for word in column_words:
            if current_line_top is None or abs(word['top'] - current_line_top) > 5:
                if line_text:
                    column_data.append(line_text.strip())
                line_text = word['text']
                current_line_top = word['top']
            else:
                line_text += ' ' + word['text']
        if line_text:
            column_data.append(line_text.strip())

        # Add 'entity:' prefix to text in the same column with lowercase and x0 less than 'Mr.'
        if x0_first_column_mr and x0_first_column_mr[0] > self.x0_threshold:
            for i, line in enumerate(column_data):
                if line.isupper() and all(word['x0'] < x0_first_column_mr[0]+200 for word in column_words):
                    column_data[i] = 'entity:' + line

        if x0_second_column_mr and x0_second_column_mr[0] > self.x0_threshold:
            for i, line in enumerate(column_data):
                if line.isupper() and all(word['x0'] < x0_second_column_mr[0]+200 for word in column_words):
                    column_data[i] = 'entity:' + line


    def extract_each_person_details(self, all_pages_data):
        """ Extract individual details of each person from the provided data. """

        # Extracting individual persons' details using the title pattern
        persons_data = []
        current_person = []

        # Initialize x0 values for 'Mr.' in each column for each page
        for page_data in all_pages_data:
            x0_first_column_mr = None
            x0_second_column_mr = None
            for line in page_data:
                try:
                    if 'Mr.' in line:
                        if x0_first_column_mr is None:
                            x0_first_column_mr = [word['x0'] for word in page_data if word['text'] == 'Mr.' and word['x0'] <= 275]
                        elif x0_second_column_mr is None:
                            x0_second_column_mr = [word['x0'] for word in page_data if word['text'] == 'Mr.' and word['x0'] > 275]
                except:
                    pass

            for line in page_data:
                if title_match_pattern.match(line):
                    # Start a new person's details
                    if current_person:
                        persons_data.append(current_person)
                    current_person = [line]
                else:
                    # Add 'entity:' prefix to text in the same column with lowercase and x0 less than 'Mr.'
                    if x0_first_column_mr and 'entity:' not in line and all(word['x0'] < min(x0_first_column_mr) for word in page_data):
                        current_person.append('entity:' + line)
                    elif x0_second_column_mr and 'entity:' not in line and all(word['x0'] < min(x0_second_column_mr) for word in page_data):
                        current_person.append('entity:' + line)
                    else:
                        current_person.append(line)

        return persons_data


    def separate_entity_lines(self, persons_data):
        """ Separate the lines of text into entities and other information. """

        new_persons_data = []  # Initialize list to store separated lines of text
        for person in persons_data:
            per_data = []  # Initialize list to store non-entity lines
            entity_lines = []  # List to store consecutive entity lines
            for line in person:
                if line.startswith('entity:'):
                    entity_lines.append(line[7:])  # Add line to entity_lines, removing the 'entity:' prefix
                else:
                    if entity_lines:
                        # Join all entity lines and add as a single entry, then reset entity_lines
                        new_persons_data.append('entity:' + ' '.join(entity_lines))
                        entity_lines = []
                    per_data.append(line)

            # Add non-entity lines
            if per_data:
                new_persons_data.append(per_data)

            # Handle any remaining entity lines after the loop
            if entity_lines:
                new_persons_data.append(['entity:' + ' '.join(entity_lines)])

        return new_persons_data


    def clean_list(self, main_data_list):
        """  Remove list items that are either all numbers or contain numbers but no alphabets. """

        cleaned_list = []
        for data_list in main_data_list:
            items = []
            for item in data_list:
                # Check if the item is not all digits and contains at least one alphabet character
                if not item.replace(' ', '').isdigit() and any(char.isalpha() for char in item):
                    items.append(item)

            cleaned_list.append(items)
        return cleaned_list

    def filter_strings(self, persons_data):
        """ Filter out strings from nested lists based on specified criteria, while retaining the nested list structure. """

        filtered_data = []
        for item in persons_data:
            if isinstance(item, list):  # Check if the item is a list
                filtered_sublist = [s for s in item if not self.is_excluded(s)]
                if filtered_sublist:  # Add the sublist only if it's not empty
                    filtered_data.append(filtered_sublist)
            elif isinstance(item, str):  # Check if the item is a string
                if not self.is_excluded(item):
                    filtered_data.append(item)

        return filtered_data

    def is_excluded(self, string):
        """ Determine if the string should be excluded based on the specified criteria. """

        lower_string = string.lower()
        return (any(c.isdigit() for c in lower_string) or
                'email' in lower_string or 'T:' in lower_string or
                'P.O.' in lower_string or 'F:' in lower_string)

class ExtractThreeColumnsPdfData:
    """Extract text from 2 and 3 columns PDF data and print as a list of lists."""

    def __init__(self, pdf_path, observed_pdf_line_width):
        """Initialize the ExtractTwoAndThreeColumnsPdfData object with the PDF file path."""
        self.pdf_path = pdf_path
        self.observed_pdf_line_width = observed_pdf_line_width

    def run(self):
        """Extract the data from the given PDF and print it."""

        grouped_output = self.extract_with_bold_annotations()  # Extract data with bold annotations

        # Remove lines that start with certain text
        pattern = re.compile(r'^.*\bFCCC/CP/2005/INF.2\b.*$')

        # Iterate over the grouped output list
        for index, line in enumerate(grouped_output):
            # Assuming the regex should match the first item in the sublist
            if pattern.match(line[0]):
                # Check if there's a line before it to avoid IndexError
                if index > 0:
                    # Remove the current and previous lines
                    del grouped_output[index]
                    del grouped_output[index - 1]
                else:
                    # If it's the first line, just remove the current line
                    del grouped_output[index]

        # Iterate over the remaining lines in grouped_output
        for d in grouped_output:
            # Check if the first item exists and does not contain '(continued)'
            if d[0] and '(continued)' not in d[0]:
                print(d)  # Print the line

        return

    def should_add_bold_prefix(self, text):
        """Check if a bold prefix should be added to the given text."""

        # Define the pattern to match numerical values
        pattern = r'^[-+]?\d*\.?\d+$'

        # Check if the text starts with "FCCC/CP" or matches the numerical pattern
        if text.startswith("FCCC/CP") or re.match(pattern, text):
            return False  # No bold prefix needed

        return True  # Add bold prefix

    def merge_entity_lines(self, groups):
        """Merge lines within groups where the first line starts with 'Entity:'."""

        merged_groups = []  # Initialize list to store merged groups
        i = 0  # Initialize index for iterating through groups

        # Iterate through each group
        while i < len(groups):
            group = groups[i]  # Get current group

            # Check if first line starts with 'Entity:' and group has more than one line
            if group[0].startswith("Entity:") and len(group) > 1:
                merged_group = [" ".join(group)]  # Merge lines within the group
                merged_group = [merged_group[0].replace(' Entity:', ' ')]  # Replace ' Entity:' with space
                merged_groups.append(merged_group)  # Append merged group to the list
            else:
                merged_groups.append(group)  # If conditions are not met, append the original group
            i += 1  # Move to the next group

        return merged_groups  # Return the list of merged groups

    def extract_with_bold_annotations(self):
        """Extract text from a PDF with bold annotations."""

        nested_lines_with_bold = []  # Initialize list to store nested lines with bold annotations
        current_group = []  # Initialize list to store lines within a group
        current_line = ""  # Initialize variable to store current line of text
        last_y = None  # Initialize variable to store last y-coordinate
        last_char_height = 0  # Initialize variable to store height of last character
        previous_y = float('inf')  # Initialize variable to store previous y-coordinate

        # Open the PDF file
        with pdfplumber.open(self.pdf_path) as pdf:
            # Iterate through each page of the PDF
            for page in pdf.pages:
                chars = page.chars  # Get characters on the page

                # Iterate through each character on the page
                for ch in chars:
                    # Detect a new column by comparing the y-coordinate
                    if ch["top"] < previous_y:
                        if current_line:
                            current_group.append(current_line.strip())
                            current_line = ""
                        if current_group:
                            nested_lines_with_bold.append(current_group)
                            current_group = []

                    # Check for a new line by comparing the y-coordinate
                    if last_y is not None and ch["top"] > (last_y + last_char_height):
                        if current_line:
                            current_group.append(current_line.strip())
                            current_line = ""

                        # Check for a larger than usual gap between lines
                        if ch["top"] - last_y > observed_pdf_line_width * last_char_height:
                            if current_group:
                                nested_lines_with_bold.append(current_group)
                                current_group = []

                    # Check if text should be prefixed with bold
                    if "Bold" in ch["fontname"] and not current_line.startswith("Entity:") and \
                            self.should_add_bold_prefix(ch["text"]):

                        current_line += "Entity:"

                    current_line += ch["text"]  # Append character to current line
                    last_y = ch["top"]  # Update last y-coordinate
                    last_char_height = ch["height"]  # Update height of last character
                    previous_y = last_y  # Store the last y-coordinate for next iteration's comparison

                # Ensure the last line and group are added
                if current_line:
                    current_group.append(current_line.strip())
                if current_group:
                    nested_lines_with_bold.append(current_group)
                    current_group = []

        # Merge lines with bold annotations
        nested_lines_with_bold = self.merge_entity_lines(nested_lines_with_bold)

        # Return nested lines with bold annotations
        return nested_lines_with_bold

    def split_data(self, lst):
        """Split a list into sublists whenever a new entity or title is encountered."""

        current = []  # Initialize list to store current sublist

        # Iterate through each item in the list
        for item in lst:
            # Check if item starts with 'Entity:' or matches a title pattern
            if item.startswith('Entity:') or re.match(r'(Mr\.\s*|Mrs\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme\ \s*|S\.E\.\s*|Sr\.\s*|Sra\.\s*|H\. E\.\s*|H\.E\.\s*)', item):
                if current:
                    yield current  # Yield current sublist
                    current = []  # Reset current sublist

            current.append(item)  # Add item to current sublist
        if current:
            yield current  # Yield last sublist if not empty

    def clean_item(self, item):
        """Clean a text item by removing certain patterns and splitting it into multiple items if needed."""

        output_items = []  # Initialize list to store cleaned text items

        # Check for the presence of 'FCCC' and two occurrences of 'Entity:'
        if 'FCCC' in item and len(re.findall(r'Entity:', item)) == 2:
            item = item.split('Entity:', 2)[-1]  # Keep everything after the second 'Entity:'
        # Check for the presence of two 'Entity:' and absence of 'FCCC'
        elif len(re.findall(r'Entity:', item)) == 2 and 'FCCC' not in item:
            item = item.replace('Entity:', '')  # Remove 'Entity:' prefix
        # Check for one 'Entity:' not at the start
        elif item.find('Entity:') > 0:
            prefix, entity_content = item.split('Entity:', 1)
            if prefix.strip():
                output_items.append(prefix.strip())  # Add prefix to output
            item = 'Entity:' + entity_content.strip()  # Add 'Entity:' prefix to the rest of the content

        # The remaining standard cleaning
        item = re.sub(r'Entity:\s*\d+\s*', '', item)  # Remove 'Entity:' followed by numbers
        item = re.sub(r'Entity: FCCC/[\w/]+', '', item)  # Remove 'Entity: FCCC/CP/2014/INF.2' pattern
        output_items.append(item.strip())  # Add cleaned item to output list

        return output_items  # Return cleaned text items

    def clean_list(self, lst):
        """Clean a list of text items by applying the clean_item function to each item."""

        cleaned = []  # Initialize list to store cleaned text items

        # Iterate through each item in the list
        for item in lst:
            cleaned_items = self.clean_item(item)  # Clean the current item

            # Create a nested list if the condition is met
            if len(cleaned_items) == 2 and 'Entity:' not in cleaned_items[0] and 'Entity:' in cleaned_items[1]:
                output_list = [[item] for item in cleaned_items]
                return output_list  # Return nested list if condition is met

            cleaned.extend(cleaned_items)  # Add cleaned items to the list using extend

        return cleaned if cleaned else None  # Return the cleaned list or None if it's empty

class SingleColumnImagePDFTextExtraction():
    """ Extract data from Single Column image files """
    def __init__(self, pdf_path):
        self.pdf_path = pdf_path

    def run(self):
        extracted_text = self.extract_text_from_pdf()

        # Print or process the extracted text
        for i, text in enumerate(extracted_text):
            print(f"Page {i + 1}:\n{text}\n")

    def extract_text_from_pdf(self):
        # Convert PDF pages to images
        images = convert_from_path(self.pdf_path)

        extracted_text = []

        for i, img in enumerate(images):
            # Save the image to a temporary file
            img_path = f"temp_page_{i}.png"
            img.save(img_path, 'PNG')

            # Use Tesseract to extract text from the image
            text = pytesseract.image_to_string(Image.open(img_path))

            # Append the extracted text to the result list
            extracted_text.append(text)

            # Remove the temporary image file
            os.remove(img_path)

        return extracted_text


class ImageSingleColumnCorrectExtractedContents:
    """ Correct the extracted data from the single column image files """
    def __init__(self, input_list, title_match_pattern):
        self.input_list = input_list
        self.title_match_pattern = title_match_pattern

    def preprocess_input(self):
        output_list = []
        current_title_group = []

        for item in self.input_list:
            if not item.isupper() or 'suite' in item or 'continued' in item:
                if self.title_match_pattern.search(item):
                    if current_title_group:
                        output_list.append(current_title_group)
                        current_title_group = [item]
                    else:
                        current_title_group.append(item)
                else:
                    current_title_group.append(item)

            if item.isupper() or 'suite' in item or 'continued' in item:
                if current_title_group:
                    output_list.append(current_title_group)
                    current_title_group = []

                kk = ['Entity:' + item]
                output_list.append(kk)

        if current_title_group:
            output_list.append(current_title_group)

        self.input_list = output_list.copy()

    def filter_strings(self):
        """
        Filter out strings from nested lists based on specified criteria,
        while retaining the nested list structure.
        """
        filtered_data = []
        for item in self.input_list:
            if isinstance(item, list):  # Check if the item is a list
                filtered_sublist = [s for s in item if not self.is_excluded(s)]
                if filtered_sublist:  # Add the sublist only if it's not empty
                    filtered_data.append(filtered_sublist)
            elif isinstance(item, str):  # Check if the item is a string
                if not self.is_excluded(item):
                    filtered_data.append(item)
        return filtered_data

    def is_excluded(self, string):
        """
        Determine if the string should be excluded based on the specified criteria.
        """
        lower_string = string.lower()
        return (any(c.isdigit() for c in lower_string) or
                'email' in lower_string or 'T:' in lower_string or
                'P.O.' in lower_string or 'F:' in lower_string)

    def split_strings(self, filtered_data):
        output_list = []

        # Iterate through the input lists
        for inner_list in filtered_data:
            new_lst = []
            for each_str in inner_list:
                # Split the string using commas and strip extra whitespaces
                split_strings = [part.strip() for part in each_str.split(',')]
                new_lst.extend(split_strings)

            # Append the split strings as a new list to the output list
            output_list.append(new_lst)

        return output_list

    def separate_titles(self, filtered_data):
        op = []

        for inner_list in filtered_data:
            is_present = False
            inner_last_split_idx = 0

            if len(inner_list) > 2:
                for item_idx, item in enumerate(inner_list[1:]):

                    regex = title_match_pattern.search(item)
                    if regex:
                        is_present = True

                        kk = item.split(regex.group())
                        first_person_detail = inner_list[inner_last_split_idx:item_idx + 1]
                        first_person_detail.append(kk[0])

                        rem_person_detail = inner_list[item_idx + 2:]
                        # rem_person_detail[0] = str(regex.group()) + rem_person_detail[0]
                        rem_person_detail.insert(0, kk[1])
                        rem_person_detail[0] = str(regex.group()) + rem_person_detail[0]

                        op.append(first_person_detail)
                        op.append(rem_person_detail)
                        inner_last_split_idx = item_idx + 1

                    else:
                        # If no title is found, add to the current list
                        # new_data[-1].append(item)
                        pass

            if not is_present:
                op.append(inner_list)

        for k in op:
            if 'continued' in k[-1].lower():
                k.pop(-1)

        for k in range(len(op)-1):
            if 'Entity:' + str(op[k][-1]) == op[k+1][0]:
                op[k].pop(-1)

        return op


    def run(self):
        self.preprocess_input()
        filtered_data = self.filter_strings()
        separated_titles = self.separate_titles(filtered_data)

        non_empty_person_data = []
        final_person_data = []

        for person_data in separated_titles:
            if person_data and person_data[0]:
                non_empty_person_data.append(person_data)

        for each_person in non_empty_person_data:
            each_person = [person_detail for person_detail in each_person if person_detail]
            final_person_data.append(each_person)

        person_details = []

        # some persons data is splitted, so correcting it.
        for k in final_person_data:
            if not 'Entity' in k[0] and not 'Group' in k[0]:
                jk = k[0]
                jk = jk.split()[::-1]
                try:
                    surname_idx = [idx for idx in range(len(jk)) if jk[idx].isupper()]

                    if surname_idx:
                        surname_idx = surname_idx[0]
                    else:
                        continue

                except Exception as e:
                    import ipdb;ipdb.set_trace()

                k.pop(0)
                k.insert(0, ' '.join(jk[surname_idx:][::-1]))
                k.insert(1, ' '.join(jk[:surname_idx][::-1]))

            person_details.append(k)

        for each_person in person_details:
            each_person = [person_detail for person_detail in each_person if person_detail]
            print(each_person)


# ***************************************
# Uncomment the below to start Execution:



# Uncomment the below method for Extracting data from "Single Column" text PDF file.
# ExtractOneColumnPdfData(pdf_path).run()

# Image PDF files converted to Text PDF files using Online "I love PDF" website
# ExtractTwoColumnsPDF(pdf_path, 275).run()

# Uncomment the below method for Extracting data from "Double or Triple" Column text PDF file.

# This is the gap from each person to another person, for some pdf, it is 2, for some it is 1.38, ....
# if this is not set properly, then the person details will become messy.
# observed_pdf_line_width = 2
# ExtractThreeColumnsPdfData(pdf_path, observed_pdf_line_width).run()

# 1. After running this, we have to add the Group Names from the Participants column of the pdf.
# 2. we have to convert it to a list of lists.
# --------------------------------------------------
# Extract from Single Column Image PDF
# SingleColumnImagePDFTextExtraction(pdf_path).run()


# ** manually take the output of this function(which is printed in the shell) and verify it for Entity names, as if they are correct or not and then manually correct
# something which is not correct and not handled in this below functions. Once it is ready, keep the corrected data in this list, in the below format:


# This is the Example format:
# [['Raju', 'Engineer', 'Customer Acquisition Department', 'Sugar Cosmetics'], ['Kiran', 'Professor', 'Data Science Department', 'University of Delaware']]

# manually_corrected_ip_data = []
#
#ImageSingleColumnCorrectExtractedContents(manually_corrected_ip_data, title_match_pattern).run()
