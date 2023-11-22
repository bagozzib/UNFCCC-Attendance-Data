import pdfplumber
import re

title_match_pattern = re.compile(
    r"(Mr\.\s*|S\$ra\.|HE\ Mr\.|HE\ Mr\.|S\:E\.\ Sra\.|HE\ Mr|St\.|1LE\ Mr\.|Mr\_|H\.RH\.|SE\.|HE\.|\$\.E\.|SE\.Sr\.|HLE\.|HEMr\.|ILE\.\ Mr\.|HE\.\ Ms\.|HE\.\ Mr\.|H\.R\.H\.\s*|Mx\.\s*|H\.H\.\s*|Ind\.\s*|His\ |Ind\ |Ms\ |Mr\ |Sra\ |Sr\ |M\ |On\ |M\ |Fr\ |H\.O\.\s*|Rev\ |Mme\ |Sr\ |Msgr\ |On\.\s*|Fr\.\s*|Rev\.\s*|H\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |Sra\.\s*|Mme|Sr\.\s*|Msgr\.\s*))?|Msgr\.\s*|Mrs\.\s*|Sra\.\s*|Sr\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme|Ms|S\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Mme|Mr|Ms|Dr|Msgr\.\s*|M\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |M\ |Sra\.\s*|Sr\.\s*))?)")

def extract_page_wise_column_data(file_path, x0_threshold):
    with pdfplumber.open(file_path) as pdf:
        all_pages_data = []

        for page in pdf.pages:
            page_data = []

            words = page.extract_words()
            # Extract x0 values for 'Mr.' in each column
            x0_first_column_mr = [word['x0'] for word in words if word['text'] == 'Mr.' and word['x0'] <= x0_threshold]
            x0_second_column_mr = [word['x0'] for word in words if word['text'] == 'Mr.' and word['x0'] > x0_threshold]

            # Process the first column
            first_column_words = [word for word in words if word['x0'] <= x0_threshold]
            process_column_words(first_column_words, page_data, x0_first_column_mr, x0_second_column_mr, x0_threshold)

            # Process the second column
            second_column_words = [word for word in words if word['x0'] > x0_threshold]
            process_column_words(second_column_words, page_data, x0_first_column_mr, x0_second_column_mr, x0_threshold)

            all_pages_data.append(page_data)

        return all_pages_data

def process_column_words(column_words, column_data, x0_first_column_mr, x0_second_column_mr, x0_threshold):
    """
    Processes sorted words of a column and appends them line-wise to the column data.
    """
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
    if x0_first_column_mr and x0_first_column_mr[0] > x0_threshold:
        for i, line in enumerate(column_data):
            if line.isupper() and all(word['x0'] < x0_first_column_mr[0] for word in column_words):
                column_data[i] = 'entity:' + line

    if x0_second_column_mr and x0_second_column_mr[0] > x0_threshold:
        for i, line in enumerate(column_data):
            if line.isupper() and all(word['x0'] < x0_second_column_mr[0] for word in column_words):
                column_data[i] = 'entity:' + line

file_path = "C:\\Users\\rakes\\Music\\pdfconverted image files\\1997_COP_3_Kyoto.pdf"
all_pages_data = extract_page_wise_column_data(file_path, 300)

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
                    x0_first_column_mr = [word['x0'] for word in page_data if word['text'] == 'Mr.' and word['x0'] <= 300]
                elif x0_second_column_mr is None:
                    x0_second_column_mr = [word['x0'] for word in page_data if word['text'] == 'Mr.' and word['x0'] > 300]
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

def separate_entity_lines(persons_data):
    new_persons_data = []
    for person in persons_data:
        per_data = []
        for line in person:
            if line.startswith('entity:'):
                new_persons_data.append([line])
                del line
            else:
                per_data.append(line)

        new_persons_data.append(per_data)

    return new_persons_data

persons_data = separate_entity_lines(persons_data)
persons_data = [dt for dt in persons_data if dt]

def clean_list(main_data_list):
    """
    Remove list items that are either all numbers or contain numbers but no alphabets.
    """
    cleaned_list = []
    for data_list in main_data_list:
        items = []
        for item in data_list:
            # Check if the item is not all digits and contains at least one alphabet character
            if not item.replace(' ', '').isdigit() and any(char.isalpha() for char in item):
                items.append(item)

        cleaned_list.append(items)
    return cleaned_list

# Test the function with the provided example
persons_data = clean_list(persons_data)

# Print the individual persons' details with 'entity:' lines separated
# for person in persons_data:
#     for item in person:
#         if isinstance(item, list):
#             print(item)
#         else:
#             print(item)
#     print('\n')
for kd in persons_data:
    print(kd)
