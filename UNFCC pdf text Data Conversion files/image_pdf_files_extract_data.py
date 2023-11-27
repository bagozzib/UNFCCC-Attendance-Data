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
            if line.isupper() and all(word['x0'] < x0_first_column_mr[0]+280 for word in column_words):
                column_data[i] = 'entity:' + line

    if x0_second_column_mr and x0_second_column_mr[0] > x0_threshold:
        for i, line in enumerate(column_data):
            if line.isupper() and all(word['x0'] < x0_second_column_mr[0]+280 for word in column_words):
                column_data[i] = 'entity:' + line

file_path = "C:\\Users\\rakes\\Music\\pdfconverted image files\\1995_COP_1_Berlin.pdf"
all_pages_data = extract_page_wise_column_data(file_path, 275)

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

def separate_entity_lines(persons_data):
    new_persons_data = []
    for person in persons_data:
        per_data = []
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
# persons_data = clean_list(persons_data)

# Print the individual persons' details with 'entity:' lines separated
# for person in persons_data:
#     for item in person:
#         if isinstance(item, list):
#             print(item)
#         else:
#             print(item)
#     print('\n')

def filter_strings(persons_data):
    """
    Filter out strings from nested lists based on specified criteria,
    while retaining the nested list structure.

    :param persons_data: List of strings and nested lists of strings to filter.
    :return: Filtered list with the same structure as persons_data.
    """
    filtered_data = []
    for item in persons_data:
        if isinstance(item, list):  # Check if the item is a list
            filtered_sublist = [s for s in item if not is_excluded(s)]
            if filtered_sublist:  # Add the sublist only if it's not empty
                filtered_data.append(filtered_sublist)
        elif isinstance(item, str):  # Check if the item is a string
            if not is_excluded(item):
                filtered_data.append(item)

    return filtered_data

def is_excluded(string):
    """
    Determine if the string should be excluded based on the specified criteria.

    :param string: The string to check.
    :return: True if the string should be excluded, False otherwise.
    """
    lower_string = string.lower()
    return (any(c.isdigit() for c in lower_string) or
            'email' in lower_string or 'T:' in lower_string or
            'P.O.' in lower_string or 'F:' in lower_string)

# persons_data = [['Mr. Stephan SINGER', '‘WWF - Germany', 'Hellderichstr. 115', 'D-60591 Frankfurt Germany', 'Email:'], 'entity:T: (31-3404)3-7326 F: (31-3404)1-2064', ['Mr. Meindert BROUWER', 'Postbus 7', 'NL-3700 AA Zeist Netherlands', 'Email:'], 'entity:T: (41-22)364-9111 F: (41-22)364-4238', ['Mr. Andrew KERR', 'Avenue du Mont-Blanc', 'CH-1196 Gland Switzerland', 'Email: wwfgland@gn.apc.org'], 'entity:T: (41-22)364-9111 F: (41-22)364-4238', ['Mr. Sacha FRIEDLI', 'Avenue du Mont-Blanc', 'CH-1196 Gland Switzerland', 'Email: wwfgland@gn.apc.org'], ['entity:WUPPERTAL INSTITUT FOR CLIMATE, ENVIRONMENT AND ENERGY'], 'entity:T: (49-202)249-2135 (49-202)2-4920 F: (49-', ['Mr. Reinhard LOSKE', 'Doppersberg 19', 'Postfach 10 04 80', 'D-42103 Wuppertal Germany', '202)249-2108', 'Email:']]
persons_data = filter_strings(persons_data)
# print(persons_data)
for kd in persons_data:
    print(kd)
