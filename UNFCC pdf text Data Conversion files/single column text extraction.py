# to remove

# ['k']
# ['kk']
# ['x']
# ['x x']
# ['kk H.E. M. G. REITANO', 'Ambassador,', 'Ministry of Foreign Affairs']
# ['Fk kx Mr. Barry MAWHINNEY', 'Legal Advisor,', 'Department of External Affairs', 'and International Trade']
# ['kk H.E. Mr. Vicente SANCHEZ', 'Ambassador to Kenya and', 'Permanent Representative to', 'UNEP and HABITAT']
# ['Mr. Shaheen A. GILLANI']
# ['Counsellor']
# ['# Mg. Jill BARRETT', 'Foreign and Commonwealth Office']

import pdfplumber
import re
title_match_pattern = re.compile(
    r"(Mr\.\s*|S\$ra\.|HE\ Mr\.|HE\ Mr\.|S\:E\.\ Sra\.|HE\ Mr|St\.|1LE\ Mr\.|Mr\_|H\.RH\.|SE\.|HE\.|\$\.E\.|SE\.Sr\.|HLE\.|HEMr\.|ILE\.\ Mr\.|HE\.\ Ms\.|HE\.\ Mr\.|H\.R\.H\.\s*|Mx\.\s*|H\.H\.\s*|Ind\.\s*|His\ |Ind\ |Ms\ |Mr\ |Sra\ |Sr\ |M\ |On\ |M\ |Fr\ |H\.O\.\s*|Rev\ |Mme\ |Sr\ |Msgr\ |On\.\s*|Fr\.\s*|Rev\.\s*|H\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |Sra\.\s*|Mme|Sr\.\s*|Msgr\.\s*))?|Msgr\.\s*|Mrs\.\s*|Sra\.\s*|Sr\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme|Ms|S\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Mme|Mr|Ms|Dr|Msgr\.\s*|M\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |M\ |Sra\.\s*|Sr\.\s*))?)")

def extract_text_and_process(pdf_path, space_threshold=1.5):
    """
    Extracts text from a PDF, groups lines based on vertical spacing, removes specific characters,
    and processes uppercase text by prepending 'Entity:'.

    :param pdf_path: Path to the PDF file.
    :param space_threshold: The threshold for vertical spacing to start a new group.
    :return: Nested list of processed text groups.
    """
    nested_lines = []
    current_group = []
    last_y = None
    last_char_height = None

    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            chars = page.chars

            for ch in chars:
                # Start a new line group if the vertical space exceeds the threshold
                if last_y is not None and (ch["top"] > last_y + space_threshold * last_char_height):
                    if current_group:
                        nested_lines.append(current_group)
                        current_group = []

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
                current_group = []

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

# Replace with your specific PDF file path
pdf_path = "C:\\Users\\rakes\\Downloads\\1991_INC_3_Nairobi.pdf"
processed_extracted_lines = extract_text_and_process(pdf_path)

new_processed_words_list = []

for each_person_data in processed_extracted_lines:
    new_list = []
    if not title_match_pattern.match(each_person_data[0]):
        new_processed_words_list.append(each_person_data)
        continue
    for each_str_item in each_person_data:
        if title_match_pattern.match(each_str_item):
            new_list.append([each_str_item])
        else:
            new_list[-1].append(each_str_item)

    new_processed_words_list.extend(new_list)

pattern = re.compile(r"^\d+$")
# Display the processed internal list
for sublist in new_processed_words_list:
    if not pattern.match(sublist[0]) and sublist[0] and '(continued)' not in sublist[0]:
        print(sublist)
