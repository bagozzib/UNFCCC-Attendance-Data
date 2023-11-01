import pdfplumber
import re
from inputs_file import pdf_path, title_match_pattern

def should_add_bold_prefix(text):
    """ Some lines should not be prefixed with 'Entity:' keyword even though, they are in bold letters """
    # pattern = r'^[-+]?\d*\.?\d+$'
    # if text.startswith("FCCC/CP") or re.match(pattern, text):
    #     return False

    return True

def merge_entity_lines(groups):
    """ if the entity text is split across lines,  merge them """
    merged_groups = []
    i = 0
    while i < len(groups):
        group = groups[i]
        if group[0].startswith("Entity:") and len(group) > 1:
            merged_group = [" ".join(group)]
            merged_group = [merged_group[0].replace(' Entity:', ' ')]
            merged_groups.append(merged_group)
        else:
            merged_groups.append(group)
        i += 1

    return merged_groups

def old_extract_with_bold_annotations(pdf_path):
    """ Extract the data from the pdf and add 'Entity:' to the bold data """
    nested_lines_with_bold = []
    current_group = []
    current_line = ""
    last_y = None
    last_char_height = 0
    previous_y = float('inf')
    add_bold_prefix = False

    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            chars = page.chars

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
                # if last_y is not None and ch["top"] > (last_y + last_char_height):
                    if current_line:
                        current_group.append(current_line.strip())
                        current_line = ""
                    # Check for a larger than usual gap between lines
                    if ch["top"] - last_y > 1.39 * last_char_height:
                        if current_group:
                            nested_lines_with_bold.append(current_group)
                            current_group = []
                # print('-'*30)
                # Check if text should be prefixed with bold
                # if ch["height"] == '12' and not current_line.startswith("Entity:") and should_add_bold_prefix(ch["text"]):
                #     current_line += "Entity:"

                current_line += ch["text"]
                last_y = ch["top"]
                last_char_height = ch["height"]
                previous_y = last_y  # Store the last y-coordinate for next iteration's comparison

            # Ensure the last line and group are added
            if current_line:
                current_group.append(current_line.strip())
            if current_group:
                nested_lines_with_bold.append(current_group)
                current_group = []

    nested_lines_with_bold = merge_entity_lines(nested_lines_with_bold)
    return nested_lines_with_bold

import pdfplumber
import re

person_titles_regex = r"(Mr\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |M\ |On\ |M\ |Fr\ |Rev\ |Mme\ |Sr\ |Msgr\ |On\.\s*|Fr\.\s*|Rev\.\s*|H\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |Sra\.\s*|Mme|Sr\.\s*|Msgr\.\s*))?|Msgr\.\s*|Mrs\.\s*|Sra\.\s*|Sr\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme|Ms|S\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Mme|Mr|Ms|Dr|Msgr\.\s*|M\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |M\ |Sra\.\s*|Sr\.\s*))?)"

def extract_with_bold_annotations(pdf_path):
    data_groups = []
    current_group = []
    current_person_data = []

    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            lines = page.extract_text().split('\n')

            for line in lines:
                if re.match(person_titles_regex, line):
                    if current_person_data:
                        current_group.append(current_person_data)
                    current_person_data = [line]
                else:
                    current_person_data.append(line)

            if current_group:
                data_groups.append(current_group)
                current_group = []

    return data_groups

def clean_item(item):
    """ conditions to clean the data """
    output_items = []

        # Check for the presence of 'FCCC' and two occurrences of 'Entity:'
    if 'FCCC' in item and len(re.findall(r'Entity:', item)) == 2:
        item = item.split('Entity:', 2)[-1]  # Keep everything after the second 'Entity:'

        # Check for the presence of two 'Entity:' and absence of 'FCCC'
    elif len(re.findall(r'Entity:', item)) == 2 and 'FCCC' not in item:
        item = item.replace('Entity:', '')

        # Check for one 'Entity:' not at the start
    elif item.find('Entity:') > 0:
        prefix, entity_content = item.split('Entity:', 1)

        if prefix.strip():
            output_items.append(prefix.strip())

        item = 'Entity:' + entity_content.strip()

    # The remaining standard cleaning
    item = re.sub(r'Entity:\s*\d+\s*', '', item)  # Remove 'bold' + numbers
    item = re.sub(r'Entity: FCCC/[\w/]+', '', item)  # Remove 'Entity: FCCC/CP/2014/INF.2' pattern
    output_items.append(item.strip())

    return output_items

def clean_list(lst):
    cleaned = []
    for item in lst:
        cleaned_items = clean_item(item)
        if len(cleaned_items) == 2 and 'Entity:' not in cleaned_items[0] and 'Entity:' in cleaned_items[1]:
            output_list = [[item] for item in cleaned_items]
            return output_list

        cleaned.extend(cleaned_items)  # We use extend because clean_item returns a list
    return cleaned if cleaned else None

# This contains the list of pdf extracted data.
# grouped_output = extract_with_bold_annotations(pdf_path)

grouped_output = extract_with_bold_annotations("C:\\Users\\rakes\\Music\\pdfconverted image files\\1995_COP_1_Berlin.pdf")

def remove_text_patterns(grouped_output):
    # *******************************************
    # for each pdf, the starting of the each page contains differnt things like 'page numbers' or 'FCCC/CP' like that,
    # we will carefully analyze and remove them
    # pattern to remove the row containing FCCC and its previous row
    pattern = re.compile(r'^.*\bFCCC/CP/\b.*$')

    # pattern to remove the row containing page numbers and its previous row
    # pattern = re.compile(r"^\d+$")

    # be careful with this line to remove the pattern containing fccc and the nest line
    for index, line in enumerate(grouped_output):
        if pattern.match(line[0]):  # assuming the regex should match the first item in the sublist
            if index > 0:  # to ensure there's a line before it
                del grouped_output[index]
                del grouped_output[index - 1]
            else:
                del grouped_output[index]

    return grouped_output

grouped_output = remove_text_patterns(grouped_output)

# out_list = []
# for d in grouped_output:
#     if d[0] and '(continued)' not in d[0] and 'Page' not in d[0] and '*' not in d[0]:
#         in_list = []
#         for each_inner_item in d:
#             if '' == each_inner_item:
#                 out_list.append(in_list)
#                 in_list = []
#             else:
#                 in_list.append(each_inner_item)
#
#         out_list.append(in_list)
#
# out_list1 = []
# for input_list in out_list:
#     entity_list = []
#     non_entity_list = []
#     current_entity = []
#
#     for item in input_list:
#         if item.startswith('Entity:'):
#             if item != 'Entity:':
#                 current_entity.append(item)
#         else:
#             if current_entity:
#                 entity_list.append('Entity:' + ' '.join(current_entity).replace('Entity:', ''))
#                 current_entity = []
#             non_entity_list.append(item)
#
#     # Append the last entity group if it ends with 'Entity:'
#     if current_entity:
#         entity_list.append('Entity:' + ' '.join(current_entity).replace('Entity:', ''))
#
#     out_list1.append(entity_list)
#     out_list1.append(non_entity_list)
#
# for kk in out_list1:
#     if kk:
#         print(kk)



for d in grouped_output:
    if d[0] and '(continued)' not in d[0] and d[0]!='Entity:':
            # and 'Page' not in d[0]\
            # and '*' not in d[0]\

        print(d)
