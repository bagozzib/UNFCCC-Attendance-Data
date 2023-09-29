import pdfplumber
import re

def should_add_bold_prefix(text):
    pattern = r'^[-+]?\d*\.?\d+$'
    if text.startswith("FCCC/CP") or re.match(pattern, text):
        return False
    return True

def merge_entity_lines(groups):
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

def extract_with_bold_annotations(pdf_path):
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
                if last_y is not None and ch["top"] > (last_y + last_char_height):
                    if current_line:
                        current_group.append(current_line.strip())
                        current_line = ""
                    # Check for a larger than usual gap between lines
                    if ch["top"] - last_y > 2 * last_char_height:
                        if current_group:
                            nested_lines_with_bold.append(current_group)
                            current_group = []

                # Check if text should be prefixed with bold
                if "Bold" in ch["fontname"] and not current_line.startswith("Entity:") and should_add_bold_prefix(ch["text"]):
                    current_line += "Entity:"

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

def split_data(lst):
    current = []
    for item in lst:
        if item.startswith('Entity:') or re.match(r'(Mr\.\s*|Mrs\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme\ \s*|S\.E\.\s*|Sr\.\s*|Sra\.\s*|H\. E\.\s*|H\.E\.\s*)', item):

            if current:
                yield current
                current = []
        current.append(item)
    if current:
        yield current

def clean_item(item):
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


pdf_path = "C:\\Users\\rakes\\Downloads\\2005_COP_11_Montreal_Part_2_Final (2).pdf"
grouped_output = extract_with_bold_annotations(pdf_path)
pattern = re.compile(r'^.*\bFCCC/CP/2005/INF.2\b.*$')

# be careful with this line
for index, line in enumerate(grouped_output):
    if pattern.match(line[0]):  # assuming the regex should match the first item in the sublist
        if index > 0:  # to ensure there's a line before it
            del grouped_output[index]
            del grouped_output[index - 1]
        else:
            del grouped_output[index]

for d in grouped_output:
    if d[0] and '(continued)' not in d[0]:
        print(d)
