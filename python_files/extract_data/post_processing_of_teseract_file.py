# split the strings, so each person comes into a list
# does all of the work

import re

title_match_pattern = re.compile(
    r"(Mr\.\s*|S\$ra\.|HE\ Mr\.|HE\ Mr\.|S\:E\.\ Sra\.|HE\ Mr|St\.|1LE\ Mr\.|Mr\_|H\.RH\.|SE\.|HE\.|\$\.E\.|SE\.Sr\.|HLE\.|HEMr\.|ILE\.\ Mr\.|HE\.\ Ms\.|HE\.\ Mr\.|H\.R\.H\.\s*|Mx\.\s*|H\.H\.\s*|Ind\.\s*|His\ |Ind\ |Ms\ |Mr\ |Sra\ |Sr\ |M\ |On\ |M\ |Fr\ |H\.O\.\s*|Rev\ |Mme\ |Sr\ |Msgr\ |On\.\s*|Fr\.\s*|Rev\.\s*|H\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |Sra\.\s*|Mme|Sr\.\s*|Msgr\.\s*))?|Msgr\.\s*|Mrs\.\s*|Sra\.\s*|Sr\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme|Ms|S\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Mme|Mr|Ms|Dr|Msgr\.\s*|M\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |M\ |Sra\.\s*|Sr\.\s*))?)")

# give teh input list
input_list = []

output_list = []
current_title_group = []

kk = []
for item in input_list:

    if not item.isupper() or 'suite' in item or 'continued' in item:
        if title_match_pattern.search(item):
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

input_list = output_list.copy()

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

    # if any(c.isdigit() for c in lower_string):
    #     print(lower_string)
    #     print('__________')

    return (any(c.isdigit() for c in lower_string) or
            'email' in lower_string or 'T:' in lower_string or
            'P.O.' in lower_string or 'F:' in lower_string)

# persons_data = [['Mr. Stephan SINGER', '‘WWF - Germany', 'Hellderichstr. 115', 'D-60591 Frankfurt Germany', 'Email:'], 'entity:T: (31-3404)3-7326 F: (31-3404)1-2064', ['Mr. Meindert BROUWER', 'Postbus 7', 'NL-3700 AA Zeist Netherlands', 'Email:'], 'entity:T: (41-22)364-9111 F: (41-22)364-4238', ['Mr. Andrew KERR', 'Avenue du Mont-Blanc', 'CH-1196 Gland Switzerland', 'Email: wwfgland@gn.apc.org'], 'entity:T: (41-22)364-9111 F: (41-22)364-4238', ['Mr. Sacha FRIEDLI', 'Avenue du Mont-Blanc', 'CH-1196 Gland Switzerland', 'Email: wwfgland@gn.apc.org'], ['entity:WUPPERTAL INSTITUT FOR CLIMATE, ENVIRONMENT AND ENERGY'], 'entity:T: (49-202)249-2135 (49-202)2-4920 F: (49-', ['Mr. Reinhard LOSKE', 'Doppersberg 19', 'Postfach 10 04 80', 'D-42103 Wuppertal Germany', '202)249-2108', 'Email:']]
persons_data = filter_strings(input_list)

# Output list
output_list = []

# Iterate through the input lists
for inner_list in persons_data:
    new_lst = []
    for each_str in inner_list:
        # Split the string using commas and strip extra whitespaces
        split_strings = [part.strip() for part in each_str.split(',')]

        new_lst.extend(split_strings)

    # Append the split strings as a new list to the output list
    output_list.append(new_lst)

# # Print the result
# for k in output_list:
#     print(k)


data = output_list.copy()

op = []
for inner_list in data:
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
        # print(kk[k+1][0])
        op[k].pop(-1)


# persons_data = []

# uncomment this
for j in op:
    if j and j[0]:
        # persons_data.append(j)
        print(j)


# give the list
ip_list = []

person_details = []

for k in ip_list:
    if not 'Entity' in k[0] and not 'Group' in k[0]:
        j = k[0]
        j = j.split()[::-1]
        surname_idx = [idx for idx in range(len(j)) if j[idx].isupper()][0]
        k.pop(0)
        k.insert(0, ' '.join(j[surname_idx:][::-1]))
        k.insert(1, ' '.join(j[:surname_idx][::-1]))

    person_details.append(k)

for each_person in person_details:
    each_person = [person_detail for person_detail in each_person if person_detail]
    print(each_person)
