import pdfplumber
from inputs_file import title_match_pattern

class ExtractImageMultipleColumnsPDF():
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


file_path = "C:\\Users\\emuru\\Downloads\\1994_INC_10_Geneva.pdf"
all_pages_data = ExtractMultipleColumnsPDF(file_path, 275).run()
