# -*- coding: utf-8 -*-

# job titles and department titles lemitization and comparision is not working properly
# teh text is not divided by '-', this may be because of the space also before the word as Daria Suggested.
# Energy Unit why this has been classified as Affiliation
# Planning and Administration Department in Affiliation
# Environment Policy Division

# new additions
# misclasfication in job title
# Windwärts Energie
# Nepal Trade Union Congress (NTCUC)
# Bangladesh Free Trade Union (BFUC)
# European Trade Union Confederation
# Clean Development Mechanism and Joint Implementation Markets
#International Confederation of Free Trade Unions
#Fisheries Joint Management Committee Student Mentoring Program
#
#
#
#
#
#
#
#
#
#
#
#
#
#
"""
add new column after job title that says translated text to english, so easy for segregating
mark the columns that contains division or company names as red or even commas or 'The' or length greater than 30
"""

import pandas as pd
import spacy
from spacy.lang.en.stop_words import STOP_WORDS
from spacy.matcher import Matcher
from nltk.corpus import stopwords
from inputs_file import *
import re
from docx import Document
from langdetect import detect
from flair.models import SequenceTagger
from flair.data import Sentence

# Load stop words
stop_words = set(stopwords.words('english'))
# Load the English NLP model
nlp = spacy.load("en_core_web_lg")
matcher = Matcher(nlp.vocab)

# Pattern for organization names
pattern = [{"LABEL": "ORG", "OP": "+"}]

pattern = re.compile(
    r'(Mr\.\s*|Mrs\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme\ \s*|S\.E\.\s*|Sr\.\s*|Sra\.\s*|H\. E\.\s*|H\.E\.\s*)')


class get_manual_strings(object):
    def __init__(self, file_path):
        self.file_path = file_path

    def is_valid_org(self, ent):
        """
        Check if the token before the detected entity is not a stop word.
        """
        prev_token = ent.doc[ent.start - 1] if ent.start > 0 else None
        if prev_token and prev_token.is_stop:
            return False
        return True

    def print_docx_contents(self, file_path):
        doc = Document(file_path)
        clean_text_list = []

        for para in doc.paragraphs:
            line = para.text.strip()

            if re.search(
                    r"(Mr\.\s*|Mrs\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme\ \s*|S\.E\.\s*|Sr\.\s*|Sra\.\s*|H\. E\.\s*|H\.E\.\s*)",
                    line):
                continue

            if ' ' not in line:
                continue

            if re.match(r"^\d+$", line.replace(" ", "")):
                continue

            clean_text_list.append(line)

        return clean_text_list

    def split_on_organization(self, text):
        # Check if 'ministry' is in the sentence
        if 'ministry' in text.lower():
            # Split the text at the word 'ministry', retaining 'ministry' with the second portion
            split_points = re.split(r'(?<=\s)(?=\bministry\b)', text, flags=re.IGNORECASE, maxsplit=1)

            # Cleaning up the results
            return [s.strip() for s in split_points if s]
        else:
            return [text]

    def run(self):
        # Use the function
        file_path = self.file_path
        clean_text_list = self.print_docx_contents(file_path)
        text_set = set(clean_text_list)

        # Iterate through the texts
        for text in clean_text_list:
            splits = self.split_on_organization(text)

            # If the text was split into more than one string
            if len(splits) > 1:
                # Remove the original text from the set
                text_set.discard(text)

                # Add the split strings to the set
                for split_text in splits:
                    text_set.add(split_text)

        return text_set


class SegregateData(object):
    def __init__(self, data_list, department_names, profession_names_set):
        self.data = data_list
        self.department_names = department_names
        self.profession_names_set = profession_names_set
        self.nlp_en = spacy.load("en_core_web_lg")
        self.flair_tagger = SequenceTagger.load('ner')

    def check_organization(self, text):
        # Detect the language of the text
        detected_language = 'en'
        try:
            detected_language = detect(text)
        except:
            pass

        # If not English, translate it
        if detected_language != "en":
            text = self.translate_text(text, detected_language)

        # Using Spacy
        org_doc_en = self.nlp_en(text)
        en_orgs = [ent.text for ent in org_doc_en.ents if ent.label_ == "ORG"]

        # Using Flair
        sentence = Sentence(text)
        self.flair_tagger.predict(sentence)
        flair_orgs = [entity.text for entity in sentence.get_spans('ner') if entity.tag == 'ORG']

        if en_orgs and flair_orgs:
            return "true"
        elif not en_orgs and not flair_orgs:
            return "false"
        else:
            return "partial_true"

    def check_person(self, text):
        # Detect the language of the text
        detected_language = 'en'
        try:
            detected_language = detect(text)
        except:
            pass

        # If not English, translate it
        if detected_language != "en":
            text = self.translate_text(text, detected_language)

        # Using Spacy
        org_doc_en = self.nlp_en(text)
        en_orgs = [ent.text for ent in org_doc_en.ents if ent.label_ == "PERSON"]

        # Using Flair
        sentence = Sentence(text)
        self.flair_tagger.predict(sentence)
        flair_orgs = [entity.text for entity in sentence.get_spans('ner') if entity.tag == 'PER']

        if en_orgs and flair_orgs:
            return "true"
        elif not en_orgs and not flair_orgs:
            return "false"
        else:
            return "partial_true"

    def _contains_department_name(self, text):
        # Detect the language of the text
        detected_language = 'en'
        try:
            detected_language = detect(text)
        except:
            pass

        # If not English, translate it
        if detected_language != "en":
            text = self.translate_text(text, detected_language)

        # Lemmatization and filtering of words
        lemmatized_words = {word.lemma_.lower().replace(" ", "") for word in self.nlp_en(text) if
                            word.is_alpha and word.text.lower() not in spacy.lang.en.stop_words.STOP_WORDS}

        # Using set intersection to find matching department names
        matching_words = lemmatized_words.intersection(self.department_names)

        return bool(matching_words)

    def translate_text(self, text, from_code):
        translated_text = text
        try:
            to_code = "en"
            # translated_text = argostranslate.translate.translate(text, from_code, to_code)
        except:
            print('translation failed for text', text)
        return translated_text

    def _is_profession(self, text):
        text = text.replace('-', '')
        words = {word.lemma_.lower().replace(" ", "") for word in self.nlp_en(text) if
                 word.is_alpha and word.text.lower() not in spacy.lang.en.stop_words.STOP_WORDS}

        if not words.intersection(self.profession_names_set):
            try:
                detected_language = detect(text)
            except:
                detected_language = 'en'

            if detected_language != "en":
                translated_text = self.translate_text(text, detected_language)
                words = {word.lemma_.lower().replace(" ", "") for word in self.nlp_en(translated_text) if
                         word.is_alpha and word.text.lower() not in spacy.lang.en.stop_words.STOP_WORDS}

        return bool(words.intersection(self.profession_names_set))

    def extract_information(self):
        current_group = ""
        current_entity = ""
        results = []

        title_pattern = re.compile(
            r"(Mr\.\s*|Mrs\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme\ \s*|S\.E\.\s*|Sr\.\s*|Sra\.\s*)")

        for item in self.data:
            # Update group or entity if found
            if "Group:" in item[0]:
                current_group = item[0].replace("Group:", "").strip()
                continue
            elif "Entity:" in item[0]:
                current_entity = item[0].replace("Entity:", "").strip()
                continue

            # Extract title
            title_match = title_pattern.search(item[0])
            title = title_match.group().strip() if title_match else ""
            name = item[0].replace(title, "").strip() if title_match else item[0]

            doubt, division_doubt, company_doubt, job_title, division, affiliation, extras = "", "", "", "", "", "", ""
            remaining_items = item[1:]

            # Handling job_title based on your original conditions
            if len(remaining_items) >= 3:
                if self._is_profession(remaining_items[0]):
                    job_title, division, affiliation = remaining_items[:3]
                    extras = " ".join(remaining_items[3:])
                else:
                    try:
                        doubt += " " + remaining_items[0]
                        if self._is_profession(remaining_items[1]):
                            job_title = remaining_items[1]
                            org_check_div_or_aff = self.check_organization(remaining_items[2])

                            if org_check_div_or_aff == 'true' or remaining_items[2].lower() in affiliation_words:
                                affiliation = remaining_items[2]
                            else:
                                division = remaining_items[2]

                            extras = " ".join(remaining_items[3:])
                        else:
                            doubt += " " + remaining_items[0]
                            division_doubt, company_doubt = remaining_items[1:3]
                            extras = " ".join(remaining_items[3:])
                    except Exception as e:
                        import ipdb;ipdb.set_trace()
                        print('segregation not properly happend, because of', e)

            elif len(remaining_items) == 2:
                if self._is_profession(remaining_items[0]):
                    job_title = remaining_items[0]

                    org_check_div_or_aff = self.check_organization(remaining_items[1])
                    if org_check_div_or_aff == 'true':
                        if self._contains_department_name(remaining_items[1]) and not remaining_items[1] in affiliation_words:
                            division = remaining_items[1]
                        else:
                            affiliation = remaining_items[1]
                    elif org_check_div_or_aff == 'partial_true':
                        company_doubt = remaining_items[1]
                    else:
                        if self._contains_department_name(remaining_items[1]):
                            division = remaining_items[1]
                        else:
                            division_doubt = remaining_items[1]

                elif self._is_profession(remaining_items[1]):
                    doubt += " " + remaining_items[0]
                    job_title = remaining_items[1]
                else:
                    org_check_div_or_aff = self.check_organization(remaining_items[0])
                    if org_check_div_or_aff == 'true':
                        if self._contains_department_name(remaining_items[0]) and remaining_items[1] in affiliation_words:
                            division = remaining_items[0]
                            affiliation = remaining_items[1]
                        else:
                            affiliation = remaining_items[0]
                            extras = remaining_items[1]

                    elif org_check_div_or_aff == 'partial_true':
                        company_doubt = remaining_items[0]
                        extras = remaining_items[1]
                    else:
                        if self._contains_department_name(remaining_items[0]):
                            division = remaining_items[0]
                            company_doubt = remaining_items[1]
                        else:
                            if self.check_person(remaining_items[0]):
                                doubt += " " + remaining_items[0]
                                division_doubt = remaining_items[1]
                            else:
                                division_doubt = remaining_items[0]
                                company_doubt = remaining_items[1]

            elif len(remaining_items) == 1:
                if self._is_profession(remaining_items[0]):
                    job_title = remaining_items[0]
                else:
                    org_check_div_or_aff = self.check_organization(remaining_items[0])
                    if org_check_div_or_aff == 'true':
                        if self._contains_department_name(remaining_items[0]):
                            division = remaining_items[0]
                        else:
                            affiliation = remaining_items[0]

                    elif org_check_div_or_aff == 'partial_true':
                        company_doubt = remaining_items[0]
                    else:
                        if self._contains_department_name(remaining_items[0]):
                            division = remaining_items[0]
                        else:
                            if self.check_person(remaining_items[0]):
                                doubt += " " + remaining_items[0]
                            else:
                                division_doubt = remaining_items[0]
            if not affiliation and (division_doubt or company_doubt):
                affiliation_val = division_doubt if division_doubt else company_doubt
                affiliation = 'color:red' + affiliation_val

            results.append(
                [current_group, current_entity, title, name, doubt, job_title, division, affiliation, extras])

        return results


def join_based_on_stopwords(data):
    merged_data = []

    for inner_list in data:
        i = 1
        while i < len(inner_list) - 1:
            current_str_last_word = inner_list[i].split()[-1]
            next_str_first_word = inner_list[i + 1].split()[0]
            end_patterns = ["'s", "'re", "'ve", "'ll", "'d", "n't", ",", "-"]

            # Check the conditions for merging the strings
            if (current_str_last_word.lower() in stop_words or
                    next_str_first_word.lower() in stop_words or
                    next_str_first_word[0].islower() or
                    any(current_str_last_word.endswith(pattern) for pattern in end_patterns) or
                    current_str_last_word.endswith('&') or
                    next_str_first_word.startswith('&')):
                merged_string = inner_list[i] + " " + inner_list[i + 1]
                inner_list[i] = merged_string
                del inner_list[i + 1]
            else:
                i += 1

        merged_data.append(inner_list)

    return merged_data

def merge_strings_based_on_set(full_strings_list, normalized_strings_set):
    result = []

    for strings_list in full_strings_list:
        i = 0
        while i < len(strings_list) - 1:
            combined_str = strings_list[i] + " " + strings_list[i + 1]
            normalized_combined_str = combined_str.replace(" ", "").lower()

            if normalized_combined_str in normalized_strings_set:
                strings_list[i] = combined_str
                del strings_list[i + 1]
            else:
                i += 1
        result.append(strings_list)

    return result

def lemmatize_professions(professions):
    lemmatized_set = set()
    for profession in professions:
        doc = nlp(profession)
        lemmatized_profession = ''.join(
            [token.lemma_ if token.lemma_ != '-PRON-' else token.text for token in doc]).lower().replace(" ", "")
        lemmatized_set.add(lemmatized_profession)

    return lemmatized_set

processed_data = join_based_on_stopwords(ip_data)
manual_strs = get_manual_strings(docx_file_path).run()
normalized_strings = {s.replace(" ", "").lower() for s in manual_strs}

processed_data = merge_strings_based_on_set(processed_data, normalized_strings)
segregate_data_obj = SegregateData(processed_data, department_names_set, profession_names_lemmatized_set)
output = segregate_data_obj.extract_information()

df = pd.DataFrame(output, columns=output_excel_column_names)
excel_writer = pd.ExcelWriter(output_excel_file_path, engine='xlsxwriter')

df.to_excel(excel_writer, sheet_name='Sheet1', index=False)

# Get the xlsxwriter workbook and worksheet objects
workbook = excel_writer.book
worksheet = excel_writer.sheets['Sheet1']

# Define a format with red text color
red_text_format = workbook.add_format({'color': 'red'})

for row_num, value in enumerate(df['Affiliation'], start=2):
    if 'color:red' in value:
        worksheet.write(f'H{row_num}', value, red_text_format)

excel_writer.close()
