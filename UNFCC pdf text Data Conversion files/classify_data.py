# -*- coding: utf-8 -*-

import warnings
warnings.filterwarnings('ignore')

# all the packages needed
import pandas as pd
import argostranslate.package
import argostranslate.translate
import argostranslate
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

# define the global pattern, that matches with person names, there is another local variable 'title_pattern',
# which doesnt match some titles like 'H.E' e.t.c, because we are keeping Mr. in title and H.E. in person name
# change the 'title_pattern' local variable to change this behaviour
# title_match_pattern = re.compile(r"(Mr\.|H\.E(?:\.\s*(?:Ms.|Mr.|Ms|Mr|))?|H\.E(?:\.\s*(?:Sra|Sr))?|Mrs\.|Ms\.|Dr\.|Prof\.|M\.|Mme|S\.E(?:\.\s*(?:Sra|Sr))?|Sr\.|Sra\.)")
title_match_pattern = re.compile(
    r"(Mr\.\s*|H\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |Sra\.\s*|Mme|Sr\.\s*))?|Mrs\.\s*|Sra\.\s*|Sr\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme|Ms|S\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Mme|M\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |M\ |Sra\.\s*|Sr\.\s*))?)")


class get_manual_strings(object):
    """ get the strings that are joined by placing the text in multiple columns to a single column from the word file
        - from the docx file, we are ignoring all the lines that start with a person title like 'Mr, Mrs ..'
        - we are ignoring all the lines that starts with a stop word, because code is present to merge the words
        - that are ending or starting with stop words or other pattern.
    """

    def __init__(self, file_path):
        # path of the word file, in which the data is in a single column instead of 3 columns
        self.file_path = file_path

    def is_valid_org(self, ent):
        """
        Check if the token before the detected entity is not a stop word.
        """
        prev_token = ent.doc[ent.start - 1] if ent.start > 0 else None
        if prev_token and prev_token.is_stop:
            return False

        return True

    def ignore_specific_lines_in_docx(self):
        """ function to ignore the lines that are starting with a title, as we dont need to join them, as name won't be much long usually """

        doc = Document(self.file_path)
        clean_text_list = []

        for para in doc.paragraphs:
            line = para.text.strip()

            if title_match_pattern.search(line):
                continue

            if ' ' not in line:
                continue

            if re.match(r"^\d+$", line.replace(" ", "")):
                continue

            clean_text_list.append(line)

        return clean_text_list

    def split_on_organization(self, text):
        # Check if 'ministry' is in the sentence and make it as a new item.

        if 'ministry' in text.lower():
            # Split the text at the word 'ministry', retaining 'ministry' with the second portion
            split_points = re.split(r'(?<=\s)(?=\bministry\b)', text, flags=re.IGNORECASE, maxsplit=1)

            # Cleaning up the results
            return [s.strip() for s in split_points if s]
        else:
            return [text]

    def run(self):
        """ get the data from the word file and ignore specific lines """
        clean_text_list = self.ignore_specific_lines_in_docx()
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
    """ CLassify the data to Group, Entity, Title, Name, Name doubt, English Translated Job Title, Job Title,
                        English Translated Division, Division, English Translated Affiliation, Affiliation, Extras """

    def __init__(self, data_list, department_names, profession_names_set, affiliation_names, separator_items,
                 merged_docx_strings):

        # the data from the pdf file which is like a list of lists
        # ex: [['Group:PARTIES'],
        # ['Entity:Albania'],
        # ['Mr. Besnik BARAJ', 'Deputy Minister of Environment', 'Ministry of Environment']]
        self.data = data_list

        # the set of names, that are basis of classification
        self.department_names = department_names
        self.department_names = department_names
        self.affiliation_names = affiliation_names
        self.profession_names_set = profession_names_set

        # patterns that are majorly identified to divide the merged Items:
        # for ex: 'Director The Trendlyne Team', 'The' is the seperator item
        self.separator_items = separator_items
        # the list of strings that are merged, converted to lower case after removing spaces useful to merge the split words
        # ex: merged_docx_strings is like: 'industrialunioncongressofsouthafricantradeunions'
        self.merged_docx_strings = merged_docx_strings
        # load the spacy large english package to identify the organization(Affiliation) Names
        self.nlp_en = spacy.load("en_core_web_lg")
        # load the Flair large english package to identify the organization(Affiliation) Names
        self.flair_tagger = SequenceTagger.load('ner')
        # add the characters to be removed from the data
        self.special_chars_list = ['*']
        # the replacement string to be used inplace of special_chars_list items
        self.replacement_string = ''
        # the minimum str length to mege the next single word to current text ex: ['str of len greater than 30', 'single word'] merged.
        self.min_previous_str_len_merge_single_wrd = 31

    def run(self):
        # in the pdf extracted data if we have words that are split across lines, combining them based on comparision 
        # of joined text from the word file.
        processed_data = self.join_split_job_titles()
        # remove the special characters like '*' or any other characters, to remove them declare them in self.special_chars_list above
        processed_data = self.replace_special_chars_in_list(processed_data)
        # classify the extracted data into columns
        classified_list_data = self.classify_extracted_data(processed_data)

        return classified_list_data

    def contains_affiliation_name(self, text):
        """ returns True if the text contains any word form the predefined Affiliation names """

        # converting to lower case, lemmatizating and also removing the stop words
        lemmatized_words = {word.lemma_.lower().replace(" ", "") for word in self.nlp_en(text) if
                            word.is_alpha and word.text.lower() not in spacy.lang.en.stop_words.STOP_WORDS}

        # Using set intersection to find matching department names
        matching_words = lemmatized_words.intersection(self.affiliation_names)

        return bool(matching_words)

    def check_affiliation(self, text):
        """ check if the text is an affiliation name or not by using spacy and flair packages.
            - "true" if both packages classify the text as an organization.
            - "false" if both packages doesnot classify the text as an organization.
            - "partial_true" if only any one of the package classify the text as an organization.
         """
        # if the text contains a division name, which is in our declared division names list, then it is a division.
        if self.contains_division_name(text):
            return "false"

        # remove the '-' just for comparision, as it is making difficult for the verification.
        text = text.replace('-', '')

        # detect the language of the text, default is english.
        detected_language = 'en'
        try:
            detected_language = detect(text)
        except:
            pass

        # If not English, translate it to english, as spacy and flair packages are working best with english and also
        # english text translation api is working best
        if detected_language != "en":
            text = self.translate_text(text, detected_language)

        # check if the text contains the word Affiliation.
        if self.contains_affiliation_name(text):
            return "true"

        # check if the text contains organization by using Spacy
        org_doc_en = self.nlp_en(text)
        en_orgs = [ent.text for ent in org_doc_en.ents if ent.label_ == "ORG"]

        # check if the text contains organization by using Flair
        sentence = Sentence(text)
        self.flair_tagger.predict(sentence)
        flair_orgs = [entity.text for entity in sentence.get_spans('ner') if entity.tag == 'ORG']

        # - "true" if both packages classify the text as an organization.
        # - "false" if both packages doesnot classify the text as an organization.
        # - "partial_true" if only any one of the package classify the text as an organization.
        if en_orgs and flair_orgs:
            return "true"
        elif not en_orgs and not flair_orgs:
            return "false"
        else:
            return "partial_true"

    def check_person(self, text):
        """ check if the text contains person name by using the spacy and flair packages """
        if self.check_affiliation(text) == "true":
            return False

        text = text.replace('-', '')

        detected_language = 'en'

        try:
            detected_language = detect(text)
        except:
            pass

        # If not English, translate it to english
        if detected_language != "en":
            text = self.translate_text(text, detected_language)

        # check if the text contains organization by using Spacy
        org_doc_en = self.nlp_en(text)
        en_orgs = [ent.text for ent in org_doc_en.ents if ent.label_ == "PERSON"]

        # check if the text contains organization by using Flair
        sentence = Sentence(text)
        self.flair_tagger.predict(sentence)
        flair_orgs = [entity.text for entity in sentence.get_spans('ner') if entity.tag == 'PER']

        if en_orgs and flair_orgs:
            # return True if the text is classified as Person by both Spacy and Flair
            return True
        else:
            # return True if the text is classified as Person by both Spacy and Flair
            return False

    def contains_division_name(self, text):
        """ check if the text contains any division name that is present in our predefined division names list """
        if self.contains_affiliation_name(text):
            return False

        # detect the language, default is english
        detected_language = 'en'

        # replace the '-' in the text
        text = text.replace('-', '')

        try:
            detected_language = detect(text)
        except:
            pass

        # If not English, translate it to english
        if detected_language != "en":
            text = self.translate_text(text, detected_language)

        # Lemmatizating and removing stop words for comparision with our predefined words list
        lemmatized_words = {word.lemma_.lower().replace(" ", "") for word in self.nlp_en(text) if
                            word.is_alpha and word.text.lower() not in spacy.lang.en.stop_words.STOP_WORDS}

        # Using set intersection to find matching department names
        matching_words = lemmatized_words.intersection(self.department_names)

        return bool(matching_words)

    def translate_text(self, text, from_code=None):
        """ translate the given text from teh language with code 'from_code' to english """

        # detect the source code of the language, if it's not given.
        if not from_code:
            try:
                from_code = detect(text)
            except:
                from_code = 'en'

        # translate the text to english by using 'argostranslate' package API.
        translated_text = text
        try:
            to_code = "en"
            translated_text = argostranslate.translate.translate(text, from_code, to_code)
        except:
            # there are some words that are same across languages, translation is failed for such text.
            # print('translation failed for text', text)
            pass

        return translated_text

    def is_profession(self, text):
        """ check if the given text is a profession or not """

        # there are some words that are joined using '-' or '/'. we cant directly remove them or cant directly split
        # the text with these characters. So, we are checking by splitting the text and also by removing to check if they
        # contain any word related to profession.

        split_words_list = ['-', '/']

        for split_char in split_words_list:
            if split_char in text:
                split_words = text.split(split_char)

                if self.is_profession(split_words[0]) or self.is_profession(split_words[1]):
                    return True

                text = text.replace(split_char, '')

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

    def split_text_at_separator(self, input_data):
        # Initialize the output list
        output_data = []

        # Iterate through each item in the input data
        for item in input_data:
            # Split the item at the separator words
            parts = item.split()

            # Initialize variables to keep track of the current output item and preceding word
            current_output = ""
            preceding_word = None

            # Iterate through the parts of the item
            for idx, part in enumerate(parts):
                # Check if the current part is a separator word
                if part in self.separator_items:
                    if not preceding_word and idx != 0:
                        preceding_word = self.separator_items[idx - 1]

                    # If it's not preceded by a stop word, add the current output to the result
                    if preceding_word not in self.separator_items and preceding_word not in stop_words:
                        output_data.append(current_output)
                        current_output = ""
                    # Add the separator word to the result
                    current_output += part + " "
                else:
                    # Add the current part to the current output
                    current_output += part + " "

                # # Update the preceding word
                preceding_word = part

            # Add the last output item to the result
            output_data.append(current_output.strip())

        return output_data

    def split_data_with_comma(self, data_list):
        """ if there is a single comma in the given text, split the text at that comma position """
        for idx, data_item in enumerate(data_list):
            if '-' not in data_item:
                if self.is_profession(data_item) or self.contains_division_name(data_item):
                    comma_count = data_item.count(',')
                    if comma_count == 1:
                        split_items = data_item.split(',')
                        if self.is_profession(split_items[0]) and not self.is_profession(split_items[1]):
                            data_list[idx] = split_items[0]
                            data_list.insert(idx + 1, split_items[1])
                        elif self.contains_division_name(split_items[0]) and not self.contains_division_name(
                                split_items[1]):
                            data_list[idx] = split_items[0]
                            data_list.insert(idx + 1, split_items[1])

        return data_list

    def join_split_job_titles(self):
        """ if the job title is split across lines, joining them """
        result = []
        for strings_list in self.data:
            # checking this to not do for Entity or Group text.
            title_match = title_match_pattern.search(strings_list[0])
            if title_match:
                i = 1
                while i < len(strings_list) - 1:
                    if '-' in strings_list[i]:
                        i += 1
                        continue
                    # check if the text is a job title or not
                    profession_check = self.is_profession(strings_list[i])
                    # if the given text is a profession
                    if profession_check:
                        # checking if the next item in the list is also a job title or not
                        c1 = profession_check and self.is_profession(strings_list[i + 1])
                        # check if the next item is a division or not
                        contains_division_name = self.contains_division_name(strings_list[i + 1])
                        # check if the next item is an affiliation or not
                        contains_affiliation_name = True if self.check_affiliation(
                            strings_list[i + 1]) == 'true' else False

                        # if the current item is a profession and the next items is not either division or an Affiliation.
                        c2 = profession_check and not (
                                contains_division_name or contains_affiliation_name)

                        # if the current and next items are Job Titles or the current item is
                        # job title and the next items are not Division or Affiliation.
                        if c1 or c2:

                            combined_str = strings_list[i] + " " + strings_list[i + 1]

                            if ',' in combined_str or 'The' in combined_str:
                                i += 1
                                continue

                            normalized_combined_str = combined_str.replace(" ", "").lower()
                            # double checking if it is present in the merged_docx_strings
                            if normalized_combined_str in self.merged_docx_strings:
                                strings_list[i] = combined_str
                                del strings_list[i + 1]
                    else:
                        # if the current item is not a job title, then also checking if we can merge them by validating
                        # against self.merged_docx_strings
                        combined_str = strings_list[i] + " " + strings_list[i + 1]

                        normalized_combined_str = combined_str.replace(" ", "").lower()

                        if normalized_combined_str in self.merged_docx_strings:
                            strings_list[i] = combined_str
                            del strings_list[i + 1]

                    i += 1

            result.append(strings_list)

        return result

    def check_date_items(self, item):
        """ remove the items which are mistakenly entered as dates """
        # Check if there are more than one '/' characters in the item
        if re.search(r'\b\d{1,2}/\d{1,2}(/\d{2,4})?\b', item):
            return False

        return True

    def replace_special_chars_in_list(self, processed_data):
        """ there are some irrelevant and not useful special items, removing them """
        # replace the special chars like '*', to remove more add them to 'self.special_chars_list'
        # to change the replacement string add it to 'self.replacement_string'
        op_data_list = []

        for data_items_list in processed_data:
            # Escape special characters for safe use in a regex pattern
            escaped_chars = [re.escape(char) for char in self.special_chars_list]

            # Create a regex pattern that matches any of the escaped special characters
            pattern = '|'.join(escaped_chars)

            # Initialize a list to store the results
            result_list = []

            # Iterate through the input list and replace special characters in each string
            for input_string in data_items_list:
                # Use re.sub to replace matched characters with the replacement string
                result_string = re.sub(pattern, self.replacement_string, input_string)

                # Remove consecutive spaces resulting from consecutive special characters
                result_string = re.sub(' +', ' ', result_string)

                result_list.append(result_string)

            op_data_list.append(result_list)

        return op_data_list

    def handle_single_misclassified_words(self, name, doubt, job_title, division, affiliation, extras):
        """ there are single words that are split across lines, joining them """
        doubt = doubt.strip()
        extras = extras.strip()

        # checking for the single words
        doubt_words_len = len(doubt.split(' '))
        extras_words_len = len(extras.split(' '))

        if doubt_words_len == 1 and doubt:
            name += ' ' + doubt
            doubt = ''

        if extras_words_len == 1 and extras:
            affiliation += ' ' + extras
            extras = ''

        job_title, division, affiliation, extras = job_title.strip(), division.strip(), affiliation.strip(), extras.strip()
        division_words_list = division.split(' ') if division else []
        affiliation_words_list = affiliation.split(' ') if division else []
        extras_words_list = extras.split(' ') if division else []

        # if the current length of the line is greater than self.min_previous_str_len_merge_single_wrd characters, and the next is a single word,
        # then it is more probable to join the next single word to the current string.

        # checking for job title, division, Affiliation seperately.
        if len(division_words_list) == 1:
            if len(job_title) >= self.min_previous_str_len_merge_single_wrd:
                job_title += ' ' + division
                division, division_words_list = '', []
            else:
                if self.contains_division_name(affiliation):
                    division += ' ' + affiliation
                    affiliation = ''

                if self.contains_division_name(extras):
                    division += ' ' + extras
                    division = ''

        if len(division) >= self.min_previous_str_len_merge_single_wrd and len(affiliation_words_list) == 1:
            division += ' ' + affiliation
            affiliation, affiliation_words_list = '', []

        if len(affiliation) >= self.min_previous_str_len_merge_single_wrd and len(extras_words_list) == 1:
            affiliation += ' ' + extras
            extras, extras_words_list = '', []

        if not affiliation and extras:
            affiliation, extras = extras, ''

        return name, doubt, job_title, division, affiliation, extras

    def classify_extracted_data(self, processed_data):
        """ classify the extracted data to multiple columns """
        current_group = ""
        current_entity = ""
        results = []

        for item in processed_data:
            # remove the spaces
            item = [data.strip() for data in item]

            # Update group or entity if found
            if "Group:" in item[0]:
                current_group = item[0].replace("Group:", "").strip()
                continue
            elif "Entity:" in item[0]:
                current_entity = item[0].replace("Entity:", "").strip()
                continue

            # Extract title
            title_match = title_match_pattern.search(item[0])
            title = title_match.group().strip() if title_match else ""
            name = item[0].replace(title, "").strip() if title_match else item[0]

            doubt, division_doubt, company_doubt, job_title, division, affiliation, extras = "", "", "", "", "", "", ""
            remaining_items = item[1:]

            remaining_items = self.split_text_at_separator(remaining_items)
            if len(remaining_items) > 1:
                if self.is_profession(remaining_items[0]) and self.is_profession(remaining_items[1]):
                    remaining_items[0] += ' ' + remaining_items[1]
                    remaining_items.remove(remaining_items[1])
                elif self.contains_division_name(remaining_items[0]) and self.contains_division_name(
                        remaining_items[1]):
                    remaining_items[0] += ' ' + remaining_items[1]
                    remaining_items.remove(remaining_items[1])
                elif self.contains_affiliation_name(remaining_items[0]) and self.contains_affiliation_name(
                        remaining_items[1]):
                    remaining_items[0] += ' ' + remaining_items[1]
                    remaining_items.remove(remaining_items[1])

            # handle the ',' case, dont split if there are multiple commas and also check the part after splitting is job title or division or affiliation
            if len(remaining_items) > 2:
                if self.contains_division_name(remaining_items[1]) and self.contains_division_name(remaining_items[2]):
                    remaining_items[1] += ' ' + remaining_items[2]
                    remaining_items.remove(remaining_items[2])

                elif self.contains_affiliation_name(remaining_items[1]) and self.contains_affiliation_name(
                        remaining_items[2]):
                    remaining_items[1] += ' ' + remaining_items[2]
                    remaining_items.remove(remaining_items[2])

            remaining_items = self.split_data_with_comma(remaining_items)

            remaining_items = [item for item in remaining_items if self.check_date_items(item)]

            # Handling job_title based on your original conditions
            if len(remaining_items) >= 3:
                if self.is_profession(remaining_items[0]):
                    job_title, division, affiliation = remaining_items[:3]
                    extras = " ".join(remaining_items[3:])
                elif self.contains_division_name(remaining_items[0]):
                    division, affiliation = remaining_items[:2]
                    extras = " ".join(remaining_items[2:])
                else:
                    try:
                        doubt += " " + remaining_items[0]
                        if self.is_profession(remaining_items[1]):
                            job_title = remaining_items[1]
                            org_check_div_or_aff = self.check_affiliation(remaining_items[2])

                            if org_check_div_or_aff == 'true':
                                affiliation = remaining_items[2]
                            else:
                                division = remaining_items[2]

                            extras = " ".join(remaining_items[3:])
                        else:
                            division_doubt, company_doubt = remaining_items[1:3]
                            extras = " ".join(remaining_items[3:])
                    except Exception as e:
                        print('segregation not properly happend, because of', e)

            elif len(remaining_items) == 2:
                if self.is_profession(remaining_items[0]):
                    job_title = remaining_items[0]

                    org_check_div_or_aff = self.check_affiliation(remaining_items[1])
                    if org_check_div_or_aff == 'true':
                        if self.contains_division_name(remaining_items[1]):
                            division = remaining_items[1]
                        else:
                            affiliation = remaining_items[1]
                    elif org_check_div_or_aff == 'partial_true':
                        company_doubt = remaining_items[1]
                    else:
                        if self.contains_division_name(remaining_items[1]):
                            division = remaining_items[1]
                        else:
                            division_doubt = remaining_items[1]

                elif self.is_profession(remaining_items[1]):
                    doubt += " " + remaining_items[0]
                    job_title = remaining_items[1]
                else:
                    org_check_div_or_aff = self.check_affiliation(remaining_items[0])
                    if org_check_div_or_aff in ['true', 'partial_true']:
                        if self.contains_division_name(remaining_items[0]):
                            division = remaining_items[0]
                            org_check_div_or_aff = self.check_affiliation(remaining_items[1])
                            if org_check_div_or_aff == "true":
                                affiliation = remaining_items[1]
                            else:
                                extras = remaining_items[1]
                        else:
                            affiliation = remaining_items[0]
                            extras = remaining_items[1]
                    else:
                        if self.contains_division_name(remaining_items[0]):
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
                if self.is_profession(remaining_items[0]):
                    job_title = remaining_items[0]
                else:
                    org_check_div_or_aff = self.check_affiliation(remaining_items[0])
                    if org_check_div_or_aff == 'true':
                        if self.contains_division_name(remaining_items[0]):
                            division = remaining_items[0]
                        else:
                            affiliation = remaining_items[0]

                    elif org_check_div_or_aff == 'partial_true':
                        company_doubt = remaining_items[0]
                    else:
                        if self.contains_division_name(remaining_items[0]):
                            division = remaining_items[0]
                        else:
                            if self.check_person(remaining_items[0]):
                                doubt += " " + remaining_items[0]
                            else:
                                division_doubt = remaining_items[0]

            # this is not 100 percent correct but it is right in most of the cases.
            # if the doubt has only a single word, then merging it with the name.
            # similarly if there is a single word in extras also we will merge it with Affiliation

            affiliation_add_flag = division_add_flag = False

            if not affiliation:
                if (division_doubt or company_doubt):
                    affiliation_val = division_doubt if division_doubt else company_doubt
                    affiliation = affiliation_val
                    affiliation_add_flag = True

                if (division_doubt and company_doubt):
                    division += ' ' + division_doubt
                    affiliation = company_doubt
                    division_add_flag = True

            name, doubt, job_title, division, affiliation, extras = self.handle_single_misclassified_words(name, doubt,
                                                                                                           job_title,
                                                                                                           division,
                                                                                                           affiliation,
                                                                                                           extras)

            if affiliation_add_flag or division_add_flag:
                affiliation = 'color:red_affiliation' + affiliation

            # if not affiliation:
            #     if (division_doubt or company_doubt):
            #         affiliation_val = division_doubt if division_doubt else company_doubt
            #         affiliation = 'color:red_affiliation' + affiliation_val
            #
            #     if (division_doubt and company_doubt):
            #         division += ' ' + division_doubt
            #         affiliation = 'color:red_affiliation' + company_doubt

            if self.check_affiliation(job_title) in ["true", "partial_true"] or self.contains_division_name(job_title):

                job_title = job_title.strip()
                # if the last word is a job title, then no need to mark as red
                job_title_words = job_title.split(' ')

                if not self.is_profession(job_title_words[-1]):
                    job_title = 'color:red_job' + job_title

            if self.check_affiliation(division) in ["true", "partial_true"]:
                division = 'color:red_division' + division

            en_trans_job_title = self.translate_text(job_title)
            en_trans_division = self.translate_text(division)
            en_trans_affiliation = self.translate_text(affiliation)

            processed_op_data = [current_group, current_entity, title, name, doubt, en_trans_job_title, job_title,
                                 en_trans_division, division, en_trans_affiliation, affiliation, extras]

            processed_op_data = [data.strip() for data in processed_op_data]

            # to print the data for debugging
            for k, m in zip(output_excel_column_names, processed_op_data):
                print(k, ':', m)

            print('*' * 80)

            results.append(processed_op_data)

        return results


def join_based_on_stopwords(data):
    """ if the text is divided to 2 lines, and if it is starting or ending with a stop word or some of the below
    patterns, then joining those 2 lines """
    merged_data = []

    for inner_list in data:
        i = 1
        while i < len(inner_list) - 1:
            # last word of the line
            current_str_last_word = inner_list[i].split()[-1]
            # first word of the next line
            next_str_first_word = inner_list[i + 1].split()[0]

            # declare the end of the line patterns
            end_patterns = ["'s", "'re", "'ve", "'ll", "'d", "n't", ",", "-"]

            # Check the conditions for merging the strings
            # Check if any of the following conditions are met:
            # 1. The last word of the current string is in the list of stop words.
            # 2. The first word of the next string is in the list of stop words.
            # 3. The first character of the next string is lowercase.
            # 4. The last word of the current string ends with any pattern in the list of end patterns.
            # 5. The last word of the current string ends with '&'.
            # 6. The first word of the next string starts with '&'.

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


def save_data_to_excel_file(output):
    """ save the data to the excel file in the given path, with the given variable name """

    df = pd.DataFrame(output, columns=output_excel_column_names)
    excel_writer = pd.ExcelWriter(output_excel_file_path, engine='xlsxwriter')

    df.to_excel(excel_writer, sheet_name='Sheet1', index=False)

    # Get the xlsxwriter workbook and worksheet objects
    workbook = excel_writer.book
    worksheet = excel_writer.sheets['Sheet1']

    # Define a format with red text color
    # this marks the red color cells
    red_text_format = workbook.add_format({'color': 'red'})

    for row_num, value in enumerate(df['Affiliation'], start=2):
        if 'color:red_affiliation' in value:
            worksheet.write(f'K{row_num}', value, red_text_format)

    for row_num, value in enumerate(df['Job Title'], start=2):
        if 'color:red_job' in value:
            worksheet.write(f'G{row_num}', value, red_text_format)

    for row_num, value in enumerate(df['Division'], start=2):
        if 'color:red_division' in value:
            worksheet.write(f'I{row_num}', value, red_text_format)

    excel_writer.close()


def classify_data():
    """ this function preprocess the given pdf list data, and get the joined data from the word document and saves
     the classified data to an excel file """

    # join the data based on the stop words
    processed_data = join_based_on_stopwords(ip_data)
    # get the manual strings form the file
    manual_strs = get_manual_strings(docx_file_path).run()
    # remove the empty lines from the word document file
    merged_docx_strings = {s.replace(" ", "").lower() for s in manual_strs}

    # segregate the data to predefined columns
    segregate_data_obj = SegregateData(processed_data, department_names_set, profession_names_lemmatized_set,
                                       affiliation_names_set, text_separator_items, merged_docx_strings)
    output = segregate_data_obj.run()
    # save_data_to_excel_file(output)

    return output


classify_data()
