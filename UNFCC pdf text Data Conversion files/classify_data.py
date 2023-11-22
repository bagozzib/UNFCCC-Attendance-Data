# -*- coding: utf-8 -*-

# pending:
# If a job title, division, or affiliation text span ENDS in one of the following two lower-case words/bigrams/trigrams: " et", " et de", " et de la" or " y", " y de", " y de la", "y del" (case sensitive...these are basically the french and spanish words for "and" or "and of (the)"), always join the text in the subsequent cell to this text. For example, row 1758 has the job title "Directeur des Activités Combustibles et" which should also contain the text that was split into division, so that the job title should correctly read as "Directeur des Activités Combustibles et Matières Premières de Substitution".

# you can use '', to find the missing persons data
# correct data with "' s"

import warnings
warnings.filterwarnings('ignore')

import functools
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
failed_minister_gvrnmt_split_list = []
# Load the English NLP model
nlp = spacy.load("en_core_web_lg")
matcher = Matcher(nlp.vocab)

the_keyword_mis_split_list = []

# define the global pattern, that matches with person names, there is another local variable 'title_pattern',
# which doesnt match some titles like 'H.E' e.t.c, because we are keeping Mr. in title and H.E. in person name
# change the 'title_pattern' local variable to change this behaviour

# title_match_pattern = re.compile(
#     r"(Mr\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |M\ |On\ |M\ |Fr\ |Rev\ |Mme\ |Sr\ |Msgr\ |On\.\s*|Fr\.\s*|Rev\.\s*|H\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |Sra\.\s*|Mme|Sr\.\s*|Msgr\.\s*))?|Msgr\.\s*|Mrs\.\s*|Sra\.\s*|Sr\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|M\.\s*|Mme|Ms|S\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Mme|Mr|Ms|Dr|Msgr\.\s*|M\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |M\ |Sra\.\s*|Sr\.\s*))?)")

# Create a regular expression pattern to match the department names and 'of' or 'for'
# division_names_pattern = rf"(.+?)\s({'|'.join(re.escape(word) for word in department_names_set)})\s(of|for)\s(.+)"
division_names_pattern = rf"({'|'.join(re.escape(word) for word in department_names_set)})\s(of|for)\s(.+)"

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

def join_based_on_stopwords(data):
    """ if the text is divided to 2 lines, and if it is starting or ending with a stop word or some of the below
    patterns, then joining those 2 lines """

    def check_merge_conditions(current_str_last_word, next_str_first_word):
        if (current_str_last_word.lower() in stop_words or
                    next_str_first_word.lower() in stop_words or
                    # next_str_first_word[0].islower() or   # this makes to generate false positives
                    any(current_str_last_word.lower().endswith(pattern) for pattern in end_patterns) or
                    current_str_last_word.lower() in exact_match_words or
                    current_str_last_word.lower() in other_lang_start_stop_words_set or
                    next_str_first_word.lower() in other_lang_start_stop_words_set or
                    current_str_last_word.endswith('&') or
                    next_str_first_word.startswith('&')):

            return True

        return False

    merged_data = []

    if len(data) == 2:
        if not data[0] or not data[1]:
            return data

        # last word of the line
        current_str_last_word = data[0].strip().split()[-1]
        # first word of the next line
        next_str_first_word = data[1].strip().split()[0]

        if check_merge_conditions(current_str_last_word, next_str_first_word):
            merged_string = data[0] + ' ' + data[1]

            return [merged_string, '']
        else:
            return data

    for inner_list in data:
        i = 1
        while i < len(inner_list) - 1:
            # last word of the line
            current_str_last_word = inner_list[i].strip().split()[-1]
            try:
                # first word of the next line
                next_str_first_word = inner_list[i + 1].strip().split()[0]
            except:
                import ipdb;ipdb.set_trace()

            # Check the conditions for merging the strings
            # Check if any of the following conditions are met:
            # 1. The last word of the current string is in the list of stop words.
            # 2. The first word of the next string is in the list of stop words.
            # 3. The first character of the next string is lowercase.
            # 4. The last word of the current string ends with any pattern in the list of end patterns.
            # 5. The last word of the current string ends with '&'.
            # 6. The first word of the next string starts with '&'.

            if check_merge_conditions(current_str_last_word, next_str_first_word):
                merged_string = inner_list[i] + " " + inner_list[i + 1]
                inner_list[i] = merged_string
                del inner_list[i + 1]

            else:
                i += 1

        merged_data.append(inner_list)

    return merged_data

class SegregateData(object):
    """ CLassify the data to Group, Entity, Title, Name, Name doubt, English Translated Job Title, Job Title,
                        English Translated Division, Division, English Translated Affiliation, Affiliation, Extras """

    def __init__(self, data_list, department_names, profession_names_set, affiliation_names, separator_items_set,
                 merged_docx_strings):

        # the data from the pdf file which is like a list of lists
        # ex: [['Group:PARTIES'],
        # ['Entity:Albania'],
        # ['Mr. Besnik BARAJ', 'Deputy Minister of Environment', 'Ministry of Environment']]
        self.data = data_list

        # the set of names, that are basis of classification
        self.department_names = department_names
        self.affiliation_names = affiliation_names
        self.profession_names_set = profession_names_set

        # patterns that are majorly identified to divide the merged Items:
        # for ex: 'Director The Trendlyne Team', 'The' is the seperator item
        self.separator_items_set = separator_items_set
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
        self.min_previous_str_len_merge_single_wrd = 24
        self.cache = {}
        self.special_checklist = []

    def run(self):
        # in the pdf extracted data if we have words that are split across lines, combining them based on comparision
        # of joined text from the word file.
        processed_data = self.join_split_job_titles()
        # remove the special characters like '*' or any other characters, to remove them declare them in self.special_chars_list above
        processed_data = self.replace_special_chars_in_list(processed_data)
        # classify the extracted data into columns
        classified_list_data = self.classify_extracted_data(processed_data)

        return classified_list_data

    @functools.lru_cache(maxsize=None)
    def contains_affiliation_name(self, text):
        """ returns True if the text contains any word form the predefined Affiliation names """

        # converting to lower case, lemmatizating and also removing the stop words
        lemmatized_words = {word.lemma_.lower().replace(" ", "") for word in self.nlp_en(text) if
                            word.is_alpha and word.text.lower() not in spacy.lang.en.stop_words.STOP_WORDS}

        # Using set intersection to find matching department names
        matching_words = lemmatized_words.intersection(self.affiliation_names)

        return bool(matching_words)

    @functools.lru_cache(maxsize=None)
    def check_affiliation(self, text):
        """ check if the text is an affiliation name or not by using spacy and flair packages.
            - "true" if both packages classify the text as an organization.
            - "false" if both packages doesnot classify the text as an organization.
            - "partial_true" if only any one of the package classify the text as an organization.
         """
        # if the text contains a division name, which is in our declared division names list, then it is a division.
        if self.contains_division_name(text):
            return "false"

        if self.check_multi_words_cndtn(text, multiple_words_affltn_list):
            return "true"

        if self.is_profession(text):
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

    @functools.lru_cache(maxsize=None)
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

    @functools.lru_cache(maxsize=None)
    def contains_division_name(self, text):
        """ check if the text contains any division name that is present in our predefined division names list """
        if self.contains_affiliation_name(text) or self.check_multi_words_cndtn(text, multiple_words_affltn_list):
            return False

        # check if the given word is a profession
        if self.is_profession(text):
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

    @functools.lru_cache(maxsize=None)
    def translate_text(self, text, from_code=None):
        """ translate the given text from teh language with code 'from_code' to english """

        # detect the source code of the language, if it's not given.
        if not from_code:
            try:
                from_code = detect(text)
            except:
                from_code = 'en'

        if from_code == 'en':
            return text

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

    def check_multi_words_cndtn(self, text, multiple_words_items_list):
        # Lemmatize the input string using spaCy and remove stop words
        lemmatized_words = {word.lemma_.lower().replace(" ", "") for word in nlp(text) if
                            word.is_alpha and word.text.lower() not in nlp.Defaults.stop_words}

        # Check if all the words in a word set are present in the lemmatized_words
        for word_set in multiple_words_items_list:
            if all(word in lemmatized_words for word in word_set):
                return True
            else:
                detected_language = 'en'
                try:
                    detected_language = detect(text)
                except:
                    pass

                translated_text = self.translate_text(text, detected_language)
                # Lemmatize the input string using spaCy and remove stop words
                lemmatized_words = {word.lemma_.lower().replace(" ", "") for word in nlp(translated_text) if
                                    word.is_alpha and word.text.lower() not in nlp.Defaults.stop_words}

                # Check if all the words in a word set are present in the lemmatized_words
                for word_set in multiple_words_items_list:
                    if all(word in lemmatized_words for word in word_set):
                        return True

        return False
    @functools.lru_cache(maxsize=None)
    def is_profession(self, text):
        """ check if the given text is a profession or not """

        # there are some words that are joined using '-' or '/'. we cant directly remove them or cant directly split
        # the text with these characters. So, we are checking by splitting the text and also by removing to check if they
        # contain any word related to profession.

        split_words_list = ['-', '/']
        if self.check_multi_words_cndtn(text, multiple_words_affltn_list) or re.search(division_names_pattern, text, re.IGNORECASE):
            return False
        elif self.check_multi_words_cndtn(text, multiple_words_profession_list):
            return True

        for split_char in split_words_list:
            if split_char in text:
                # there are some words that are with some seperators like '-' for some words, we have to remove the seperator and join them
                # for some words we have to divide them to two words and check them individually
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

                if re.search(division_names_pattern, translated_text, re.IGNORECASE):
                    return False

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
                if part in self.separator_items_set:
                    if not preceding_word and idx != 0:
                        preceding_word = self.separator_items_set[idx - 1]

                    # If it's not preceded by a stop word, add the current output to the result
                    if preceding_word not in self.separator_items_set and preceding_word not in stop_words:
                        output_data.append(current_output)
                        current_output = ""
                    # Add the separator word to the result
                    current_output += part + " "
                else:
                    # Add the current part to the current output
                    current_output += part + " "

                # Update the preceding word
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

    def empty_division_store_affiliation(self, division, affiliation):
        affiliation = affiliation.strip()

        reformatted_affiliation = affiliation.replace(' ', '').lower()
        if not division and reformatted_affiliation in empty_division_affiliation_vals:
            division = affiliation
            affiliation = ''

        return division, affiliation


    def empty_job_division_join_affiliation_name(self, name, affiliation):
        affiliation = affiliation.strip()
        affiliation_words_len = len(affiliation.split(' '))

        if affiliation_words_len == 1 and self.check_affiliation(affiliation) != 'true':
            name += ' ' + affiliation
            affiliation = ''

        return name, affiliation

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
                        contains_affiliation_name_check = True if self.check_affiliation(strings_list[i + 1]) == 'true' else False

                        # if the current item is a profession and the next items is not either division or an Affiliation.
                        c2 = profession_check and not (
                                contains_division_name or contains_affiliation_name_check)

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
                        # # if the current item is not a job title, then also checking if we can merge them by validating
                        # # against self.merged_docx_strings
                        # combined_str = strings_list[i] + " " + strings_list[i + 1]
                        #
                        # normalized_combined_str = combined_str.replace(" ", "").lower()
                        #
                        # if normalized_combined_str in self.merged_docx_strings:
                        #     strings_list[i] = combined_str
                        #     del strings_list[i + 1]
                        pass

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
        division_words_list = division.split()
        affiliation_words_list = affiliation.split()
        extras_words_list = extras.split()

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

        if len(job_title) >= self.min_previous_str_len_merge_single_wrd and len(affiliation_words_list) == 1:
            if not (affiliation.isupper()):
                job_title += ' ' + affiliation
                affiliation, affiliation_words_list = '', []

        if len(division) >= self.min_previous_str_len_merge_single_wrd and len(affiliation_words_list) == 1:
            if not (affiliation.isupper()):
                division += ' ' + affiliation
                affiliation, affiliation_words_list = '', []

        if len(affiliation) >= self.min_previous_str_len_merge_single_wrd and len(extras_words_list) == 1:
            affiliation += ' ' + extras
            extras, extras_words_list = '', []

        if not affiliation and extras:
            affiliation, extras = extras, ''

        return name, doubt, job_title, division, affiliation, extras

    def split_text_by_keyword(self, text, classify_text):
        # Split the text into sentences based on your requirements
        sentences = []
        words = text.split()
        ministry_of_stop_words = stop_words.union(minister_of_startswith_ignore_list)
        classify_text_words_lst = classify_text.split()
        i = 0
        while i < len(words):
            if self.translate_text(words[i]).lower() == classify_text_words_lst[0] and i < len(
                    words) - 1 and self.translate_text(words[i + 1]).lower() == classify_text_words_lst[
                1] and i >= 1 and (self.translate_text(words[i - 1]) not in ministry_of_stop_words):
                sentences.append(" ".join(words[:i]))
                words = words[i:]
                break

            i += 1

        # Append the last sentence
        if words:
            sentences.append(" ".join(words))

        return sentences

    def seperate_and_classify_text_by_keyword(self, name, doubt, job_title, division, affiliation, extras, en_trans_job_title,
                                              en_trans_division,
                                              en_trans_affiliation, en_trans_extras):

        if doubt:
            regex = r'(de|del|van)$'
            name_match = re.search(regex, name, re.IGNORECASE)
            if name_match:
                name += ' ' + doubt
                doubt = ''

        # check if the job title, Division, Affiliation starts or ends with certain text
        pattern_starts_with_regex = '^(de|du|des|del)\s'
        pattern_ends_with_regex = r'(de|du|des|del)$'
        cols_list = [doubt, job_title, division, affiliation, extras]

        for idx in range(len(cols_list[1:])):
            start_data_match = re.search(pattern_starts_with_regex, cols_list[idx], re.IGNORECASE)
            end_data_match = re.search(pattern_ends_with_regex, cols_list[idx], re.IGNORECASE)

            if start_data_match:
                cols_list[idx - 1] += ' ' + cols_list[idx]
                cols_list[idx] = ''

            if end_data_match and idx < len(cols_list) - 1:
                cols_list[idx] += ' ' + cols_list[idx + 1]
                cols_list[idx + 1] = ''

        doubt, job_title, division, affiliation, extras = cols_list
        try:
            # if there is some text that starts with 'ministry' and also contains 'government' in 'affiliation' or 'extras',
            # then we should not divide it.
            pattern = r'(?=.*ministry of)(?=.*government of)'
            mnstry_affltn_cdtn = re.search(pattern, en_trans_affiliation, re.IGNORECASE)
            mnstry_extras_cdtn = re.search(pattern, en_trans_extras, re.IGNORECASE)

            if mnstry_affltn_cdtn or mnstry_extras_cdtn:
                affiliation_split_data_and_classify.remove('government of')
            else:
                affiliation_split_data_and_classify.add('government of')

            for classify_text in affiliation_split_data_and_classify:
                if not en_trans_job_title.startswith(classify_text) and (
                        classify_text in job_title.lower() or classify_text in en_trans_job_title.lower()):

                    words_list = self.split_text_by_keyword(job_title, classify_text)
                    job_title = words_list[0]

                    if len(words_list) >= 2:
                        affiliation = words_list[1] + ' ' + division + ' ' + affiliation
                        division = ''

                elif not en_trans_division.startswith(classify_text) and (
                        classify_text in division.lower() or classify_text in en_trans_division.lower()):

                    words_list = self.split_text_by_keyword(division, classify_text)
                    if len(words_list) >= 2:
                        division = words_list[0]

                        if len(words_list) >= 2:
                            affiliation = words_list[1] + ' ' + affiliation

                elif not en_trans_affiliation.lower().startswith(classify_text) and (
                        classify_text in affiliation.lower() or classify_text in en_trans_affiliation.lower()):

                    words_list = self.split_text_by_keyword(affiliation, classify_text)
                    division += ' ' + words_list[0]

                    if len(words_list) >= 2:
                        affiliation = words_list[1]
                    else:
                        affiliation = ''

                elif (classify_text in extras.lower() or classify_text in en_trans_extras.lower()):
                    if en_trans_extras.lower().startswith(classify_text):
                        words_list = self.split_text_by_keyword(extras, classify_text)
                        division += ' ' + affiliation
                        affiliation = words_list[0]

                    else:
                        words_list = self.split_text_by_keyword(extras, classify_text)
                        division += ' ' + affiliation + ' ' + words_list[0]
                        affiliation = words_list[1]

                    extras = ''

                en_trans_job_title, en_trans_division, en_trans_affiliation, en_trans_extras = self.translate_text(job_title), \
                                                                                               self.translate_text(division),\
                                                                                               self.translate_text(affiliation), \
                                                                                               self.translate_text(extras)
        except Exception as e:
            issue_str = name + '  ' + '***************>' + '  ' + str(e)
            failed_minister_gvrnmt_split_list.append(issue_str)

        if division.split():
            division_last_word = division.split()[-1].strip().lower()
            if division_last_word in minister_of_startswith_ignore_list:
                # if the division is 'Federal' and the affilaition is 'Ministry of Economic'.
                affiliation = division.split()[-1] + ' ' + affiliation
                division = ' '.join(division.split()[:-1])

        return name, doubt, job_title, division, affiliation, extras

    def classify_data_final_stage(self, job_title, division, affiliation, extras):
        if self.is_profession(extras):
            job_title += ' ' + division + ' ' + affiliation + ' ' + extras
            extras = ''
            division = ''
            affiliation = ''

        if self.contains_division_name(extras):
            division += ' ' + affiliation + ' ' + extras
            affiliation = ''
            extras = ''

        if self.check_affiliation(extras) == "true":
            affiliation += ' ' + extras
            extras = ''

        affiliation_words_lst = affiliation.split()

        if len(affiliation_words_lst) > 1:
            first_word = affiliation_words_lst[0]
            if self.contains_division_name(first_word):
                division += ' ' + affiliation
                affiliation = ''

        return job_title, division, affiliation, extras

    def handle_division_field(self, job_title, division, affiliation):
        if affiliation:
            if self.contains_division_name(affiliation):
                if self.contains_division_name(division):
                    division += ' ' + affiliation
                    affiliation = ''
                else:
                    job_title += ' ' + division
                    division = affiliation
                    affiliation = ''

        return job_title, division, affiliation

    def join_split_data_across_cols(self, input_list, special_chars_set):
        common_chr_cnt = 0
        start_idx = None

        for idx, string in enumerate(input_list):
            string_set = set(string)
            common_characters = string_set.intersection(special_chars_set)
            if common_characters:
                common_chr_cnt += 1
                if not start_idx:
                    start_idx = idx

            if common_chr_cnt == 2:
                common_char_fnd = list(common_characters)[0]
                before_special_chr, after_special_chr = input_list[idx].split(common_char_fnd)
                input_list[start_idx] = ' '.join(input_list[start_idx:idx]) + ' ' + before_special_chr + common_char_fnd
                input_list[idx] = after_special_chr
                input_list[start_idx + 1:idx] = [''] * (idx - (start_idx + 1))
                common_chr_cnt = 0
                start_idx = None

        return input_list

    def set_strict_affiliation_words(self, data_cols_list):

        for idx, each_str_item in enumerate(data_cols_list):

            pattern = r'(?<!\bof\s)(?<!\bfor\s)\b(' + '|'.join(re.escape(phrase) for phrase in strict_affiliation_keywords) + r')\b'

            # Split the input string using the regular expression pattern
            result = re.split(pattern, each_str_item, flags=re.IGNORECASE)

            # Remove any empty strings from the result
            result = [part for part in result if part and part.split() and len(part.split()[0]) > 1]

            if len(result) > 1:
                # this means the keyword is not in affiliation but in job title or division cols
                if idx < len(data_cols_list) - 1:
                    data_cols_list[idx] = result[0]
                    data_cols_list[-1] = ' '.join(result[1:]) + data_cols_list[-1]

                # this means the keyword is not in affiliation, but it got joined
                elif idx == len(data_cols_list) - 1:
                    data_cols_list[idx - 1] += ' ' + result[0]
                    data_cols_list[-1] = ' '.join(result[1:])

        return data_cols_list

    def job_title_handle_the_keyword_correctly(self, job_title, division, affiliation):
        """ the words containing 'The' is not being handled properly """
        split_txt = None

        # if job_title contains only one word 'The'
        if job_title and job_title.split(split_txt)[0] == 'The':
            job_title = ''
            if division:
                division = 'The ' + division
            elif affiliation:
                affiliation = 'The ' + affiliation

            return job_title, division, affiliation

        if ' The ' in job_title or ' The' in job_title:
            split_txt = 'The'

        if split_txt:
            job_title_words = job_title.split(split_txt)

            if len(job_title_words) > 2:
                the_keyword_mis_split_list.append([division, affiliation])

                return division, affiliation

            # if job_title_words[-1] is space
            job_title_words[1] = job_title_words[1] if job_title_words[1] else ''

            if job_title_words[1]:
                txt = 'The' + ' ' + job_title_words[1]

                if self.check_affiliation(txt) in ['true', 'partial_true']:
                    affiliation = 'The ' + job_title_words[1] + ' ' + affiliation
                else:
                    division = 'The ' + job_title_words[1] + ' ' + division

            else:
                if division:
                    division = 'The ' + job_title_words[1] + ' ' + division

                elif affiliation:
                    affiliation = 'The ' + job_title_words[1] + ' ' + affiliation

            job_title = job_title_words[0]

        return job_title, division, affiliation

    def division_handle_the_keyword_correctly(self, division, affiliation):
        """ the words containing 'The' is not being handled properly """
        split_txt = None

        division_split = division.split(split_txt)
        # if the division starts with 'The'
        if division and division_split[0] == 'The':
            affiliation = 'The ' + ' '.join(division_split[1:]) + affiliation
            division = ''

            return division, affiliation

        if ' The ' in division or ' The' in division:
            split_txt = 'The'

        if split_txt:
            division_words = division.split(split_txt)
            # if 'The' is the last word, then only we will join with the Affiliation
            if not division_words[-1]:
                if len(division_words) > 2:
                    the_keyword_mis_split_list.append([division, affiliation])

                    return division, affiliation

                division_words[1] = division_words[1] if division_words[1] else ''
                affiliation = 'The ' + division_words[1] + affiliation

                division = division_words[0]

        return division, affiliation

    def handle_full_stop_at_end(self, data_list):
        for item_num in range(len(data_list)-1):
            if data_list[item_num] and data_list[item_num][-1] == '.':
                data_list[item_num] += ' ' + data_list[item_num + 1]
                data_list[item_num + 1] = ''

        return data_list

    def handle_job_tite_text_keywords(self, job_title, division, affiliation):
        head_of_division_pattern = r'(?i)(.*?)(Head of Division)(.*?)("(?:[^"]*)"|$)'

        head_of_division_match = re.search(head_of_division_pattern, job_title, re.IGNORECASE)

        if head_of_division_match:
            first_part = head_of_division_match.group(1) + head_of_division_match.group(2)
            second_part = head_of_division_match.group(3) + head_of_division_match.group(4).strip('" ')

            stop_wrds_pattern = r'^(of|for|at)\b'
            if not re.search(stop_wrds_pattern, second_part.strip()):
                capitalized_word_pattern = r'\b[A-Z][a-z]*\b'

                if bool(re.search(capitalized_word_pattern, second_part)):
                    job_title = first_part

                    if self.check_affiliation(second_part) in ['true', 'partial_true']:
                        affiliation = second_part + ' ' + division + ' ' + affiliation
                    else:
                        division = second_part + ' ' + division

        job_title, division, affiliation = self.job_title_handle_the_keyword_correctly(job_title, division, affiliation)

        return job_title, division, affiliation

    def correct_job_keywords_for_job_title(self, name, job_title, en_trans_job_title, division):
        # check for '-'
        original_division = division
        original_job_title = job_title
        hyphen_cnt = job_title.count('-')
        if hyphen_cnt == 1:
            if not self.is_profession(job_title):
                job_title_words = job_title.split('-')
                if len(job_title_words) == 2 and job_title_words[1] and len(job_title_words[0]) > 3:
                    if not self.is_profession(job_title_words[1]):
                        division = job_title_words[1] + ' ' + division
                        job_title = job_title_words[0]

        # Use re.search to find the match
        division_names_en_job_title_match = re.search(division_names_pattern, en_trans_job_title, re.IGNORECASE)
        division_names_job_title_match = re.search(division_names_pattern, job_title, re.IGNORECASE)
        exclude_text_pattern = r'\b(?:' + '|'.join(map(re.escape, job_title_startswith_ignore_set)) + r')\b'
        check_exclude_pattern = re.search(exclude_text_pattern, en_trans_job_title, re.IGNORECASE)

        if division_names_en_job_title_match and division_names_job_title_match:
            # there are some names like 'federal department of', in this case we should not split the string.
            # so, we are handling these cases

            # Extract the matched parts
            job_title = division_names_job_title_match.group(1)
            try:
                matched_division_name = f"{division_names_job_title_match.group(2)} {division_names_job_title_match.group(3)}"  # Include 'of' or 'for'
                part2 = division_names_job_title_match.group(4)

                division = matched_division_name + ' ' + part2 + ' ' + division
            except:
                self.special_checklist.append(name)

            if check_exclude_pattern:
                # Split the string from the right end, removing the last word
                split_parts = job_title.rsplit(' ', 1)

                # Join the split parts to form the modified string
                job_title = split_parts[0]
                if len(split_parts) > 1:
                    division = split_parts[1] + ' ' + division

            if job_title.split()[-1] in stop_words:
                job_title = original_job_title
                division = original_division

        # if the text is in other languages
        elif division_names_en_job_title_match and not division_names_job_title_match:

            job_title_words = job_title.split()
            # go for each word
            for idx in range(1, len(job_title_words) - 1):
                # if the before word is not a stop word and the current word is like division or department
                if job_title_words[idx-1].lower() not in other_lang_start_stop_words_set and (self.translate_text(job_title_words[idx]).lower() in department_names_set or job_title_words[idx].lower() in department_names_set):
                    # if the next word is a stop word like 'of/for...'
                    if job_title_words[idx+1].lower() in other_lang_start_stop_words_set or job_title_words[idx + 1].lower() in stop_words:
                        # if it contains words like 'federal' or 'national'
                        if check_exclude_pattern:
                            # the word before the word should not be a stop word for ex: we will avoid - 'for federal department of'
                            if job_title_words[idx-2].lower() not in other_lang_start_stop_words_set and job_title_words[idx - 2].lower() not in stop_words:
                                # check if the word before the current word is federal/national
                                if self.translate_text(job_title_words[idx-1]).lower() in job_title_startswith_ignore_set or job_title_words[idx-1].lower() in job_title_startswith_ignore_set:
                                    # classify the word such that for ex: 'federal department of' will be in the 'division' column
                                    job_title = ' '.join(job_title_words[:idx-1])
                                    division = ' '.join(job_title_words[idx-1:]) + ' ' + division
                                    break
                        else:
                            job_title = ' '.join(job_title_words[:idx])
                            division = ' '.join(job_title_words[idx:]) + ' ' + division

        return job_title, division

    def classify_extracted_data(self, processed_data):
        """ classify the extracted data to multiple columns """
        current_group = ""
        current_entity = ""
        results = []
        # there are some records, whose names are mixed up in the data, we will find them
        title_misclassified_names = []

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
                    if not self.check_multi_words_cndtn(remaining_items[1], multiple_words_affltn_list):
                        remaining_items[0] += ' ' + remaining_items[1]
                        remaining_items.remove(remaining_items[1])

                elif self.contains_division_name(remaining_items[0]) and self.contains_division_name(
                        remaining_items[1]):
                    comb_str = remaining_items[0] + remaining_items[1]
                    job_title_division_comb_set = set(comb_str.split())

                    if not job_title_division_comb_set.intersection(text_separator_items_set):
                        if not self.check_multi_words_cndtn(remaining_items[1], multiple_words_affltn_list):
                            remaining_items[0] += ' ' + remaining_items[1]
                            remaining_items.remove(remaining_items[1])

                elif self.check_affiliation(remaining_items[0]) in ['true', 'partial_true'] and self.check_affiliation(remaining_items[1]) in ['true', 'partial_true']:
                    remaining_items[0] += ' ' + remaining_items[1]
                    remaining_items.remove(remaining_items[1])

            # handle the ',' case, dont split if there are multiple commas and also check the part after splitting is job title or division or affiliation
            if len(remaining_items) > 2:
                if self.contains_division_name(remaining_items[1]) and self.contains_division_name(remaining_items[2]):
                    remaining_items[1] += ' ' + remaining_items[2]
                    remaining_items.remove(remaining_items[2])

                elif self.check_affiliation(remaining_items[1]) in ['true', 'partial_true'] and self.check_affiliation(remaining_items[2]) in ['true', 'partial_true']:
                    remaining_items[1] += ' ' + remaining_items[2]
                    remaining_items.remove(remaining_items[2])

            remaining_items = self.split_data_with_comma(remaining_items)

            remaining_items = [item for item in remaining_items if self.check_date_items(item)]

            # Handling job_title based on your original conditions
            if len(remaining_items) >= 3:
                if self.is_profession(remaining_items[0]):
                    job_title = remaining_items[0]

                    if self.check_affiliation(remaining_items[2]) == 'false' and self.contains_division_name(remaining_items[2]):
                        division += ' ' + ' '.join(remaining_items[1:3])
                        affiliation = ''

                    elif self.check_affiliation(remaining_items[1]) == 'true' and not self.contains_division_name(remaining_items[1]):
                        affiliation = ' '.join(remaining_items[1:])
                        division = ''
                    else:
                        division, affiliation = remaining_items[1:3]

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

                        elif self.check_affiliation(remaining_items[2]) == 'false' and self.contains_division_name(remaining_items[2]):
                            division += ' ' + remaining_items[2]
                            affiliation = ''
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
                                extras += remaining_items[1]
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

            en_trans_job_title = self.translate_text(job_title)
            en_trans_division = self.translate_text(division)
            en_trans_affiliation = self.translate_text(affiliation)
            en_trans_extras = self.translate_text(extras)

            # there are a lot of people with the surname as M., but there are some text whose details got mixed up with
            # the next person details, so to seperate them, we are removing M. from the title regular expression
            title_mispresent_cols = [name, job_title, division, affiliation, extras]
            title_pattern_without_m = re.compile(
                r"(Mr\.\s*|Rev\.\s*|Entity\:\s*|Msgr\.\s*|H\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Msgr\.\s*|Ms\ |Mr\ |Sra\ |Sr\ |Sra\.\s*|Mme|Sr\.\s*))?|Mrs\.\s*|Sra\.\s*|Sr\.\s*|Ms\.\s*|Dr\.\s*|Prof\.\s*|Mme|Ms|S\.E(?:\.\s*(?:Ms\.\s*|Mr\.\s*|Mme|Ms\ |Mr\ |Msgr\.\s*|Sra\ |Sr\ |Sra\.\s*|Sr\.\s*))?)")

            for each_col in title_mispresent_cols:
                if title_pattern_without_m.search(each_col):
                    title_misclassified_names.append(name)

            name, doubt, job_title, division, affiliation, extras = self.seperate_and_classify_text_by_keyword(name, doubt, job_title, division,
                                                                                                  affiliation, extras,
                                                                                                  en_trans_job_title,
                                                                                                  en_trans_division,
                                                                                                  en_trans_affiliation,
                                                                                                  en_trans_extras)

            job_title, division, affiliation, extras = self.classify_data_final_stage(job_title, division, affiliation, extras)

            en_trans_job_title = self.translate_text(job_title)
            job_title, division = self.correct_job_keywords_for_job_title(name, job_title, en_trans_job_title, division)

            # job_title, division = join_based_on_stopwords([job_title, division])

            data_cols_list = [doubt, job_title, division, affiliation]

            doubt, job_title, division, affiliation = self.join_split_data_across_cols(data_cols_list, special_chars_set)

            data_cols_list.remove(doubt)

            job_title, division, affiliation = self.set_strict_affiliation_words(data_cols_list)

            job_title, division, affiliation = self.handle_job_tite_text_keywords(job_title, division, affiliation)
            division, affiliation = self.division_handle_the_keyword_correctly(division, affiliation)

            job_title, division, affiliation = self.handle_full_stop_at_end([job_title, division, affiliation])

            # extra commas at the end of the text is making them to join again for ex: director, Trendlyne division is
            # seperated to job_title, division earlier, but not if there is trailing comma, it is being joined again

            job_title, division, affiliation = job_title.strip(','), division.strip(','), affiliation.strip(',')

            job_title, division = join_based_on_stopwords([job_title, division])

            division, affiliation = self.empty_division_store_affiliation(division, affiliation)

            if not (job_title and division and doubt):
                name, affiliation = self.empty_job_division_join_affiliation_name(name, affiliation)

            # extras field is mostly containing Affiliation data, so combining it with Affiliation
            if extras:
                affiliation += ' ' + extras
                extras = ''

            en_trans_job_title = self.translate_text(job_title)
            en_trans_division = self.translate_text(division)
            en_trans_affiliation = self.translate_text(affiliation)

            if affiliation_add_flag or division_add_flag:
                affiliation = 'color:red_affiliation' + affiliation

            if self.check_affiliation(job_title) in ["true", "partial_true"] or self.contains_division_name(job_title):

                job_title = job_title.strip()
                # if the last word is a job title, then no need to mark as red
                job_title_words = job_title.split(' ')

                if not self.is_profession(job_title_words[-1]):
                    job_title = 'color:red_job' + job_title

            if self.check_affiliation(division) in ["true", "partial_true"]:
                division = 'color:red_division' + division

            processed_op_data = [current_group, current_entity, title, name, doubt, en_trans_job_title, job_title,
                                 en_trans_division, division, en_trans_affiliation, affiliation, extras]

            processed_op_data = [data.strip() for data in processed_op_data]

            # if debug_mode:
            # to print the data for debugging
            for k, m in zip(output_excel_column_names, processed_op_data):
                print(k, ':', m)

            print('*' * 80)

            print(name)
            results.append(processed_op_data)

        print('$' * 100)
        print('title_misclassified_names', title_misclassified_names)
        print('$' * 100)
        print('\n'*1)
        print('$' * 100)
        print('the_keyword_mis_split_list', the_keyword_mis_split_list)
        print('$' * 100)
        print('\n')
        print('\\' * 100)
        print('special_checklist, missed because of some errors', self.special_checklist)
        print('\\' * 100)

        return results

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
                                       affiliation_names_set, text_separator_items_set, merged_docx_strings)

    output = segregate_data_obj.run()

    if not debug_mode:
        save_data_to_excel_file(output)

    print('failed_minister_gvrnmt_split_list', failed_minister_gvrnmt_split_list)

    return output


classify_data()
