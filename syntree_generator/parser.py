import spacy
import re
from tqdm import tqdm
import os
from pathlib import Path

class SyntaxTreeGenerator:
    def __init__(self, language='fr'):
        # Load the appropriate spaCy model
        if language == 'fr':
            try:
                self.nlp = spacy.load('fr_core_news_sm')
            except OSError:
                print("French model not found. Installing...")
                spacy.cli.download("fr_core_news_sm")
                self.nlp = spacy.load('fr_core_news_sm')
        else:
            try:
                self.nlp = spacy.load('en_core_web_sm')
            except OSError:
                print("English model not found. Installing...")
                spacy.cli.download("en_core_web_sm")
                self.nlp = spacy.load('en_core_web_sm')
        
        # POS mapping to constituent labels
        self.pos_map = {
            "DET": "DET",    # Determiner
            "NOUN": "N",     # Noun
            "VERB": "V",     # Verb
            "ADP": "P",      # Preposition
            "ADJ": "ADJ",    # Adjective
            "ADV": "ADV",    # Adverb
            "PRON": "PRO",   # Pronoun
            "CCONJ": "CONJ", # Conjunction
            "SCONJ": "SUB",  # Subordinating conjunction
            "PART": "PART",  # Particle
            "NUM": "NUM",    # Numeral
            "AUX": "V",      # Auxiliary verb (mapped to V)
        }
        
        # Dependency mapping for phrase identification
        self.dep_phrase_map = {
            "nsubj": "NP",   # Nominal subject
            "obj": "NP",     # Object
            "iobj": "NP",    # Indirect object
            "pobj": "NP",    # Object of preposition
            "ROOT": "S",     # Root (sentence)
            "prep": "PP",    # Prepositional phrase
            "compound": "NP", # Compound
            "amod": "ADJ",   # Adjectival modifier
            "advmod": "ADVP", # Adverbial modifier
            "relcl": "REL",  # Relative clause
        }

    def chunk_text(self, text, chunk_size=5000):
        """Split text into manageable chunks."""
        # Split by paragraphs
        paragraphs = re.split(r'\n\s*\n', text)
        
        chunks = []
        current_chunk = []
        current_length = 0
        
        for para in paragraphs:
            if current_length + len(para) > chunk_size and current_chunk:
                chunks.append('\n\n'.join(current_chunk))
                current_chunk = [para]
                current_length = len(para)
            else:
                current_chunk.append(para)
                current_length += len(para)
        
        if current_chunk:
            chunks.append('\n\n'.join(current_chunk))
        
        return chunks

    def identify_phrase_type(self, token):
        """Identify the type of phrase based on token and dependencies."""
        if token.pos_ == "VERB":
            return "VP"
        elif token.pos_ == "NOUN":
            return "NP"
        elif token.pos_ == "ADP":  # Preposition
            return "PP"
        elif token.pos_ == "ADV":
            return "ADVP"
        elif token.dep_ == "relcl":
            return "REL"
        
        # Default cases
        dep_type = self.dep_phrase_map.get(token.dep_, "")
        if dep_type:
            return dep_type
            
        # Fallback
        if list(token.children):
            for child in token.children:
                if child.dep_ == "det":
                    return "NP"
        
        return "X"  # Unknown phrase type
    
    def token_to_pos(self, token):
        """Convert token to part-of-speech label."""
        return self.pos_map.get(token.pos_, token.pos_)

    def build_subtree(self, token, depth=1, indent="  "):
        """Recursively build S-expression subtree for a token."""
        if not list(token.children):
            # Leaf node
            pos = self.token_to_pos(token)
            return f'({pos} "{token.text}")'
        
        # Determine phrase type and collect children
        phrase_type = self.identify_phrase_type(token)
        
        # Process token itself
        token_expr = f'({self.token_to_pos(token)} "{token.text}")'
        
        # Group children by dependency and sort
        children_by_dep = {}
        for child in token.children:
            dep = child.dep_
            if dep not in children_by_dep:
                children_by_dep[dep] = []
            children_by_dep[dep].append(child)
        
        # Process children
        children_exprs = []
        
        # Process by dependency type with specific ordering
        for dep_type in ["det", "amod", "nmod", "compound", "nsubj", "aux", "advmod", "dobj", "pobj", "prep"]:
            if dep_type in children_by_dep:
                for child in sorted(children_by_dep[dep_type], key=lambda c: c.i):
                    children_exprs.append(self.build_subtree(child, depth+1))
        
        # Process any remaining children
        for dep_type, children in children_by_dep.items():
            if dep_type not in ["det", "amod", "nmod", "compound", "nsubj", "aux", "advmod", "dobj", "pobj", "prep"]:
                for child in sorted(children, key=lambda c: c.i):
                    children_exprs.append(self.build_subtree(child, depth+1))
        
        # Build the phrase expression
        if phrase_type == "X":
            # If unknown phrase type, just use token's POS
            phrase_expr = f'({self.token_to_pos(token)}\n{indent}  {token_expr}'
        else:
            # Regular phrase
            phrase_expr = f'({phrase_type}\n{indent}  {token_expr}'
        
        # Add children
        for child_expr in children_exprs:
            phrase_expr += f'\n{indent}  {child_expr}'
        
        phrase_expr += f'\n{indent})'
        
        return phrase_expr

    def sentence_to_sexpr(self, sent):
        """Convert a sentence to S-expression format."""
        # Find the root of the sentence
        root = None
        for token in sent:
            if token.dep_ == "ROOT":
                root = token
                break
        
        if not root:
            return "(ROOT\n  (S\n    (FRAG)))"
        
        # Build the tree starting from the root
        tree = f'(ROOT\n  (S\n    {self.build_subtree(root, 2)}\n  )\n)'
        
        return tree

    def process_text(self, text, chunk_size=5000):
        """Process full text and generate S-expressions."""
        # Clean text
        text = re.sub(r'\s+', ' ', text).strip()
        
        # Split into chunks
        chunks = self.chunk_text(text, chunk_size)
        
        # Process each chunk
        all_sexprs = []
        for i, chunk in enumerate(tqdm(chunks, desc="Processing chunks")):
            doc = self.nlp(chunk)
            
            # Process each sentence in the chunk
            chunk_sexprs = []
            for sent in doc.sents:
                sexpr = self.sentence_to_sexpr(sent)
                chunk_sexprs.append(sexpr)
            
            all_sexprs.append((i, chunk_sexprs))
        
        return all_sexprs

    def process_file(self, input_file, output_file, chunk_size=5000):
        """Process a file and write results to output file."""
        print(f"Processing {input_file}...")
        
        with open(input_file, 'r', encoding='utf-8') as file:
            text = file.read()
        
        # Skip Project Gutenberg boilerplate
        match = re.search(r'\*\*\* START OF .+? \*\*\*(.*?)\*\*\* END OF', text, re.DOTALL)
        if match:
            text = match.group(1).strip()
        
        results = self.process_text(text, chunk_size)
        
        with open(output_file, 'w', encoding='utf-8') as out_file:
            for chunk_idx, sentences in results:
                out_file.write(f"CHUNK {chunk_idx+1}\n")
                out_file.write("="*80 + "\n\n")
                
                for i, sexpr in enumerate(sentences):
                    out_file.write(f"SENTENCE {i+1}\n")
                    out_file.write(sexpr)
                    out_file.write("\n\n")
                
                out_file.write("\n" + "="*80 + "\n\n")
        
        print(f"S-expressions written to {output_file}")
        return output_file

    def extract_samples(self, file_path, n, output_file=None):
        """Extract first n S-expressions from a file into a new sample file."""
        if n <= 0:
            return None
            
        samples = []
        current_sample = []
        count = 0
        
        with open(file_path, 'r', encoding='utf-8') as file:
            for line in file:
                if line.startswith('SENTENCE'):
                    if current_sample:
                        samples.append('\n'.join(current_sample))
                        current_sample = []
                        count += 1
                        if count >= n:
                            break
                    current_sample = [line]
                elif current_sample:
                    current_sample.append(line.rstrip())
        
        # Add the last sample if there is one
        if current_sample and count < n:
            samples.append('\n'.join(current_sample))
        
        # Determine output file path if not provided
        if not output_file:
            # Use the input file's directory for the output path
            input_path = Path(file_path)
            output_file = input_path.parent / f"{input_path.stem}.sample.lisp"
        else:
            # Ensure the output_file is a Path object
            output_file = Path(output_file)
        
        # Create parent directories if they don't exist
        output_file.parent.mkdir(parents=True, exist_ok=True)
        
        # Write samples to the new file
        with open(output_file, 'w', encoding='utf-8') as out_file:
            for i, sample in enumerate(samples):
                out_file.write(f"SAMPLE {i+1}\n")
                out_file.write(sample)
                out_file.write("\n\n" + "="*50 + "\n\n")
        
        print(f"Extracted {len(samples)} samples to {output_file}")
        return output_file
