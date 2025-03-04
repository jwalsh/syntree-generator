import pytest
from syntree_generator.parser import SyntaxTreeGenerator

def test_initialization():
    parser = SyntaxTreeGenerator(language='fr')
    assert parser is not None
    assert parser.nlp is not None

def test_chunk_text():
    parser = SyntaxTreeGenerator()
    text = "First paragraph.\n\nSecond paragraph.\n\nThird paragraph."
    chunks = parser.chunk_text(text, chunk_size=20)
    assert len(chunks) > 0

def test_simple_sentence():
    parser = SyntaxTreeGenerator(language='fr')
    text = "Le chat dort."
    doc = parser.nlp(text)
    for sent in doc.sents:
        sexpr = parser.sentence_to_sexpr(sent)
        assert sexpr.startswith("(ROOT")
        assert "(S" in sexpr
        assert "(V" in sexpr

def test_pos_mapping():
    parser = SyntaxTreeGenerator()
    assert parser.pos_map["DET"] == "DET"
    assert parser.pos_map["NOUN"] == "N"
    assert parser.pos_map["VERB"] == "V"

def test_extract_samples():
    # Create a temporary file with some content
    import tempfile
    with tempfile.NamedTemporaryFile(mode='w+', delete=False) as temp:
        temp.write("SENTENCE 1\n(ROOT\n  (S\n    (NP\n      (DET \"Le\")\n      (N \"chat\"))\n  ))\n\n")
        temp.write("SENTENCE 2\n(ROOT\n  (S\n    (VP\n      (V \"dort\"))\n  ))\n\n")
        temp_name = temp.name
    
    parser = SyntaxTreeGenerator()
    with tempfile.NamedTemporaryFile(mode='w+') as sample_file:
        result = parser.extract_samples(temp_name, 1, sample_file.name)
        
        # Read the sample file
        sample_file.seek(0)
        content = sample_file.read()
        
        assert "SAMPLE 1" in content
        assert "SENTENCE 1" in content
        assert "Le" in content
        
    # Clean up
    import os
    os.unlink(temp_name)
