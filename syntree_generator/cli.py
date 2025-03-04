#!/usr/bin/env python3
"""
Command line interface for syntree-generator
"""
import argparse
import os
from pathlib import Path
from syntree_generator.parser import SyntaxTreeGenerator

def parse_args():
    parser = argparse.ArgumentParser(
        description="Convert text to S-expression syntax trees"
    )
    parser.add_argument(
        "input_file", 
        help="Input text file to parse"
    )
    parser.add_argument(
        "-o", "--output", 
        dest="output_file",
        default="parsed_output.lisp",
        help="Output file for S-expressions (default: parsed_output.lisp)"
    )
    parser.add_argument(
        "-c", "--chunk-size", 
        dest="chunk_size",
        type=int,
        default=5000,
        help="Size of text chunks to process (default: 5000 characters)"
    )
    parser.add_argument(
        "-l", "--language", 
        dest="language",
        default="fr",
        choices=["fr", "en"],
        help="Language of the input text (default: fr)"
    )
    parser.add_argument(
        "-s", "--sample", 
        dest="sample_size",
        type=int,
        default=0,
        help="Extract a sample of N S-expressions from the output (default: 0, meaning no sampling)"
    )
    
    return parser.parse_args()

def main():
    args = parse_args()
    
    # Resolve input file path
    input_path = Path(args.input_file)
    if not input_path.exists():
        # Try in data directory
        data_dir = Path(__file__).parent.parent / "data"
        alt_path = data_dir / input_path.name
        if alt_path.exists():
            input_path = alt_path
        else:
            print(f"Error: Input file not found at {input_path} or {alt_path}")
            return 1
    
    # Determine output path - use output directory
    output_dir = Path(__file__).parent.parent / "output"
    output_dir.mkdir(exist_ok=True)
    output_path = output_dir / Path(args.output_file).name
    
    # Initialize the parser
    parser = SyntaxTreeGenerator(args.language)
    
    # Run the parser
    output_file = parser.process_file(
        str(input_path), 
        str(output_path),
        args.chunk_size
    )
    
    # Extract samples if requested
    if args.sample_size > 0:
        parser.extract_samples(output_file, args.sample_size)
    
    return 0

if __name__ == "__main__":
    exit(main())
