# French Literary Texts

This directory contains French literary texts used for testing and demonstrating the syntree-generator's capability to parse complex French syntax.

## Text Collection

The texts are downloaded from Project Gutenberg and are in the public domain. They include:

| Title | Author | Filename | Complexity |
|-------|--------|----------|------------|
| Sodome et Gomorrhe - Première partie | Marcel Proust | pg15288.txt | Very High |
| Sodome et Gomorrhe - Deuxième partie | Marcel Proust | pg15075.txt | Very High |
| La Prisonnière (Sodome et Gomorrhe III) | Marcel Proust | pg60720.txt | Very High |
| Du côté de chez Swann | Marcel Proust | pg2650.txt | Very High |
| Les Fleurs du Mal | Charles Baudelaire | pg6099.txt | High |
| Le livre de la Jungle | Rudyard Kipling (tr.) | pg54183.txt | Medium |

## Download Instructions

You can download these texts using the `gutenberg.org` org-mode file at the root of the repository:

```bash
# Open the file in Emacs and execute the code blocks
emacs gutenberg.org

# Or use this command from the project root
make download-texts
```

## Usage

These texts can be processed with the syntree-generator tool:

```bash
# Example: Parse Du côté de chez Swann
make run INPUT_FILE=data/pg2650.txt OUTPUT_FILE=output/swann.sexp

# Example: Parse Les Fleurs du Mal
make run INPUT_FILE=data/pg6099.txt OUTPUT_FILE=output/baudelaire.sexp
```

## Notes on Text Complexity

- **Marcel Proust** texts have extremely complex sentence structures, making them ideal for testing the parser's ability to handle nested clauses and lengthy subordinate structures.
- **Baudelaire's** poetry uses more concise but syntactically rich structures.
- **Le livre de la Jungle** (French translation) offers more straightforward narrative prose for comparison.

## Copyright Notice

These texts are in the public domain. The text files themselves are not included in the git repository due to their size, but can be downloaded using the provided instructions.
