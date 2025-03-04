.PHONY: setup clean test run samples help format serve

PYTHON = poetry run python
INPUT_FILE ?= data/pg15288.txt
OUTPUT_FILE ?= output/proust.sexp
CHUNK_SIZE ?= 5000
LANGUAGE ?= fr
SAMPLE_SIZE ?= 10
PORT ?= 8000

help:
	@echo "Available commands:"
	@echo "  make setup      - Set up the project environment"
	@echo "  make run        - Run the parser on the default input file"
	@echo "  make samples    - Generate sample S-expressions from the output"
	@echo "  make test       - Run tests"
	@echo "  make format     - Format code with black and isort"
	@echo "  make clean      - Clean up generated files"
	@echo "  make serve      - Serve the web UI on localhost"
	@echo ""
	@echo "Variables:"
	@echo "  INPUT_FILE      - Input file to parse (default: data/pg15288.txt)"
	@echo "  OUTPUT_FILE     - Output file for S-expressions (default: output/proust.sexp)"
	@echo "  CHUNK_SIZE      - Size of text chunks to process (default: 5000)"
	@echo "  LANGUAGE        - Language of the input text (default: fr)"
	@echo "  SAMPLE_SIZE     - Number of sample S-expressions to extract (default: 10)"
	@echo "  PORT            - Port for web server (default: 8000)"

setup:
	@echo "Setting up poetry environment..."
	@if [ ! -d ".venv" ]; then \
		poetry config virtualenvs.in-project true; \
		poetry install; \
	else \
		poetry install; \
	fi
	$(PYTHON) -m spacy download fr_core_news_sm

run: setup
	@echo "Running parser..."
	$(PYTHON) -m syntree_generator.cli $(INPUT_FILE) -o $(OUTPUT_FILE) -c $(CHUNK_SIZE) -l $(LANGUAGE)

samples: run
	@echo "Generating samples..."
	$(PYTHON) -m syntree_generator.cli $(INPUT_FILE) -o $(OUTPUT_FILE) -c $(CHUNK_SIZE) -l $(LANGUAGE) -s $(SAMPLE_SIZE)

test: setup
	@echo "Running tests..."
	$(PYTHON) -m pytest tests/

format: setup
	@echo "Formatting code..."
	poetry run black syntree_generator/ tests/
	poetry run isort syntree_generator/ tests/

serve:
	@echo "Starting HTTP server on port $(PORT)..."
	cd public && $(PYTHON) -m http.server $(PORT)

clean:
	@echo "Cleaning up..."
	rm -rf output/*.sexp
	rm -rf __pycache__
	rm -rf syntree_generator/__pycache__
	rm -rf tests/__pycache__
	find . -type d -name "*.egg-info" -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete
	find . -type d -name "__pycache__" -exec rm -rf {} +
