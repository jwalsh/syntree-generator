.PHONY: setup clean test run samples help format serve tangle publish docs download-texts

PYTHON = poetry run python
INPUT_FILE ?= data/pg15288.txt
OUTPUT_FILE ?= output/proust.lisp
CHUNK_SIZE ?= 5000
LANGUAGE ?= fr
SAMPLE_SIZE ?= 10
PORT ?= 8765
EMACS ?= ~/opt/emacs/src/emacs-31.0.50.2

help:
	@echo "Available commands:"
	@echo "  make setup          - Set up the project environment"
	@echo "  make run            - Run the parser on the default input file"
	@echo "  make samples        - Generate sample S-expressions from the output"
	@echo "  make test           - Run tests"
	@echo "  make format         - Format code with black and isort"
	@echo "  make clean          - Clean up generated files"
	@echo "  make serve          - Serve the web UI on localhost"
	@echo "  make tangle         - Tangle org files to generate examples"
	@echo "  make publish        - Publish org files to HTML"
	@echo "  make docs           - Generate all documentation"
	@echo "  make download-texts - Download French literary texts from Gutenberg"
	@echo "  make screenshots    - Capture screenshots of the web UI"
	@echo "  make serve-bg       - Start web server in background for screenshots"
	@echo "  make kill-server    - Stop the background web server"
	@echo "  make screenshots-clean - Clean up screenshots and stop server"
	@echo ""
	@echo "Variables:"
	@echo "  INPUT_FILE      - Input file to parse (default: data/pg15288.txt)"
	@echo "  OUTPUT_FILE     - Output file for S-expressions (default: output/proust.lisp)"
	@echo "  CHUNK_SIZE      - Size of text chunks to process (default: 5000)"
	@echo "  LANGUAGE        - Language of the input text (default: fr)"
	@echo "  SAMPLE_SIZE     - Number of sample S-expressions to extract (default: 10)"
	@echo "  PORT            - Port for web server (default: 8765)"
	@echo "  EMACS           - Path to Emacs executable (default: ~/opt/emacs/src/emacs-31.0.50.2)"

setup:
	@echo "Setting up poetry environment..."
	@if [ ! -d ".venv" ]; then \
		poetry config virtualenvs.in-project true; \
	fi
	poetry install --no-interaction
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

tangle:
	@echo "Tangling org files..."
	$(EMACS) --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"sexp-grammar-examples.org\")"
	$(EMACS) --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"gutenberg.org\")"

download-texts:
	@echo "Downloading French literary texts from Project Gutenberg..."
	$(EMACS) --batch --eval "(require 'org)" --eval "(org-babel-execute-file \"gutenberg.org\")"
	@echo "Done. Texts downloaded to data/ directory."

publish:
	@echo "Publishing org files..."
	$(EMACS) --batch -l publish.el --eval "(org-publish-all t)"

docs: tangle publish

clean:
	@echo "Cleaning up..."
	rm -rf output/*.lisp
	rm -rf __pycache__
	rm -rf syntree_generator/__pycache__
	rm -rf tests/__pycache__
	find . -type d -name "*.egg-info" -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete
	find . -type d -name "__pycache__" -exec rm -rf {} +

.PHONY: screenshots

# Add this to your existing Makefile

screenshots: serve-bg
	@echo "Capturing screenshots using shot-scraper..."
	mkdir -p screenshots
	shot-scraper multi shot-scraper.yml
	@echo "Screenshots saved to screenshots/ directory"

serve-bg:
	@echo "Starting HTTP server in the background..."
	mkdir -p public
	cd public && ($(PYTHON) -m http.server $(PORT) & echo $$! > ../.server.pid)
	@echo "Waiting for server to start..."
	sleep 2

kill-server:
	@if [ -f .server.pid ]; then \
		echo "Stopping background server..."; \
		kill `cat .server.pid` || true; \
		rm .server.pid; \
	else \
		echo "No server running."; \
	fi

screenshots-clean: kill-server
	@echo "Cleaning screenshot files..."
	rm -rf screenshots/*.png
