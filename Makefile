.PHONY: setup clean test run samples help format serve tangle publish docs download-texts screenshots serve-bg kill-server screenshots-clean

PYTHON = poetry run python
INPUT_FILE ?= data/pg15288.txt
OUTPUT_FILE ?= output/proust.lisp
CHUNK_SIZE ?= 5000
LANGUAGE ?= fr
SAMPLE_SIZE ?= 10
PORT ?= 8765
EMACS ?= ~/opt/emacs/src/emacs-31.0.50.2

# Dynamic help system
# Format: "target_name # Description of the target"
help: ## Show this help message
	@echo "Available commands:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36mmake %-16s\033[0m %s\n", $$1, $$2}'
	@echo ""
	@echo "Variables:"
	@echo "  INPUT_FILE      - Input file to parse (default: data/pg15288.txt)"
	@echo "  OUTPUT_FILE     - Output file for S-expressions (default: output/proust.lisp)"
	@echo "  CHUNK_SIZE      - Size of text chunks to process (default: 5000)"
	@echo "  LANGUAGE        - Language of the input text (default: fr)"
	@echo "  SAMPLE_SIZE     - Number of sample S-expressions to extract (default: 10)"
	@echo "  PORT            - Port for web server (default: 8765)"
	@echo "  EMACS           - Path to Emacs executable (default: $(EMACS))"

setup: ## Set up the project environment
	@echo "Setting up poetry environment..."
	@if [ ! -d ".venv" ]; then \
		poetry config virtualenvs.in-project true; \
	fi
	poetry install --no-interaction
	$(PYTHON) -m spacy download fr_core_news_sm

run: setup ## Run the parser on the default input file
	@echo "Running parser..."
	$(PYTHON) -m syntree_generator.cli $(INPUT_FILE) -o $(OUTPUT_FILE) -c $(CHUNK_SIZE) -l $(LANGUAGE)

samples: run ## Generate sample S-expressions from the output
	@echo "Generating samples..."
	$(PYTHON) -m syntree_generator.cli $(INPUT_FILE) -o $(OUTPUT_FILE) -c $(CHUNK_SIZE) -l $(LANGUAGE) -s $(SAMPLE_SIZE)

test: setup ## Run tests
	@echo "Running tests..."
	$(PYTHON) -m pytest tests/

test-one: setup ## Run a single test (e.g., make test-one TEST=test_simple_sentence)
	@echo "Running single test: $(TEST)..."
	$(PYTHON) -m pytest tests/test_parser.py::$(TEST) -v

format: setup ## Format code with black and isort
	@echo "Formatting code..."
	poetry run black syntree_generator/ tests/
	poetry run isort syntree_generator/ tests/

serve: ## Serve the web UI on localhost
	@echo "Starting HTTP server on port $(PORT)..."
	cd public && $(PYTHON) -m http.server $(PORT)

tangle: ## Tangle org files to generate examples
	@echo "Tangling org files..."
	$(EMACS) --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"sexp-grammar-examples.org\")"
	$(EMACS) --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"gutenberg.org\")"

download-texts: ## Download French literary texts from Gutenberg
	@echo "Downloading French literary texts from Project Gutenberg..."
	$(EMACS) --batch --eval "(require 'org)" --eval "(org-babel-execute-file \"gutenberg.org\")"
	@echo "Done. Texts downloaded to data/ directory."

publish: ## Publish org files to HTML
	@echo "Publishing org files..."
	$(EMACS) --batch -l publish.el --eval "(org-publish-all t)"

docs: tangle publish ## Generate all documentation

serve-bg: ## Start web server in background for screenshots
	@echo "Starting HTTP server in the background..."
	mkdir -p public
	cd public && ($(PYTHON) -m http.server $(PORT) & echo $$! > ../.server.pid)
	@echo "Waiting for server to start..."
	sleep 2

kill-server: ## Stop the background web server
	@if [ -f .server.pid ]; then \
		echo "Stopping background server..."; \
		kill `cat .server.pid` || true; \
		rm .server.pid; \
	else \
		echo "No server running."; \
	fi

screenshots: serve-bg ## Capture screenshots of the web UI
	@echo "Capturing screenshots using shot-scraper..."
	mkdir -p screenshots
	shot-scraper multi shot-scraper.yml
	@echo "Screenshots saved to screenshots/ directory"

screenshots-clean: kill-server ## Clean up screenshots and stop server
	@echo "Cleaning screenshot files..."
	rm -rf screenshots/*.png

clean: ## Clean up generated files
	@echo "Cleaning up..."
	rm -rf output/*.lisp
	rm -rf __pycache__
	rm -rf syntree_generator/__pycache__
	rm -rf tests/__pycache__
	find . -type d -name "*.egg-info" -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete
	find . -type d -name "__pycache__" -exec rm -rf {} +
