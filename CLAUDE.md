# syntree-generator Development Guide

## Build Commands
- Setup environment: `make setup`
- Run all tests: `make test`
- Run single test: `make test-one TEST=test_function_name`
- Format code: `make format`
- Run parser: `make run INPUT_FILE=path/to/input.txt OUTPUT_FILE=path/to/output.lisp`
- Generate samples: `make samples SAMPLE_SIZE=5`
- Serve UI: `make serve`
- Capture screenshots: `make screenshots`

## Code Style Guidelines
- Python 3.8+ compatibility
- Black formatting (line length: 88)
- isort for imports (profile: black)
- Use type hints where possible
- Exception handling: catch specific exceptions, not generic Exception
- Function/variable naming: snake_case
- Class naming: PascalCase
- Constants: UPPERCASE_WITH_UNDERSCORES
- Docstrings for all public functions, classes, and modules
- Test coverage for new functionality