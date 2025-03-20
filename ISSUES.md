# GitHub Issues to Create

## Feature Requests
1. **Add Single Test Run Command**: Create a dedicated make target for running individual tests (e.g., `make test-one TEST=test_simple_sentence`)
2. **Create Comprehensive Examples**: Add more examples showcasing different linguistic structures for French and English
3. **Document Output Format**: Add detailed documentation explaining the S-expression output format
4. **Add CLI Option for Output Format**: Allow users to select different output formats (JSON, XML, etc.)

## Bug Fixes 
1. **Fix Sample Extraction Path**: The extract_samples function doesn't handle paths correctly when output_file is not provided
2. **Remove References to _.sh Script**: The README references a shell script that doesn't exist
3. **Documentation Path Error**: Documentation references incorrect path for viewing (docs/index.html vs. served content)

## Testing
1. **Add Integration Tests**: Create integration tests for the complete pipeline
2. **Add More Unit Tests**: Increase test coverage, especially for edge cases