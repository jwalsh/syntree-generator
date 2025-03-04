#!/bin/bash
# Setup script for shot-scraper integration

# Install shot-scraper
pip install shot-scraper

# Create screenshots directory
mkdir -p screenshots

# Add .server.pid to .gitignore
if ! grep -q ".server.pid" .gitignore; then
  echo ".server.pid" >> .gitignore
fi

# Add screenshots directory to .gitignore but keep .gitkeep
if ! grep -q "screenshots/" .gitignore; then
  echo "screenshots/*.png" >> .gitignore
  touch screenshots/.gitkeep
fi

echo "shot-scraper has been set up. You can now use:"
echo "  make screenshots - to capture screenshots of the web UI"
echo "  make screenshots-clean - to clean up screenshots and kill the server"
