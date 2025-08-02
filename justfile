# PureScript Hello World Development Tasks

# Show available commands
default:
    @just --list

# Build the PureScript project and bundle JavaScript
build:
    @echo "Building PureScript project..."
    @mkdir -p public output/Main
    @if [ ! -f src/Main.purs ]; then echo "Error: src/Main.purs not found!"; exit 1; fi
    @npx spago build
    @echo "Bundling JavaScript..."
    @echo 'import * as Main from "../output/Main/index.js";' > output/entry.js
    @echo 'function main() { Main.main(); }' >> output/entry.js
    @echo 'if (typeof window !== "undefined") { window.addEventListener("load", main); }' >> output/entry.js
    @npx esbuild output/entry.js --bundle --outfile=public/bundle.js --platform=browser
    @rm output/entry.js
    @echo "Build complete! Run 'just serve' to serve the application"

# Build quietly (for internal use)
_build-quiet:
    @mkdir -p public output/Main
    @if [ ! -f src/Main.purs ]; then exit 1; fi
    @npx spago build > /dev/null 2>&1 || exit 1
    @echo 'import * as Main from "../output/Main/index.js";' > output/entry.js
    @echo 'function main() { Main.main(); }' >> output/entry.js
    @echo 'if (typeof window !== "undefined") { window.addEventListener("load", main); }' >> output/entry.js
    @npx esbuild output/entry.js --bundle --outfile=public/bundle.js --platform=browser > /dev/null 2>&1 || exit 1
    @rm output/entry.js

# Run PureScript tests
test:
    @echo "Running PureScript tests..."
    @npx spago test

# Serve the application locally
serve:
    @echo "Starting development server..."
    @if [ ! -f public/bundle.js ]; then echo "Bundle not found. Running build first..."; just build; fi
    @echo "Serving files from public directory..."
    @echo "Bundle location: $(pwd)/public/bundle.js"
    @echo "Files in public directory:"
    @ls public/
    @echo ""
    @pkill -f "http-server.*public" 2>/dev/null || true
    @echo "Application will be available at:"
    @echo "  http://127.0.0.1:8080"
    @echo "Hit CTRL-C to stop the server"
    @echo ""
    @npx http-server public -c-1 -p 8080 || npx http-server public -c-1 -p 8081

# Watch for changes and rebuild automatically
watch:
    @echo "Starting watch mode..."
    @echo "Running initial build..."
    @just build || (echo "Initial build failed. Watching for changes..." && exit 1)
    @echo "Initial build successful!"
    @echo ""
    @pkill -f "http-server.*public" 2>/dev/null || true
    @echo "Application available at:"
    @echo "  http://127.0.0.1:8080"
    @echo "Hit CTRL-C to stop the server"
    @echo "Watching for changes..."
    @echo ""
    @npx http-server public -c-1 -p 8080 > /dev/null 2>&1 & echo $$! > .server.pid
    @find src/ -name "*.purs" -o -name "*.js" > /tmp/purescript_files.txt 2>/dev/null || true
    @while true; do \
        if command -v inotifywait >/dev/null 2>&1; then \
            inotifywait -q -r -e modify src/ > /dev/null 2>&1; \
            echo "Changes detected, rebuilding..."; \
        else \
            sleep 2; \
            if find src/ -name "*.purs" -o -name "*.js" -newer /tmp/purescript_files.txt 2>/dev/null | head -1 | grep -q .; then \
                echo "Changes detected, rebuilding..."; \
                find src/ -name "*.purs" -o -name "*.js" > /tmp/purescript_files.txt 2>/dev/null || true; \
            else \
                continue; \
            fi; \
        fi; \
        if just _build-quiet; then \
            echo "Build successful! ($(date '+%H:%M:%S'))"; \
        else \
            just build; \
            echo "Build failed. Waiting for changes..."; \
        fi; \
    done

# Clean build artifacts
clean:
    @echo "Cleaning build artifacts..."
    @rm -rf output/ .spago/ public/bundle.js
    @rm -f .server.pid
    @echo "Clean complete!"

# Install PureScript dependencies
install:
    @echo "Installing PureScript dependencies..."
    @npx spago install 