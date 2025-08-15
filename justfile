# PureScript Feels Model Development Tasks

# Show available commands
default:
    @just --list

# Ensure logs directory exists
_ensure-logs:
    @mkdir -p logs

# Build the PureScript project and bundle JavaScript
build:
    @echo "Preparing build directories..."
    @mkdir -p dist output/Main
    @echo "Compiling SCSS..."
    @sass assets/styles.scss dist/styles.css --style=compressed --source-map
    @echo "Copying static assets..."
    @cp assets/index.html dist/
    @cp assets/favicon.ico dist/
    @cp assets/favicon.svg dist/
    @cp assets/feels_guy.png dist/
    @cp assets/websocket-client.js dist/ 2>/dev/null || true
    @echo "Building PureScript project..."
    @if [ ! -f src/Main.purs ]; then echo "Error: src/Main.purs not found!"; exit 1; fi
    @npx spago bundle --module Main --outfile dist/bundle.js
    @echo "Build complete! Run 'just serve' to serve the application"

# Build quietly (for internal use)
_build-quiet:
    @mkdir -p dist output/Main
    @sass assets/styles.scss dist/styles.css --style=compressed --source-map > /dev/null 2>&1 || exit 1
    @cp assets/index.html dist/ > /dev/null 2>&1 || exit 1
    @cp assets/favicon.ico dist/ > /dev/null 2>&1 || exit 1
    @cp assets/favicon.svg dist/ > /dev/null 2>&1 || exit 1
    @cp assets/feels_guy.png dist/ > /dev/null 2>&1 || exit 1
    @cp assets/websocket-client.js dist/ 2>/dev/null || true
    @if [ ! -f src/Main.purs ]; then exit 1; fi
    @npx spago bundle --module Main --outfile dist/bundle.js > /dev/null 2>&1 || exit 1

# Run PureScript tests
test:
    @echo "Running PureScript tests..."
    @npx spago test

# Start WebSocket control server (development)
ws-server:
    @echo "Starting WebSocket control server..."
    @echo "WebSocket: ws://localhost:3002"
    @echo "HTTP API: http://localhost:3002"
    @echo "Hit CTRL-C to stop"
    @echo ""
    @node proxy/websocket-server.js

# Send command via WebSocket CLI
ws-cmd action="ping":
    @node proxy/websocket-cli.js {{action}}

# Send ping command (shortcut)
ping:
    @just ws-cmd ping

# Trigger simulation via WebSocket
sim:
    @just ws-cmd runSimulation

# Serve the application locally (with log mirroring)
serve:
    @echo "Starting development servers..."
    @if [ ! -f dist/bundle.js ]; then echo "Bundle not found. Running build first..."; just build; fi
    @echo "Copying log-client.js to dist..."
    @cp assets/log-client.js dist/
    @echo "Installing express temporarily and starting browser log mirror server..."
    @pkill -f "node.*proxy/log-server.js" 2>/dev/null || true
    @pkill -f "http-server.*dist" 2>/dev/null || true
    @echo ""
    @echo "Browser Log Mirror: http://localhost:3001"
    @echo "Application: http://127.0.0.1:9000"
    @echo "Logs will be saved to: logs/feels-browser-console.log"
    @echo "To tail logs in another terminal: tail -f logs/feels-browser-console.log"
    @echo "Hit CTRL-C to stop both servers"
    @echo ""
    @mkdir -p logs
    @# Install express temporarily and start log server in background
    @(npm install express@4.19.2 --no-save > /dev/null 2>&1 && node proxy/log-server.js) > /dev/null 2>&1 & echo $$! > logs/.feels-logserver.pid
    @sleep 2
    @# Start static file server
    @npx http-server dist -c-1 -p 9000 --no-dotfiles || npx http-server dist -c-1 -p 9001 --no-dotfiles

# Serve without log mirroring (original behavior)
serve-simple:
    @echo "Starting simple development server (no log mirroring)..."
    @if [ ! -f dist/bundle.js ]; then echo "Bundle not found. Running build first..."; just build; fi
    @echo "Serving files from dist directory..."
    @echo "Bundle location: $(pwd)/dist/bundle.js"
    @echo "Files in dist directory:"
    @ls dist/
    @echo ""
    @pkill -f "http-server.*dist" 2>/dev/null || true
    @echo "Application will be available at:"
    @echo "  http://127.0.0.1:9000"
    @echo "Hit CTRL-C to stop the server"
    @echo ""
    @npx http-server dist -c-1 -p 9000 --no-dotfiles || npx http-server dist -c-1 -p 9001 --no-dotfiles

# Watch for changes and rebuild automatically
watch:
    @echo "Starting watch mode..."
    @echo "Running initial build..."
    @just build || (echo "Initial build failed. Watching for changes..." && exit 1)
    @echo "Initial build successful!"
    @echo ""
    @pkill -f "http-server.*public" 2>/dev/null || true
    @echo "Application available at:"
    @echo "  http://127.0.0.1:9000"
    @echo "Hit CTRL-C to stop the server"
    @echo "Watching for changes..."
    @echo ""
    @mkdir -p logs
    @npx http-server dist -c-1 -p 9000 > /dev/null 2>&1 & echo $$! > logs/.feels-httpserver.pid
    @# Initialize watch state for fallback method
    @find src test -name "*.purs" -exec ls -la {} + 2>/dev/null > logs/.feels-watch-state.txt || touch logs/.feels-watch-state.txt
    @ls -la assets/styles.scss 2>/dev/null >> logs/.feels-watch-state.txt || true
    @while true; do \
        if command -v inotifywait >/dev/null 2>&1; then \
            echo "Using inotifywait for file watching (Linux)"; \
            inotifywait -q -e modify -r src/ test/ assets/styles.scss > /dev/null 2>&1; \
            echo "Changes detected, rebuilding..."; \
        elif command -v fswatch >/dev/null 2>&1; then \
            echo "Using fswatch for file watching (macOS)"; \
            fswatch -1 -r src/ test/ assets/styles.scss > /dev/null 2>&1; \
            echo "Changes detected, rebuilding..."; \
        else \
            echo "No file watcher available, using polling fallback (2s intervals)"; \
            sleep 2; \
            find src test -name "*.purs" -exec ls -la {} + 2>/dev/null > logs/.feels-watch-state-new.txt || touch logs/.feels-watch-state-new.txt; \
            ls -la assets/styles.scss 2>/dev/null >> logs/.feels-watch-state-new.txt || true; \
            if ! diff logs/.feels-watch-state.txt logs/.feels-watch-state-new.txt > /dev/null 2>&1; then \
                echo "Changes detected, rebuilding..."; \
                mv logs/.feels-watch-state-new.txt logs/.feels-watch-state.txt; \
            else \
                rm -f logs/.feels-watch-state-new.txt; \
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
    @pkill -f "node.*proxy/log-server.js" 2>/dev/null || true
    @pkill -f "http-server.*dist" 2>/dev/null || true
    @rm -rf output/ .spago/ dist/ .sass-cache/
    @rm -f logs/.feels-httpserver.pid logs/.feels-logserver.pid logs/.feels-watch-state.txt logs/.feels-watch-state-new.txt index.js
    @echo "Clean complete!"

# Clean and also remove browser logs
clean-all:
    @echo "Cleaning build artifacts and browser logs..."
    @pkill -f "node.*proxy/log-server.js" 2>/dev/null || true
    @pkill -f "http-server.*dist" 2>/dev/null || true
    @rm -rf output/ .spago/ dist/ .sass-cache/
    @rm -f logs/.feels-httpserver.pid logs/.feels-logserver.pid logs/.feels-watch-state.txt logs/.feels-watch-state-new.txt index.js
    @echo "Clean complete!"

# Build production version without remote control and logging
build-prod:
    @echo "Building production version..."
    @mkdir -p dist output/Main
    @echo "Compiling SCSS..."
    @sass assets/styles.scss dist/styles.css --style=compressed --source-map
    @echo "Copying static assets..."
    @cp assets/index.production.html dist/index.html
    @cp assets/favicon.ico dist/
    @cp assets/favicon.svg dist/
    @cp assets/feels_guy.png dist/
    @cp assets/websocket-client.js dist/ 2>/dev/null || true
    @echo "Building PureScript project..."
    @if [ ! -f src/Main.purs ]; then echo "Error: src/Main.purs not found!"; exit 1; fi
    @npx spago bundle --module Main --outfile dist/bundle.js
    @echo "Production build complete!"

# Install PureScript dependencies
install:
    @echo "Installing PureScript dependencies..."
    @npx spago install

# Deploy to NixOS server
deploy:
    #!/usr/bin/env bash
    set -e
    set -a
    source .env
    set +a
    echo "Deploying feels-model website to NixOS server..."
    echo "Server: root@${feels_server}"
    echo ""
    echo "Building production version..."
    just build-prod
    if [ ! -d "dist" ] || [ ! -f "dist/index.html" ]; then
        echo "Error: Build failed or dist directory not found"
        exit 1
    fi
    echo "Production build completed successfully"
    echo ""
    echo "Syncing files to server..."
    rsync -avz --delete \
        --exclude='.DS_Store' \
        --exclude='*.map' \
        "dist/" \
        "root@${feels_server}:/var/www/feels-model/"
    if [ $? -eq 0 ]; then
        echo "Deployment successful!"
        echo ""
        echo "Website is now available at:"
        echo "   https://model.feels.timewave.computer"
        echo "   http://model.feels.timewave.computer (redirects to HTTPS)"
        echo ""
        echo "To check server status:"
        echo "   ssh root@${feels_server} 'systemctl status nginx'"
    else
        echo "Deployment failed!"
        exit 1
    fi 