// Verify POL history tracking and floor price increases

const WebSocket = require('ws');

console.log('Verifying POL history tracking...\n');

const ws = new WebSocket('ws://localhost:3002');

let historyLogs = [];
let floorLogs = [];

ws.on('open', () => {
    console.log('Connected. Running simulation...\n');
    
    setTimeout(() => {
        ws.send(JSON.stringify({
            action: 'runSimulation',
            params: { scenario: 'bear' }
        }));
    }, 1000);
});

ws.on('message', (data) => {
    try {
        const msg = JSON.parse(data);
        
        if (msg.type === 'console' && msg.message) {
            const log = msg.message;
            
            // Capture POL history logs
            if (log.includes('POL allocation history has')) {
                console.log(log);
                historyLogs.push(log);
            }
            
            // Capture floor price logs with timestamps
            if (log.includes('Floor price for') && log.includes('at block')) {
                floorLogs.push(log);
            }
            
            // Look for specific tokens increasing
            if (log.includes('OIP') || log.includes('OYG') || log.includes('HIH')) {
                if (log.includes('Floor price for') || log.includes('POL allocation at this time')) {
                    console.log(log);
                }
            }
            
            // Detect completion
            if (log.includes('Simulation completed')) {
                console.log('\n=== SIMULATION COMPLETE ===\n');
                
                // Analyze floor price progression
                console.log('Analyzing floor price progression...\n');
                
                // Group floor logs by token
                const tokenFloors = {};
                floorLogs.forEach(log => {
                    const match = log.match(/Floor price for (\w+) at block (\d+): ([\d.]+) \/ [\d.]+ = [\d.]+ -> ([\d.]+)/);
                    if (match) {
                        const [_, token, block, allocation, floor] = match;
                        if (!tokenFloors[token]) tokenFloors[token] = [];
                        tokenFloors[token].push({
                            block: parseInt(block),
                            allocation: parseFloat(allocation),
                            floor: parseFloat(floor)
                        });
                    }
                });
                
                // Check for increases
                let hasIncreases = false;
                Object.entries(tokenFloors).forEach(([token, history]) => {
                    if (history.length > 1) {
                        // Sort by block
                        history.sort((a, b) => a.block - b.block);
                        
                        const first = history[0];
                        const last = history[history.length - 1];
                        
                        if (last.floor > first.floor) {
                            console.log(`✓ ${token}: Floor increased from ${first.floor} to ${last.floor}`);
                            console.log(`  POL: ${first.allocation} -> ${last.allocation}`);
                            hasIncreases = true;
                        } else if (last.allocation > first.allocation && last.floor === 0.01) {
                            console.log(`- ${token}: POL increased (${first.allocation} -> ${last.allocation}) but floor stayed at minimum 0.01`);
                        }
                    }
                });
                
                console.log('\n=== VERDICT ===');
                if (hasIncreases) {
                    console.log('✅ SUCCESS: Floor prices are increasing over time!');
                } else {
                    console.log('❌ FAILURE: No floor price increases detected over time');
                }
                
                setTimeout(() => ws.close(), 2000);
            }
        }
    } catch (e) {
        // Ignore
    }
});

ws.on('close', () => {
    console.log('\nTest complete');
    process.exit(0);
});

// Timeout
setTimeout(() => {
    console.log('\nTimeout');
    ws.close();
}, 60000);