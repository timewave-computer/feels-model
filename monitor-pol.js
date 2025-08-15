// Monitor POL allocations via WebSocket

const WebSocket = require('ws');

console.log('Monitoring POL allocations...\n');

const ws = new WebSocket('ws://localhost:3002');

let polMessages = [];
let floorMessages = [];

ws.on('open', () => {
    console.log('Connected. Running simulation in 2 seconds...\n');
    
    setTimeout(() => {
        ws.send(JSON.stringify({
            action: 'runSimulation',
            params: { scenario: 'bear' }
        }));
    }, 2000);
});

ws.on('message', (data) => {
    try {
        const msg = JSON.parse(data);
        
        if (msg.type === 'console' && msg.message) {
            const log = msg.message;
            
            // Capture POL-related messages
            if (log.includes('POL allocations:') || 
                log.includes('Pool') && log.includes('permanent=') ||
                log.includes('Looking up pool') ||
                log.includes('Found allocation:') ||
                log.includes('No allocation found') ||
                log.includes('Floor price for')) {
                
                console.log(log);
                
                if (log.includes('permanent=')) {
                    polMessages.push(log);
                }
                if (log.includes('Floor price for')) {
                    floorMessages.push(log);
                }
            }
            
            // Detect simulation completion
            if (log.includes('Simulation completed')) {
                console.log('\n=== SIMULATION COMPLETE ===\n');
                
                // Analyze POL allocations
                console.log('POL Allocations found:');
                polMessages.forEach(msg => console.log(msg));
                
                console.log('\nFloor price calculations:');
                floorMessages.forEach(msg => console.log(msg));
                
                // Check if any allocations were made
                const hasAllocations = polMessages.some(msg => {
                    const match = msg.match(/permanent=(\d+\.?\d*)/);
                    return match && parseFloat(match[1]) > 0;
                });
                
                console.log('\n=== ANALYSIS ===');
                if (hasAllocations) {
                    console.log('✓ POL was allocated to pools');
                } else {
                    console.log('❌ No POL allocations found');
                }
                
                if (floorMessages.length > 0) {
                    const hasIncreases = floorMessages.some(msg => {
                        // Check if floor price > 0.01
                        const match = msg.match(/-> (\d+\.?\d*)/);
                        return match && parseFloat(match[1]) > 0.01;
                    });
                    
                    if (hasIncreases) {
                        console.log('✓ Some floor prices increased above 0.01');
                    } else {
                        console.log('⚠️  All floor prices stayed at 0.01');
                    }
                } else {
                    console.log('❌ No floor price calculations logged');
                }
                
                setTimeout(() => {
                    ws.close();
                }, 2000);
            }
        }
    } catch (e) {
        // Ignore parse errors
    }
});

ws.on('close', () => {
    console.log('\nMonitoring complete');
    process.exit(0);
});

// Timeout
setTimeout(() => {
    console.log('\nTimeout reached');
    ws.close();
}, 60000);