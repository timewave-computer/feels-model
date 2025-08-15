#!/usr/bin/env node

// Script to verify floor prices using WebSocket direct connection

const WebSocket = require('ws');

function connectAndVerify() {
    return new Promise((resolve, reject) => {
        const ws = new WebSocket('ws://localhost:3002');
        let simulationComplete = false;
        let initialFloorData = null;
        let finalFloorData = null;
        
        ws.on('open', () => {
            console.log('Connected to WebSocket server');
            
            // Wait a bit for connection to stabilize
            setTimeout(() => {
                console.log('\n1. Getting initial chart data...');
                ws.send(JSON.stringify({
                    action: 'getChartData',
                    params: {}
                }));
            }, 1000);
        });
        
        ws.on('message', (data) => {
            try {
                const msg = JSON.parse(data);
                
                // Ignore console messages
                if (msg.type === 'console') {
                    // Log important POL messages
                    if (msg.message && (msg.message.includes('POL') || msg.message.includes('Allocated'))) {
                        console.log(`[POL] ${msg.message}`);
                    }
                    return;
                }
                
                // Handle responses
                if (msg.status === 'success' && msg.result) {
                    if (msg.result.datasets && !simulationComplete && !initialFloorData) {
                        // This is initial chart data
                        initialFloorData = msg.result.datasets.filter(ds => 
                            ds.label && ds.label.includes('Floor')
                        );
                        console.log(`Found ${initialFloorData.length} floor datasets initially`);
                        
                        // Now run simulation
                        console.log('\n2. Running bear market simulation...');
                        setTimeout(() => {
                            ws.send(JSON.stringify({
                                action: 'runSimulation',
                                params: { scenario: 'bear' }
                            }));
                        }, 500);
                    } else if (msg.result.message && msg.result.message.includes('Simulation completed')) {
                        console.log('Simulation completed!');
                        simulationComplete = true;
                        
                        // Get final chart data
                        setTimeout(() => {
                            console.log('\n3. Getting final chart data...');
                            ws.send(JSON.stringify({
                                action: 'getChartData',
                                params: {}
                            }));
                        }, 2000);
                    } else if (msg.result.datasets && simulationComplete) {
                        // This is final chart data
                        finalFloorData = msg.result.datasets.filter(ds => 
                            ds.label && ds.label.includes('Floor')
                        );
                        
                        // Analyze results
                        analyzeResults(initialFloorData, finalFloorData);
                        ws.close();
                        resolve();
                    }
                }
            } catch (e) {
                // Ignore parse errors
            }
        });
        
        ws.on('error', (error) => {
            console.error('WebSocket error:', error);
            reject(error);
        });
        
        ws.on('close', () => {
            console.log('\nWebSocket connection closed');
        });
        
        // Timeout after 30 seconds
        setTimeout(() => {
            console.log('\nTimeout reached');
            ws.close();
            reject(new Error('Timeout'));
        }, 30000);
    });
}

function analyzeResults(initialData, finalData) {
    console.log('\n=== FLOOR PRICE ANALYSIS ===');
    console.log(`Initial floor datasets: ${initialData ? initialData.length : 0}`);
    console.log(`Final floor datasets: ${finalData ? finalData.length : 0}`);
    
    if (!finalData || finalData.length === 0) {
        console.log('\n❌ No floor price data found!');
        return;
    }
    
    let hasIncreases = false;
    let unchangedCount = 0;
    
    finalData.forEach(ds => {
        if (ds.firstDataPoint && ds.lastDataPoint) {
            const first = ds.firstDataPoint.y;
            const last = ds.lastDataPoint.y;
            const change = last - first;
            
            console.log(`\n${ds.label}:`);
            console.log(`  First: ${first.toFixed(6)}`);
            console.log(`  Last: ${last.toFixed(6)}`);
            console.log(`  Change: ${change.toFixed(6)}`);
            
            if (change > 0.000001) {  // Small threshold for floating point
                console.log(`  ✓ Increased`);
                hasIncreases = true;
            } else if (Math.abs(change) < 0.000001) {
                console.log(`  - Unchanged`);
                unchangedCount++;
            } else {
                console.log(`  ❌ Decreased (ERROR!)`);
            }
        }
    });
    
    console.log('\n=== VERDICT ===');
    if (hasIncreases) {
        console.log('✅ SUCCESS: Some floor prices increased!');
    } else if (unchangedCount === finalData.length) {
        console.log('⚠️  WARNING: All floor prices remained at 0.01');
        console.log('\nThis suggests:');
        console.log('- POL is not being allocated to pools');
        console.log('- Or allocated amounts are too small to increase floor above 0.01');
        console.log('- Need at least 1000 POL per pool to see increase (0.01 * 100000 tokens)');
    } else {
        console.log('❌ FAILURE: Floor prices not working correctly');
    }
}

// Run verification
console.log('=== FLOOR PRICE VERIFICATION ===\n');
connectAndVerify().catch(error => {
    console.error('Verification failed:', error.message);
    process.exit(1);
});