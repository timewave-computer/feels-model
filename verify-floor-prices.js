#!/usr/bin/env node

// Script to verify floor prices are increasing correctly using the proxy tool

const http = require('http');

function sendCommand(action, params = {}) {
    return new Promise((resolve, reject) => {
        const postData = JSON.stringify({ action, params });
        
        const options = {
            hostname: 'localhost',
            port: 3002,
            path: '/command',
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Content-Length': Buffer.byteLength(postData)
            }
        };
        
        const req = http.request(options, (res) => {
            let body = '';
            res.on('data', (chunk) => {
                body += chunk;
            });
            
            res.on('end', () => {
                try {
                    const response = JSON.parse(body);
                    resolve(response);
                } catch (e) {
                    reject(new Error('Failed to parse response: ' + body));
                }
            });
        });
        
        req.on('error', reject);
        req.write(postData);
        req.end();
    });
}

async function verifyFloorPrices() {
    console.log('=== FLOOR PRICE VERIFICATION ===\n');
    
    try {
        // First, get initial chart data
        console.log('1. Getting initial chart data...');
        const initialData = await sendCommand('getChartData');
        
        if (!initialData.success || !initialData.data || !initialData.data.datasets) {
            console.error('Failed to get initial chart data');
            return;
        }
        
        const initialFloorDatasets = initialData.data.datasets.filter(ds => 
            ds.label && ds.label.includes('Floor')
        );
        
        console.log(`Found ${initialFloorDatasets.length} floor price datasets before simulation`);
        
        // Run bear market simulation
        console.log('\n2. Running bear market simulation...');
        const simResult = await sendCommand('runSimulation', { scenario: 'bear' });
        console.log('Simulation result:', simResult.status);
        
        // Wait for simulation to complete
        console.log('\n3. Waiting for simulation to complete...');
        await new Promise(resolve => setTimeout(resolve, 5000));
        
        // Get chart data after simulation
        console.log('\n4. Getting chart data after simulation...');
        const finalData = await sendCommand('getChartData');
        
        if (!finalData.success || !finalData.data || !finalData.data.datasets) {
            console.error('Failed to get final chart data');
            return;
        }
        
        const finalFloorDatasets = finalData.data.datasets.filter(ds => 
            ds.label && ds.label.includes('Floor')
        );
        
        console.log(`\nFound ${finalFloorDatasets.length} floor price datasets after simulation`);
        
        // Analyze floor price changes
        console.log('\n=== FLOOR PRICE ANALYSIS ===');
        
        let hasIncreases = false;
        finalFloorDatasets.forEach(ds => {
            if (ds.firstDataPoint && ds.lastDataPoint) {
                const first = ds.firstDataPoint.y;
                const last = ds.lastDataPoint.y;
                const change = last - first;
                const percentChange = ((last - first) / first * 100).toFixed(2);
                
                console.log(`\n${ds.label}:`);
                console.log(`  First price: ${first}`);
                console.log(`  Last price: ${last}`);
                console.log(`  Change: ${change.toFixed(6)} (${percentChange}%)`);
                
                if (change > 0) {
                    console.log(`  ✓ Floor price increased`);
                    hasIncreases = true;
                } else if (change === 0) {
                    console.log(`  ⚠️  Floor price unchanged`);
                } else {
                    console.log(`  ❌ Floor price decreased (should not happen!)`);
                }
            }
        });
        
        // Get POL allocation details
        console.log('\n5. Checking POL allocations...');
        const debugData = await sendCommand('debugChartData');
        
        if (debugData.success) {
            console.log('\n=== TOKEN PROGRESSION ===');
            if (debugData.data && debugData.data.tokenProgression) {
                console.log('Token count over time:');
                debugData.data.tokenProgression.forEach(point => {
                    if (typeof point === 'object') {
                        console.log(`  Block: ${point.timestamp}, Tokens: ${point.tokenCount}`);
                    }
                });
            }
        }
        
        // Final verdict
        console.log('\n=== VERDICT ===');
        if (hasIncreases) {
            console.log('✅ SUCCESS: Floor prices are increasing as expected!');
        } else {
            console.log('❌ FAILURE: Floor prices are not increasing');
            console.log('\nPossible issues:');
            console.log('- POL may not be allocated to pools');
            console.log('- Pool IDs may still be mismatched');
            console.log('- Fee collection may not be working');
            console.log('- Token creation may be failing');
        }
        
    } catch (error) {
        console.error('Error during verification:', error.message);
    }
}

// Run the verification
verifyFloorPrices();