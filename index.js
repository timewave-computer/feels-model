#!/usr/bin/env node
(() => {
  // output/Effect.Console/foreign.js
  var log = function(s) {
    return function() {
      console.log(s);
    };
  };
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      // eslint-disable-line no-control-regex
      function(c, i2) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i2 + 1;
        var empty7 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty7;
      }
    ) + '"';
  };
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i2 = 0, l = xs.length; i2 < l; i2++) {
        ss[i2] = f(xs[i2]);
      }
      return "[" + ss.join(",") + "]";
    };
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var showBoolean = {
    show: function(v) {
      if (v) {
        return "true";
      }
      ;
      if (!v) {
        return "false";
      }
      ;
      throw new Error("Failed pattern match at Data.Show (line 29, column 1 - line 31, column 23): " + [v.constructor.name]);
    }
  };
  var show = function(dict) {
    return dict.show;
  };
  var showArray = function(dictShow) {
    return {
      show: showArrayImpl(show(dictShow))
    };
  };

  // output/FFI/foreign.js
  var sqrt = (x) => Math.sqrt(x);
  var sin = (x) => Math.sin(x);
  var log2 = (x) => Math.log(x);
  var exp = (x) => Math.exp(x);
  var pow = (x) => (y) => Math.pow(x, y);
  var currentTime = () => Date.now();
  var generateId = (timestamp) => Math.floor(timestamp * Math.random());
  var getTextContent = function(element3) {
    return function() {
      return element3.textContent || "";
    };
  };
  var floor = (n) => Math.floor(n);
  var setTimeout2 = (fn) => (delay) => () => {
    console.log("setTimeout called with delay:", delay);
    return window.setTimeout(() => {
      console.log("setTimeout executing callback");
      const result = fn();
      console.log("Callback result type:", typeof result);
      if (typeof result === "function") {
        console.log("Result is a function, executing it");
        result();
      }
    }, delay);
  };
  var onDOMReady = function(callback) {
    return function() {
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", callback);
      } else {
        callback();
      }
    };
  };
  var registerRemoteAction = function(name16) {
    return function(handler3) {
      return function() {
        if (window.remoteControl && window.remoteControl.registerAction) {
          window.remoteControl.registerAction(name16, function(params) {
            return new Promise((resolve, reject) => {
              try {
                const result = handler3(params)();
                resolve(result);
              } catch (e) {
                reject(e);
              }
            });
          });
          console.log(`Registered remote action: ${name16}`);
        } else {
          console.warn("Remote control not available");
        }
      };
    };
  };
  var triggerUIAction = function(actionName) {
    return function() {
      console.log(`Triggering UI action: ${actionName}`);
      if (actionName === "testTokenValidation") {
        console.log("Testing token validation with scdf/sfd values...");
        const tickerInput = document.querySelector("#token-ticker");
        const nameInput = document.querySelector("#token-name");
        if (tickerInput && nameInput) {
          tickerInput.value = "scdf";
          nameInput.value = "sfd";
          tickerInput.dispatchEvent(new Event("input", { bubbles: true }));
          nameInput.dispatchEvent(new Event("input", { bubbles: true }));
          tickerInput.dispatchEvent(new Event("change", { bubbles: true }));
          nameInput.dispatchEvent(new Event("change", { bubbles: true }));
          console.log("Token validation test values set and events dispatched");
          return true;
        } else {
          console.error("Token input fields not found");
          return false;
        }
      }
      const actionMap = {
        "runSimulation": "button#run-simulation-btn",
        "refreshData": "button#refresh-btn",
        "enterGateway": "button#gateway-btn"
      };
      const selector = actionMap[actionName];
      if (selector) {
        const button2 = document.querySelector(selector);
        if (button2) {
          console.log(`Found button for ${actionName}, clicking...`);
          button2.click();
          return true;
        } else {
          console.error(`Button not found for action ${actionName} (selector: ${selector})`);
          return false;
        }
      } else {
        console.error(`Unknown UI action: ${actionName}`);
        return false;
      }
    };
  };
  var chartInstance = null;
  var checkAndInitializeChart = function() {
    console.log("checkAndInitializeChart wrapper called");
    return function() {
      console.log("=== CHECKING CHART READINESS ===");
      console.log("Document ready state:", document.readyState);
      console.log("Called at:", (/* @__PURE__ */ new Date()).toISOString());
      if (typeof Chart === "undefined") {
        console.error("Chart.js is not loaded! Retrying in 500ms...");
        window.setTimeout(() => checkAndInitializeChart()(), 500);
        return;
      }
      console.log("Chart.js version:", Chart.version);
      const canvas2 = document.getElementById("price-chart");
      const dataElement = document.getElementById("chart-data-hidden");
      console.log("Canvas found:", !!canvas2);
      console.log("Data element found:", !!dataElement);
      if (!canvas2) {
        console.error('Canvas element not found! Looking for id="price-chart"');
        console.log("All canvas elements:", document.querySelectorAll("canvas").length);
      }
      if (!dataElement) {
        console.error('Data element not found! Looking for class="chart-data"');
        console.log("All elements with chart-data class:", document.querySelectorAll(".chart-data").length);
        const panelElements = document.querySelectorAll(".panel");
        console.log("Number of panel elements:", panelElements.length);
        panelElements.forEach((panel, i2) => {
          if (panel.textContent.includes("Price History")) {
            console.log(`Panel ${i2} contains Price History section`);
            console.log("Panel HTML:", panel.innerHTML.substring(0, 300));
          }
        });
      }
      if (!canvas2 || !dataElement) {
        console.warn("Chart elements not ready, retrying in 250ms...");
        window.setTimeout(() => checkAndInitializeChart()(), 250);
        return;
      }
      if (canvas2.width === 0 || canvas2.height === 0) {
        console.warn("Canvas has zero dimensions, retrying in 250ms...");
        window.setTimeout(() => checkAndInitializeChart()(), 250);
        return;
      }
      const rawData = dataElement.textContent;
      console.log("Data element content length:", rawData?.length || 0);
      console.log("Data element content preview:", rawData?.substring(0, 100));
      if (!rawData || rawData.trim() === "" || rawData === "[]") {
        console.warn("Chart data not ready or empty, retrying in 250ms...");
        window.setTimeout(() => checkAndInitializeChart()(), 250);
        return;
      }
      console.log("Chart elements ready, initializing chart...");
      initializePriceChart()();
    };
  };
  var initializePriceChart = function() {
    return function() {
      console.log("=== CHART INITIALIZATION STARTING ===");
      const canvas2 = document.getElementById("price-chart");
      const dataElement = document.getElementById("chart-data-hidden");
      console.log("Canvas found:", !!canvas2);
      console.log("Canvas ID:", canvas2?.id);
      console.log("Canvas parent:", canvas2?.parentElement?.className);
      console.log("Data element found:", !!dataElement);
      console.log("Data element class:", dataElement?.className);
      console.log("Canvas dimensions:", canvas2?.width, "x", canvas2?.height);
      console.log("Canvas computed style:", canvas2 ? window.getComputedStyle(canvas2).display : "no canvas");
      if (typeof Chart === "undefined") {
        console.error("Chart.js is not loaded!");
        return;
      }
      console.log("Chart.js version:", Chart.version);
      if (!canvas2 || !dataElement) {
        console.warn("Chart canvas or data not found");
        return;
      }
      try {
        const rawData = dataElement.textContent;
        console.log("Raw chart data length:", rawData?.length || 0);
        console.log("Raw chart data preview:", rawData?.substring(0, 200) || "empty");
        console.log("Raw chart data full:", rawData);
        if (!rawData || rawData.trim() === "" || rawData === "[]") {
          console.log("No chart data available yet");
          console.log("Drawing placeholder on canvas...");
          const ctx = canvas2.getContext("2d");
          ctx.fillStyle = "#1a1a1a";
          ctx.fillRect(0, 0, canvas2.width, canvas2.height);
          ctx.fillStyle = "#666";
          ctx.font = "14px sans-serif";
          ctx.fillText("No data - run simulation first", 10, canvas2.height / 2);
          console.log("Placeholder drawn");
          return;
        }
        const data = JSON.parse(rawData);
        console.log("Parsed chart data:", data);
        console.log("Data points count:", data.length);
        console.log("First data point:", data[0]);
        console.log("Last data point:", data[data.length - 1]);
        if (!Array.isArray(data) || data.length === 0) {
          console.log("Empty data array");
          return;
        }
        console.log("=== CALLING RENDER CHART ===");
        renderChartJS(canvas2, data);
        console.log("=== CHART RENDER COMPLETE ===");
      } catch (e) {
        console.error("Failed to parse chart data:", e);
        console.error("Error stack:", e.stack);
        const ctx = canvas2.getContext("2d");
        ctx.fillStyle = "red";
        ctx.fillText("Chart Error: " + e.message, 10, 20);
      }
    };
  };
  var getElementById = function(elementId) {
    return function() {
      return document.getElementById(elementId);
    };
  };
  function renderChartJS(canvas2, data) {
    console.log("renderChartJS called with data:", data);
    console.log("Data length:", data.length);
    console.log("First data point:", data[0]);
    console.log("Canvas element:", canvas2);
    console.log("Canvas width:", canvas2.width, "height:", canvas2.height);
    const containerWidth = canvas2.parentElement ? canvas2.parentElement.clientWidth : 800;
    canvas2.width = Math.max(containerWidth - 40, 400);
    canvas2.height = 500;
    console.log("Set canvas dimensions to", canvas2.width, "x", canvas2.height);
    if (chartInstance) {
      console.log("Destroying existing chart instance");
      chartInstance.destroy();
      chartInstance = null;
    }
    const ctx = canvas2.getContext("2d");
    console.log("Got 2D context:", !!ctx);
    const isMultiToken = data.length > 0 && data[0].tokens && Array.isArray(data[0].tokens);
    console.log("Is multi-token data:", isMultiToken);
    console.log("First data point has tokens:", !!data[0]?.tokens);
    console.log("Tokens field is array:", Array.isArray(data[0]?.tokens));
    if (isMultiToken) {
      console.log("Calling renderMultiTokenChart");
      renderMultiTokenChart(ctx, data);
    } else {
      console.log("Calling renderSingleTokenChart");
      renderSingleTokenChart(ctx, data);
    }
  }
  function renderSingleTokenChart(ctx, data) {
    console.log("renderSingleTokenChart called with", data.length, "data points");
    try {
      console.log("Filtering data. First point:", data[0]);
      const validData = data.filter((point) => {
        const isValid = point && (point.block != null || point.timestamp) && point.price != null && !isNaN(point.price) && isFinite(point.price);
        if (!isValid) {
          console.log("Invalid point:", point);
        }
        return isValid;
      });
      console.log("Valid data points before deduplication:", validData.length);
      const timestampMap = /* @__PURE__ */ new Map();
      validData.forEach((point) => {
        const timestamp = point.timestamp || point.block || 0;
        timestampMap.set(timestamp, point);
      });
      const deduplicatedData = Array.from(timestampMap.values()).sort((a2, b2) => {
        const aTime = a2.timestamp || a2.block || 0;
        const bTime = b2.timestamp || b2.block || 0;
        return aTime - bTime;
      });
      console.log("Valid data points after deduplication:", deduplicatedData.length);
      if (deduplicatedData.length === 0) {
        console.error("No valid data points to chart");
        return;
      }
      const priceData = deduplicatedData.map((point, index4) => ({
        x: point.timestamp ? Number(point.timestamp) : index4,
        // Use timestamp or index for x-axis
        y: Number(point.price)
      }));
      console.log("Price data sample:", priceData.slice(0, 3));
      const polData = deduplicatedData.map((point, index4) => ({
        x: point.timestamp ? Number(point.timestamp) : index4,
        // Use timestamp or index for x-axis
        y: point.nfvValue != null && !isNaN(point.nfvValue) && isFinite(point.nfvValue) ? Number(point.nfvValue) : 0
      }));
      try {
        console.log("About to create Chart.js instance...");
        console.log("Chart constructor available:", typeof Chart);
        console.log("priceData length:", priceData.length);
        console.log("First few priceData points:", priceData.slice(0, 3));
        console.log("polData length:", polData.length);
        console.log("First few polData points:", polData.slice(0, 3));
        const chartType = "line";
        console.log("Chart data ready:", priceData.slice(0, 3));
        console.log("Creating Chart instance with type:", chartType);
        console.log("Chart data points:", priceData.length);
        console.log("POL data points:", polData.length);
        chartInstance = new Chart(ctx, {
          type: chartType,
          data: {
            datasets: [{
              label: "JitoSOL/FeelsSOL Price",
              data: priceData,
              borderColor: "#ffc0cb",
              backgroundColor: "rgba(255, 192, 203, 0.1)",
              borderWidth: 2,
              pointRadius: 1,
              tension: 0.1,
              fill: false
            }, {
              label: "POL Floor",
              type: "line",
              data: polData,
              borderColor: "#f59e0b",
              backgroundColor: "rgba(245, 158, 11, 0.1)",
              borderWidth: 2,
              pointRadius: 0,
              fill: true,
              yAxisID: "y2"
            }]
          },
          options: {
            responsive: false,
            maintainAspectRatio: false,
            animation: false,
            plugins: {
              legend: {
                display: true,
                labels: {
                  color: "#000000",
                  font: {
                    family: "monospace"
                  }
                }
              },
              tooltip: {
                mode: "index",
                intersect: false,
                backgroundColor: "rgba(0, 0, 0, 0.8)",
                titleColor: "#e5e7eb",
                bodyColor: "#e5e7eb",
                borderColor: "#374151",
                borderWidth: 1,
                callbacks: {
                  label: function(context) {
                    if (!context.parsed || context.parsed.y == null) {
                      return "No data";
                    }
                    return `${context.dataset.label}: ${context.parsed.y.toFixed(4)}`;
                  }
                }
              }
            },
            scales: {
              x: {
                type: "linear",
                title: {
                  display: true,
                  text: "Time",
                  color: "#000000",
                  font: {
                    family: "monospace"
                  }
                },
                min: deduplicatedData.length > 0 ? Math.min(...deduplicatedData.map((d, i2) => d.timestamp || i2)) : 0,
                max: deduplicatedData.length > 0 ? Math.max(...deduplicatedData.map((d, i2) => d.timestamp || i2)) : 100,
                grid: {
                  color: "#1f2937",
                  drawBorder: false
                },
                ticks: {
                  color: "#000000",
                  font: {
                    family: "monospace"
                  },
                  autoSkip: true,
                  maxTicksLimit: 10
                }
              },
              y: {
                position: "left",
                grid: {
                  color: "#1f2937",
                  drawBorder: false
                },
                ticks: {
                  color: "#000000",
                  font: {
                    family: "monospace"
                  },
                  callback: function(value14) {
                    return value14 != null && typeof value14 === "number" ? value14.toFixed(3) : "0.000";
                  }
                },
                title: {
                  display: true,
                  text: "Price (JitoSOL/FeelsSOL)",
                  color: "#000000"
                }
              },
              y2: {
                position: "right",
                grid: {
                  drawOnChartArea: false
                },
                ticks: {
                  color: "#f59e0b",
                  font: {
                    family: "monospace"
                  },
                  callback: function(value14) {
                    return value14 != null && typeof value14 === "number" ? value14.toFixed(3) : "0.000";
                  }
                },
                title: {
                  display: true,
                  text: "POL Floor",
                  color: "#f59e0b"
                }
              }
            }
          }
        });
        console.log("Chart.js chart created successfully");
        console.log("Chart instance available:", !!chartInstance);
        console.log("Chart instance ID:", chartInstance?.id);
        console.log("Chart datasets:", chartInstance?.data?.datasets?.length);
        window.currentChartInstance = chartInstance;
      } catch (error4) {
        console.error("Error creating Chart.js chart:", error4);
        console.error("Error stack:", error4.stack);
      }
    } catch (error4) {
      console.error("Error in renderSingleTokenChart:", error4);
    }
  }
  function renderMultiTokenChart(ctx, data) {
    console.log("*** MULTI-TOKEN CHART FUNCTION ENTRY ***");
    console.log("Rendering multi-token chart");
    const containerWidth = ctx.canvas.parentElement ? ctx.canvas.parentElement.clientWidth : 800;
    ctx.canvas.width = Math.max(containerWidth - 40, 400);
    ctx.canvas.height = 500;
    console.log("Set canvas dimensions to", ctx.canvas.width, "x", ctx.canvas.height, "(multi-token)");
    console.log("*** STARTING DEDUPLICATION ***");
    console.log("Data points before deduplication:", data.length);
    const timestampMap = /* @__PURE__ */ new Map();
    data.forEach((point) => {
      timestampMap.set(point.timestamp, point);
    });
    const deduplicatedData = Array.from(timestampMap.values()).sort((a2, b2) => a2.timestamp - b2.timestamp);
    console.log("Data points after deduplication:", deduplicatedData.length);
    const tokenTickers = deduplicatedData.length > 0 && Array.isArray(deduplicatedData[0].tokens) ? deduplicatedData[0].tokens.map((t) => t.ticker).filter((ticker) => ticker !== "JitoSOL") : [];
    console.log("Token tickers found:", tokenTickers);
    console.log("Processing", tokenTickers.length, "tokens for chart");
    const colors = [
      "#ffc0cb",
      // pink
      "#ffb6c1",
      // light pink
      "#f59e0b",
      // amber
      "#ef4444",
      // red
      "#8b5cf6",
      // violet
      "#06b6d4",
      // cyan
      "#ec4899",
      // pink
      "#14b8a6"
      // teal
    ];
    const datasets = [];
    const jitoData = deduplicatedData.map((point, index4) => {
      const x = point.timestamp ? Number(point.timestamp) : index4;
      const price = point.price || 1.22;
      return {
        x,
        // Use timestamp for x-axis
        y: price
      };
    });
    datasets.push({
      label: "JitoSOL/FeelsSOL",
      type: "line",
      data: jitoData,
      borderColor: "#000000",
      backgroundColor: "rgba(0, 0, 0, 0.1)",
      borderWidth: 2,
      pointRadius: 1,
      tension: 0.1,
      fill: false
    });
    tokenTickers.forEach((ticker, index4) => {
      const baseColor = colors[index4 % colors.length];
      const tokenData = deduplicatedData.map((point, idx) => {
        const tokenInfo = point.tokens && Array.isArray(point.tokens) ? point.tokens.find((t) => t.ticker === ticker) : null;
        return {
          x: point.timestamp ? Number(point.timestamp) : idx,
          y: tokenInfo && tokenInfo.price > 0 ? tokenInfo.price : null
          // Use null for missing/invalid data
        };
      });
      const validPricePoints = tokenData.filter((point) => point.y !== null).length;
      if (validPricePoints > 0) {
        datasets.push({
          label: `${ticker} Price`,
          type: "line",
          data: tokenData,
          borderColor: baseColor,
          backgroundColor: "transparent",
          borderWidth: 2,
          pointRadius: 0,
          tension: 0.1,
          fill: false,
          spanGaps: true
          // Connect lines across null values
        });
      } else {
        console.log(`Skipping ${ticker} Price - no valid data points`);
      }
      const polData = deduplicatedData.map((point, idx) => {
        const tokenInfo = point.tokens && Array.isArray(point.tokens) ? point.tokens.find((t) => t.ticker === ticker) : null;
        return {
          x: point.timestamp ? Number(point.timestamp) : idx,
          y: tokenInfo && tokenInfo.polFloor > 0 ? tokenInfo.polFloor : null
          // Use null for missing/invalid data
        };
      });
      const validPolPoints = polData.filter((point) => point.y !== null).length;
      if (validPolPoints > 0) {
        datasets.push({
          label: `${ticker} POL Floor`,
          type: "line",
          data: polData,
          borderColor: baseColor,
          backgroundColor: "transparent",
          borderWidth: 1,
          borderDash: [4, 4],
          // Dotted line pattern
          pointRadius: 0,
          tension: 0.1,
          fill: false,
          spanGaps: true
          // Connect lines across null values
        });
      } else {
        console.log(`Skipping ${ticker} POL Floor - no valid data points`);
      }
    });
    console.log("Total datasets created:", datasets.length);
    console.log("Dataset labels:", datasets.map((d) => d.label));
    chartInstance = new Chart(ctx, {
      type: "line",
      data: {
        datasets
      },
      options: {
        responsive: false,
        maintainAspectRatio: false,
        animation: false,
        interaction: {
          mode: "index",
          intersect: false
        },
        plugins: {
          legend: {
            display: true,
            position: "top",
            labels: {
              color: "#000000",
              font: {
                family: "monospace",
                size: 11
              },
              boxWidth: 15,
              padding: 10,
              usePointStyle: true
            }
          },
          tooltip: {
            mode: "index",
            intersect: false,
            backgroundColor: "rgba(0, 0, 0, 0.9)",
            titleColor: "#e5e7eb",
            bodyColor: "#e5e7eb",
            borderColor: "#374151",
            borderWidth: 1,
            padding: 10,
            displayColors: true,
            callbacks: {
              label: function(context) {
                if (!context.parsed || context.parsed.y == null) {
                  return "No data";
                }
                return `${context.dataset.label}: ${context.parsed.y.toFixed(4)}`;
              }
            }
          }
        },
        scales: {
          x: {
            type: "linear",
            title: {
              display: true,
              text: "Time",
              color: "#000000",
              font: {
                family: "monospace"
              }
            },
            min: deduplicatedData.length > 0 ? Math.min(...deduplicatedData.map((d, i2) => d.timestamp || i2)) : 0,
            max: deduplicatedData.length > 0 ? Math.max(...deduplicatedData.map((d, i2) => d.timestamp || i2)) : 100,
            grid: {
              color: "#1f2937",
              drawBorder: false
            },
            ticks: {
              color: "#000000",
              font: {
                family: "monospace",
                size: 11
              },
              autoSkip: true,
              maxTicksLimit: 10
            }
          },
          y: {
            position: "left",
            grid: {
              color: "#1f2937",
              drawBorder: false
            },
            ticks: {
              color: "#000000",
              font: {
                family: "monospace",
                size: 11
              },
              callback: function(value14) {
                return value14 != null && typeof value14 === "number" ? value14.toFixed(3) : "0.000";
              }
            },
            title: {
              display: true,
              text: "Price vs FeelsSOL",
              color: "#000000"
            }
          }
        }
      }
    });
    console.log("Multi-token chart created successfully");
    console.log("Chart instance available:", !!chartInstance);
    console.log("Chart datasets:", chartInstance?.data?.datasets?.length);
    window.currentChartInstance = chartInstance;
  }
  if (typeof window !== "undefined") {
    window.checkAndInitializeChart = checkAndInitializeChart;
    window.initializePriceChart = initializePriceChart;
  }
  var setChartData = function(data) {
    return function() {
      const element3 = document.getElementById("chart-data-hidden");
      if (element3) {
        element3.textContent = JSON.stringify(data);
        console.log("Chart data set:", data.length, "points");
      }
    };
  };

  // output/Control.Bind/foreign.js
  var arrayBind = typeof Array.prototype.flatMap === "function" ? function(arr) {
    return function(f) {
      return arr.flatMap(f);
    };
  } : function(arr) {
    return function(f) {
      var result = [];
      var l = arr.length;
      for (var i2 = 0; i2 < l; i2++) {
        var xs = f(arr[i2]);
        var k = xs.length;
        for (var j = 0; j < k; j++) {
          result.push(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i2 = 0; i2 < l; i2++) {
        var f = fs[i2];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b2) {
      return function(a2) {
        return f(a2)(b2);
      };
    };
  };
  var $$const = function(a2) {
    return function(v) {
      return a2;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(arr[i2]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map111 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map111($$const(x))(f);
      };
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map37 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map37($$const(identity2))(a2))(b2);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure111 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure111(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure111 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure111(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    var pure111 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply2(pure111(f))(a2);
      };
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped12 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bindFlipped12(f)(g(a2));
        };
      };
    };
  };
  var composeKleisli = function(dictBind) {
    var bind16 = bind(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bind16(f(a2))(g);
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Data.Semigroup/foreign.js
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0) return ys;
      if (ys.length === 0) return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq5) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq5 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordNumberImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqNumberImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Eq/index.js
  var eqString = {
    eq: eqStringImpl
  };
  var eqNumber = {
    eq: eqNumberImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eqBoolean = {
    eq: eqBooleanImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eq2 = /* @__PURE__ */ eq(eqBoolean);
  var notEq = function(dictEq) {
    var eq32 = eq(dictEq);
    return function(x) {
      return function(y) {
        return eq2(eq32(x)(y))(false);
      };
    };
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();
  var eqOrdering = {
    eq: function(v) {
      return function(v1) {
        if (v instanceof LT && v1 instanceof LT) {
          return true;
        }
        ;
        if (v instanceof GT && v1 instanceof GT) {
          return true;
        }
        ;
        if (v instanceof EQ && v1 instanceof EQ) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };
  var numSub = function(n1) {
    return function(n2) {
      return n1 - n2;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };
  var numAdd = function(n1) {
    return function(n2) {
      return n1 + n2;
    };
  };
  var numMul = function(n1) {
    return function(n2) {
      return n1 * n2;
    };
  };

  // output/Data.Semiring/index.js
  var zero = function(dict) {
    return dict.zero;
  };
  var semiringNumber = {
    add: numAdd,
    zero: 0,
    mul: numMul,
    one: 1
  };
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };
  var add = function(dict) {
    return dict.add;
  };

  // output/Data.Ring/index.js
  var sub = function(dict) {
    return dict.sub;
  };
  var ringNumber = {
    sub: numSub,
    Semiring0: function() {
      return semiringNumber;
    }
  };
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };
  var negate = function(dictRing) {
    var sub1 = sub(dictRing);
    var zero2 = zero(dictRing.Semiring0());
    return function(a2) {
      return sub1(zero2)(a2);
    };
  };

  // output/Data.Ord/index.js
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordNumber = /* @__PURE__ */ function() {
    return {
      compare: ordNumberImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqNumber;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };
  var comparing = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(f) {
      return function(x) {
        return function(y) {
          return compare32(f(x))(f(y));
        };
      };
    };
  };
  var greaterThanOrEq = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare32(a1)(a2);
        if (v instanceof LT) {
          return false;
        }
        ;
        return true;
      };
    };
  };
  var max = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare32(x)(y);
        if (v instanceof LT) {
          return y;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return x;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [v.constructor.name]);
      };
    };
  };
  var min = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare32(x)(y);
        if (v instanceof LT) {
          return x;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return y;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 172, column 3 - line 175, column 12): " + [v.constructor.name]);
      };
    };
  };
  var abs = function(dictOrd) {
    var greaterThanOrEq1 = greaterThanOrEq(dictOrd);
    return function(dictRing) {
      var zero2 = zero(dictRing.Semiring0());
      var negate1 = negate(dictRing);
      return function(x) {
        var $99 = greaterThanOrEq1(x)(zero2);
        if ($99) {
          return x;
        }
        ;
        return negate1(x);
      };
    };
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a2) {
    return maybe(a2)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error4) {
        return left(error4);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left(error4))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size5 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size5 !== 0) {
          size5--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i2, tmp;
          if (size5 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size5) % limit] = cb;
          size5++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill2(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill2(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step3 = aff;
      var fail2 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step3 = bhead(step3);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail2 = util.left(e);
                step3 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step3)) {
                status = RETURN;
                fail2 = step3;
                step3 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step3 = util.fromRight(step3);
              }
              break;
            case CONTINUE:
              switch (step3.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step3._2;
                  status = CONTINUE;
                  step3 = step3._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step3 = util.right(step3._1);
                  } else {
                    status = STEP_BIND;
                    step3 = step3._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step3 = runSync(util.left, util.right, step3._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step3 = runAsync(util.left, step3._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step3 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail2 = util.left(step3._1);
                  step3 = null;
                  break;
                // Enqueue the Catch so that we can call the error handler later on
                // in case of an exception.
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step3, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step3, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step3 = step3._1;
                  break;
                // Enqueue the Bracket so that we can call the appropriate handlers
                // after resource acquisition.
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step3, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step3, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step3 = step3._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step3._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step3._1) {
                    tmp.run();
                  }
                  step3 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step3 = sequential3(util, supervisor, step3._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step3 = interrupt || fail2 || step3;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  // We cannot recover from an unmasked interrupt. Otherwise we should
                  // continue stepping, or run the exception handler if an exception
                  // was raised.
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail2) {
                      status = CONTINUE;
                      step3 = attempt._2(util.fromLeft(fail2));
                      fail2 = null;
                    }
                    break;
                  // We cannot resume from an unmasked interrupt or exception.
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step3 = util.fromRight(step3);
                    }
                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.
                  case BRACKET:
                    bracketCount--;
                    if (fail2 === null) {
                      result = util.fromRight(step3);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step3 = attempt._3(result);
                      }
                    }
                    break;
                  // Enqueue the appropriate handler. We increase the bracket count
                  // because it should not be cancelled.
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step3, fail2), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step3 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail2) {
                      step3 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                    } else {
                      step3 = attempt._1.completed(util.fromRight(step3))(attempt._2);
                    }
                    fail2 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step3, fail2), attempts, interrupt);
                    status = CONTINUE;
                    step3 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step3 = attempt._1;
                    fail2 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step3));
                }
              }
              joins = null;
              if (interrupt && fail2) {
                setTimeout(function() {
                  throw util.fromLeft(fail2);
                }, 0);
              } else if (util.isLeft(step3) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step3);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step3)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join4;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill2(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error4);
              status = COMPLETED;
              step3 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step3(error4)), attempts, interrupt);
                }
                status = RETURN;
                step3 = null;
                fail2 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step3 = null;
                fail2 = null;
              }
          }
          return canceler;
        };
      }
      function join3(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill: kill2,
        join: join3,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill2(error4, par2, cb2) {
        var step3 = par2;
        var head4 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop: while (true) {
          tmp = null;
          switch (step3.tag) {
            case FORKED:
              if (step3._3 === EMPTY) {
                tmp = fibers[step3._1];
                kills2[count++] = tmp.kill(error4, function(result) {
                  return function() {
                    count--;
                    if (count === 0) {
                      cb2(result)();
                    }
                  };
                });
              }
              if (head4 === null) {
                break loop;
              }
              step3 = head4._2;
              if (tail2 === null) {
                head4 = null;
              } else {
                head4 = tail2._1;
                tail2 = tail2._2;
              }
              break;
            case MAP:
              step3 = step3._2;
              break;
            case APPLY:
            case ALT:
              if (head4) {
                tail2 = new Aff2(CONS, head4, tail2);
              }
              head4 = step3;
              step3 = step3._1;
              break;
          }
        }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head4, tail2) {
        var fail2, step3, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail2 = result;
          step3 = null;
        } else {
          step3 = result;
          fail2 = null;
        }
        loop: while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null;
          if (interrupt !== null) {
            return;
          }
          if (head4 === null) {
            cb(fail2 || step3)();
            return;
          }
          if (head4._3 !== EMPTY) {
            return;
          }
          switch (head4.tag) {
            case MAP:
              if (fail2 === null) {
                head4._3 = util.right(head4._1(util.fromRight(step3)));
                step3 = head4._3;
              } else {
                head4._3 = fail2;
              }
              break;
            case APPLY:
              lhs = head4._1._3;
              rhs = head4._2._3;
              if (fail2) {
                head4._3 = fail2;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, fail2 === lhs ? head4._2 : head4._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join3(fail2, null, null);
                    } else {
                      join3(fail2, tail2._1, tail2._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              } else if (lhs === EMPTY || rhs === EMPTY) {
                return;
              } else {
                step3 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head4._3 = step3;
              }
              break;
            case ALT:
              lhs = head4._1._3;
              rhs = head4._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail2 = step3 === lhs ? rhs : lhs;
                step3 = null;
                head4._3 = fail2;
              } else {
                head4._3 = step3;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, step3 === lhs ? head4._2 : head4._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join3(step3, null, null);
                    } else {
                      join3(step3, tail2._1, tail2._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              }
              break;
          }
          if (tail2 === null) {
            head4 = null;
          } else {
            head4 = tail2._1;
            tail2 = tail2._2;
          }
        }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join3(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step3 = par;
        var head4 = null;
        var tail2 = null;
        var tmp, fid;
        loop: while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step3.tag) {
                case MAP:
                  if (head4) {
                    tail2 = new Aff2(CONS, head4, tail2);
                  }
                  head4 = new Aff2(MAP, step3._1, EMPTY, EMPTY);
                  step3 = step3._2;
                  break;
                case APPLY:
                  if (head4) {
                    tail2 = new Aff2(CONS, head4, tail2);
                  }
                  head4 = new Aff2(APPLY, EMPTY, step3._2, EMPTY);
                  step3 = step3._1;
                  break;
                case ALT:
                  if (head4) {
                    tail2 = new Aff2(CONS, head4, tail2);
                  }
                  head4 = new Aff2(ALT, EMPTY, step3._2, EMPTY);
                  step3 = step3._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step3;
                  step3 = new Aff2(FORKED, fid, new Aff2(CONS, head4, tail2), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step3)
                  })();
                  fibers[fid] = tmp;
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
              }
              break;
            case RETURN:
              if (head4 === null) {
                break loop;
              }
              if (head4._1 === EMPTY) {
                head4._1 = step3;
                status = CONTINUE;
                step3 = head4._2;
                head4._2 = EMPTY;
              } else {
                head4._2 = step3;
                step3 = head4;
                if (tail2 === null) {
                  head4 = null;
                } else {
                  head4 = tail2._1;
                  tail2 = tail2._2;
                }
              }
          }
        }
        root = step3;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill2(error4, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential3(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value14) {
          return Aff.Pure(f(value14));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  function _fork(immediate) {
    return function(aff) {
      return Aff.Fork(immediate, aff);
    };
  }
  var _liftEffect = Aff.Sync;
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  var makeAff = Aff.Async;
  function generalBracket(acquire) {
    return function(options2) {
      return function(k) {
        return Aff.Bracket(acquire, options2, k);
      };
    };
  }
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _sequential = Aff.Seq;

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind7 = bind(dictMonad.Bind1());
    var unless3 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind7(mb)(function(b2) {
          return unless3(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind7 = bind(dictMonad.Bind1());
    var pure27 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind7(f)(function(f$prime) {
          return bind7(a2)(function(a$prime) {
            return pure27(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  // output/Effect/foreign.js
  var pureE = function(a2) {
    return function() {
      return a2;
    };
  };
  var bindE = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0) return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0) return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Effect.Exception/foreign.js
  function error2(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error2($4));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map37 = map(Monad0.Bind1().Apply0().Functor0());
    var pure27 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map37(Right.create)(a2))(function($52) {
        return pure27(Left.create($52));
      });
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref2) {
    return function() {
      return ref2.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref2) {
      return function() {
        var t = f(ref2.value);
        ref2.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref2) {
      return function() {
        ref2.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_ = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map3 = /* @__PURE__ */ map(functorEffect);
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var monadRecEffect = {
    tailRecM: function(f) {
      return function(a2) {
        var fromDone = function(v) {
          if (v instanceof Done) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 137, column 30 - line 137, column 44): " + [v.constructor.name]);
        };
        return function __do7() {
          var r = bindFlipped2($$new)(f(a2))();
          (function() {
            while (!function __do8() {
              var v = read(r)();
              if (v instanceof Loop) {
                var e = f(v.value0)();
                write(e)(r)();
                return false;
              }
              ;
              if (v instanceof Done) {
                return true;
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 128, column 22 - line 133, column 28): " + [v.constructor.name]);
            }()) {
            }
            ;
            return {};
          })();
          return map3(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a2) {
      return function() {
        return f(a2());
      };
    };
  };
  var pure_ = function(a2) {
    return function() {
      return a2;
    };
  };
  var bind_ = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };
  var foreach = function(as) {
    return function(f) {
      return function() {
        for (var i2 = 0, l = as.length; i2 < l; i2++) {
          f(as[i2])();
        }
      };
    };
  };

  // output/Control.Monad.ST.Internal/index.js
  var $runtime_lazy2 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var functorST = {
    map: map_
  };
  var monadST = {
    Applicative0: function() {
      return applicativeST;
    },
    Bind1: function() {
      return bindST;
    }
  };
  var bindST = {
    bind: bind_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var applicativeST = {
    pure: pure_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy2("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b2) {
    return !b2;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a2) {
      return function(b2) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };
  var heytingAlgebraFunction = function(dictHeytingAlgebra) {
    var ff1 = ff(dictHeytingAlgebra);
    var tt1 = tt(dictHeytingAlgebra);
    var implies1 = implies(dictHeytingAlgebra);
    var conj1 = conj(dictHeytingAlgebra);
    var disj1 = disj(dictHeytingAlgebra);
    var not1 = not(dictHeytingAlgebra);
    return {
      ff: function(v) {
        return ff1;
      },
      tt: function(v) {
        return tt1;
      },
      implies: function(f) {
        return function(g) {
          return function(a2) {
            return implies1(f(a2))(g(a2));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a2) {
            return conj1(f(a2))(g(a2));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a2) {
            return disj1(f(a2))(g(a2));
          };
        };
      },
      not: function(f) {
        return function(a2) {
          return not1(f(a2));
        };
      }
    };
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var snd = function(v) {
    return v.value1;
  };
  var showTuple = function(dictShow) {
    var show26 = show(dictShow);
    return function(dictShow1) {
      var show112 = show(dictShow1);
      return {
        show: function(v) {
          return "(Tuple " + (show26(v.value0) + (" " + (show112(v.value1) + ")")));
        }
      };
    };
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var modify_2 = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var get = function(dictMonadState) {
    return state(dictMonadState)(function(s) {
      return new Tuple(s, s);
    });
  };

  // output/Effect.Class/index.js
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map4 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var runExceptT = function(v) {
    return v;
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map111 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map111(map4(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind7 = bind(dictMonad.Bind1());
    var pure27 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind7(v)(either(function($193) {
            return pure27(Left.create($193));
          })(function(a2) {
            var v1 = k(a2);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $194 = pure(dictMonad.Applicative0());
        return function($195) {
          return ExceptT($194(Right.create($195)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $204 = pure(dictMonad.Applicative0());
        return function($205) {
          return ExceptT($204(Left.create($205)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure27 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond2(f($454));
        })(pure27(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_14 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_14(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var sum = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictSemiring) {
      return foldl22(add(dictSemiring))(zero(dictSemiring));
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty3;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append8 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append8(f(x))(acc);
          };
        })(mempty3);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var find = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(p2) {
      var go2 = function(v) {
        return function(v1) {
          if (v instanceof Nothing && p2(v1)) {
            return new Just(v1);
          }
          ;
          return v;
        };
      };
      return foldl22(go2)(Nothing.value);
    };
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = /* @__PURE__ */ function() {
    function array1(a2) {
      return [a2];
    }
    function array2(a2) {
      return function(b2) {
        return [a2, b2];
      };
    }
    function array3(a2) {
      return function(b2) {
        return function(c) {
          return [a2, b2, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply2) {
      return function(map37) {
        return function(pure27) {
          return function(f) {
            return function(array) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure27([]);
                  case 1:
                    return map37(array1)(f(array[bot]));
                  case 2:
                    return apply2(map37(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply2(apply2(map37(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply2(map37(concat2)(go2(bot, pivot)))(go2(pivot, top3));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity4);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };
  var sequence = function(dict) {
    return dict.sequence;
  };

  // output/Control.Parallel/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential3 = sequential(dictParallel);
    var parallel4 = parallel(dictParallel);
    return function(dictApplicative) {
      var traverse_10 = traverse_(dictApplicative);
      return function(dictFoldable) {
        var traverse_14 = traverse_10(dictFoldable);
        return function(f) {
          var $51 = traverse_14(function($53) {
            return parallel4(f($53));
          });
          return function($52) {
            return sequential3($51($52));
          };
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    var parTraverse_1 = parTraverse_(dictParallel);
    return function(dictApplicative) {
      var parTraverse_2 = parTraverse_1(dictApplicative);
      return function(dictFoldable) {
        return parTraverse_2(dictFoldable)(identity5);
      };
    };
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy3 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var map5 = /* @__PURE__ */ map(functorEffect);
  var Canceler = function(x) {
    return x;
  };
  var suspendAff = /* @__PURE__ */ _fork(false);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var map1 = /* @__PURE__ */ map(functorAff);
  var forkAff = /* @__PURE__ */ _fork(true);
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do7() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var bracket = function(acquire) {
    return function(completed) {
      return generalBracket(acquire)({
        killed: $$const(completed),
        failed: $$const(completed),
        completed: $$const(completed)
      });
    };
  };
  var applyParAff = {
    apply: _parAffApply,
    Functor0: function() {
      return functorParAff;
    }
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy3("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var applyAff = /* @__PURE__ */ $lazy_applyAff(73);
  var pure22 = /* @__PURE__ */ pure(applicativeAff);
  var bind1 = /* @__PURE__ */ bind(bindAff);
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindAff);
  var $$finally = function(fin) {
    return function(a2) {
      return bracket(pure22(unit))($$const(fin))($$const(a2));
    };
  };
  var parallelAff = {
    parallel: unsafeCoerce2,
    sequential: _sequential,
    Apply0: function() {
      return applyAff;
    },
    Apply1: function() {
      return applyParAff;
    }
  };
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var applicativeParAff = {
    pure: function($76) {
      return parallel2(pure22($76));
    },
    Apply0: function() {
      return applyParAff;
    }
  };
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var effectCanceler = function($77) {
    return Canceler($$const(liftEffect2($77)));
  };
  var joinFiber = function(v) {
    return makeAff(function(k) {
      return map5(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map1(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void3(v.kill(e, $$const(pure2(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map5(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped3(function($83) {
        return liftEffect2(k($83));
      })($$try2(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void3(runAff(k)(aff));
    };
  };
  var monadRecAff = {
    tailRecM: function(k) {
      var go2 = function(a2) {
        return bind1(k(a2))(function(res) {
          if (res instanceof Done) {
            return pure22(res.value0);
          }
          ;
          if (res instanceof Loop) {
            return go2(res.value0);
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 104, column 7 - line 106, column 23): " + [res.constructor.name]);
        });
      };
      return go2;
    },
    Monad0: function() {
      return monadAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));

  // output/Effect.Aff.Class/index.js
  var monadAffAff = {
    liftAff: /* @__PURE__ */ identity(categoryFn),
    MonadEffect0: function() {
      return monadEffectAff;
    }
  };

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name16) {
    return function(node) {
      return function() {
        return node[name16];
      };
    };
  };
  var children = getEffProp("children");
  var _firstElementChild = getEffProp("firstElementChild");
  var _lastElementChild = getEffProp("lastElementChild");
  var childElementCount = getEffProp("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.DOM.ParentNode/index.js
  var map6 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map6(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener2(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return doc.readyState;
  }

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading = /* @__PURE__ */ function() {
    function Loading2() {
    }
    ;
    Loading2.value = new Loading2();
    return Loading2;
  }();
  var Interactive = /* @__PURE__ */ function() {
    function Interactive2() {
    }
    ;
    Interactive2.value = new Interactive2();
    return Interactive2;
  }();
  var Complete = /* @__PURE__ */ function() {
    function Complete2() {
    }
    ;
    Complete2.value = new Complete2();
    return Complete2;
  }();
  var parse = function(v) {
    if (v === "loading") {
      return new Just(Loading.value);
    }
    ;
    if (v === "interactive") {
      return new Just(Interactive.value);
    }
    ;
    if (v === "complete") {
      return new Just(Complete.value);
    }
    ;
    return Nothing.value;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var map7 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = function(doc) {
    return map7(function() {
      var $4 = fromMaybe(Loading.value);
      return function($5) {
        return $4(parse($5));
      };
    }())(function() {
      return _readyState(doc);
    });
  };

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value14) {
    var tag = Object.prototype.toString.call(value14);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value14);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode = unsafeCoerce2;
  var fromElement = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var maybe2 = f(value14);
                if (isNothing2(maybe2)) return result;
                var tuple = fromJust5(maybe2);
                result.push(fst2(tuple));
                value14 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var tuple = f(value14);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2)) return result;
                value14 = fromJust5(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };

  // output/Data.Enum/index.js
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum1 = toEnum(dictBoundedEnum);
    var fromEnum1 = fromEnum(dictBoundedEnum);
    var bottom22 = bottom(dictBoundedEnum.Bounded0());
    return function(low2) {
      return function(high2) {
        return function(x) {
          var v = toEnum1(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x < fromEnum1(bottom22);
            if ($140) {
              return low2;
            }
            ;
            return high2;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Web.HTML.Window/foreign.js
  function document2(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Web.HTML.Event.EventTypes/index.js
  var domcontentloaded = "DOMContentLoaded";
  var change = "change";

  // output/Halogen.Aff.Util/index.js
  var bind2 = /* @__PURE__ */ bind(bindAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure3 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var pure1 = /* @__PURE__ */ pure(applicativeEffect);
  var map8 = /* @__PURE__ */ map(functorEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query2) {
    return bind2(liftEffect3(bindFlipped4(composeKleisliFlipped2(function() {
      var $16 = querySelector(query2);
      return function($17) {
        return $16(toParentNode($17));
      };
    }())(document2))(windowImpl)))(function(mel) {
      return pure3(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do7() {
      var rs = bindFlipped4(readyState)(bindFlipped4(document2)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map8(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener2(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });
  var awaitBody = /* @__PURE__ */ discard2(bindAff)(awaitLoad)(function() {
    return bind2(selectElement("body"))(function(body2) {
      return maybe(throwError2(error2("Could not find body")))(pure3)(body2);
    });
  });

  // output/Data.Exists/index.js
  var runExists = unsafeCoerce2;
  var mkExists = unsafeCoerce2;

  // output/Data.Coyoneda/index.js
  var CoyonedaF = /* @__PURE__ */ function() {
    function CoyonedaF2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CoyonedaF2.create = function(value0) {
      return function(value1) {
        return new CoyonedaF2(value0, value1);
      };
    };
    return CoyonedaF2;
  }();
  var unCoyoneda = function(f) {
    return function(v) {
      return runExists(function(v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };
  var coyoneda = function(k) {
    return function(fi) {
      return mkExists(new CoyonedaF(k, fi));
    };
  };
  var functorCoyoneda = {
    map: function(f) {
      return function(v) {
        return runExists(function(v1) {
          return coyoneda(function($180) {
            return f(v1.value0($180));
          })(v1.value1);
        })(v);
      };
    }
  };
  var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

  // output/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(i2)(xs[i2]);
      }
      return result;
    };
  };

  // output/Data.FunctorWithIndex/index.js
  var mapWithIndex = function(dict) {
    return dict.mapWithIndex;
  };
  var functorWithIndexArray = {
    mapWithIndex: mapWithIndexArray,
    Functor0: function() {
      return functorArray;
    }
  };

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var singleton2 = function(dictPlus) {
    var empty7 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty7);
    };
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil2() {
    }
    ;
    Nil2.value = new Nil2();
    return Nil2;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons2.create = function(value0) {
      return function(value1) {
        return new Cons2(value0, value1);
      };
    };
    return Cons2;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var map9 = /* @__PURE__ */ map(functorList);
  var foldableList = {
    foldr: function(f) {
      return function(b2) {
        var rev3 = function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b2);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b2, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b2;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b2)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty3);
      };
    }
  };
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var monoidList = /* @__PURE__ */ function() {
    return {
      mempty: Nil.value,
      Semigroup0: function() {
        return semigroupList;
      }
    };
  }();
  var applyList = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v instanceof Cons) {
          return append1(map9(v.value0)(v1))(apply(applyList)(v.value1)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Types (line 157, column 1 - line 159, column 48): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorList;
    }
  };
  var bindList = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v instanceof Cons) {
          return append1(v1(v.value0))(bind(bindList)(v.value1)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Types (line 164, column 1 - line 166, column 37): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyList;
    }
  };
  var applicativeList = {
    pure: function(a2) {
      return new Cons(a2, Nil.value);
    },
    Apply0: function() {
      return applyList;
    }
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();

  // output/Data.Map.Internal/index.js
  var $runtime_lazy4 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Node = /* @__PURE__ */ function() {
    function Node2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    Node2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new Node2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return Node2;
  }();
  var IterLeaf = /* @__PURE__ */ function() {
    function IterLeaf2() {
    }
    ;
    IterLeaf2.value = new IterLeaf2();
    return IterLeaf2;
  }();
  var IterEmit = /* @__PURE__ */ function() {
    function IterEmit2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    IterEmit2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new IterEmit2(value0, value1, value22);
        };
      };
    };
    return IterEmit2;
  }();
  var IterNode = /* @__PURE__ */ function() {
    function IterNode2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    IterNode2.create = function(value0) {
      return function(value1) {
        return new IterNode2(value0, value1);
      };
    };
    return IterNode2;
  }();
  var Split = /* @__PURE__ */ function() {
    function Split2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Split2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Split2(value0, value1, value22);
        };
      };
    };
    return Split2;
  }();
  var SplitLast = /* @__PURE__ */ function() {
    function SplitLast2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    SplitLast2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new SplitLast2(value0, value1, value22);
        };
      };
    };
    return SplitLast2;
  }();
  var unsafeNode = function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return new Node(1, 1, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 702, column 5 - line 706, column 39): " + [r.constructor.name]);
    }
    ;
    if (l instanceof Node) {
      if (r instanceof Leaf) {
        return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + function() {
          var $280 = l.value0 > r.value0;
          if ($280) {
            return l.value0;
          }
          ;
          return r.value0;
        }() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 708, column 5 - line 712, column 68): " + [r.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 700, column 32 - line 712, column 68): " + [l.constructor.name]);
  };
  var toMapIter = /* @__PURE__ */ function() {
    return flip(IterNode.create)(IterLeaf.value);
  }();
  var stepWith = function(f) {
    return function(next) {
      return function(done) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v instanceof IterLeaf) {
              $tco_done = true;
              return done(unit);
            }
            ;
            if (v instanceof IterEmit) {
              $tco_done = true;
              return next(v.value0, v.value1, v.value2);
            }
            ;
            if (v instanceof IterNode) {
              $copy_v = f(v.value1)(v.value0);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 940, column 8 - line 946, column 20): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2;
      };
    };
  };
  var size3 = function(v) {
    if (v instanceof Leaf) {
      return 0;
    }
    ;
    if (v instanceof Node) {
      return v.value1;
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 618, column 8 - line 620, column 24): " + [v.constructor.name]);
  };
  var singleton3 = function(k) {
    return function(v) {
      return new Node(1, 1, k, v, Leaf.value, Leaf.value);
    };
  };
  var unsafeBalancedNode = /* @__PURE__ */ function() {
    var height9 = function(v) {
      if (v instanceof Leaf) {
        return 0;
      }
      ;
      if (v instanceof Node) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 757, column 12 - line 759, column 26): " + [v.constructor.name]);
    };
    var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
      if (rl instanceof Node && rl.value0 > height9(rr)) {
        return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
      }
      ;
      return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
    };
    var rotateRight = function(k, v, lk, lv, ll, lr, r) {
      if (lr instanceof Node && height9(ll) <= lr.value0) {
        return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
      }
      ;
      return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
    };
    return function(k, v, l, r) {
      if (l instanceof Leaf) {
        if (r instanceof Leaf) {
          return singleton3(k)(v);
        }
        ;
        if (r instanceof Node && r.value0 > 1) {
          return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      if (l instanceof Node) {
        if (r instanceof Node) {
          if (r.value0 > (l.value0 + 1 | 0)) {
            return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
          }
          ;
          if (l.value0 > (r.value0 + 1 | 0)) {
            return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
          }
          ;
        }
        ;
        if (r instanceof Leaf && l.value0 > 1) {
          return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 717, column 40 - line 738, column 34): " + [l.constructor.name]);
    };
  }();
  var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy4("unsafeSplit", "Data.Map.Internal", function() {
    return function(comp, k, m) {
      if (m instanceof Leaf) {
        return new Split(Nothing.value, Leaf.value, Leaf.value);
      }
      ;
      if (m instanceof Node) {
        var v = comp(k)(m.value2);
        if (v instanceof LT) {
          var v1 = $lazy_unsafeSplit(793)(comp, k, m.value4);
          return new Split(v1.value0, v1.value1, unsafeBalancedNode(m.value2, m.value3, v1.value2, m.value5));
        }
        ;
        if (v instanceof GT) {
          var v1 = $lazy_unsafeSplit(796)(comp, k, m.value5);
          return new Split(v1.value0, unsafeBalancedNode(m.value2, m.value3, m.value4, v1.value1), v1.value2);
        }
        ;
        if (v instanceof EQ) {
          return new Split(new Just(m.value3), m.value4, m.value5);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 791, column 5 - line 799, column 30): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 787, column 34 - line 799, column 30): " + [m.constructor.name]);
    };
  });
  var unsafeSplit = /* @__PURE__ */ $lazy_unsafeSplit(786);
  var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy4("unsafeSplitLast", "Data.Map.Internal", function() {
    return function(k, v, l, r) {
      if (r instanceof Leaf) {
        return new SplitLast(k, v, l);
      }
      ;
      if (r instanceof Node) {
        var v1 = $lazy_unsafeSplitLast(779)(r.value2, r.value3, r.value4, r.value5);
        return new SplitLast(v1.value0, v1.value1, unsafeBalancedNode(k, v, l, v1.value2));
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 776, column 37 - line 780, column 57): " + [r.constructor.name]);
    };
  });
  var unsafeSplitLast = /* @__PURE__ */ $lazy_unsafeSplitLast(775);
  var unsafeJoinNodes = function(v, v1) {
    if (v instanceof Leaf) {
      return v1;
    }
    ;
    if (v instanceof Node) {
      var v2 = unsafeSplitLast(v.value2, v.value3, v.value4, v.value5);
      return unsafeBalancedNode(v2.value0, v2.value1, v2.value2, v1);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 764, column 25 - line 768, column 38): " + [v.constructor.name, v1.constructor.name]);
  };
  var lookup = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Node) {
            var v1 = compare6(k)(v.value2);
            if (v1 instanceof LT) {
              $copy_v = v.value4;
              return;
            }
            ;
            if (v1 instanceof GT) {
              $copy_v = v.value5;
              return;
            }
            ;
            if (v1 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value3);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 283, column 7 - line 286, column 22): " + [v1.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 280, column 8 - line 286, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var iterMapL = /* @__PURE__ */ function() {
    var go2 = function($copy_iter) {
      return function($copy_v) {
        var $tco_var_iter = $copy_iter;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(iter, v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return iter;
          }
          ;
          if (v instanceof Node) {
            if (v.value5 instanceof Leaf) {
              $tco_var_iter = new IterEmit(v.value2, v.value3, iter);
              $copy_v = v.value4;
              return;
            }
            ;
            $tco_var_iter = new IterEmit(v.value2, v.value3, new IterNode(v.value5, iter));
            $copy_v = v.value4;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 951, column 13 - line 958, column 48): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_iter, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go2;
  }();
  var stepAscCps = /* @__PURE__ */ stepWith(iterMapL);
  var stepUnfoldr = /* @__PURE__ */ function() {
    var step3 = function(k, v, next) {
      return new Just(new Tuple(new Tuple(k, v), next));
    };
    return stepAscCps(step3)(function(v) {
      return Nothing.value;
    });
  }();
  var toUnfoldable = function(dictUnfoldable) {
    var $784 = unfoldr(dictUnfoldable)(stepUnfoldr);
    return function($785) {
      return $784(toMapIter($785));
    };
  };
  var insert = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var go2 = function(v1) {
          if (v1 instanceof Leaf) {
            return singleton3(k)(v);
          }
          ;
          if (v1 instanceof Node) {
            var v2 = compare6(k)(v1.value2);
            if (v2 instanceof LT) {
              return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
            }
            ;
            if (v2 instanceof GT) {
              return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
            }
            ;
            if (v2 instanceof EQ) {
              return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 471, column 7 - line 474, column 35): " + [v2.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 468, column 8 - line 474, column 35): " + [v1.constructor.name]);
        };
        return go2;
      };
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy4("go", "Data.Map.Internal", function() {
          return function(m$prime, z$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(172)(m$prime.value4, f(m$prime.value3)($lazy_go(172)(m$prime.value5, z$prime)));
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 169, column 26 - line 172, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(169);
        return function(m) {
          return go2(m, z);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy4("go", "Data.Map.Internal", function() {
          return function(z$prime, m$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(178)(f($lazy_go(178)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 175, column 26 - line 178, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(175);
        return function(m) {
          return go2(z, m);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      var append14 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty3;
          }
          ;
          if (v instanceof Node) {
            return append14(go2(v.value4))(append14(f(v.value3))(go2(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 181, column 10 - line 184, column 28): " + [v.constructor.name]);
        };
        return go2;
      };
    }
  };
  var values = /* @__PURE__ */ function() {
    return foldr(foldableMap)(Cons.create)(Nil.value);
  }();
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var fromFoldable = function(dictOrd) {
    var insert14 = insert(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert14(v.value0)(v.value1)(m);
        };
      })(empty2);
    };
  };
  var $$delete = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(k) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare6(k)(v.value2);
          if (v1 instanceof LT) {
            return unsafeBalancedNode(v.value2, v.value3, go2(v.value4), v.value5);
          }
          ;
          if (v1 instanceof GT) {
            return unsafeBalancedNode(v.value2, v.value3, v.value4, go2(v.value5));
          }
          ;
          if (v1 instanceof EQ) {
            return unsafeJoinNodes(v.value4, v.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 7 - line 501, column 43): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 8 - line 501, column 43): " + [v.constructor.name]);
      };
      return go2;
    };
  };
  var alter = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = unsafeSplit(compare6, k, m);
          var v2 = f(v.value0);
          if (v2 instanceof Nothing) {
            return unsafeJoinNodes(v.value1, v.value2);
          }
          ;
          if (v2 instanceof Just) {
            return unsafeBalancedNode(k, v2.value0, v.value1, v.value2);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 514, column 3 - line 518, column 41): " + [v2.constructor.name]);
        };
      };
    };
  };

  // output/Halogen.Data.Slot/index.js
  var foreachSlot = function(dictApplicative) {
    var traverse_10 = traverse_(dictApplicative)(foldableMap);
    return function(v) {
      return function(k) {
        return traverse_10(function($54) {
          return k($54);
        })(v);
      };
    };
  };
  var empty3 = empty2;

  // output/Data.String.Common/foreign.js
  var split = function(sep) {
    return function(s) {
      return s.split(sep);
    };
  };
  var toUpper = function(s) {
    return s.toUpperCase();
  };
  var trim = function(s) {
    return s.trim();
  };

  // output/DOM.HTML.Indexed.InputType/index.js
  var InputButton = /* @__PURE__ */ function() {
    function InputButton2() {
    }
    ;
    InputButton2.value = new InputButton2();
    return InputButton2;
  }();
  var InputCheckbox = /* @__PURE__ */ function() {
    function InputCheckbox2() {
    }
    ;
    InputCheckbox2.value = new InputCheckbox2();
    return InputCheckbox2;
  }();
  var InputColor = /* @__PURE__ */ function() {
    function InputColor2() {
    }
    ;
    InputColor2.value = new InputColor2();
    return InputColor2;
  }();
  var InputDate = /* @__PURE__ */ function() {
    function InputDate2() {
    }
    ;
    InputDate2.value = new InputDate2();
    return InputDate2;
  }();
  var InputDatetimeLocal = /* @__PURE__ */ function() {
    function InputDatetimeLocal2() {
    }
    ;
    InputDatetimeLocal2.value = new InputDatetimeLocal2();
    return InputDatetimeLocal2;
  }();
  var InputEmail = /* @__PURE__ */ function() {
    function InputEmail2() {
    }
    ;
    InputEmail2.value = new InputEmail2();
    return InputEmail2;
  }();
  var InputFile = /* @__PURE__ */ function() {
    function InputFile2() {
    }
    ;
    InputFile2.value = new InputFile2();
    return InputFile2;
  }();
  var InputHidden = /* @__PURE__ */ function() {
    function InputHidden2() {
    }
    ;
    InputHidden2.value = new InputHidden2();
    return InputHidden2;
  }();
  var InputImage = /* @__PURE__ */ function() {
    function InputImage2() {
    }
    ;
    InputImage2.value = new InputImage2();
    return InputImage2;
  }();
  var InputMonth = /* @__PURE__ */ function() {
    function InputMonth2() {
    }
    ;
    InputMonth2.value = new InputMonth2();
    return InputMonth2;
  }();
  var InputNumber = /* @__PURE__ */ function() {
    function InputNumber2() {
    }
    ;
    InputNumber2.value = new InputNumber2();
    return InputNumber2;
  }();
  var InputPassword = /* @__PURE__ */ function() {
    function InputPassword2() {
    }
    ;
    InputPassword2.value = new InputPassword2();
    return InputPassword2;
  }();
  var InputRadio = /* @__PURE__ */ function() {
    function InputRadio2() {
    }
    ;
    InputRadio2.value = new InputRadio2();
    return InputRadio2;
  }();
  var InputRange = /* @__PURE__ */ function() {
    function InputRange2() {
    }
    ;
    InputRange2.value = new InputRange2();
    return InputRange2;
  }();
  var InputReset = /* @__PURE__ */ function() {
    function InputReset2() {
    }
    ;
    InputReset2.value = new InputReset2();
    return InputReset2;
  }();
  var InputSearch = /* @__PURE__ */ function() {
    function InputSearch2() {
    }
    ;
    InputSearch2.value = new InputSearch2();
    return InputSearch2;
  }();
  var InputSubmit = /* @__PURE__ */ function() {
    function InputSubmit2() {
    }
    ;
    InputSubmit2.value = new InputSubmit2();
    return InputSubmit2;
  }();
  var InputTel = /* @__PURE__ */ function() {
    function InputTel2() {
    }
    ;
    InputTel2.value = new InputTel2();
    return InputTel2;
  }();
  var InputText = /* @__PURE__ */ function() {
    function InputText2() {
    }
    ;
    InputText2.value = new InputText2();
    return InputText2;
  }();
  var InputTime = /* @__PURE__ */ function() {
    function InputTime2() {
    }
    ;
    InputTime2.value = new InputTime2();
    return InputTime2;
  }();
  var InputUrl = /* @__PURE__ */ function() {
    function InputUrl2() {
    }
    ;
    InputUrl2.value = new InputUrl2();
    return InputUrl2;
  }();
  var InputWeek = /* @__PURE__ */ function() {
    function InputWeek2() {
    }
    ;
    InputWeek2.value = new InputWeek2();
    return InputWeek2;
  }();
  var renderInputType = function(v) {
    if (v instanceof InputButton) {
      return "button";
    }
    ;
    if (v instanceof InputCheckbox) {
      return "checkbox";
    }
    ;
    if (v instanceof InputColor) {
      return "color";
    }
    ;
    if (v instanceof InputDate) {
      return "date";
    }
    ;
    if (v instanceof InputDatetimeLocal) {
      return "datetime-local";
    }
    ;
    if (v instanceof InputEmail) {
      return "email";
    }
    ;
    if (v instanceof InputFile) {
      return "file";
    }
    ;
    if (v instanceof InputHidden) {
      return "hidden";
    }
    ;
    if (v instanceof InputImage) {
      return "image";
    }
    ;
    if (v instanceof InputMonth) {
      return "month";
    }
    ;
    if (v instanceof InputNumber) {
      return "number";
    }
    ;
    if (v instanceof InputPassword) {
      return "password";
    }
    ;
    if (v instanceof InputRadio) {
      return "radio";
    }
    ;
    if (v instanceof InputRange) {
      return "range";
    }
    ;
    if (v instanceof InputReset) {
      return "reset";
    }
    ;
    if (v instanceof InputSearch) {
      return "search";
    }
    ;
    if (v instanceof InputSubmit) {
      return "submit";
    }
    ;
    if (v instanceof InputTel) {
      return "tel";
    }
    ;
    if (v instanceof InputText) {
      return "text";
    }
    ;
    if (v instanceof InputTime) {
      return "time";
    }
    ;
    if (v instanceof InputUrl) {
      return "url";
    }
    ;
    if (v instanceof InputWeek) {
      return "week";
    }
    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.InputType (line 33, column 19 - line 55, column 22): " + [v.constructor.name]);
  };

  // output/Halogen.Query.Input/index.js
  var RefUpdate = /* @__PURE__ */ function() {
    function RefUpdate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RefUpdate2.create = function(value0) {
      return function(value1) {
        return new RefUpdate2(value0, value1);
      };
    };
    return RefUpdate2;
  }();
  var Action = /* @__PURE__ */ function() {
    function Action3(value0) {
      this.value0 = value0;
    }
    ;
    Action3.create = function(value0) {
      return new Action3(value0);
    };
    return Action3;
  }();

  // output/Data.Array/foreign.js
  var rangeImpl = function(start2, end) {
    var step3 = start2 > end ? -1 : 1;
    var result = new Array(step3 * (end - start2) + 1);
    var i2 = start2, n = 0;
    while (i2 !== end) {
      result[n++] = i2;
      i2 += step3;
    }
    result[n] = i2;
    return result;
  };
  var replicateFill = function(count, value14) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value14);
  };
  var replicatePolyfill = function(count, value14) {
    var result = [];
    var n = 0;
    for (var i2 = 0; i2 < count; i2++) {
      result[n++] = value14;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = /* @__PURE__ */ function() {
    function Cons2(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head4) {
      return function(tail2) {
        return new Cons2(head4, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr4, xs) {
      return listToArray(foldr4(curryCons)(emptyList)(xs));
    };
  }();
  var length3 = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty7, next, xs) {
    return xs.length === 0 ? empty7({}) : next(xs[0])(xs.slice(1));
  };
  var indexImpl = function(just, nothing, xs, i2) {
    return i2 < 0 || i2 >= xs.length ? nothing : just(xs[i2]);
  };
  var findIndexImpl = function(just, nothing, f, xs) {
    for (var i2 = 0, l = xs.length; i2 < l; i2++) {
      if (f(xs[i2])) return just(i2);
    }
    return nothing;
  };
  var _deleteAt = function(just, nothing, i2, l) {
    if (i2 < 0 || i2 >= l.length) return nothing;
    var l1 = l.slice();
    l1.splice(i2, 1);
    return just(l1);
  };
  var filterImpl = function(f, xs) {
    return xs.filter(f);
  };
  var sortByImpl = /* @__PURE__ */ function() {
    function mergeFromTo(compare6, fromOrdering, xs1, xs2, from2, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to - from2 >> 1);
      if (mid - from2 > 1) mergeFromTo(compare6, fromOrdering, xs2, xs1, from2, mid);
      if (to - mid > 1) mergeFromTo(compare6, fromOrdering, xs2, xs1, mid, to);
      i2 = from2;
      j = mid;
      k = from2;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare6(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare6, fromOrdering, xs) {
      var out;
      if (xs.length < 2) return xs;
      out = xs.slice(0);
      mergeFromTo(compare6, fromOrdering, out, xs.slice(0), 0, xs.length);
      return out;
    };
  }();
  var sliceImpl = function(s, e, l) {
    return l.slice(s, e);
  };
  var zipWithImpl = function(f, xs, ys) {
    var l = xs.length < ys.length ? xs.length : ys.length;
    var result = new Array(l);
    for (var i2 = 0; i2 < l; i2++) {
      result[i2] = f(xs[i2])(ys[i2]);
    }
    return result;
  };
  var unsafeIndexImpl = function(xs, n) {
    return xs[n];
  };

  // output/Data.Array.ST/foreign.js
  function unsafeFreezeThawImpl(xs) {
    return xs;
  }
  var unsafeFreezeImpl = unsafeFreezeThawImpl;
  var unsafeThawImpl = unsafeFreezeThawImpl;
  var pushImpl = function(a2, xs) {
    return xs.push(a2);
  };

  // output/Control.Monad.ST.Uncurried/foreign.js
  var runSTFn1 = function runSTFn12(fn) {
    return function(a2) {
      return function() {
        return fn(a2);
      };
    };
  };
  var runSTFn2 = function runSTFn22(fn) {
    return function(a2) {
      return function(b2) {
        return function() {
          return fn(a2, b2);
        };
      };
    };
  };

  // output/Data.Array.ST/index.js
  var unsafeThaw = /* @__PURE__ */ runSTFn1(unsafeThawImpl);
  var unsafeFreeze = /* @__PURE__ */ runSTFn1(unsafeFreezeImpl);
  var push = /* @__PURE__ */ runSTFn2(pushImpl);

  // output/Data.Function.Uncurried/foreign.js
  var runFn2 = function(fn) {
    return function(a2) {
      return function(b2) {
        return fn(a2, b2);
      };
    };
  };
  var runFn3 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return fn(a2, b2, c);
        };
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d) {
            return fn(a2, b2, c, d);
          };
        };
      };
    };
  };

  // output/Data.Array/index.js
  var $$void4 = /* @__PURE__ */ $$void(functorST);
  var map10 = /* @__PURE__ */ map(functorMaybe);
  var map12 = /* @__PURE__ */ map(functorArray);
  var map22 = /* @__PURE__ */ map(functorST);
  var fromJust4 = /* @__PURE__ */ fromJust();
  var when2 = /* @__PURE__ */ when(applicativeST);
  var notEq2 = /* @__PURE__ */ notEq(eqOrdering);
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var zipWith = /* @__PURE__ */ runFn3(zipWithImpl);
  var zip = /* @__PURE__ */ function() {
    return zipWith(Tuple.create);
  }();
  var unsafeIndex = function() {
    return runFn2(unsafeIndexImpl);
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var uncons = /* @__PURE__ */ function() {
    return runFn3(unconsImpl)($$const(Nothing.value))(function(x) {
      return function(xs) {
        return new Just({
          head: x,
          tail: xs
        });
      };
    });
  }();
  var sortBy = function(comp) {
    return runFn3(sortByImpl)(comp)(function(v) {
      if (v instanceof GT) {
        return 1;
      }
      ;
      if (v instanceof EQ) {
        return 0;
      }
      ;
      if (v instanceof LT) {
        return -1 | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 897, column 38 - line 900, column 11): " + [v.constructor.name]);
    });
  };
  var sortWith = function(dictOrd) {
    var comparing2 = comparing(dictOrd);
    return function(f) {
      return sortBy(comparing2(f));
    };
  };
  var sortWith1 = /* @__PURE__ */ sortWith(ordInt);
  var slice = /* @__PURE__ */ runFn3(sliceImpl);
  var take = function(n) {
    return function(xs) {
      var $152 = n < 1;
      if ($152) {
        return [];
      }
      ;
      return slice(0)(n)(xs);
    };
  };
  var singleton4 = function(a2) {
    return [a2];
  };
  var range2 = /* @__PURE__ */ runFn2(rangeImpl);
  var $$null = function(xs) {
    return length3(xs) === 0;
  };
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var index2 = /* @__PURE__ */ function() {
    return runFn4(indexImpl)(Just.create)(Nothing.value);
  }();
  var last = function(xs) {
    return index2(xs)(length3(xs) - 1 | 0);
  };
  var head = function(xs) {
    return index2(xs)(0);
  };
  var nubBy = function(comp) {
    return function(xs) {
      var indexedAndSorted = sortBy(function(x) {
        return function(y) {
          return comp(snd(x))(snd(y));
        };
      })(mapWithIndex2(Tuple.create)(xs));
      var v = head(indexedAndSorted);
      if (v instanceof Nothing) {
        return [];
      }
      ;
      if (v instanceof Just) {
        return map12(snd)(sortWith1(fst)(function __do7() {
          var result = unsafeThaw(singleton4(v.value0))();
          foreach(indexedAndSorted)(function(v1) {
            return function __do8() {
              var lst = map22(/* @__PURE__ */ function() {
                var $183 = function($185) {
                  return fromJust4(last($185));
                };
                return function($184) {
                  return snd($183($184));
                };
              }())(unsafeFreeze(result))();
              return when2(notEq2(comp(lst)(v1.value1))(EQ.value))($$void4(push(v1)(result)))();
            };
          })();
          return unsafeFreeze(result)();
        }()));
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 1115, column 17 - line 1123, column 28): " + [v.constructor.name]);
    };
  };
  var nub = function(dictOrd) {
    return nubBy(compare(dictOrd));
  };
  var fromFoldable2 = function(dictFoldable) {
    return runFn2(fromFoldableImpl)(foldr(dictFoldable));
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var foldM = function(dictMonad) {
    var pure111 = pure(dictMonad.Applicative0());
    var bind16 = bind(dictMonad.Bind1());
    return function(f) {
      return function(b2) {
        return runFn3(unconsImpl)(function(v) {
          return pure111(b2);
        })(function(a2) {
          return function(as) {
            return bind16(f(b2)(a2))(function(b$prime) {
              return foldM(dictMonad)(f)(b$prime)(as);
            });
          };
        });
      };
    };
  };
  var findIndex = /* @__PURE__ */ function() {
    return runFn4(findIndexImpl)(Just.create)(Nothing.value);
  }();
  var find2 = function(f) {
    return function(xs) {
      return map10(unsafeIndex1(xs))(findIndex(f)(xs));
    };
  };
  var filter = /* @__PURE__ */ runFn2(filterImpl);
  var drop = function(n) {
    return function(xs) {
      var $173 = n < 1;
      if ($173) {
        return xs;
      }
      ;
      return slice(n)(length3(xs))(xs);
    };
  };
  var deleteAt = /* @__PURE__ */ function() {
    return runFn4(_deleteAt)(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i2) {
          return fromJust4(deleteAt(i2)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };
  var cons = function(x) {
    return function(xs) {
      return append2([x])(xs);
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $189 = maybe([])(singleton4);
      return function($190) {
        return $189(f($190));
      };
    }());
  };

  // output/Halogen.VDom.Machine/index.js
  var Step = /* @__PURE__ */ function() {
    function Step3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Step3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Step3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Step3;
  }();
  var unStep = unsafeCoerce2;
  var step2 = function(v, a2) {
    return v.value2(v.value1, a2);
  };
  var mkStep = unsafeCoerce2;
  var halt = function(v) {
    return v.value3(v.value1);
  };
  var extract2 = /* @__PURE__ */ unStep(function(v) {
    return v.value0;
  });

  // output/Halogen.VDom.Types/index.js
  var map11 = /* @__PURE__ */ map(functorArray);
  var map13 = /* @__PURE__ */ map(functorTuple);
  var Text = /* @__PURE__ */ function() {
    function Text2(value0) {
      this.value0 = value0;
    }
    ;
    Text2.create = function(value0) {
      return new Text2(value0);
    };
    return Text2;
  }();
  var Elem = /* @__PURE__ */ function() {
    function Elem2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Elem2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Elem2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Elem2;
  }();
  var Keyed = /* @__PURE__ */ function() {
    function Keyed2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Keyed2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Keyed2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Keyed2;
  }();
  var Widget = /* @__PURE__ */ function() {
    function Widget2(value0) {
      this.value0 = value0;
    }
    ;
    Widget2.create = function(value0) {
      return new Widget2(value0);
    };
    return Widget2;
  }();
  var Grafted = /* @__PURE__ */ function() {
    function Grafted2(value0) {
      this.value0 = value0;
    }
    ;
    Grafted2.create = function(value0) {
      return new Grafted2(value0);
    };
    return Grafted2;
  }();
  var Graft = /* @__PURE__ */ function() {
    function Graft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Graft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Graft2(value0, value1, value22);
        };
      };
    };
    return Graft2;
  }();
  var unGraft = function(f) {
    return function($61) {
      return f($61);
    };
  };
  var graft = unsafeCoerce2;
  var bifunctorGraft = {
    bimap: function(f) {
      return function(g) {
        return unGraft(function(v) {
          return graft(new Graft(function($63) {
            return f(v.value0($63));
          }, function($64) {
            return g(v.value1($64));
          }, v.value2));
        });
      };
    }
  };
  var bimap2 = /* @__PURE__ */ bimap(bifunctorGraft);
  var runGraft = /* @__PURE__ */ unGraft(function(v) {
    var go2 = function(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }
      ;
      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map11(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map11(map13(go2))(v2.value3));
      }
      ;
      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }
      ;
      if (v2 instanceof Grafted) {
        return new Grafted(bimap2(v.value0)(v.value1)(v2.value0));
      }
      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };
    return go2(v.value2);
  });

  // output/Halogen.VDom.Util/foreign.js
  function unsafeGetAny(key, obj) {
    return obj[key];
  }
  function unsafeHasAny(key, obj) {
    return obj.hasOwnProperty(key);
  }
  function unsafeSetAny(key, val, obj) {
    obj[key] = val;
  }
  function forE2(a2, f) {
    var b2 = [];
    for (var i2 = 0; i2 < a2.length; i2++) {
      b2.push(f(i2, a2[i2]));
    }
    return b2;
  }
  function forEachE(a2, f) {
    for (var i2 = 0; i2 < a2.length; i2++) {
      f(a2[i2]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i2 = 0; i2 < ks.length; i2++) {
      var k = ks[i2];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i2 = 0;
    while (1) {
      if (i2 < l1) {
        if (i2 < l2) {
          a3.push(f1(i2, a1[i2], a2[i2]));
        } else {
          f2(i2, a1[i2]);
        }
      } else if (i2 < l2) {
        a3.push(f3(i2, a2[i2]));
      } else {
        break;
      }
      i2++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      o[k] = f(k, i2, a2);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i2, o1[k], a2);
      } else {
        o2[k] = f3(k, i2, a2);
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k]);
    }
    return o2;
  }
  function refEq2(a2, b2) {
    return a2 === b2;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name16, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name16);
    } else {
      return doc.createElement(name16);
    }
  }
  function insertChildIx(i2, a2, b2) {
    var n = b2.childNodes.item(i2) || null;
    if (n !== a2) {
      b2.insertBefore(a2, n);
    }
  }
  function removeChild(a2, b2) {
    if (b2 && a2.parentNode === b2) {
      b2.removeChild(a2);
    }
  }
  function parentNode(a2) {
    return a2.parentNode;
  }
  function setAttribute(ns, attr3, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr3, val);
    } else {
      el.setAttribute(attr3, val);
    }
  }
  function removeAttribute(ns, attr3, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr3);
    } else {
      el.removeAttribute(attr3);
    }
  }
  function hasAttribute(ns, attr3, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr3);
    } else {
      return el.hasAttribute(attr3);
    }
  }
  function addEventListener3(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener2(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup = unsafeGetAny;
  var unsafeFreeze2 = unsafeCoerce2;
  var pokeMutMap = unsafeSetAny;
  var newMutMap = newImpl;

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name16) {
    return function(doctype) {
      return doctype[name16];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.Element/index.js
  var toNode2 = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy5 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy5("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step2(state3.widget, vdom.value0);
        var res$prime = unStep(function(v) {
          return mkStep(new Step(v.value0, {
            build: state3.build,
            widget: res
          }, $lazy_patchWidget(296), haltWidget));
        })(res);
        return res$prime;
      }
      ;
      haltWidget(state3);
      return state3.build(vdom);
    };
  });
  var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
  var haltText = function(v) {
    var parent2 = parentNode(v.node);
    return removeChild(v.node, parent2);
  };
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy5("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text) {
        if (state3.value === vdom.value0) {
          return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
        }
        ;
        if (otherwise) {
          var nextState = {
            build: state3.build,
            node: state3.node,
            value: vdom.value0
          };
          setTextContent(vdom.value0, state3.node);
          return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
        }
        ;
      }
      ;
      haltText(state3);
      return state3.build(vdom);
    };
  });
  var patchText = /* @__PURE__ */ $lazy_patchText(77);
  var haltKeyed = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forInE(v.children, function(v1, s) {
      return halt(s);
    });
    return halt(v.attrs);
  };
  var haltElem = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forEachE(v.children, halt);
    return halt(v.attrs);
  };
  var eqElemSpec = function(ns1, v, ns2, v1) {
    var $63 = v === v1;
    if ($63) {
      if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
        return true;
      }
      ;
      if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
        return true;
      }
      ;
      return false;
    }
    ;
    return false;
  };
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy5("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length3(vdom.value3);
        var v1 = length3(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step2(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(ix, s, v2) {
          var res = step2(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step2(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
      }
      ;
      haltElem(state3);
      return state3.build(vdom);
    };
  });
  var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy5("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length3(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step2(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children,
            length: 0
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(v2, ix$prime, s, v3) {
          var res = step2(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step2(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2,
          length: v
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
      }
      ;
      haltKeyed(state3);
      return state3.build(vdom);
    };
  });
  var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
  var buildWidget = function(v, build, w) {
    var res = v.buildWidget(v)(w);
    var res$prime = unStep(function(v1) {
      return mkStep(new Step(v1.value0, {
        build,
        widget: res
      }, patchWidget, haltWidget));
    })(res);
    return res$prime;
  };
  var buildText = function(v, build, s) {
    var node = createTextNode(s, v.document);
    var state3 = {
      build,
      node,
      value: s
    };
    return mkStep(new Step(node, state3, patchText, haltText));
  };
  var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(v1, ix, v2) {
      var res = build(v2.value1);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = strMapWithIxE(ch1, fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2,
      length: length3(ch1)
    };
    return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(ix, child) {
      var res = build(child);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = forE2(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2
    };
    return mkStep(new Step(node, state3, patchElem, haltElem));
  };
  var buildVDom = function(spec) {
    var $lazy_build = $runtime_lazy5("build", "Halogen.VDom.DOM", function() {
      return function(v) {
        if (v instanceof Text) {
          return buildText(spec, $lazy_build(59), v.value0);
        }
        ;
        if (v instanceof Elem) {
          return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Keyed) {
          return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Widget) {
          return buildWidget(spec, $lazy_build(62), v.value0);
        }
        ;
        if (v instanceof Grafted) {
          return $lazy_build(63)(runGraft(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
      };
    });
    var build = $lazy_build(58);
    return build;
  };

  // output/Foreign/foreign.js
  function typeOf(value14) {
    return typeof value14;
  }
  function tagOf(value14) {
    return Object.prototype.toString.call(value14).slice(8, -1);
  }
  var isArray = Array.isArray || function(value14) {
    return Object.prototype.toString.call(value14) === "[object Array]";
  };

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber2 = function(n) {
    return n;
  };
  var fromStringAsImpl = function(just) {
    return function(nothing) {
      return function(radix) {
        var digits;
        if (radix < 11) {
          digits = "[0-" + (radix - 1).toString() + "]";
        } else if (radix === 11) {
          digits = "[0-9a]";
        } else {
          digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
        }
        var pattern2 = new RegExp("^[\\+\\-]?" + digits + "+$", "i");
        return function(s) {
          if (pattern2.test(s)) {
            var i2 = parseInt(s, radix);
            return (i2 | 0) === i2 ? just(i2) : nothing;
          } else {
            return nothing;
          }
        };
      };
    };
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  function fromStringImpl(str, isFinite2, just, nothing) {
    var num = parseFloat(str);
    if (isFinite2(num)) {
      return just(num);
    } else {
      return nothing;
    }
  }
  var floor2 = Math.floor;
  var round = Math.round;
  var sqrt2 = Math.sqrt;

  // output/Data.Number/index.js
  var fromString = function(str) {
    return fromStringImpl(str, isFiniteImpl, Just.create, Nothing.value);
  };

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var fromStringAs = /* @__PURE__ */ function() {
    return fromStringAsImpl(Just.create)(Nothing.value);
  }();
  var fromString2 = /* @__PURE__ */ fromStringAs(10);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber2(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber2(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var round2 = function($37) {
    return unsafeClamp(round($37));
  };
  var floor3 = function($39) {
    return unsafeClamp(floor2($39));
  };

  // output/Data.List/index.js
  var reverse2 = /* @__PURE__ */ function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  }();
  var $$null2 = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };

  // output/Data.List.NonEmpty/index.js
  var singleton5 = /* @__PURE__ */ function() {
    var $200 = singleton2(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  }();
  var cons2 = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var toCharArray = function(s) {
    return s.split("");
  };
  var singleton6 = function(c) {
    return c;
  };
  var length5 = function(s) {
    return s.length;
  };
  var _indexOf = function(just) {
    return function(nothing) {
      return function(x) {
        return function(s) {
          var i2 = s.indexOf(x);
          return i2 === -1 ? nothing : just(i2);
        };
      };
    };
  };
  var drop3 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i2) {
    return function(s) {
      if (i2 >= 0 && i2 < s.length) return s.charAt(i2);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.CodeUnits/index.js
  var indexOf = /* @__PURE__ */ function() {
    return _indexOf(Just.create)(Nothing.value);
  }();
  var contains = function(pat) {
    var $23 = indexOf(pat);
    return function($24) {
      return isJust($23($24));
    };
  };

  // output/Foreign/index.js
  var TypeMismatch = /* @__PURE__ */ function() {
    function TypeMismatch2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch2.create = function(value0) {
      return function(value1) {
        return new TypeMismatch2(value0, value1);
      };
    };
    return TypeMismatch2;
  }();
  var unsafeToForeign = unsafeCoerce2;
  var unsafeFromForeign = unsafeCoerce2;
  var fail = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton5($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure111 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value14) {
        if (tagOf(value14) === tag) {
          return pure111(unsafeFromForeign(value14));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value14)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value14.constructor.name]);
      };
    };
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
  };

  // output/Foreign.Object/foreign.js
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Foreign.Object/index.js
  var lookup2 = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy6 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Created = /* @__PURE__ */ function() {
    function Created2(value0) {
      this.value0 = value0;
    }
    ;
    Created2.create = function(value0) {
      return new Created2(value0);
    };
    return Created2;
  }();
  var Removed = /* @__PURE__ */ function() {
    function Removed2(value0) {
      this.value0 = value0;
    }
    ;
    Removed2.create = function(value0) {
      return new Removed2(value0);
    };
    return Removed2;
  }();
  var Attribute = /* @__PURE__ */ function() {
    function Attribute2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Attribute2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Attribute2(value0, value1, value22);
        };
      };
    };
    return Attribute2;
  }();
  var Property = /* @__PURE__ */ function() {
    function Property2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property2.create = function(value0) {
      return function(value1) {
        return new Property2(value0, value1);
      };
    };
    return Property2;
  }();
  var Handler = /* @__PURE__ */ function() {
    function Handler2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Handler2.create = function(value0) {
      return function(value1) {
        return new Handler2(value0, value1);
      };
    };
    return Handler2;
  }();
  var Ref = /* @__PURE__ */ function() {
    function Ref2(value0) {
      this.value0 = value0;
    }
    ;
    Ref2.create = function(value0) {
      return new Ref2(value0);
    };
    return Ref2;
  }();
  var unsafeGetProperty = unsafeGetAny;
  var setProperty = unsafeSetAny;
  var removeProperty = function(key, el) {
    var v = hasAttribute(nullImpl, key, el);
    if (v) {
      return removeAttribute(nullImpl, key, el);
    }
    ;
    var v1 = typeOf(unsafeGetAny(key, el));
    if (v1 === "string") {
      return unsafeSetAny(key, "", el);
    }
    ;
    if (key === "rowSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    if (key === "colSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    return unsafeSetAny(key, jsUndefined, el);
  };
  var propToStrKey = function(v) {
    if (v instanceof Attribute && v.value0 instanceof Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }
    ;
    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }
    ;
    if (v instanceof Property) {
      return "prop/" + v.value0;
    }
    ;
    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }
    ;
    if (v instanceof Ref) {
      return "ref";
    }
    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };
  var propFromString = unsafeCoerce2;
  var propFromNumber = unsafeCoerce2;
  var propFromInt = unsafeCoerce2;
  var propFromBoolean = unsafeCoerce2;
  var buildProp = function(emit) {
    return function(el) {
      var removeProp = function(prevEvents) {
        return function(v, v1) {
          if (v1 instanceof Attribute) {
            return removeAttribute(toNullable(v1.value0), v1.value1, el);
          }
          ;
          if (v1 instanceof Property) {
            return removeProperty(v1.value0, el);
          }
          ;
          if (v1 instanceof Handler) {
            var handler3 = unsafeLookup(v1.value0, prevEvents);
            return removeEventListener2(v1.value0, fst(handler3), el);
          }
          ;
          if (v1 instanceof Ref) {
            return unit;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };
      var mbEmit = function(v) {
        if (v instanceof Just) {
          return emit(v.value0)();
        }
        ;
        return unit;
      };
      var haltProp = function(state3) {
        var v = lookup2("ref")(state3.props);
        if (v instanceof Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }
        ;
        return unit;
      };
      var diffProp = function(prevEvents, events) {
        return function(v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $66 = v11.value2 === v2.value2;
            if ($66) {
              return v2;
            }
            ;
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v11 instanceof Property && v2 instanceof Property) {
            var v4 = refEq2(v11.value1, v2.value1);
            if (v4) {
              return v2;
            }
            ;
            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $75 = refEq2(elVal, v2.value1);
              if ($75) {
                return v2;
              }
              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }
            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler3 = unsafeLookup(v2.value0, prevEvents);
            write(v2.value1)(snd(handler3))();
            pokeMutMap(v2.value0, handler3, events);
            return v2;
          }
          ;
          return v2;
        };
      };
      var applyProp = function(events) {
        return function(v, v1, v2) {
          if (v2 instanceof Attribute) {
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v2 instanceof Property) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v2 instanceof Handler) {
            var v3 = unsafeGetAny(v2.value0, events);
            if (unsafeHasAny(v2.value0, events)) {
              write(v2.value1)(snd(v3))();
              return v2;
            }
            ;
            var ref2 = $$new(v2.value1)();
            var listener = eventListener(function(ev) {
              return function __do7() {
                var f$prime = read(ref2)();
                return mbEmit(f$prime(ev));
              };
            })();
            pokeMutMap(v2.value0, new Tuple(listener, ref2), events);
            addEventListener3(v2.value0, listener, el);
            return v2;
          }
          ;
          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };
      var $lazy_patchProp = $runtime_lazy6("patchProp", "Halogen.VDom.DOM.Prop", function() {
        return function(state3, ps2) {
          var events = newMutMap();
          var onThis = removeProp(state3.events);
          var onThese = diffProp(state3.events, events);
          var onThat = applyProp(events);
          var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
          var nextState = {
            events: unsafeFreeze2(events),
            props
          };
          return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
        };
      });
      var patchProp = $lazy_patchProp(87);
      var renderProp = function(ps1) {
        var events = newMutMap();
        var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
        var state3 = {
          events: unsafeFreeze2(events),
          props: ps1$prime
        };
        return mkStep(new Step(unit, state3, patchProp, haltProp));
      };
      return renderProp;
    };
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x) {
    return x;
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text5 = function($29) {
    return HTML(Text.create($29));
  };
  var prop = function(dictIsProp) {
    var toPropValue1 = toPropValue(dictIsProp);
    return function(v) {
      var $31 = Property.create(v);
      return function($32) {
        return $31(toPropValue1($32));
      };
    };
  };
  var isPropString = {
    toPropValue: propFromString
  };
  var isPropNumber = {
    toPropValue: propFromNumber
  };
  var isPropInt = {
    toPropValue: propFromInt
  };
  var isPropInputType = {
    toPropValue: function($45) {
      return propFromString(renderInputType($45));
    }
  };
  var isPropBoolean = {
    toPropValue: propFromBoolean
  };
  var handler = /* @__PURE__ */ function() {
    return Handler.create;
  }();
  var element = function(ns) {
    return function(name16) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name16, props, children2);
        };
      };
    };
  };
  var attr = function(ns) {
    return function(v) {
      return Attribute.create(ns)(v);
    };
  };

  // output/Control.Applicative.Free/index.js
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var Pure = /* @__PURE__ */ function() {
    function Pure2(value0) {
      this.value0 = value0;
    }
    ;
    Pure2.create = function(value0) {
      return new Pure2(value0);
    };
    return Pure2;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  }();
  var Ap = /* @__PURE__ */ function() {
    function Ap2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Ap2.create = function(value0) {
      return function(value1) {
        return new Ap2(value0, value1);
      };
    };
    return Ap2;
  }();
  var mkAp = function(fba) {
    return function(fb) {
      return new Ap(fba, fb);
    };
  };
  var liftFreeAp = /* @__PURE__ */ function() {
    return Lift.create;
  }();
  var goLeft = function(dictApplicative) {
    var pure27 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure27(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Lift) {
                return new Tuple(new Cons({
                  func: nat(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Ap) {
                return goLeft(dictApplicative)(fStack)(cons2(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
            };
          };
        };
      };
    };
  };
  var goApply = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    return function(fStack) {
      return function(vals) {
        return function(gVal) {
          if (fStack instanceof Nil) {
            return new Left(gVal);
          }
          ;
          if (fStack instanceof Cons) {
            var gRes = apply2(fStack.value0.func)(gVal);
            var $31 = fStack.value0.count === 1;
            if ($31) {
              if (fStack.value1 instanceof Nil) {
                return new Left(gRes);
              }
              ;
              return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
            }
            ;
            if (vals instanceof Nil) {
              return new Left(gRes);
            }
            ;
            if (vals instanceof Cons) {
              return new Right(new Tuple(new Cons({
                func: gRes,
                count: fStack.value0.count - 1 | 0
              }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
        };
      };
    };
  };
  var functorFreeAp = {
    map: function(f) {
      return function(x) {
        return mkAp(new Pure(f))(x);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    var goApply1 = goApply(dictApplicative);
    var pure27 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure27(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Lift) {
              var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Ap) {
              var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2(new Tuple(Nil.value, singleton5(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity6);
  };
  var applyFreeAp = {
    apply: function(fba) {
      return function(fb) {
        return mkAp(fba)(fb);
      };
    },
    Functor0: function() {
      return functorFreeAp;
    }
  };
  var applicativeFreeAp = /* @__PURE__ */ function() {
    return {
      pure: Pure.create,
      Apply0: function() {
        return applyFreeAp;
      }
    };
  }();
  var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
  var hoistFreeAp = function(f) {
    return foldFreeAp1(function($54) {
      return liftFreeAp(f($54));
    });
  };

  // output/Data.CatQueue/index.js
  var CatQueue = /* @__PURE__ */ function() {
    function CatQueue2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatQueue2.create = function(value0) {
      return function(value1) {
        return new CatQueue2(value0, value1);
      };
    };
    return CatQueue2;
  }();
  var uncons3 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v.value0 instanceof Nil) {
        $copy_v = new CatQueue(reverse2(v.value1), Nil.value);
        return;
      }
      ;
      if (v.value0 instanceof Cons) {
        $tco_done = true;
        return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var snoc2 = function(v) {
    return function(a2) {
      return new CatQueue(v.value0, new Cons(a2, v.value1));
    };
  };
  var $$null3 = function(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var empty5 = /* @__PURE__ */ function() {
    return new CatQueue(Nil.value, Nil.value);
  }();

  // output/Data.CatList/index.js
  var CatNil = /* @__PURE__ */ function() {
    function CatNil2() {
    }
    ;
    CatNil2.value = new CatNil2();
    return CatNil2;
  }();
  var CatCons = /* @__PURE__ */ function() {
    function CatCons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatCons2.create = function(value0) {
      return function(value1) {
        return new CatCons2(value0, value1);
      };
    };
    return CatCons2;
  }();
  var link = function(v) {
    return function(v1) {
      if (v instanceof CatNil) {
        return v1;
      }
      ;
      if (v1 instanceof CatNil) {
        return v;
      }
      ;
      if (v instanceof CatCons) {
        return new CatCons(v.value0, snoc2(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr3 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl5 = function($copy_v) {
          return function($copy_v1) {
            return function($copy_v2) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, v2) {
                if (v2 instanceof Nil) {
                  $tco_done = true;
                  return v1;
                }
                ;
                if (v2 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_v1 = v(v1)(v2.value0);
                  $copy_v2 = v2.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
              }
              ;
              return $tco_result;
            };
          };
        };
        var go2 = function($copy_xs) {
          return function($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(xs, ys) {
              var v = uncons3(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl5(function(x) {
                  return function(i2) {
                    return i2(x);
                  };
                })(b2)(ys);
              }
              ;
              if (v instanceof Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Cons(k(v.value0.value0), ys);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }
            ;
            return $tco_result;
          };
        };
        return go2(q2)(Nil.value);
      };
    };
  };
  var uncons4 = function(v) {
    if (v instanceof CatNil) {
      return Nothing.value;
    }
    ;
    if (v instanceof CatCons) {
      return new Just(new Tuple(v.value0, function() {
        var $66 = $$null3(v.value1);
        if ($66) {
          return CatNil.value;
        }
        ;
        return foldr3(link)(CatNil.value)(v.value1);
      }()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty6 = /* @__PURE__ */ function() {
    return CatNil.value;
  }();
  var append3 = link;
  var semigroupCatList = {
    append: append3
  };
  var snoc3 = function(cat) {
    return function(a2) {
      return append3(cat)(new CatCons(a2, empty5));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy7 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var append4 = /* @__PURE__ */ append(semigroupCatList);
  var Free = /* @__PURE__ */ function() {
    function Free2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Free2.create = function(value0) {
      return function(value1) {
        return new Free2(value0, value1);
      };
    };
    return Free2;
  }();
  var Return = /* @__PURE__ */ function() {
    function Return2(value0) {
      this.value0 = value0;
    }
    ;
    Return2.create = function(value0) {
      return new Return2(value0);
    };
    return Return2;
  }();
  var Bind = /* @__PURE__ */ function() {
    function Bind2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Bind2.create = function(value0) {
      return function(value1) {
        return new Bind2(value0, value1);
      };
    };
    return Bind2;
  }();
  var toView = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      var runExpF = function(v22) {
        return v22;
      };
      var concatF = function(v22) {
        return function(r) {
          return new Free(v22.value0, append4(v22.value1)(r));
        };
      };
      if (v.value0 instanceof Return) {
        var v2 = uncons4(v.value1);
        if (v2 instanceof Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }
        ;
        if (v2 instanceof Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }
      ;
      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function(a2) {
          return concatF(v.value0.value1(a2))(v.value1);
        });
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var fromView = function(f) {
    return new Free(f, empty6);
  };
  var freeMonad = {
    Applicative0: function() {
      return freeApplicative;
    },
    Bind1: function() {
      return freeBind;
    }
  };
  var freeFunctor = {
    map: function(k) {
      return function(f) {
        return bindFlipped(freeBind)(function() {
          var $189 = pure(freeApplicative);
          return function($190) {
            return $189(k($190));
          };
        }())(f);
      };
    }
  };
  var freeBind = {
    bind: function(v) {
      return function(k) {
        return new Free(v.value0, snoc3(v.value1)(k));
      };
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var freeApplicative = {
    pure: function($191) {
      return fromView(Return.create($191));
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy7("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var pure4 = /* @__PURE__ */ pure(freeApplicative);
  var liftF = function(f) {
    return fromView(new Bind(f, function($192) {
      return pure4($192);
    }));
  };
  var foldFree = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map111 = map(Monad0.Bind1().Apply0().Functor0());
    var pure111 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map111(Done.create)(pure111(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map111(function($199) {
            return Loop.create(v.value1($199));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM4(go2);
    };
  };

  // output/Halogen.Query.ChildQuery/index.js
  var unChildQueryBox = unsafeCoerce2;

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a2) {
    return function(b2) {
      return a2 === b2;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/Halogen.Subscription/index.js
  var $$void5 = /* @__PURE__ */ $$void(functorEffect);
  var bind3 = /* @__PURE__ */ bind(bindEffect);
  var append5 = /* @__PURE__ */ append(semigroupArray);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
  var unsubscribe = function(v) {
    return v;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(function($76) {
        return $$void5(k($76));
      });
    };
  };
  var notify = function(v) {
    return function(a2) {
      return v(a2);
    };
  };
  var create3 = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do7() {
          modify_(function(v) {
            return append5(v)([k]);
          })(subscribers)();
          return modify_(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind3(read(subscribers))(traverse_1(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var SubscriptionId = function(x) {
    return x;
  };
  var ForkId = function(x) {
    return x;
  };
  var State = /* @__PURE__ */ function() {
    function State2(value0) {
      this.value0 = value0;
    }
    ;
    State2.create = function(value0) {
      return new State2(value0);
    };
    return State2;
  }();
  var Subscribe = /* @__PURE__ */ function() {
    function Subscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Subscribe2.create = function(value0) {
      return function(value1) {
        return new Subscribe2(value0, value1);
      };
    };
    return Subscribe2;
  }();
  var Unsubscribe = /* @__PURE__ */ function() {
    function Unsubscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Unsubscribe2.create = function(value0) {
      return function(value1) {
        return new Unsubscribe2(value0, value1);
      };
    };
    return Unsubscribe2;
  }();
  var Lift2 = /* @__PURE__ */ function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  }();
  var ChildQuery2 = /* @__PURE__ */ function() {
    function ChildQuery3(value0) {
      this.value0 = value0;
    }
    ;
    ChildQuery3.create = function(value0) {
      return new ChildQuery3(value0);
    };
    return ChildQuery3;
  }();
  var Raise = /* @__PURE__ */ function() {
    function Raise2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Raise2.create = function(value0) {
      return function(value1) {
        return new Raise2(value0, value1);
      };
    };
    return Raise2;
  }();
  var Par = /* @__PURE__ */ function() {
    function Par2(value0) {
      this.value0 = value0;
    }
    ;
    Par2.create = function(value0) {
      return new Par2(value0);
    };
    return Par2;
  }();
  var Fork = /* @__PURE__ */ function() {
    function Fork2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Fork2.create = function(value0) {
      return function(value1) {
        return new Fork2(value0, value1);
      };
    };
    return Fork2;
  }();
  var Join = /* @__PURE__ */ function() {
    function Join2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Join2.create = function(value0) {
      return function(value1) {
        return new Join2(value0, value1);
      };
    };
    return Join2;
  }();
  var Kill = /* @__PURE__ */ function() {
    function Kill2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Kill2.create = function(value0) {
      return function(value1) {
        return new Kill2(value0, value1);
      };
    };
    return Kill2;
  }();
  var GetRef = /* @__PURE__ */ function() {
    function GetRef2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetRef2.create = function(value0) {
      return function(value1) {
        return new GetRef2(value0, value1);
      };
    };
    return GetRef2;
  }();
  var HalogenM = function(x) {
    return x;
  };
  var ordSubscriptionId = ordInt;
  var ordForkId = ordInt;
  var monadHalogenM = freeMonad;
  var monadStateHalogenM = {
    state: function($181) {
      return HalogenM(liftF(State.create($181)));
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };
  var monadEffectHalogenM = function(dictMonadEffect) {
    return {
      liftEffect: function() {
        var $186 = liftEffect(dictMonadEffect);
        return function($187) {
          return HalogenM(liftF(Lift2.create($186($187))));
        };
      }(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var functorHalogenM = freeFunctor;
  var fork = function(hmu) {
    return liftF(new Fork(hmu, identity7));
  };
  var bindHalogenM = freeBind;
  var applicativeHalogenM = freeApplicative;

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ function() {
    function Initialize3(value0) {
      this.value0 = value0;
    }
    ;
    Initialize3.create = function(value0) {
      return new Initialize3(value0);
    };
    return Initialize3;
  }();
  var Finalize = /* @__PURE__ */ function() {
    function Finalize2(value0) {
      this.value0 = value0;
    }
    ;
    Finalize2.create = function(value0) {
      return new Finalize2(value0);
    };
    return Finalize2;
  }();
  var Receive = /* @__PURE__ */ function() {
    function Receive2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Receive2.create = function(value0) {
      return function(value1) {
        return new Receive2(value0, value1);
      };
    };
    return Receive2;
  }();
  var Action2 = /* @__PURE__ */ function() {
    function Action3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Action3.create = function(value0) {
      return function(value1) {
        return new Action3(value0, value1);
      };
    };
    return Action3;
  }();
  var Query = /* @__PURE__ */ function() {
    function Query2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Query2.create = function(value0) {
      return function(value1) {
        return new Query2(value0, value1);
      };
    };
    return Query2;
  }();

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy8 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var unsafeEqThunk = function(v, v1) {
    return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
  };
  var runThunk = function(v) {
    return v.value2(v.value3);
  };
  var buildThunk = function(toVDom) {
    var haltThunk = function(state3) {
      return halt(state3.vdom);
    };
    var $lazy_patchThunk = $runtime_lazy8("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
          return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step2(state3.vdom, toVDom(runThunk(t2)));
        return mkStep(new Step(extract2(vdom), {
          vdom,
          thunk: t2
        }, $lazy_patchThunk(115), haltThunk));
      };
    });
    var patchThunk = $lazy_patchThunk(108);
    var renderThunk = function(spec) {
      return function(t) {
        var vdom = buildVDom(spec)(toVDom(runThunk(t)));
        return mkStep(new Step(extract2(vdom), {
          thunk: t,
          vdom
        }, patchThunk, haltThunk));
      };
    };
    return renderThunk;
  };

  // output/Halogen.Component/index.js
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
  var traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
  var map14 = /* @__PURE__ */ map(functorHalogenM);
  var pure5 = /* @__PURE__ */ pure(applicativeHalogenM);
  var ComponentSlot = /* @__PURE__ */ function() {
    function ComponentSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ComponentSlot2.create = function(value0) {
      return new ComponentSlot2(value0);
    };
    return ComponentSlot2;
  }();
  var ThunkSlot = /* @__PURE__ */ function() {
    function ThunkSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkSlot2.create = function(value0) {
      return new ThunkSlot2(value0);
    };
    return ThunkSlot2;
  }();
  var unComponentSlot = unsafeCoerce2;
  var unComponent = unsafeCoerce2;
  var mkEval = function(args) {
    return function(v) {
      if (v instanceof Initialize) {
        return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
      }
      ;
      if (v instanceof Finalize) {
        return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
      }
      ;
      if (v instanceof Receive) {
        return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
      }
      ;
      if (v instanceof Action2) {
        return voidLeft2(args.handleAction(v.value0))(v.value1);
      }
      ;
      if (v instanceof Query) {
        return unCoyoneda(function(g) {
          var $45 = map14(maybe(v.value1(unit))(g));
          return function($46) {
            return $45(args.handleQuery($46));
          };
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
    };
  };
  var mkComponent = unsafeCoerce2;
  var defaultEval = /* @__PURE__ */ function() {
    return {
      handleAction: $$const(pure5(unit)),
      handleQuery: $$const(pure5(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();

  // output/Halogen.HTML.Elements/index.js
  var element2 = /* @__PURE__ */ function() {
    return element(Nothing.value);
  }();
  var h2 = /* @__PURE__ */ element2("h2");
  var h2_ = /* @__PURE__ */ h2([]);
  var h3 = /* @__PURE__ */ element2("h3");
  var h3_ = /* @__PURE__ */ h3([]);
  var h4 = /* @__PURE__ */ element2("h4");
  var h4_ = /* @__PURE__ */ h4([]);
  var input = function(props) {
    return element2("input")(props)([]);
  };
  var label4 = /* @__PURE__ */ element2("label");
  var option = /* @__PURE__ */ element2("option");
  var p = /* @__PURE__ */ element2("p");
  var p_ = /* @__PURE__ */ p([]);
  var select3 = /* @__PURE__ */ element2("select");
  var span3 = /* @__PURE__ */ element2("span");
  var span_ = /* @__PURE__ */ span3([]);
  var strong = /* @__PURE__ */ element2("strong");
  var strong_ = /* @__PURE__ */ strong([]);
  var div2 = /* @__PURE__ */ element2("div");
  var div_ = /* @__PURE__ */ div2([]);
  var canvas = function(props) {
    return element2("canvas")(props)([]);
  };
  var button = /* @__PURE__ */ element2("button");

  // output/Halogen.HTML.Properties/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop1 = /* @__PURE__ */ prop2(isPropBoolean);
  var prop22 = /* @__PURE__ */ prop2(isPropString);
  var prop3 = /* @__PURE__ */ prop2(isPropInt);
  var prop4 = /* @__PURE__ */ prop2(isPropNumber);
  var selected2 = /* @__PURE__ */ prop1("selected");
  var type_17 = function(dictIsProp) {
    return prop2(dictIsProp)("type");
  };
  var value12 = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };
  var width8 = /* @__PURE__ */ prop3("width");
  var placeholder3 = /* @__PURE__ */ prop22("placeholder");
  var name15 = /* @__PURE__ */ prop22("name");
  var min5 = /* @__PURE__ */ prop4("min");
  var max6 = /* @__PURE__ */ prop4("max");
  var id2 = /* @__PURE__ */ prop22("id");
  var height8 = /* @__PURE__ */ prop3("height");
  var $$for = /* @__PURE__ */ prop22("htmlFor");
  var class_ = /* @__PURE__ */ function() {
    var $36 = prop22("className");
    return function($37) {
      return $36(unwrap2($37));
    };
  }();
  var attr2 = /* @__PURE__ */ function() {
    return attr(Nothing.value);
  }();
  var style = /* @__PURE__ */ attr2("style");

  // output/Control.Monad.Fork.Class/index.js
  var monadForkAff = {
    suspend: suspendAff,
    fork: forkAff,
    join: joinFiber,
    Monad0: function() {
      return monadAff;
    },
    Functor1: function() {
      return functorFiber;
    }
  };
  var fork2 = function(dict) {
    return dict.fork;
  };

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX = unsafeCoerce2;
  var unDriverStateX = unsafeCoerce2;
  var renderStateX_ = function(dictApplicative) {
    var traverse_10 = traverse_(dictApplicative)(foldableMaybe);
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_10(f)(st.rendering);
      });
    };
  };
  var mkRenderStateX = unsafeCoerce2;
  var renderStateX = function(dictFunctor) {
    return function(f) {
      return unDriverStateX(function(st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };
  var mkDriverStateXRef = unsafeCoerce2;
  var mapDriverState = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var initDriverState = function(component2) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do7() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty3)();
            var childrenOut = $$new(empty3)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh2 = $$new(1)();
            var subscriptions = $$new(new Just(empty2))();
            var forks = $$new(empty2)();
            var ds = {
              component: component2,
              state: component2.initialState(input3),
              refs: empty2,
              children: empty3,
              childrenIn,
              childrenOut,
              selfRef,
              handlerRef,
              pendingQueries,
              pendingOuts,
              pendingHandlers,
              rendering: Nothing.value,
              fresh: fresh2,
              subscriptions,
              forks,
              lifecycleHandlers: lchs
            };
            write(ds)(selfRef)();
            return mkDriverStateXRef(selfRef);
          };
        };
      };
    };
  };

  // output/Halogen.Aff.Driver.Eval/index.js
  var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var lookup4 = /* @__PURE__ */ lookup(ordSubscriptionId);
  var bind12 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard3 = /* @__PURE__ */ discard(discardUnit);
  var discard1 = /* @__PURE__ */ discard3(bindAff);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var fork3 = /* @__PURE__ */ fork2(monadForkAff);
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var pure6 = /* @__PURE__ */ pure(applicativeAff);
  var map16 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel3 = /* @__PURE__ */ parallel(parallelAff);
  var map17 = /* @__PURE__ */ map(functorAff);
  var sequential2 = /* @__PURE__ */ sequential(parallelAff);
  var map23 = /* @__PURE__ */ map(functorMaybe);
  var insert3 = /* @__PURE__ */ insert(ordSubscriptionId);
  var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
  var $$delete2 = /* @__PURE__ */ $$delete(ordForkId);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var insert1 = /* @__PURE__ */ insert(ordForkId);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var lookup1 = /* @__PURE__ */ lookup(ordForkId);
  var lookup22 = /* @__PURE__ */ lookup(ordString);
  var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
  var alter2 = /* @__PURE__ */ alter(ordString);
  var unsubscribe3 = function(sid) {
    return function(ref2) {
      return function __do7() {
        var v = read(ref2)();
        var subs = read(v.subscriptions)();
        return traverse_4(unsubscribe)(bindFlipped5(lookup4(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref2) {
    return function(au) {
      return bind12(liftEffect4(read(ref2)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect4(write(new Just(new Cons(au, v.value0)))(ref2));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard1(liftEffect4(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind12(liftEffect4(f))(function(result) {
          return bind12(liftEffect4(read(lchs)))(function(v) {
            return discard1(traverse_22(fork3)(v.finalizers))(function() {
              return discard1(parSequence_2(v.initializers))(function() {
                return pure6(result);
              });
            });
          });
        });
      });
    };
  };
  var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
  var fresh = function(f) {
    return function(ref2) {
      return bind12(liftEffect4(read(ref2)))(function(v) {
        return liftEffect4(modify$prime(function(i2) {
          return {
            state: i2 + 1 | 0,
            value: f(i2)
          };
        })(v.fresh));
      });
    };
  };
  var evalQ = function(render2) {
    return function(ref2) {
      return function(q2) {
        return bind12(liftEffect4(read(ref2)))(function(v) {
          return evalM(render2)(ref2)(v["component"]["eval"](new Query(map16(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render2) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind12(liftEffect4(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel3(bind12(liftEffect4(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render2)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map17(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref2) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure6(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard1(liftEffect4(write({
                    component: v2.component,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers,
                    state: v3.value1
                  })(ref2)))(function() {
                    return discard1(handleLifecycle(v2.lifecycleHandlers)(render2(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure6(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind12(fresh(SubscriptionId)(ref2))(function(sid) {
                return bind12(liftEffect4(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render2)(ref2)(new Action(act)));
                })))(function(finalize) {
                  return bind12(liftEffect4(read(ref2)))(function(v2) {
                    return discard1(liftEffect4(modify_(map23(insert3(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure6(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard1(liftEffect4(unsubscribe3(v1.value0)(ref2)))(function() {
                return pure6(v1.value1);
              });
            }
            ;
            if (v1 instanceof Lift2) {
              return v1.value0;
            }
            ;
            if (v1 instanceof ChildQuery2) {
              return evalChildQuery(ref2)(v1.value0);
            }
            ;
            if (v1 instanceof Raise) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.handlerRef)))(function(handler3) {
                  return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure6(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp(function() {
                var $119 = evalM(render2)(ref2);
                return function($120) {
                  return parallel3($119($120));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind12(fresh(ForkId)(ref2))(function(fid) {
                return bind12(liftEffect4(read(ref2)))(function(v2) {
                  return bind12(liftEffect4($$new(false)))(function(doneRef) {
                    return bind12(fork3($$finally(liftEffect4(function __do7() {
                      modify_($$delete2(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render2)(ref2)(v1.value0))))(function(fiber) {
                      return discard1(liftEffect4(unlessM2(read(doneRef))(modify_(insert1(fid)(fiber))(v2.forks))))(function() {
                        return pure6(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(joinFiber)(lookup1(v1.value0)(forkMap)))(function() {
                    return pure6(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(killFiber(error2("Cancelled")))(lookup1(v1.value0)(forkMap)))(function() {
                    return pure6(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return pure6(v1.value1(lookup22(v1.value0)(v2.refs)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
          };
        };
        return foldFree2(go2(initRef))(v);
      };
    };
  };
  var evalF = function(render2) {
    return function(ref2) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect4(flip(modify_)(ref2)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers,
              refs: alter2($$const(v.value1))(v.value0)(st.refs)
            };
          })));
        }
        ;
        if (v instanceof Action) {
          return bind12(liftEffect4(read(ref2)))(function(v1) {
            return evalM(render2)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var bind4 = /* @__PURE__ */ bind(bindEffect);
  var discard4 = /* @__PURE__ */ discard(discardUnit);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
  var fork4 = /* @__PURE__ */ fork2(monadForkAff);
  var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindEffect);
  var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
  var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
  var discard22 = /* @__PURE__ */ discard4(bindAff);
  var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var pure7 = /* @__PURE__ */ pure(applicativeEffect);
  var map18 = /* @__PURE__ */ map(functorEffect);
  var pure12 = /* @__PURE__ */ pure(applicativeAff);
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
  var $$void6 = /* @__PURE__ */ $$void(functorAff);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var bind13 = /* @__PURE__ */ bind(bindAff);
  var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
  var newLifecycleHandlers = /* @__PURE__ */ function() {
    return $$new({
      initializers: Nil.value,
      finalizers: Nil.value
    });
  }();
  var handlePending = function(ref2) {
    return function __do7() {
      var queue = read(ref2)();
      write(Nothing.value)(ref2)();
      return for_2(queue)(function() {
        var $59 = traverse_5(fork4);
        return function($60) {
          return handleAff($59(reverse2($60)));
        };
      }())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do7() {
      bindFlipped6(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped6(traverse_33(function() {
        var $61 = killFiber(error2("finalized"));
        return function($62) {
          return handleAff($61($62));
        };
      }()))(read(v.forks))();
      return write(empty2)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component2) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render2)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_(function(handlers) {
                return {
                  initializers: new Cons(discard22(parSequence_3(reverse2(handlers.initializers)))(function() {
                    return discard22(parentInitializer)(function() {
                      return liftEffect5(function __do7() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };
        var runComponent = function(lchs) {
          return function(handler3) {
            return function(j) {
              return unComponent(function(c) {
                return function __do7() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                  var pre2 = read(lchs)();
                  write({
                    initializers: Nil.value,
                    finalizers: pre2.finalizers
                  })(lchs)();
                  bindFlipped6(unDriverStateX(function() {
                    var $63 = render2(lchs);
                    return function($64) {
                      return $63(function(v) {
                        return v.selfRef;
                      }($64));
                    };
                  }()))(read($$var2))();
                  bindFlipped6(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                  return $$var2;
                };
              });
            };
          };
        };
        var renderChild = function(lchs) {
          return function(handler3) {
            return function(childrenInRef) {
              return function(childrenOutRef) {
                return unComponentSlot(function(slot) {
                  return function __do7() {
                    var childrenIn = map18(slot.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do8() {
                            flip(write)(st.handlerRef)(function() {
                              var $65 = maybe(pure12(unit))(handler3);
                              return function($66) {
                                return $65(slot.output($66));
                              };
                            }())();
                            return handleAff(evalM(render2)(st.selfRef)(st["component"]["eval"](new Receive(slot.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $67 = maybe(pure12(unit))(handler3);
                          return function($68) {
                            return $67(slot.output($68));
                          };
                        }())(slot.input)(slot.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map18(function($69) {
                      return isJust(slot.get($69));
                    })(read(childrenOutRef))();
                    when3(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_(slot.set($$var2))(childrenOutRef)();
                    return bind4(read($$var2))(renderStateX2(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure7(renderSpec2.renderChild(v.value0));
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };
        var render2 = function(lchs) {
          return function($$var2) {
            return function __do7() {
              var v = read($$var2)();
              var shouldProcessHandlers = map18(isNothing)(read(v.pendingHandlers))();
              when3(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty3)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = function() {
                var $70 = queueOrRun(v.pendingHandlers);
                var $71 = evalF(render2)(v.selfRef);
                return function($72) {
                  return $70($$void6($71($72)));
                };
              }();
              var childHandler = function() {
                var $73 = queueOrRun(v.pendingQueries);
                return function($74) {
                  return $73(handler3(Action.create($74)));
                };
              }();
              var rendering = renderSpec2.render(function($75) {
                return handleAff(handler3($75));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children2 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot2(childrenIn)(function(v1) {
                return function __do8() {
                  var childDS = read(v1)();
                  renderStateX_2(renderSpec2.removeChild)(childDS)();
                  return finalize(lchs)(childDS)();
                };
              })();
              flip(modify_)(v.selfRef)(mapDriverState(function(ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers,
                  rendering: new Just(rendering),
                  children: children2
                };
              }))();
              return when3(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                return function __do8() {
                  var handlers = read(v.pendingHandlers)();
                  write(new Just(Nil.value))(v.pendingHandlers)();
                  traverse_23(function() {
                    var $76 = traverse_5(fork4);
                    return function($77) {
                      return handleAff($76(reverse2($77)));
                    };
                  }())(handlers)();
                  var mmore = read(v.pendingHandlers)();
                  var $52 = maybe(false)($$null2)(mmore);
                  if ($52) {
                    return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
                  }
                  ;
                  return new Loop(unit);
                };
              }))();
            };
          };
        };
        var finalize = function(lchs) {
          return unDriverStateX(function(st) {
            return function __do7() {
              cleanupSubscriptionsAndForks(st)();
              var f = evalM(render2)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_(function(handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return foreachSlot2(st.children)(function(v) {
                return function __do8() {
                  var dsx = read(v)();
                  return finalize(lchs)(dsx)();
                };
              })();
            };
          });
        };
        var evalDriver = function(disposed) {
          return function(ref2) {
            return function(q2) {
              return bind13(liftEffect5(read(disposed)))(function(v) {
                if (v) {
                  return pure12(Nothing.value);
                }
                ;
                return evalQ(render2)(ref2)(q2);
              });
            };
          };
        };
        var dispose = function(disposed) {
          return function(lchs) {
            return function(dsx) {
              return handleLifecycle(lchs)(function __do7() {
                var v = read(disposed)();
                if (v) {
                  return unit;
                }
                ;
                write(true)(disposed)();
                finalize(lchs)(dsx)();
                return unDriverStateX(function(v1) {
                  return function __do8() {
                    var v2 = liftEffect1(read(v1.selfRef))();
                    return for_2(v2.rendering)(renderSpec2.dispose)();
                  };
                })(dsx)();
              });
            };
          };
        };
        return bind13(liftEffect5(newLifecycleHandlers))(function(lchs) {
          return bind13(liftEffect5($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do7() {
              var sio = create3();
              var dsx = bindFlipped6(read)(runComponent(lchs)(function() {
                var $78 = notify(sio.listener);
                return function($79) {
                  return liftEffect5($78($79));
                };
              }())(i2)(component2))();
              return unDriverStateX(function(st) {
                return pure7({
                  query: evalDriver(disposed)(st.selfRef),
                  messages: sio.emitter,
                  dispose: dispose(disposed)(lchs)(dsx)
                });
              })(dsx)();
            });
          });
        });
      };
    };
  };

  // output/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name16) {
    return function(node) {
      return function() {
        return node[name16];
      };
    };
  };
  var baseURI = getEffProp2("baseURI");
  var _ownerDocument = getEffProp2("ownerDocument");
  var _parentNode = getEffProp2("parentNode");
  var _parentElement = getEffProp2("parentElement");
  var childNodes = getEffProp2("childNodes");
  var _firstChild = getEffProp2("firstChild");
  var _lastChild = getEffProp2("lastChild");
  var _previousSibling = getEffProp2("previousSibling");
  var _nextSibling = getEffProp2("nextSibling");
  var _nodeValue = getEffProp2("nodeValue");
  var textContent = getEffProp2("textContent");
  function insertBefore(node1) {
    return function(node2) {
      return function(parent2) {
        return function() {
          parent2.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent2) {
      return function() {
        parent2.removeChild(node);
      };
    };
  }

  // output/Web.DOM.Node/index.js
  var map19 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ function() {
    var $6 = map19(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $15 = map19(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy9 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var $$void7 = /* @__PURE__ */ $$void(functorEffect);
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap3 = /* @__PURE__ */ unwrap();
  var when4 = /* @__PURE__ */ when(applicativeEffect);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map20 = /* @__PURE__ */ map(functorEffect);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void7(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void7(appendChild(v)(v2.value0));
        }
        ;
        return pure8(unit);
      };
    };
  };
  var removeChild3 = function(v) {
    return function __do7() {
      var npn = parentNode2(v.node)();
      return traverse_6(function(pn) {
        return removeChild2(v.node)(pn);
      })(npn)();
    };
  };
  var mkSpec = function(handler3) {
    return function(renderChildRef) {
      return function(document3) {
        var getNode = unRenderStateX(function(v) {
          return v.node;
        });
        var done = function(st) {
          if (st instanceof Just) {
            return halt(st.value0);
          }
          ;
          return unit;
        };
        var buildWidget2 = function(spec) {
          var buildThunk2 = buildThunk(unwrap3)(spec);
          var $lazy_patch = $runtime_lazy9("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot) {
              if (st instanceof Just) {
                if (slot instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot.value0);
                }
                ;
                if (slot instanceof ThunkSlot) {
                  var step$prime = step2(st.value0, slot.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot);
            };
          });
          var $lazy_render = $runtime_lazy9("render", "Halogen.VDom.Driver", function() {
            return function(slot) {
              if (slot instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot.value0);
              }
              ;
              if (slot instanceof ThunkSlot) {
                var step3 = buildThunk2(slot.value0);
                return mkStep(new Step(extract2(step3), new Just(step3), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot.constructor.name]);
            };
          });
          var $lazy_renderComponentSlot = $runtime_lazy9("renderComponentSlot", "Halogen.VDom.Driver", function() {
            return function(cs) {
              var renderChild = read(renderChildRef)();
              var rsx = renderChild(cs)();
              var node = getNode(rsx);
              return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
            };
          });
          var patch = $lazy_patch(91);
          var render2 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render2;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document3
        };
      };
    };
  };
  var renderSpec = function(document3) {
    return function(container) {
      var render2 = function(handler3) {
        return function(child) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do7() {
                  var renderChildRef = $$new(child)();
                  var spec = mkSpec(handler3)(renderChildRef)(document3);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void7(appendChild(node)(toNode(container)))();
                  return {
                    machine,
                    node,
                    renderChildRef
                  };
                };
              }
              ;
              if (v1 instanceof Just) {
                return function __do7() {
                  write(child)(v1.value0.renderChildRef)();
                  var parent2 = parentNode2(v1.value0.node)();
                  var nextSib = nextSibling(v1.value0.node)();
                  var machine$prime = step2(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when4(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                  return {
                    machine: machine$prime,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };
      return {
        render: render2,
        renderChild: identity8,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component2) {
    return function(i2) {
      return function(element3) {
        return bind14(liftEffect6(map20(toDocument)(bindFlipped7(document2)(windowImpl))))(function(document3) {
          return runUI(renderSpec(document3)(element3))(component2)(i2);
        });
      };
    };
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _singleton = function(fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };
  var _take = function(fallback) {
    return function(n) {
      if (hasStringIterator) {
        return function(str) {
          var accum = "";
          var iter = str[Symbol.iterator]();
          for (var i2 = 0; i2 < n; ++i2) {
            var o = iter.next();
            if (o.done) return accum;
            accum += o.value;
          }
          return accum;
        };
      }
      return fallback(n);
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.String.CodePoints/index.js
  var $runtime_lazy10 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map21 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div3 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var compare2 = /* @__PURE__ */ compare(ordInt);
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons5 = function(s) {
    var v = length5(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop3(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop3(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map21(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons5(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length5(s) > 1;
    if ($47) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length7 = function($74) {
    return length3(toCodePointArray($74));
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton6($75($76));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div3(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var singleton7 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(v) {
    return function(v1) {
      if (v < 1) {
        return "";
      }
      ;
      var v2 = uncons5(v1);
      if (v2 instanceof Just) {
        return singleton7(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
      }
      ;
      return v1;
    };
  };
  var take4 = /* @__PURE__ */ _take(takeFallback);
  var eqCodePoint = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };
  var ordCodePoint = {
    compare: function(x) {
      return function(y) {
        return compare2(x)(y);
      };
    },
    Eq0: function() {
      return eqCodePoint;
    }
  };
  var drop4 = function(n) {
    return function(s) {
      return drop3(length5(take4(n)(s)))(s);
    };
  };
  var boundedCodePoint = {
    bottom: 0,
    top: 1114111,
    Ord0: function() {
      return ordCodePoint;
    }
  };
  var boundedEnumCodePoint = /* @__PURE__ */ function() {
    return {
      cardinality: 1114111 + 1 | 0,
      fromEnum: function(v) {
        return v;
      },
      toEnum: function(n) {
        if (n >= 0 && n <= 1114111) {
          return new Just(n);
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
      },
      Bounded0: function() {
        return boundedCodePoint;
      },
      Enum1: function() {
        return $lazy_enumCodePoint(0);
      }
    };
  }();
  var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy10("enumCodePoint", "Data.String.CodePoints", function() {
    return {
      succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      Ord0: function() {
        return ordCodePoint;
      }
    };
  });

  // output/Protocol.Common/index.js
  var TokenList = /* @__PURE__ */ function() {
    function TokenList2(value0) {
      this.value0 = value0;
    }
    ;
    TokenList2.create = function(value0) {
      return new TokenList2(value0);
    };
    return TokenList2;
  }();
  var PositionList = /* @__PURE__ */ function() {
    function PositionList2(value0) {
      this.value0 = value0;
    }
    ;
    PositionList2.create = function(value0) {
      return new PositionList2(value0);
    };
    return PositionList2;
  }();
  var Balance = /* @__PURE__ */ function() {
    function Balance2(value0) {
      this.value0 = value0;
    }
    ;
    Balance2.create = function(value0) {
      return new Balance2(value0);
    };
    return Balance2;
  }();
  var TokenInfo = /* @__PURE__ */ function() {
    function TokenInfo2(value0) {
      this.value0 = value0;
    }
    ;
    TokenInfo2.create = function(value0) {
      return new TokenInfo2(value0);
    };
    return TokenInfo2;
  }();
  var LenderOfferList = /* @__PURE__ */ function() {
    function LenderOfferList2(value0) {
      this.value0 = value0;
    }
    ;
    LenderOfferList2.create = function(value0) {
      return new LenderOfferList2(value0);
    };
    return LenderOfferList2;
  }();
  var SystemStatsResult = /* @__PURE__ */ function() {
    function SystemStatsResult2(value0) {
      this.value0 = value0;
    }
    ;
    SystemStatsResult2.create = function(value0) {
      return new SystemStatsResult2(value0);
    };
    return SystemStatsResult2;
  }();
  var POLMetricsResult = /* @__PURE__ */ function() {
    function POLMetricsResult2(value0) {
      this.value0 = value0;
    }
    ;
    POLMetricsResult2.create = function(value0) {
      return new POLMetricsResult2(value0);
    };
    return POLMetricsResult2;
  }();
  var TargetTokenInfo = /* @__PURE__ */ function() {
    function TargetTokenInfo2(value0) {
      this.value0 = value0;
    }
    ;
    TargetTokenInfo2.create = function(value0) {
      return new TargetTokenInfo2(value0);
    };
    return TargetTokenInfo2;
  }();
  var ActiveOfferingsList = /* @__PURE__ */ function() {
    function ActiveOfferingsList2(value0) {
      this.value0 = value0;
    }
    ;
    ActiveOfferingsList2.create = function(value0) {
      return new ActiveOfferingsList2(value0);
    };
    return ActiveOfferingsList2;
  }();
  var OfferingStatusResult = /* @__PURE__ */ function() {
    function OfferingStatusResult2(value0) {
      this.value0 = value0;
    }
    ;
    OfferingStatusResult2.create = function(value0) {
      return new OfferingStatusResult2(value0);
    };
    return OfferingStatusResult2;
  }();
  var TokenCreated = /* @__PURE__ */ function() {
    function TokenCreated2(value0) {
      this.value0 = value0;
    }
    ;
    TokenCreated2.create = function(value0) {
      return new TokenCreated2(value0);
    };
    return TokenCreated2;
  }();
  var PositionCreated = /* @__PURE__ */ function() {
    function PositionCreated2(value0) {
      this.value0 = value0;
    }
    ;
    PositionCreated2.create = function(value0) {
      return new PositionCreated2(value0);
    };
    return PositionCreated2;
  }();
  var TokensTransferred = /* @__PURE__ */ function() {
    function TokensTransferred2(value0) {
      this.value0 = value0;
    }
    ;
    TokensTransferred2.create = function(value0) {
      return new TokensTransferred2(value0);
    };
    return TokensTransferred2;
  }();
  var FeelsSOLMinted = /* @__PURE__ */ function() {
    function FeelsSOLMinted2(value0) {
      this.value0 = value0;
    }
    ;
    FeelsSOLMinted2.create = function(value0) {
      return new FeelsSOLMinted2(value0);
    };
    return FeelsSOLMinted2;
  }();
  var FeelsSOLBurned = /* @__PURE__ */ function() {
    function FeelsSOLBurned2(value0) {
      this.value0 = value0;
    }
    ;
    FeelsSOLBurned2.create = function(value0) {
      return new FeelsSOLBurned2(value0);
    };
    return FeelsSOLBurned2;
  }();
  var UnbondingInitiated = /* @__PURE__ */ function() {
    function UnbondingInitiated2(value0) {
      this.value0 = value0;
    }
    ;
    UnbondingInitiated2.create = function(value0) {
      return new UnbondingInitiated2(value0);
    };
    return UnbondingInitiated2;
  }();
  var PositionWithdrawn = /* @__PURE__ */ function() {
    function PositionWithdrawn2(value0) {
      this.value0 = value0;
    }
    ;
    PositionWithdrawn2.create = function(value0) {
      return new PositionWithdrawn2(value0);
    };
    return PositionWithdrawn2;
  }();
  var OfferingCreated = /* @__PURE__ */ function() {
    function OfferingCreated2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    OfferingCreated2.create = function(value0) {
      return function(value1) {
        return new OfferingCreated2(value0, value1);
      };
    };
    return OfferingCreated2;
  }();
  var OfferingPhaseStarted = /* @__PURE__ */ function() {
    function OfferingPhaseStarted2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    OfferingPhaseStarted2.create = function(value0) {
      return function(value1) {
        return new OfferingPhaseStarted2(value0, value1);
      };
    };
    return OfferingPhaseStarted2;
  }();
  var OfferingPhaseCompleted = /* @__PURE__ */ function() {
    function OfferingPhaseCompleted2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    OfferingPhaseCompleted2.create = function(value0) {
      return function(value1) {
        return new OfferingPhaseCompleted2(value0, value1);
      };
    };
    return OfferingPhaseCompleted2;
  }();

  // output/Protocol.Error/index.js
  var show2 = /* @__PURE__ */ show(showNumber);
  var show1 = /* @__PURE__ */ show(showInt);
  var InvalidCommandError = /* @__PURE__ */ function() {
    function InvalidCommandError2(value0) {
      this.value0 = value0;
    }
    ;
    InvalidCommandError2.create = function(value0) {
      return new InvalidCommandError2(value0);
    };
    return InvalidCommandError2;
  }();
  var ValidationError = /* @__PURE__ */ function() {
    function ValidationError2(value0) {
      this.value0 = value0;
    }
    ;
    ValidationError2.create = function(value0) {
      return new ValidationError2(value0);
    };
    return ValidationError2;
  }();
  var InvalidAmountError = /* @__PURE__ */ function() {
    function InvalidAmountError2(value0) {
      this.value0 = value0;
    }
    ;
    InvalidAmountError2.create = function(value0) {
      return new InvalidAmountError2(value0);
    };
    return InvalidAmountError2;
  }();
  var TokenNotFoundError = /* @__PURE__ */ function() {
    function TokenNotFoundError2(value0) {
      this.value0 = value0;
    }
    ;
    TokenNotFoundError2.create = function(value0) {
      return new TokenNotFoundError2(value0);
    };
    return TokenNotFoundError2;
  }();
  var PositionNotFoundError = /* @__PURE__ */ function() {
    function PositionNotFoundError2(value0) {
      this.value0 = value0;
    }
    ;
    PositionNotFoundError2.create = function(value0) {
      return new PositionNotFoundError2(value0);
    };
    return PositionNotFoundError2;
  }();
  var UserNotFoundError = /* @__PURE__ */ function() {
    function UserNotFoundError2(value0) {
      this.value0 = value0;
    }
    ;
    UserNotFoundError2.create = function(value0) {
      return new UserNotFoundError2(value0);
    };
    return UserNotFoundError2;
  }();
  var InsufficientBalanceError = /* @__PURE__ */ function() {
    function InsufficientBalanceError2(value0) {
      this.value0 = value0;
    }
    ;
    InsufficientBalanceError2.create = function(value0) {
      return new InsufficientBalanceError2(value0);
    };
    return InsufficientBalanceError2;
  }();
  var TransferError = /* @__PURE__ */ function() {
    function TransferError2(value0) {
      this.value0 = value0;
    }
    ;
    TransferError2.create = function(value0) {
      return new TransferError2(value0);
    };
    return TransferError2;
  }();
  var MatchingError = /* @__PURE__ */ function() {
    function MatchingError2(value0) {
      this.value0 = value0;
    }
    ;
    MatchingError2.create = function(value0) {
      return new MatchingError2(value0);
    };
    return MatchingError2;
  }();
  var SystemError = /* @__PURE__ */ function() {
    function SystemError2(value0) {
      this.value0 = value0;
    }
    ;
    SystemError2.create = function(value0) {
      return new SystemError2(value0);
    };
    return SystemError2;
  }();
  var showProtocolError = {
    show: function(v) {
      if (v instanceof InvalidCommandError) {
        return "Invalid command: " + v.value0;
      }
      ;
      if (v instanceof ValidationError) {
        return "Validation error: " + v.value0;
      }
      ;
      if (v instanceof InvalidAmountError) {
        return "Invalid amount: " + show2(v.value0);
      }
      ;
      if (v instanceof TokenNotFoundError) {
        return "Token not found: " + v.value0;
      }
      ;
      if (v instanceof PositionNotFoundError) {
        return "Position not found: " + show1(v.value0);
      }
      ;
      if (v instanceof UserNotFoundError) {
        return "User not found: " + v.value0;
      }
      ;
      if (v instanceof InsufficientBalanceError) {
        return "Insufficient balance: " + v.value0;
      }
      ;
      if (v instanceof TransferError) {
        return "Transfer error: " + v.value0;
      }
      ;
      if (v instanceof MatchingError) {
        return "Matching error: " + v.value0;
      }
      ;
      if (v instanceof SystemError) {
        return "System error: " + v.value0;
      }
      ;
      throw new Error("Failed pattern match at Protocol.Error (line 45, column 1 - line 55, column 51): " + [v.constructor.name]);
    }
  };

  // output/Protocol.Position/index.js
  var Senior = /* @__PURE__ */ function() {
    function Senior2() {
    }
    ;
    Senior2.value = new Senior2();
    return Senior2;
  }();
  var Junior = /* @__PURE__ */ function() {
    function Junior2() {
    }
    ;
    Junior2.value = new Junior2();
    return Junior2;
  }();
  var Spot = /* @__PURE__ */ function() {
    function Spot2() {
    }
    ;
    Spot2.value = new Spot2();
    return Spot2;
  }();
  var Monthly = /* @__PURE__ */ function() {
    function Monthly2() {
    }
    ;
    Monthly2.value = new Monthly2();
    return Monthly2;
  }();
  var spotDuration = /* @__PURE__ */ function() {
    return Spot.value;
  }();
  var leverageMultiplier = function(v) {
    if (v instanceof Senior) {
      return 1;
    }
    ;
    if (v instanceof Junior) {
      return 3;
    }
    ;
    throw new Error("Failed pattern match at Protocol.Position (line 98, column 1 - line 98, column 41): " + [v.constructor.name]);
  };
  var eqLeverage = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Senior && y instanceof Senior) {
          return true;
        }
        ;
        if (x instanceof Junior && y instanceof Junior) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eqDuration = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Spot && y instanceof Spot) {
          return true;
        }
        ;
        if (x instanceof Monthly && y instanceof Monthly) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eq22 = /* @__PURE__ */ eq(eqDuration);
  var isSpot = function(pos) {
    return eq22(pos.duration)(Spot.value);
  };
  var createPosition = function(id3) {
    return function(owner) {
      return function(amount) {
        return function(price) {
          return function(duration2) {
            return function(leverage) {
              return function(rollover) {
                return function(shares) {
                  return function(currentBlock) {
                    return {
                      id: id3,
                      owner,
                      amount,
                      price,
                      duration: duration2,
                      leverage,
                      rollover,
                      shares,
                      createdAt: currentBlock,
                      value: amount,
                      lockedAmount: 0,
                      accumulatedYield: 0,
                      lastYieldClaim: currentBlock,
                      feeGrowthInside0: 0,
                      feeGrowthInside1: 0
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
  var blocksPerMonth = 80640;
  var isExpired = function(currentBlock) {
    return function(position2) {
      if (position2.duration instanceof Spot) {
        return false;
      }
      ;
      if (position2.duration instanceof Monthly) {
        return currentBlock >= (position2.createdAt + blocksPerMonth | 0);
      }
      ;
      throw new Error("Failed pattern match at Protocol.Position (line 232, column 35 - line 234, column 67): " + [position2.duration.constructor.name]);
    };
  };

  // output/Protocol.Token/index.js
  var compare3 = /* @__PURE__ */ compare(ordString);
  var JitoSOL = /* @__PURE__ */ function() {
    function JitoSOL2() {
    }
    ;
    JitoSOL2.value = new JitoSOL2();
    return JitoSOL2;
  }();
  var FeelsSOL = /* @__PURE__ */ function() {
    function FeelsSOL2() {
    }
    ;
    FeelsSOL2.value = new FeelsSOL2();
    return FeelsSOL2;
  }();
  var Token = /* @__PURE__ */ function() {
    function Token2(value0) {
      this.value0 = value0;
    }
    ;
    Token2.create = function(value0) {
      return new Token2(value0);
    };
    return Token2;
  }();
  var showTokenType = {
    show: function(v) {
      if (v instanceof JitoSOL) {
        return "JitoSOL";
      }
      ;
      if (v instanceof FeelsSOL) {
        return "FeelsSOL";
      }
      ;
      if (v instanceof Token) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Protocol.Token (line 52, column 1 - line 55, column 31): " + [v.constructor.name]);
    }
  };
  var eqTokenType = {
    eq: function(x) {
      return function(y) {
        if (x instanceof JitoSOL && y instanceof JitoSOL) {
          return true;
        }
        ;
        if (x instanceof FeelsSOL && y instanceof FeelsSOL) {
          return true;
        }
        ;
        if (x instanceof Token && y instanceof Token) {
          return x.value0 === y.value0;
        }
        ;
        return false;
      };
    }
  };
  var ordTokenType = {
    compare: function(x) {
      return function(y) {
        if (x instanceof JitoSOL && y instanceof JitoSOL) {
          return EQ.value;
        }
        ;
        if (x instanceof JitoSOL) {
          return LT.value;
        }
        ;
        if (y instanceof JitoSOL) {
          return GT.value;
        }
        ;
        if (x instanceof FeelsSOL && y instanceof FeelsSOL) {
          return EQ.value;
        }
        ;
        if (x instanceof FeelsSOL) {
          return LT.value;
        }
        ;
        if (y instanceof FeelsSOL) {
          return GT.value;
        }
        ;
        if (x instanceof Token && y instanceof Token) {
          return compare3(x.value0)(y.value0);
        }
        ;
        throw new Error("Failed pattern match at Protocol.Token (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqTokenType;
    }
  };
  var createToken = function(params) {
    return function __do7() {
      var timestamp = currentTime();
      var tokenId = generateId(timestamp);
      return {
        id: tokenId,
        ticker: params.ticker,
        name: params.name,
        tokenType: new Token(params.ticker),
        creator: params.creator,
        createdAt: timestamp,
        live: true,
        totalSupply: 1e9
      };
    };
  };

  // output/Effect.Random/foreign.js
  var random = Math.random;

  // output/Effect.Random/index.js
  var randomInt = function(low2) {
    return function(high2) {
      return function __do7() {
        var n = random();
        var asNumber = (toNumber2(high2) - toNumber2(low2) + 1) * n + toNumber2(low2);
        return floor3(asNumber);
      };
    };
  };

  // output/Protocol.Oracle/index.js
  var pure9 = /* @__PURE__ */ pure(applicativeEffect);
  var map24 = /* @__PURE__ */ map(functorArray);
  var sum2 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var FiveMinutes = /* @__PURE__ */ function() {
    function FiveMinutes2() {
    }
    ;
    FiveMinutes2.value = new FiveMinutes2();
    return FiveMinutes2;
  }();
  var FifteenMinutes = /* @__PURE__ */ function() {
    function FifteenMinutes2() {
    }
    ;
    FifteenMinutes2.value = new FifteenMinutes2();
    return FifteenMinutes2;
  }();
  var OneHour = /* @__PURE__ */ function() {
    function OneHour2() {
    }
    ;
    OneHour2.value = new OneHour2();
    return OneHour2;
  }();
  var windowToMs = function(v) {
    if (v instanceof FiveMinutes) {
      return 3e5;
    }
    ;
    if (v instanceof FifteenMinutes) {
      return 9e5;
    }
    ;
    if (v instanceof OneHour) {
      return 36e5;
    }
    ;
    throw new Error("Failed pattern match at Protocol.Oracle (line 177, column 1 - line 177, column 35): " + [v.constructor.name]);
  };
  var updatePriceWithTimestamp = function(newPrice) {
    return function(timestamp) {
      return function(oracleRef) {
        return modify_(function(oracle) {
          return {
            bufferConfig: oracle.bufferConfig,
            currentPrice: newPrice,
            priceHistory: take(100)(cons({
              price: newPrice,
              timestamp
            })(oracle.priceHistory)),
            lastUpdate: timestamp
          };
        })(oracleRef);
      };
    };
  };
  var initOracle = function(initialPrice) {
    return function __do7() {
      var timestamp = currentTime();
      var defaultBufferConfig = {
        targetRatio: 0.01,
        minRatio: 5e-3,
        maxRatio: 0.02,
        rebalanceThreshold: 5e-3,
        daoManaged: false
      };
      return $$new({
        currentPrice: initialPrice,
        priceHistory: [{
          price: initialPrice,
          timestamp
        }],
        lastUpdate: timestamp,
        bufferConfig: defaultBufferConfig
      })();
    };
  };
  var getVolatility = function(oracleRef) {
    return function __do7() {
      var oracle = read(oracleRef)();
      var prices = map24(function(v) {
        return v.price;
      })(take(20)(oracle.priceHistory));
      var $24 = length3(prices) < 2;
      if ($24) {
        return 0;
      }
      ;
      var mean = sum2(prices) / toNumber2(length3(prices));
      var variance = sum2(map24(function(p2) {
        return (p2 - mean) * (p2 - mean);
      })(prices)) / toNumber2(length3(prices));
      return sqrt2(variance) / mean;
    };
  };
  var getTWAP = function(window2) {
    return function(oracleRef) {
      return function __do7() {
        var oracle = read(oracleRef)();
        var timestamp = currentTime();
        var windowMs = windowToMs(window2);
        var cutoff = timestamp - windowMs;
        var relevantPrices = filter(function(p2) {
          return p2.timestamp >= cutoff;
        })(oracle.priceHistory);
        var $25 = length3(relevantPrices) === 0;
        if ($25) {
          return oracle.currentPrice;
        }
        ;
        return sum2(map24(function(v) {
          return v.price;
        })(relevantPrices)) / toNumber2(length3(relevantPrices));
      };
    };
  };
  var takeMarketSnapshot = function(oracleRef) {
    var calculateBufferHealth = function(config) {
      return pure9(config.targetRatio / config.maxRatio);
    };
    return function __do7() {
      var oracle = read(oracleRef)();
      var vol = getVolatility(oracleRef)();
      var twap5m = getTWAP(FiveMinutes.value)(oracleRef)();
      var bufferHealth = calculateBufferHealth(oracle.bufferConfig)();
      return {
        spot: oracle.currentPrice,
        twap5m,
        volatility: vol,
        timestamp: oracle.lastUpdate,
        bufferHealth
      };
    };
  };

  // output/Simulation.Agent/index.js
  var pure10 = /* @__PURE__ */ pure(applicativeEffect);
  var sequence2 = /* @__PURE__ */ sequence(traversableArray)(applicativeEffect);
  var map25 = /* @__PURE__ */ map(functorArray);
  var show3 = /* @__PURE__ */ show(showInt);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
  var Conservative = /* @__PURE__ */ function() {
    function Conservative2() {
    }
    ;
    Conservative2.value = new Conservative2();
    return Conservative2;
  }();
  var Moderate = /* @__PURE__ */ function() {
    function Moderate2() {
    }
    ;
    Moderate2.value = new Moderate2();
    return Moderate2;
  }();
  var Aggressive = /* @__PURE__ */ function() {
    function Aggressive2() {
    }
    ;
    Aggressive2.value = new Aggressive2();
    return Aggressive2;
  }();
  var Arbitrageur = /* @__PURE__ */ function() {
    function Arbitrageur2() {
    }
    ;
    Arbitrageur2.value = new Arbitrageur2();
    return Arbitrageur2;
  }();
  var Whale = /* @__PURE__ */ function() {
    function Whale2() {
    }
    ;
    Whale2.value = new Whale2();
    return Whale2;
  }();
  var Retail = /* @__PURE__ */ function() {
    function Retail2() {
    }
    ;
    Retail2.value = new Retail2();
    return Retail2;
  }();
  var generateAccounts = function(config) {
    var generateAccount = function(profiles) {
      return function(userId) {
        return function __do7() {
          var profileIndex = randomInt(0)(length3(profiles) - 1 | 0)();
          var profile = function() {
            var v = head(drop(profileIndex)(profiles));
            if (v instanceof Just) {
              return v.value0;
            }
            ;
            if (v instanceof Nothing) {
              return Moderate.value;
            }
            ;
            throw new Error("Failed pattern match at Simulation.Agent (line 113, column 21 - line 115, column 32): " + [v.constructor.name]);
          }();
          var baseBalance = random();
          var multiplier = function() {
            if (profile instanceof Whale) {
              return 1e4 + baseBalance * 9e4;
            }
            ;
            if (profile instanceof Arbitrageur) {
              return 5e3 + baseBalance * 15e3;
            }
            ;
            if (profile instanceof Aggressive) {
              return 1e3 + baseBalance * 9e3;
            }
            ;
            if (profile instanceof Moderate) {
              return 500 + baseBalance * 2e3;
            }
            ;
            if (profile instanceof Conservative) {
              return 100 + baseBalance * 900;
            }
            ;
            if (profile instanceof Retail) {
              return 50 + baseBalance * 200;
            }
            ;
            throw new Error("Failed pattern match at Simulation.Agent (line 120, column 24 - line 126, column 49): " + [profile.constructor.name]);
          }();
          return {
            id: userId,
            profile,
            jitoSOLBalance: multiplier,
            feelsSOLBalance: 0,
            activePositions: [],
            totalDeposited: 0,
            totalWithdrawn: 0,
            netPnL: 0
          };
        };
      };
    };
    return function __do7() {
      var accountIds = sequence2(map25(function(i2) {
        return pure10("user" + show3(i2));
      })(range2(1)(config.numAccounts)))();
      return traverse2(generateAccount(config.accountProfiles))(accountIds)();
    };
  };
  var eqAccountProfile = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Conservative && y instanceof Conservative) {
          return true;
        }
        ;
        if (x instanceof Moderate && y instanceof Moderate) {
          return true;
        }
        ;
        if (x instanceof Aggressive && y instanceof Aggressive) {
          return true;
        }
        ;
        if (x instanceof Arbitrageur && y instanceof Arbitrageur) {
          return true;
        }
        ;
        if (x instanceof Whale && y instanceof Whale) {
          return true;
        }
        ;
        if (x instanceof Retail && y instanceof Retail) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/Simulation.Market/index.js
  var BullMarket = /* @__PURE__ */ function() {
    function BullMarket2() {
    }
    ;
    BullMarket2.value = new BullMarket2();
    return BullMarket2;
  }();
  var BearMarket = /* @__PURE__ */ function() {
    function BearMarket2() {
    }
    ;
    BearMarket2.value = new BearMarket2();
    return BearMarket2;
  }();
  var SidewaysMarket = /* @__PURE__ */ function() {
    function SidewaysMarket2() {
    }
    ;
    SidewaysMarket2.value = new SidewaysMarket2();
    return SidewaysMarket2;
  }();
  var VolatileMarket = /* @__PURE__ */ function() {
    function VolatileMarket2() {
    }
    ;
    VolatileMarket2.value = new VolatileMarket2();
    return VolatileMarket2;
  }();
  var CrashScenario = /* @__PURE__ */ function() {
    function CrashScenario2() {
    }
    ;
    CrashScenario2.value = new CrashScenario2();
    return CrashScenario2;
  }();
  var RecoveryMarket = /* @__PURE__ */ function() {
    function RecoveryMarket2() {
    }
    ;
    RecoveryMarket2.value = new RecoveryMarket2();
    return RecoveryMarket2;
  }();
  var generateMarketScenario = function(config) {
    return function(currentBlock) {
      return function __do7() {
        var baseVolatility = random();
        var volatility2 = config.priceVolatility * baseVolatility;
        if (config.scenario instanceof BullMarket) {
          var trend = random();
          var movement = 2e-3 + trend * 0.01;
          return movement;
        }
        ;
        if (config.scenario instanceof BearMarket) {
          var trend = random();
          var movement = -0.012 - trend * 8e-3;
          return movement;
        }
        ;
        if (config.scenario instanceof SidewaysMarket) {
          var noise = random();
          return (noise - 0.5) * 3e-3;
        }
        ;
        if (config.scenario instanceof VolatileMarket) {
          var swing = random();
          var direction = random();
          var magnitude = 0.03 + swing * 0.07;
          var $14 = direction > 0.5;
          if ($14) {
            return magnitude;
          }
          ;
          return -magnitude;
        }
        ;
        if (config.scenario instanceof CrashScenario) {
          var $15 = currentBlock < 10;
          if ($15) {
            var crash = random();
            return -0.05 - crash * 0.15;
          }
          ;
          var recovery = random();
          return -1e-3 + recovery * 3e-3;
        }
        ;
        if (config.scenario instanceof RecoveryMarket) {
          var recovery = random();
          var phase = toNumber2(currentBlock) / 100;
          return 2e-3 + recovery * 8e-3 * (1 - phase);
        }
        ;
        throw new Error("Failed pattern match at Simulation.Market (line 89, column 3 - line 136, column 54): " + [config.scenario.constructor.name]);
      };
    };
  };

  // output/Utils/index.js
  var show4 = /* @__PURE__ */ show(showNumber);
  var show12 = /* @__PURE__ */ show(showInt);
  var formatAmount = function(amount) {
    var rounded = toNumber2(floor3(amount * 100)) / 100;
    var $14 = amount === toNumber2(floor3(amount));
    if ($14) {
      return show12(floor3(amount));
    }
    ;
    return show4(rounded);
  };

  // output/Simulation.Action/index.js
  var foldl3 = /* @__PURE__ */ foldl(foldableArray);
  var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
  var max7 = /* @__PURE__ */ max(ordInt);
  var pure11 = /* @__PURE__ */ pure(applicativeEffect);
  var eq3 = /* @__PURE__ */ eq(eqAccountProfile);
  var min6 = /* @__PURE__ */ min(ordNumber);
  var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumCodePoint);
  var sequence3 = /* @__PURE__ */ sequence(traversableArray)(applicativeEffect);
  var map26 = /* @__PURE__ */ map(functorArray);
  var EnterProtocol = /* @__PURE__ */ function() {
    function EnterProtocol2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    EnterProtocol2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new EnterProtocol2(value0, value1, value22);
        };
      };
    };
    return EnterProtocol2;
  }();
  var ExitProtocol = /* @__PURE__ */ function() {
    function ExitProtocol2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ExitProtocol2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ExitProtocol2(value0, value1, value22);
        };
      };
    };
    return ExitProtocol2;
  }();
  var CreateToken = /* @__PURE__ */ function() {
    function CreateToken3(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    CreateToken3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new CreateToken3(value0, value1, value22);
        };
      };
    };
    return CreateToken3;
  }();
  var CreateLendOffer = /* @__PURE__ */ function() {
    function CreateLendOffer2(value0, value1, value22, value32, value42, value52, value62) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
    }
    ;
    CreateLendOffer2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return new CreateLendOffer2(value0, value1, value22, value32, value42, value52, value62);
                };
              };
            };
          };
        };
      };
    };
    return CreateLendOffer2;
  }();
  var TakeLoan = /* @__PURE__ */ function() {
    function TakeLoan2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    TakeLoan2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new TakeLoan2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return TakeLoan2;
  }();
  var WaitBlocks = /* @__PURE__ */ function() {
    function WaitBlocks2(value0) {
      this.value0 = value0;
    }
    ;
    WaitBlocks2.create = function(value0) {
      return new WaitBlocks2(value0);
    };
    return WaitBlocks2;
  }();
  var getRecentlyCreatedTokens = function(actions) {
    var extractToken = function(acc) {
      return function(action2) {
        if (action2 instanceof CreateToken) {
          return cons(action2.value1)(acc);
        }
        ;
        return acc;
      };
    };
    return foldl3(extractToken)([])(actions);
  };
  var generateTokenSwapAction = function(config) {
    return function(state3) {
      return function(account) {
        var existingTokens = getRecentlyCreatedTokens(state3.actionHistory);
        var tokenIndex = mod3(state3.currentBlock)(max7(1)(length3(existingTokens)));
        var v = head(drop(tokenIndex)(existingTokens));
        if (v instanceof Nothing) {
          return pure11(new WaitBlocks(1));
        }
        ;
        if (v instanceof Just) {
          return function __do7() {
            generateMarketScenario(config)(state3.currentBlock)();
            var priceRand = random();
            var currentMarketPrice = function __do8() {
              var snapshot = takeMarketSnapshot(state3.oracle)();
              return snapshot.spot;
            }();
            var swapRoll = random();
            var marketBias = function() {
              if (config.scenario instanceof BullMarket) {
                return 0.7;
              }
              ;
              if (config.scenario instanceof BearMarket) {
                return 0.3;
              }
              ;
              return 0.5;
            }();
            var isAggressiveTrader = eq3(account.profile)(Aggressive.value) || eq3(account.profile)(Whale.value);
            var shouldBuy = swapRoll < marketBias + function() {
              if (isAggressiveTrader) {
                return 0.2;
              }
              ;
              return 0;
            }();
            var $134 = shouldBuy && account.feelsSOLBalance >= 50;
            if ($134) {
              var amountRand = random();
              var volatilityMultiplier = function() {
                if (config.scenario instanceof VolatileMarket) {
                  return 3;
                }
                ;
                if (config.scenario instanceof CrashScenario) {
                  return 2.5;
                }
                ;
                if (config.scenario instanceof BearMarket) {
                  return 2;
                }
                ;
                if (config.scenario instanceof RecoveryMarket) {
                  return 1.8;
                }
                ;
                return 1;
              }();
              var baseSwapAmount = function() {
                if (account.profile instanceof Whale) {
                  return 100 + amountRand * 400;
                }
                ;
                if (account.profile instanceof Aggressive) {
                  return 50 + amountRand * 150;
                }
                ;
                return 20 + amountRand * 80;
              }();
              var swapAmount = baseSwapAmount * volatilityMultiplier;
              var priceWithSpread = currentMarketPrice * (1.05 + priceRand * 0.1);
              var actualAmount = min6(swapAmount)(account.feelsSOLBalance * 0.8);
              var actionChoice = random();
              var $137 = actionChoice < 0.5;
              if ($137) {
                return new CreateLendOffer(account.id, new Token(v.value0), actualAmount, FeelsSOL.value, priceWithSpread, spotDuration, Nothing.value);
              }
              ;
              return new TakeLoan(account.id, new Token(v.value0), actualAmount, FeelsSOL.value, actualAmount, spotDuration);
            }
            ;
            var $138 = account.feelsSOLBalance >= 20;
            if ($138) {
              var amountRand = random();
              var volatilityMultiplier = function() {
                if (config.scenario instanceof VolatileMarket) {
                  return 3;
                }
                ;
                if (config.scenario instanceof CrashScenario) {
                  return 3.5;
                }
                ;
                if (config.scenario instanceof BearMarket) {
                  return 2.5;
                }
                ;
                return 1;
              }();
              var baseSwapAmount = function() {
                if (account.profile instanceof Whale) {
                  return 80 + amountRand * 320;
                }
                ;
                if (account.profile instanceof Aggressive) {
                  return 40 + amountRand * 120;
                }
                ;
                return 15 + amountRand * 60;
              }();
              var swapAmount = baseSwapAmount * volatilityMultiplier;
              var priceWithSpread = currentMarketPrice * (0.95 - priceRand * 0.1);
              var actualAmount = min6(swapAmount)(account.feelsSOLBalance * 0.6);
              var actionChoice = random();
              var $141 = actionChoice < 0.5;
              if ($141) {
                return new CreateLendOffer(account.id, FeelsSOL.value, actualAmount, new Token(v.value0), priceWithSpread, spotDuration, Nothing.value);
              }
              ;
              return new TakeLoan(account.id, FeelsSOL.value, actualAmount, new Token(v.value0), actualAmount, spotDuration);
            }
            ;
            return new WaitBlocks(1);
          };
        }
        ;
        throw new Error("Failed pattern match at Simulation.Action (line 205, column 3 - line 295, column 35): " + [v.constructor.name]);
      };
    };
  };
  var generateTokenCreationAction = function($copy_config) {
    return function($copy_state) {
      return function __do7() {
        var $tco_var_config = $copy_config;
        var $tco_var_state = $copy_state;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(config, state3) {
          var accountIndex = randomInt(0)(length3(state3.accounts) - 1 | 0)();
          var v = head(drop(accountIndex)(state3.accounts));
          if (v instanceof Just) {
            var rand1 = randomInt(65)(90)();
            var rand2 = randomInt(65)(90)();
            var rand3 = randomInt(65)(90)();
            var c3 = function() {
              var v1 = toEnum2(rand3);
              if (v1 instanceof Just) {
                return singleton7(v1.value0);
              }
              ;
              if (v1 instanceof Nothing) {
                return "C";
              }
              ;
              throw new Error("Failed pattern match at Simulation.Action (line 179, column 16 - line 181, column 32): " + [v1.constructor.name]);
            }();
            var c2 = function() {
              var v1 = toEnum2(rand2);
              if (v1 instanceof Just) {
                return singleton7(v1.value0);
              }
              ;
              if (v1 instanceof Nothing) {
                return "B";
              }
              ;
              throw new Error("Failed pattern match at Simulation.Action (line 176, column 16 - line 178, column 32): " + [v1.constructor.name]);
            }();
            var c1 = function() {
              var v1 = toEnum2(rand1);
              if (v1 instanceof Just) {
                return singleton7(v1.value0);
              }
              ;
              if (v1 instanceof Nothing) {
                return "A";
              }
              ;
              throw new Error("Failed pattern match at Simulation.Action (line 173, column 16 - line 175, column 32): " + [v1.constructor.name]);
            }();
            var ticker = c1 + (c2 + c3);
            var name16 = ticker + " Token";
            var $150 = ticker === "FEELSSOL" || (ticker === "JITOSOL" || ticker === "SOL");
            if ($150) {
              $tco_var_config = config;
              $tco_var_state = state3;
              return;
            }
            ;
            $tco_done = true;
            return new CreateToken(v.value0.id, ticker, name16);
          }
          ;
          if (v instanceof Nothing) {
            $tco_done = true;
            return new WaitBlocks(1);
          }
          ;
          throw new Error("Failed pattern match at Simulation.Action (line 165, column 3 - line 189, column 35): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_config, $tco_var_state);
        }
        ;
        return $tco_result;
      };
    };
  };
  var generateRandomAction = function(config) {
    return function(state3) {
      return function __do7() {
        var accountIndex = randomInt(0)(length3(state3.accounts) - 1 | 0)();
        var account = function() {
          var v2 = head(drop(accountIndex)(state3.accounts));
          if (v2 instanceof Just) {
            return v2.value0;
          }
          ;
          if (v2 instanceof Nothing) {
            return {
              id: "user1",
              profile: Moderate.value,
              jitoSOLBalance: 1e3,
              feelsSOLBalance: 0,
              activePositions: [],
              totalDeposited: 0,
              totalWithdrawn: 0,
              netPnL: 0
            };
          }
          ;
          throw new Error("Failed pattern match at Simulation.Action (line 334, column 17 - line 337, column 97): " + [v2.constructor.name]);
        }();
        var actionRoll = random();
        var marketBias = function() {
          if (config.scenario instanceof BearMarket) {
            return {
              exitBias: 0.7,
              entryBias: 0.1
            };
          }
          ;
          if (config.scenario instanceof CrashScenario) {
            return {
              exitBias: 0.8,
              entryBias: 0.05
            };
          }
          ;
          if (config.scenario instanceof VolatileMarket) {
            return {
              exitBias: 0.5,
              entryBias: 0.3
            };
          }
          ;
          if (config.scenario instanceof RecoveryMarket) {
            return {
              exitBias: 0.2,
              entryBias: 0.6
            };
          }
          ;
          if (config.scenario instanceof BullMarket) {
            return {
              exitBias: 0.2,
              entryBias: 0.5
            };
          }
          ;
          if (config.scenario instanceof SidewaysMarket) {
            return {
              exitBias: 0.3,
              entryBias: 0.3
            };
          }
          ;
          throw new Error("Failed pattern match at Simulation.Action (line 342, column 20 - line 348, column 60): " + [config.scenario.constructor.name]);
        }();
        var existingTokens = getRecentlyCreatedTokens(state3.actionHistory);
        var shouldCreateTokenSwap = function() {
          var $155 = length3(existingTokens) > 0 && account.feelsSOLBalance > 50;
          if ($155) {
            var swapRoll = random();
            return swapRoll < 0.5;
          }
          ;
          return false;
        }();
        if (shouldCreateTokenSwap) {
          return generateTokenSwapAction(config)(state3)(account)();
        }
        ;
        var $157 = actionRoll < marketBias.exitBias && account.feelsSOLBalance > 10;
        if ($157) {
          var amountRand = random();
          var panicMultiplier = function() {
            if (config.scenario instanceof CrashScenario) {
              return 0.8;
            }
            ;
            if (config.scenario instanceof BearMarket) {
              return 0.6;
            }
            ;
            if (config.scenario instanceof VolatileMarket) {
              return 0.4;
            }
            ;
            return 0.3;
          }();
          var exitAmount = account.feelsSOLBalance * panicMultiplier * (0.5 + amountRand * 0.5);
          return new ExitProtocol(account.id, exitAmount, FeelsSOL.value);
        }
        ;
        var $159 = actionRoll < marketBias.exitBias + marketBias.entryBias;
        if ($159) {
          var amountRand = random();
          var opportunityMultiplier = function() {
            if (config.scenario instanceof RecoveryMarket) {
              return 2;
            }
            ;
            if (config.scenario instanceof VolatileMarket) {
              return 1.5;
            }
            ;
            if (config.scenario instanceof BullMarket) {
              return 1.2;
            }
            ;
            return 1;
          }();
          var baseAmount = function() {
            if (account.profile instanceof Whale) {
              return 500 + amountRand * 2e3;
            }
            ;
            if (account.profile instanceof Aggressive) {
              return 200 + amountRand * 800;
            }
            ;
            if (account.profile instanceof Conservative) {
              return 50 + amountRand * 200;
            }
            ;
            return 100 + amountRand * 400;
          }();
          var amount = baseAmount * opportunityMultiplier;
          return new EnterProtocol(account.id, amount, JitoSOL.value);
        }
        ;
        var $162 = actionRoll < 0.7;
        if ($162) {
          var $163 = account.feelsSOLBalance > 50;
          if ($163) {
            var amountRand = random();
            var amount = min6(account.feelsSOLBalance * 0.5)(50 + amountRand * 200);
            return new ExitProtocol(account.id, amount, FeelsSOL.value);
          }
          ;
          return new WaitBlocks(1);
        }
        ;
        var $164 = actionRoll < 0.8;
        if ($164) {
          var recentTokens = getRecentlyCreatedTokens(state3.actionHistory);
          var $165 = length3(recentTokens) === 0;
          if ($165) {
            return new WaitBlocks(1);
          }
          ;
          var tokenIndex = randomInt(0)(max7(0)(length3(recentTokens) - 1 | 0))();
          var v = head(drop(tokenIndex)(recentTokens));
          if (v instanceof Just) {
            var $167 = account.feelsSOLBalance >= 100;
            if ($167) {
              return new CreateLendOffer(account.id, FeelsSOL.value, 100, new Token(v.value0), 100, Monthly.value, new Just(v.value0));
            }
            ;
            return new WaitBlocks(1);
          }
          ;
          if (v instanceof Nothing) {
            return new WaitBlocks(1);
          }
          ;
          throw new Error("Failed pattern match at Simulation.Action (line 416, column 15 - line 423, column 47): " + [v.constructor.name]);
        }
        ;
        return new WaitBlocks(1);
      };
    };
  };
  var generatePositionCreationAction = function(config) {
    return function(state3) {
      var eligibleAccounts = filter(function(acc) {
        return acc.feelsSOLBalance > 10;
      })(state3.accounts);
      var v = head(eligibleAccounts);
      if (v instanceof Just) {
        return generateTokenSwapAction(config)(state3)(v.value0);
      }
      ;
      if (v instanceof Nothing) {
        return function __do7() {
          var actionRand = random();
          var $171 = actionRand < 0.5;
          if ($171) {
            return new WaitBlocks(1);
          }
          ;
          return generateRandomAction(config)(state3)();
        };
      }
      ;
      throw new Error("Failed pattern match at Simulation.Action (line 309, column 3 - line 319, column 47): " + [v.constructor.name]);
    };
  };
  var generateTradingSequence = function(config) {
    return function(state3) {
      return function __do7() {
        var baseActions = random();
        var baseNumActions = floor3(config.actionFrequency * (0.5 + baseActions));
        var volatilityMultiplier = function() {
          if (config.scenario instanceof VolatileMarket) {
            return 3;
          }
          ;
          if (config.scenario instanceof CrashScenario) {
            return 2.5;
          }
          ;
          if (config.scenario instanceof BearMarket) {
            return 1.8;
          }
          ;
          if (config.scenario instanceof RecoveryMarket) {
            return 1.5;
          }
          ;
          if (config.scenario instanceof BullMarket) {
            return 1.2;
          }
          ;
          if (config.scenario instanceof SidewaysMarket) {
            return 1;
          }
          ;
          throw new Error("Failed pattern match at Simulation.Action (line 107, column 30 - line 113, column 30): " + [config.scenario.constructor.name]);
        }();
        var numActions = max7(3)(floor3(toNumber2(baseNumActions) * volatilityMultiplier));
        var existingTokenCount = length3(getRecentlyCreatedTokens(state3.actionHistory));
        var tokenRand = random();
        var targetTokenCount = 5 + floor3(tokenRand * 6) | 0;
        var shouldCreateToken = existingTokenCount < targetTokenCount && (state3.currentBlock > (existingTokenCount * 8 | 0) && state3.currentBlock < 80);
        var hasActiveTokens = length3(getRecentlyCreatedTokens(state3.actionHistory)) > 0;
        var actions = sequence3(map26(function(i2) {
          var $173 = i2 === 1 && shouldCreateToken;
          if ($173) {
            return generateTokenCreationAction(config)(state3);
          }
          ;
          var $174 = hasActiveTokens && toNumber2(i2) <= toNumber2(numActions) * 4 / 5;
          if ($174) {
            var eligibleAccounts = filter(function(acc) {
              return acc.feelsSOLBalance > 10;
            })(state3.accounts);
            var accountIndex = mod3((i2 * 7 | 0) + state3.currentBlock | 0)(max7(1)(length3(eligibleAccounts)));
            var v = head(drop(accountIndex)(eligibleAccounts));
            if (v instanceof Just) {
              return generateTokenSwapAction(config)(state3)(v.value0);
            }
            ;
            if (v instanceof Nothing) {
              return generatePositionCreationAction(config)(state3);
            }
            ;
            throw new Error("Failed pattern match at Simulation.Action (line 144, column 11 - line 146, column 67): " + [v.constructor.name]);
          }
          ;
          var $177 = toNumber2(i2) <= toNumber2(numActions) * 3 / 4;
          if ($177) {
            return generatePositionCreationAction(config)(state3);
          }
          ;
          return generateRandomAction(config)(state3);
        })(range2(1)(numActions)))();
        return actions;
      };
    };
  };

  // output/UI.PoolRegistry/index.js
  var insert5 = /* @__PURE__ */ insert(ordString);
  var lookup5 = /* @__PURE__ */ lookup(ordInt);
  var $$delete4 = /* @__PURE__ */ $$delete(ordInt);
  var fromFoldable4 = /* @__PURE__ */ fromFoldable2(foldableList);
  var lookup12 = /* @__PURE__ */ lookup(ordString);
  var toUnfoldable3 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var insert12 = /* @__PURE__ */ insert(ordInt);
  var updatePool = function(poolId) {
    return function(pool) {
      return function(registryRef) {
        return modify_(function(reg) {
          return {
            nextPositionId: reg.nextPositionId,
            positions: reg.positions,
            pools: insert5(poolId)(pool)(reg.pools)
          };
        })(registryRef);
      };
    };
  };
  var removePosition = function(posId) {
    return function(registryRef) {
      return function __do7() {
        var registry = read(registryRef)();
        var v = lookup5(posId)(registry.positions);
        if (v instanceof Just) {
          modify_(function(reg) {
            return {
              nextPositionId: reg.nextPositionId,
              pools: reg.pools,
              positions: $$delete4(posId)(reg.positions)
            };
          })(registryRef)();
          return unit;
        }
        ;
        if (v instanceof Nothing) {
          return unit;
        }
        ;
        throw new Error("Failed pattern match at UI.PoolRegistry (line 122, column 3 - line 132, column 25): " + [v.constructor.name]);
      };
    };
  };
  var initPoolRegistry = /* @__PURE__ */ $$new({
    pools: empty2,
    positions: empty2,
    nextPositionId: 1
  });
  var getUserPositions = function(userId) {
    return function(registryRef) {
      return function __do7() {
        var registry = read(registryRef)();
        return filter(function(p2) {
          return p2.owner === userId;
        })(fromFoldable4(values(registry.positions)));
      };
    };
  };
  var getPosition = function(posId) {
    return function(registryRef) {
      return function __do7() {
        var registry = read(registryRef)();
        return lookup5(posId)(registry.positions);
      };
    };
  };
  var getPool = function(poolId) {
    return function(registryRef) {
      return function __do7() {
        var registry = read(registryRef)();
        return lookup12(poolId)(registry.pools);
      };
    };
  };
  var getNextPositionId = function(registryRef) {
    return function __do7() {
      var registry = read(registryRef)();
      modify_(function(reg) {
        return {
          pools: reg.pools,
          positions: reg.positions,
          nextPositionId: registry.nextPositionId + 1 | 0
        };
      })(registryRef)();
      return registry.nextPositionId;
    };
  };
  var getAllPositions = function(registryRef) {
    return function __do7() {
      var registry = read(registryRef)();
      return fromFoldable4(values(registry.positions));
    };
  };
  var getAllPools = function(registryRef) {
    return function __do7() {
      var registry = read(registryRef)();
      return toUnfoldable3(registry.pools);
    };
  };
  var addPosition = function(position2) {
    return function(registryRef) {
      return function __do7() {
        modify_(function(reg) {
          return {
            nextPositionId: reg.nextPositionId,
            pools: reg.pools,
            positions: insert12(position2.id)(position2)(reg.positions)
          };
        })(registryRef)();
        return unit;
      };
    };
  };
  var addPool = function(poolId) {
    return function(pool) {
      return function(registryRef) {
        return modify_(function(reg) {
          return {
            nextPositionId: reg.nextPositionId,
            positions: reg.positions,
            pools: insert5(poolId)(pool)(reg.pools)
          };
        })(registryRef);
      };
    };
  };

  // output/Simulation.Analysis/index.js
  var map27 = /* @__PURE__ */ map(functorArray);
  var sum3 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var min7 = /* @__PURE__ */ min(ordNumber);
  var calculateResults = function(config) {
    return function(finalState) {
      var isTradingAction = function(action2) {
        if (action2 instanceof CreateLendOffer) {
          return true;
        }
        ;
        if (action2 instanceof TakeLoan) {
          return true;
        }
        ;
        return false;
      };
      var getActionVolume = function(action2) {
        if (action2 instanceof CreateLendOffer) {
          return action2.value2;
        }
        ;
        if (action2 instanceof TakeLoan) {
          return action2.value2;
        }
        ;
        return 0;
      };
      var priceChange = (finalState.currentPrice - config.initialJitoSOLPrice) / config.initialJitoSOLPrice;
      var prices = map27(function(v) {
        return v.price;
      })(finalState.priceHistory);
      var pricesArray = function() {
        var $42 = length3(prices) > 0;
        if ($42) {
          return prices;
        }
        ;
        return [config.initialJitoSOLPrice];
      }();
      var avgPrice = sum3(pricesArray) / toNumber2(length3(pricesArray));
      var variance = sum3(map27(function(p2) {
        return (p2 - avgPrice) * (p2 - avgPrice);
      })(pricesArray)) / toNumber2(length3(pricesArray));
      var volatility2 = sqrt(variance) / avgPrice;
      return function __do7() {
        var positions = getAllPositions(finalState.poolRegistry)();
        var totalLiquidity2 = sum3(map27(function(v) {
          return v.amount;
        })(positions));
        var activePositionCount = length3(positions);
        var tradeActions = filter(isTradingAction)(finalState.actionHistory);
        var totalVolume = function() {
          var $43 = length3(tradeActions) > 0;
          if ($43) {
            return sum3(map27(getActionVolume)(tradeActions));
          }
          ;
          return 1e3;
        }();
        var totalFees = totalVolume * 0.01;
        var averageUtilization = function() {
          var $44 = totalLiquidity2 > 0;
          if ($44) {
            return min7(1)(totalVolume / totalLiquidity2);
          }
          ;
          return 0.5;
        }();
        var scenarioSuccess = volatility2 < 0.2;
        return {
          totalVolume,
          totalFees,
          activePositions: activePositionCount,
          totalUsers: activePositionCount,
          priceChange,
          volatility: volatility2,
          protocolTVL: totalLiquidity2,
          averageUtilization,
          scenarioSuccess
        };
      };
    };
  };

  // output/Protocol.FeelsSOL/index.js
  var show5 = /* @__PURE__ */ show(showTokenType);
  var pure13 = /* @__PURE__ */ pure(applicativeEffect);
  var show13 = /* @__PURE__ */ show(showNumber);
  var min8 = /* @__PURE__ */ min(ordNumber);
  var validateCollateral = function(v) {
    if (v instanceof JitoSOL) {
      return new Right(unit);
    }
    ;
    if (v instanceof FeelsSOL) {
      return new Right(unit);
    }
    ;
    return new Left("Unsupported collateral type: " + show5(v));
  };
  var initFeelsSOL = function(oracle) {
    return function(entryFee) {
      return function(exitFee) {
        return function __do7() {
          var totalSupply = $$new(0)();
          var totalBacking = $$new(0)();
          var buffer = $$new(0)();
          var lastUpdate = $$new(0)();
          var cachedPrice = $$new(Nothing.value)();
          return {
            totalFeelsSOLSupply: totalSupply,
            totalJitoSOLBacking: totalBacking,
            jitoSOLBuffer: buffer,
            priceOracle: oracle,
            lastOracleUpdate: lastUpdate,
            cachedPrice,
            entryFee,
            exitFee,
            polAllocationRate: 0.25,
            bufferTargetRatio: 0.01
          };
        };
      };
    };
  };
  var getOraclePrice = function(state3) {
    return function __do7() {
      var cachedPrice = read(state3.cachedPrice)();
      var lastUpdate = read(state3.lastOracleUpdate)();
      var currentTime$prime = currentTime();
      if (cachedPrice instanceof Just && currentTime$prime - lastUpdate < 6e4) {
        return cachedPrice.value0;
      }
      ;
      var price = state3.priceOracle();
      var newPrice = {
        price,
        timestamp: currentTime$prime
      };
      write(new Just(newPrice))(state3.cachedPrice)();
      write(currentTime$prime)(state3.lastOracleUpdate)();
      return newPrice;
    };
  };
  var checkSolvency = function(state3) {
    return function __do7() {
      var totalFeelsSOL = read(state3.totalFeelsSOLSupply)();
      var totalJitoSOLBacking = read(state3.totalJitoSOLBacking)();
      var currentBuffer = read(state3.jitoSOLBuffer)();
      var oraclePrice = getOraclePrice(state3)();
      var totalJitoSOLAvailable = totalJitoSOLBacking + currentBuffer;
      var requiredJitoSOL = totalFeelsSOL / oraclePrice.price;
      var $28 = totalJitoSOLAvailable >= requiredJitoSOL;
      if ($28) {
        return true;
      }
      ;
      log("SOLVENCY ALERT: JitoSOL available (" + (show13(totalJitoSOLAvailable) + (") < Required JitoSOL (" + (show13(requiredJitoSOL) + ")"))))();
      return false;
    };
  };
  var mintFeelsSOL = function(state3) {
    return function(jitoSOLAmount) {
      var $29 = jitoSOLAmount <= 0;
      if ($29) {
        return pure13(new Left("Amount must be positive"));
      }
      ;
      var v = validateCollateral(JitoSOL.value);
      if (v instanceof Left) {
        return pure13(new Left(v.value0));
      }
      ;
      if (v instanceof Right) {
        return function __do7() {
          var oraclePrice = getOraclePrice(state3)();
          var feelsSOLAmount = jitoSOLAmount * oraclePrice.price;
          var currentSupply = read(state3.totalFeelsSOLSupply)();
          var currentBacking = read(state3.totalJitoSOLBacking)();
          var currentBuffer = read(state3.jitoSOLBuffer)();
          var bufferTarget = (currentBacking + jitoSOLAmount) * state3.bufferTargetRatio;
          var bufferAddition = function() {
            var $32 = bufferTarget > currentBuffer;
            if ($32) {
              return min8(jitoSOLAmount)(bufferTarget - currentBuffer);
            }
            ;
            return 0;
          }();
          var backingAddition = jitoSOLAmount - bufferAddition;
          write(currentSupply + feelsSOLAmount)(state3.totalFeelsSOLSupply)();
          write(currentBacking + backingAddition)(state3.totalJitoSOLBacking)();
          write(currentBuffer + bufferAddition)(state3.jitoSOLBuffer)();
          checkSolvency(state3)();
          var timestamp = currentTime();
          return new Right({
            feelsSOLMinted: feelsSOLAmount,
            jitoSOLLocked: jitoSOLAmount,
            exchangeRate: oraclePrice.price,
            fee: 0,
            timestamp
          });
        };
      }
      ;
      throw new Error("Failed pattern match at Protocol.FeelsSOL (line 118, column 7 - line 155, column 14): " + [v.constructor.name]);
    };
  };
  var enterSystem = function(state3) {
    return function(_user) {
      return function(jitoAmount) {
        var $34 = jitoAmount <= 0;
        if ($34) {
          return pure13(new Left(new InvalidAmountError(jitoAmount)));
        }
        ;
        var feeAmount = jitoAmount * state3.entryFee;
        var netJitoAmount = jitoAmount - feeAmount;
        return function __do7() {
          var mintResult = mintFeelsSOL(state3)(netJitoAmount)();
          if (mintResult instanceof Left) {
            return new Left(new InvalidCommandError(mintResult.value0));
          }
          ;
          if (mintResult instanceof Right) {
            return new Right({
              feelsSOLMinted: mintResult.value0.feelsSOLMinted,
              jitoSOLLocked: jitoAmount,
              exchangeRate: mintResult.value0.exchangeRate,
              fee: feeAmount,
              timestamp: mintResult.value0.timestamp
            });
          }
          ;
          throw new Error("Failed pattern match at Protocol.FeelsSOL (line 272, column 7 - line 281, column 14): " + [mintResult.constructor.name]);
        };
      };
    };
  };
  var burnFeelsSOL = function(state3) {
    return function(feelsSOLAmount) {
      var $38 = feelsSOLAmount <= 0;
      if ($38) {
        return pure13(new Left("Amount must be positive"));
      }
      ;
      return function __do7() {
        var currentSupply = read(state3.totalFeelsSOLSupply)();
        var $39 = feelsSOLAmount > currentSupply;
        if ($39) {
          return new Left("Insufficient FeelsSOL supply to burn");
        }
        ;
        var oraclePrice = getOraclePrice(state3)();
        var jitoSOLToRelease = feelsSOLAmount / oraclePrice.price;
        var currentBacking = read(state3.totalJitoSOLBacking)();
        var currentBuffer = read(state3.jitoSOLBuffer)();
        var totalAvailable = currentBacking + currentBuffer;
        var $40 = jitoSOLToRelease > totalAvailable;
        if ($40) {
          return new Left("Insufficient JitoSOL backing and buffer");
        }
        ;
        var fromBuffer = min8(jitoSOLToRelease)(currentBuffer);
        var fromBacking = jitoSOLToRelease - fromBuffer;
        write(currentSupply - feelsSOLAmount)(state3.totalFeelsSOLSupply)();
        write(currentBacking - fromBacking)(state3.totalJitoSOLBacking)();
        write(currentBuffer - fromBuffer)(state3.jitoSOLBuffer)();
        checkSolvency(state3)();
        var timestamp = currentTime();
        return new Right({
          feelsSOLBurned: feelsSOLAmount,
          jitoSOLReleased: jitoSOLToRelease,
          exchangeRate: oraclePrice.price,
          fee: 0,
          timestamp
        });
      };
    };
  };
  var exitSystem = function(state3) {
    return function(_user) {
      return function(feelsAmount) {
        var $41 = feelsAmount <= 0;
        if ($41) {
          return pure13(new Left(new InvalidAmountError(feelsAmount)));
        }
        ;
        return function __do7() {
          var burnResult = burnFeelsSOL(state3)(feelsAmount)();
          if (burnResult instanceof Left) {
            return new Left(new InvalidCommandError(burnResult.value0));
          }
          ;
          if (burnResult instanceof Right) {
            var feeAmount = burnResult.value0.jitoSOLReleased * state3.exitFee;
            return new Right({
              feelsSOLBurned: feelsAmount,
              jitoSOLReleased: burnResult.value0.jitoSOLReleased - feeAmount,
              exchangeRate: burnResult.value0.exchangeRate,
              fee: feeAmount,
              timestamp: burnResult.value0.timestamp
            });
          }
          ;
          throw new Error("Failed pattern match at Protocol.FeelsSOL (line 301, column 7 - line 313, column 14): " + [burnResult.constructor.name]);
        };
      };
    };
  };

  // output/Protocol.POL/index.js
  var min9 = /* @__PURE__ */ min(ordNumber);
  var fromFoldable5 = /* @__PURE__ */ fromFoldable2(foldableList);
  var foldl4 = /* @__PURE__ */ foldl(foldableArray);
  var append6 = /* @__PURE__ */ append(semigroupArray);
  var sum4 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var map28 = /* @__PURE__ */ map(functorArray);
  var sum1 = /* @__PURE__ */ sum(foldableList)(semiringNumber);
  var map110 = /* @__PURE__ */ map(functorList);
  var max8 = /* @__PURE__ */ max(ordNumber);
  var alter3 = /* @__PURE__ */ alter(ordString);
  var ManualAllocation = /* @__PURE__ */ function() {
    function ManualAllocation2() {
    }
    ;
    ManualAllocation2.value = new ManualAllocation2();
    return ManualAllocation2;
  }();
  var initPOL = function __do2() {
    var now = currentTime();
    return $$new({
      totalPOL: 1e4,
      permanentPOL: 1e4,
      borrowedPOL: 0,
      unallocated: 1e4,
      poolAllocations: empty2,
      allocationStrategy: ManualAllocation.value,
      contributionHistory: [{
        timestamp: now,
        amount: 1e4
      }]
    })();
  };
  var getUnallocatedPOL = function(polRef) {
    return function __do7() {
      var pol = read(polRef)();
      return pol.unallocated;
    };
  };
  var getTotalPOL = function(polRef) {
    return function __do7() {
      var pol = read(polRef)();
      return pol.totalPOL;
    };
  };
  var getPOLMetrics = function(polRef) {
    return function __do7() {
      var pol = read(polRef)();
      var allocations = fromFoldable5(values(pol.poolAllocations));
      var allocated = foldl4(function(acc) {
        return function(alloc) {
          return acc + alloc.allocated;
        };
      })(0)(allocations);
      var utilization = function() {
        var $55 = pol.totalPOL > 0;
        if ($55) {
          return allocated / pol.totalPOL;
        }
        ;
        return 0;
      }();
      return {
        totalValue: pol.totalPOL,
        utilizationRate: utilization,
        allocated,
        unallocated: pol.unallocated,
        poolCount: size3(pol.poolAllocations)
      };
    };
  };
  var getAllAllocations = function(polRef) {
    return function __do7() {
      var pol = read(polRef)();
      return fromFoldable5(values(pol.poolAllocations));
    };
  };
  var contribute = function(polRef) {
    return function(amount) {
      return function __do7() {
        var timestamp = currentTime();
        return modify_(function(pol) {
          var newHistory = append6(pol.contributionHistory)([{
            timestamp,
            amount
          }]);
          var trimmedHistory = function() {
            var $63 = length3(newHistory) > 100;
            if ($63) {
              return drop(length3(newHistory) - 100 | 0)(newHistory);
            }
            ;
            return newHistory;
          }();
          return {
            allocationStrategy: pol.allocationStrategy,
            borrowedPOL: pol.borrowedPOL,
            poolAllocations: pol.poolAllocations,
            totalPOL: pol.totalPOL + amount,
            permanentPOL: pol.permanentPOL + amount,
            unallocated: pol.unallocated + amount,
            contributionHistory: trimmedHistory
          };
        })(polRef)();
      };
    };
  };
  var calculateGrowthRate24h = function(polRef) {
    return function __do7() {
      var pol = read(polRef)();
      var now = currentTime();
      var dayAgo = now - 864e5;
      var recentContributions = filter(function(c) {
        return c.timestamp > dayAgo;
      })(pol.contributionHistory);
      var totalGrowth = sum4(map28(function(v) {
        return v.amount;
      })(recentContributions));
      var $64 = pol.totalPOL > 0;
      if ($64) {
        return totalGrowth / pol.totalPOL * 100;
      }
      ;
      return 0;
    };
  };
  var allocateToPool = function(polRef) {
    return function(poolId) {
      return function(amount) {
        var updateAllocation = function(v) {
          return function(v1) {
            return function(v2) {
              return function(v3) {
                if (v3 instanceof Nothing) {
                  return new Just({
                    poolId,
                    allocated: v,
                    permanentAllocated: v1,
                    borrowedAllocated: v2,
                    utilized: 0,
                    performance: 1,
                    deployedAmount: 0,
                    tickLower: 0,
                    tickUpper: 0,
                    lastDeployment: 0
                  });
                }
                ;
                if (v3 instanceof Just) {
                  return new Just({
                    deployedAmount: v3.value0.deployedAmount,
                    lastDeployment: v3.value0.lastDeployment,
                    performance: v3.value0.performance,
                    poolId: v3.value0.poolId,
                    tickLower: v3.value0.tickLower,
                    tickUpper: v3.value0.tickUpper,
                    utilized: v3.value0.utilized,
                    allocated: v3.value0.allocated + v,
                    permanentAllocated: v3.value0.permanentAllocated + v1,
                    borrowedAllocated: v3.value0.borrowedAllocated + v2
                  });
                }
                ;
                throw new Error("Failed pattern match at Protocol.POL (line 235, column 5 - line 246, column 8): " + [v.constructor.name, v1.constructor.name, v2.constructor.name, v3.constructor.name]);
              };
            };
          };
        };
        return function __do7() {
          var pol = read(polRef)();
          var $70 = amount > pol.unallocated;
          if ($70) {
            return false;
          }
          ;
          var allocatedPermanent = sum1(map110(function(v) {
            return v.permanentAllocated;
          })(values(pol.poolAllocations)));
          var unallocatedPermanent = pol.permanentPOL - allocatedPermanent;
          var permanentToAllocate = min9(amount)(max8(0)(unallocatedPermanent));
          var borrowedToAllocate = amount - permanentToAllocate;
          modify_(function(p2) {
            return {
              allocationStrategy: p2.allocationStrategy,
              borrowedPOL: p2.borrowedPOL,
              contributionHistory: p2.contributionHistory,
              permanentPOL: p2.permanentPOL,
              totalPOL: p2.totalPOL,
              unallocated: p2.unallocated - amount,
              poolAllocations: alter3(updateAllocation(amount)(permanentToAllocate)(borrowedToAllocate))(poolId)(p2.poolAllocations)
            };
          })(polRef)();
          return true;
        };
      };
    };
  };

  // output/UI.Account/index.js
  var pure14 = /* @__PURE__ */ pure(applicativeEffect);
  var fromFoldable6 = /* @__PURE__ */ fromFoldable2(foldableList);
  var bind15 = /* @__PURE__ */ bind(bindList);
  var lookup6 = /* @__PURE__ */ lookup(ordTokenType);
  var pure15 = /* @__PURE__ */ pure(applicativeList);
  var mempty2 = /* @__PURE__ */ mempty(monoidList);
  var sum5 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var lookup13 = /* @__PURE__ */ lookup(ordString);
  var insert6 = /* @__PURE__ */ insert(ordString);
  var show6 = /* @__PURE__ */ show(showNumber);
  var $$delete5 = /* @__PURE__ */ $$delete(ordTokenType);
  var insert13 = /* @__PURE__ */ insert(ordTokenType);
  var initAccountRegistry = function __do3() {
    var feelsAccounts = $$new(empty2)();
    var chainAccounts = $$new(empty2)();
    return {
      feelsAccounts,
      chainAccounts
    };
  };
  var getTotalTokenBalance = function(registry) {
    return function(token) {
      return function __do7() {
        var accounts = read(registry.feelsAccounts)();
        var allBalances = fromFoldable6(bind15(values(accounts))(function(account) {
          var v = lookup6(token)(account.balances);
          if (v instanceof Just) {
            return pure15(v.value0);
          }
          ;
          if (v instanceof Nothing) {
            return mempty2;
          }
          ;
          throw new Error("Failed pattern match at UI.Account (line 141, column 9 - line 143, column 28): " + [v.constructor.name]);
        }));
        return sum5(allBalances);
      };
    };
  };
  var getOrCreateFeelsAccount = function(registry) {
    return function(owner) {
      return function __do7() {
        var accounts = read(registry.feelsAccounts)();
        var v = lookup13(owner)(accounts);
        if (v instanceof Just) {
          return v.value0;
        }
        ;
        if (v instanceof Nothing) {
          var newAccount = {
            owner,
            balances: empty2
          };
          modify_(insert6(owner)(newAccount))(registry.feelsAccounts)();
          return newAccount;
        }
        ;
        throw new Error("Failed pattern match at UI.Account (line 82, column 3 - line 90, column 22): " + [v.constructor.name]);
      };
    };
  };
  var updateFeelsAccountBalance = function(registry) {
    return function(owner) {
      return function(token) {
        return function(newBalance) {
          return function __do7() {
            var account = getOrCreateFeelsAccount(registry)(owner)();
            var $36 = newBalance < 0;
            if ($36) {
              return new Left("Balance cannot be negative: " + show6(newBalance));
            }
            ;
            var updatedBalances = function() {
              var $37 = newBalance === 0;
              if ($37) {
                return $$delete5(token)(account.balances);
              }
              ;
              return insert13(token)(newBalance)(account.balances);
            }();
            var updatedAccount = {
              owner: account.owner,
              balances: updatedBalances
            };
            modify_(insert6(owner)(updatedAccount))(registry.feelsAccounts)();
            return new Right(unit);
          };
        };
      };
    };
  };
  var getOrCreateChainAccount = function(registry) {
    return function(owner) {
      return function __do7() {
        var accounts = read(registry.chainAccounts)();
        var v = lookup13(owner)(accounts);
        if (v instanceof Just) {
          return v.value0;
        }
        ;
        if (v instanceof Nothing) {
          var newAccount = {
            owner,
            jitoSOLBalance: 0
          };
          modify_(insert6(owner)(newAccount))(registry.chainAccounts)();
          return newAccount;
        }
        ;
        throw new Error("Failed pattern match at UI.Account (line 154, column 3 - line 159, column 22): " + [v.constructor.name]);
      };
    };
  };
  var updateChainAccountBalance = function(registry) {
    return function(owner) {
      return function(delta) {
        return function __do7() {
          var account = getOrCreateChainAccount(registry)(owner)();
          var newBalance = account.jitoSOLBalance + delta;
          var $40 = newBalance < 0;
          if ($40) {
            return new Left("Insufficient JitoSOL balance: " + (owner + (" has " + show6(account.jitoSOLBalance))));
          }
          ;
          var updatedAccount = {
            owner: account.owner,
            jitoSOLBalance: newBalance
          };
          modify_(insert6(owner)(updatedAccount))(registry.chainAccounts)();
          return new Right(unit);
        };
      };
    };
  };
  var getFeelsAccountBalance = function(registry) {
    return function(owner) {
      return function(token) {
        return function __do7() {
          var account = getOrCreateFeelsAccount(registry)(owner)();
          return fromMaybe(0)(lookup6(token)(account.balances));
        };
      };
    };
  };
  var withdrawToChain = function(registry) {
    return function(owner) {
      return function(amount) {
        var $43 = amount <= 0;
        if ($43) {
          return pure14(new Left("Withdrawal amount must be positive"));
        }
        ;
        return function __do7() {
          var feelsBalance = getFeelsAccountBalance(registry)(owner)(FeelsSOL.value)();
          var $44 = feelsBalance < amount;
          if ($44) {
            return new Left("Insufficient FeelsSOL balance: " + (owner + (" has " + show6(feelsBalance))));
          }
          ;
          updateFeelsAccountBalance(registry)(owner)(FeelsSOL.value)(feelsBalance - amount)();
          return updateChainAccountBalance(registry)(owner)(amount)();
        };
      };
    };
  };
  var getChainAccountBalance = function(registry) {
    return function(owner) {
      return function __do7() {
        var account = getOrCreateChainAccount(registry)(owner)();
        return account.jitoSOLBalance;
      };
    };
  };
  var depositFromChain = function(registry) {
    return function(owner) {
      return function(amount) {
        var $48 = amount <= 0;
        if ($48) {
          return pure14(new Left("Deposit amount must be positive"));
        }
        ;
        return function __do7() {
          var deductResult = updateChainAccountBalance(registry)(owner)(-amount)();
          if (deductResult instanceof Left) {
            return new Left(deductResult.value0);
          }
          ;
          if (deductResult instanceof Right) {
            var currentBalance = getFeelsAccountBalance(registry)(owner)(FeelsSOL.value)();
            return updateFeelsAccountBalance(registry)(owner)(FeelsSOL.value)(currentBalance + amount)();
          }
          ;
          throw new Error("Failed pattern match at UI.Account (line 193, column 7 - line 198, column 86): " + [deductResult.constructor.name]);
        };
      };
    };
  };

  // output/Simulation.Engine/index.js
  var pure16 = /* @__PURE__ */ pure(applicativeEffect);
  var initSimulationWithPoolRegistry = function(config) {
    return function(existingPoolRegistry) {
      return function(oracle) {
        return function __do7() {
          var accounts = generateAccounts({
            numAccounts: config.numAccounts,
            accountProfiles: config.accountProfiles
          })();
          var pol = initPOL();
          var priceOracle = pure16(config.initialJitoSOLPrice);
          var accountRegistry = initAccountRegistry();
          var feelsSOL = initFeelsSOL(priceOracle)(1e-3)(2e-3)();
          return {
            accounts,
            poolRegistry: existingPoolRegistry,
            feelsSOL,
            oracle,
            currentBlock: 0,
            currentPrice: config.initialJitoSOLPrice,
            priceHistory: [],
            actionHistory: [],
            nextPositionId: 1,
            polAllocationHistory: []
          };
        };
      };
    };
  };

  // output/Protocol.Pool/index.js
  var min10 = /* @__PURE__ */ min(ordNumber);
  var max9 = /* @__PURE__ */ max(ordNumber);
  var map29 = /* @__PURE__ */ map(functorArray);
  var append7 = /* @__PURE__ */ append(semigroupArray);
  var compare4 = /* @__PURE__ */ compare(ordNumber);
  var sum6 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var abs3 = /* @__PURE__ */ abs(ordNumber)(ringNumber);
  var abs1 = /* @__PURE__ */ abs(ordInt)(ringInt);
  var eq12 = /* @__PURE__ */ eq(eqLeverage);
  var updateLeverageValues = function(state3) {
    return function(newValues) {
      var updateGroup = function(g) {
        return function(v) {
          return {
            leverage: g.leverage,
            shares: g.shares,
            value: v.value
          };
        };
      };
      return {
        totalValue: state3.totalValue,
        leverageGroups: zipWith(updateGroup)(state3.leverageGroups)(newValues.values)
      };
    };
  };
  var priceToTick = function(price) {
    return floor3(log2(price) / log2(1.0001));
  };
  var sqrtPriceToTick = function(sqrtPrice) {
    return priceToTick(sqrtPrice * sqrtPrice);
  };
  var priceToSqrtPriceX96 = function(price) {
    return sqrt(price) * pow(2)(96);
  };
  var initializeLeverageState = {
    totalValue: 0,
    leverageGroups: [{
      leverage: 1,
      value: 0,
      shares: 0
    }, {
      leverage: 3,
      value: 0,
      shares: 0
    }]
  };
  var initializePool = function(token0) {
    return function(token1) {
      return function(initialPrice) {
        return function(config) {
          return {
            token0,
            token1,
            sqrtPriceX96: priceToSqrtPriceX96(initialPrice),
            liquidity: 0,
            tick: priceToTick(initialPrice),
            feeGrowthGlobal0X128: 0,
            feeGrowthGlobal1X128: 0,
            protocolFee: config.fee,
            unlocked: true,
            offering: Nothing.value,
            leverageState: initializeLeverageState,
            totalValue: 0,
            lastUpdateBlock: 0
          };
        };
      };
    };
  };
  var distributePoolPnL = function(position2) {
    return function(oldState) {
      return function(newValues) {
        var posLeverage = leverageMultiplier(position2.leverage);
        var valueGroup = find2(function(v) {
          return v.leverage === posLeverage;
        })(newValues.values);
        var leverageGroup = find2(function(g) {
          return g.leverage === posLeverage;
        })(oldState.leverageGroups);
        var perShareValue = function() {
          var $50 = {
            lg: leverageGroup,
            vg: valueGroup
          };
          if ($50.lg instanceof Just && $50.vg instanceof Just) {
            var $51 = $50.lg.value0.shares > 0;
            if ($51) {
              return $50.vg.value0.value / $50.lg.value0.shares;
            }
            ;
            return 0;
          }
          ;
          return 0;
        }();
        var newValue = position2.shares * perShareValue;
        return {
          id: position2.id,
          owner: position2.owner,
          amount: position2.amount,
          price: position2.price,
          duration: position2.duration,
          leverage: position2.leverage,
          rollover: position2.rollover,
          shares: position2.shares,
          createdAt: position2.createdAt,
          lockedAmount: position2.lockedAmount,
          accumulatedYield: position2.accumulatedYield,
          lastYieldClaim: position2.lastYieldClaim,
          feeGrowthInside0: position2.feeGrowthInside0,
          feeGrowthInside1: position2.feeGrowthInside1,
          value: newValue
        };
      };
    };
  };
  var calculatePositionYield = function(position2) {
    return function(pool) {
      return function(currentBlock) {
        var leverageBonus = leverageMultiplier(position2.leverage);
        var feeGrowthDelta1 = pool.feeGrowthGlobal1X128 - position2.feeGrowthInside1;
        var feeGrowthDelta0 = pool.feeGrowthGlobal0X128 - position2.feeGrowthInside0;
        var yieldFromFees = position2.shares * (feeGrowthDelta0 + feeGrowthDelta1) / 2;
        var durationMultiplier = function() {
          if (position2.duration instanceof Spot) {
            return 1;
          }
          ;
          if (position2.duration instanceof Monthly) {
            return 1.2;
          }
          ;
          throw new Error("Failed pattern match at Protocol.Pool (line 516, column 26 - line 518, column 21): " + [position2.duration.constructor.name]);
        }();
        var totalYield = yieldFromFees * leverageBonus * durationMultiplier;
        var blocksElapsed = toNumber2(currentBlock - position2.lastYieldClaim | 0);
        return max9(0)(totalYield);
      };
    };
  };
  var updatePositionYield = function(position2) {
    return function(pool) {
      return function(currentBlock) {
        var newYield = calculatePositionYield(position2)(pool)(currentBlock);
        var updatedPosition = {
          amount: position2.amount,
          createdAt: position2.createdAt,
          duration: position2.duration,
          id: position2.id,
          leverage: position2.leverage,
          lockedAmount: position2.lockedAmount,
          owner: position2.owner,
          price: position2.price,
          rollover: position2.rollover,
          shares: position2.shares,
          accumulatedYield: position2.accumulatedYield + newYield,
          value: position2.value + newYield,
          lastYieldClaim: currentBlock,
          feeGrowthInside0: pool.feeGrowthGlobal0X128,
          feeGrowthInside1: pool.feeGrowthGlobal1X128
        };
        return updatedPosition;
      };
    };
  };
  var syncPositionValue = function(position2) {
    return function(pool) {
      return function(currentBlock) {
        var positionWithYield = updatePositionYield(position2)(pool)(currentBlock);
        var newLeverageValues = {
          values: map29(function(g) {
            return {
              leverage: g.leverage,
              value: g.value
            };
          })(pool.leverageState.leverageGroups)
        };
        var updatedPosition = distributePoolPnL(positionWithYield)(pool.leverageState)(newLeverageValues);
        return updatedPosition;
      };
    };
  };
  var calculateLeverageValues = function(initialValue) {
    return function(currentValue) {
      return function(leverageState) {
        var distributeToGroups = function($copy_v) {
          return function($copy_v1) {
            return function($copy_v2) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, v2) {
                if (v1.length === 0) {
                  $tco_done = true;
                  return v2;
                }
                ;
                if (v === 0) {
                  $tco_done = true;
                  return append7(v2)(v1);
                }
                ;
                var v3 = uncons(v1);
                if (v3 instanceof Nothing) {
                  $tco_done = true;
                  return v2;
                }
                ;
                if (v3 instanceof Just) {
                  var maxLoss = v3.value0.head.value * 0.9;
                  var actualLoss = min10(v)(maxLoss);
                  var newValue = max9(v3.value0.head.value * 0.1)(v3.value0.head.value - actualLoss);
                  var remainingLoss = v - actualLoss;
                  $tco_var_v = remainingLoss;
                  $tco_var_v1 = v3.value0.tail;
                  $copy_v2 = append7(v2)([{
                    leverage: v3.value0.head.leverage,
                    shares: v3.value0.head.shares,
                    value: newValue
                  }]);
                  return;
                }
                ;
                throw new Error("Failed pattern match at Protocol.Pool (line 795, column 42 - line 802, column 81): " + [v3.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
              }
              ;
              return $tco_result;
            };
          };
        };
        var distributeLosses = function(v) {
          return function(v1) {
            if (v1.length === 0) {
              return {
                values: []
              };
            }
            ;
            var sorted = sortBy(function(a2) {
              return function(b2) {
                return compare4(b2.leverage)(a2.leverage);
              };
            })(v1);
            var distributed = distributeToGroups(v)(sorted)([]);
            return {
              values: map29(function(g) {
                return {
                  leverage: g.leverage,
                  value: g.value
                };
              })(distributed)
            };
          };
        };
        var pnl = currentValue - initialValue;
        var result = function() {
          var $66 = pnl >= 0;
          if ($66) {
            var totalWeightedValue = sum6(map29(function(g) {
              return g.value * g.leverage;
            })(leverageState.leverageGroups));
            var $67 = totalWeightedValue > 0;
            if ($67) {
              return {
                values: map29(function(g) {
                  return {
                    leverage: g.leverage,
                    value: g.value + pnl * (g.value * g.leverage / totalWeightedValue)
                  };
                })(leverageState.leverageGroups)
              };
            }
            ;
            return {
              values: map29(function(g) {
                return {
                  leverage: g.leverage,
                  value: g.value
                };
              })(leverageState.leverageGroups)
            };
          }
          ;
          return distributeLosses(abs3(pnl))(leverageState.leverageGroups);
        }();
        return result;
      };
    };
  };
  var updatePoolValue = function(pool) {
    return function(currentBlock) {
      var currentSqrtPrice = pool.sqrtPriceX96 / pow(2)(48);
      var reserve0 = pool.liquidity / currentSqrtPrice;
      var reserve1 = pool.liquidity * currentSqrtPrice;
      var currentValue = reserve0 + reserve1;
      var hasValueChange = currentValue !== pool.totalValue && pool.totalValue > 0;
      var newLeverageState = function() {
        if (hasValueChange) {
          var leverageValues = calculateLeverageValues(pool.totalValue)(currentValue)(pool.leverageState);
          return updateLeverageValues(pool.leverageState)(leverageValues);
        }
        ;
        return pool.leverageState;
      }();
      return {
        token0: pool.token0,
        token1: pool.token1,
        sqrtPriceX96: pool.sqrtPriceX96,
        liquidity: pool.liquidity,
        tick: pool.tick,
        feeGrowthGlobal0X128: pool.feeGrowthGlobal0X128,
        feeGrowthGlobal1X128: pool.feeGrowthGlobal1X128,
        protocolFee: pool.protocolFee,
        unlocked: pool.unlocked,
        offering: pool.offering,
        totalValue: currentValue,
        leverageState: newLeverageState,
        lastUpdateBlock: currentBlock
      };
    };
  };
  var swap = function(pool) {
    return function(params) {
      return function(currentBlock) {
        var feeRate = pool.protocolFee / 1e4;
        var feeAmount = abs3(params.amountSpecified) * feeRate;
        var currentSqrtPrice = pool.sqrtPriceX96 / pow(2)(48);
        var reserve0 = pool.liquidity / currentSqrtPrice;
        var reserve1 = pool.liquidity * currentSqrtPrice;
        var amountInAfterFee = abs3(params.amountSpecified) * (1 - feeRate);
        var swapResult = function() {
          if (params.zeroForOne) {
            var out = reserve1 * amountInAfterFee / (reserve0 + amountInAfterFee);
            return {
              amountOut: out,
              newReserve0: reserve0 + amountInAfterFee,
              newReserve1: reserve1 - out
            };
          }
          ;
          var out = reserve0 * amountInAfterFee / (reserve1 + amountInAfterFee);
          return {
            amountOut: out,
            newReserve0: reserve0 - out,
            newReserve1: reserve1 + amountInAfterFee
          };
        }();
        var finalAmount0 = function() {
          if (params.zeroForOne) {
            return params.amountSpecified;
          }
          ;
          return -swapResult.amountOut;
        }();
        var finalAmount1 = function() {
          if (params.zeroForOne) {
            return -swapResult.amountOut;
          }
          ;
          return params.amountSpecified;
        }();
        var newSqrtPrice = sqrt(swapResult.newReserve1 / swapResult.newReserve0);
        var newSqrtPriceX96 = newSqrtPrice * pow(2)(48);
        var newTick = sqrtPriceToTick(newSqrtPriceX96);
        var priceInBounds = function() {
          var $72 = params.sqrtPriceLimitX96 === 0;
          if ($72) {
            return true;
          }
          ;
          if (params.zeroForOne) {
            return newSqrtPriceX96 >= params.sqrtPriceLimitX96;
          }
          ;
          return newSqrtPriceX96 <= params.sqrtPriceLimitX96;
        }();
        var finalResult = function() {
          if (priceInBounds) {
            return {
              amount0: finalAmount0,
              amount1: finalAmount1,
              sqrtPriceX96: newSqrtPriceX96,
              liquidity: pool.liquidity,
              tick: newTick,
              gasUsed: 15e4
            };
          }
          ;
          return {
            amount0: 0,
            amount1: 0,
            sqrtPriceX96: pool.sqrtPriceX96,
            liquidity: pool.liquidity,
            tick: pool.tick,
            gasUsed: 5e4
          };
        }();
        var updatedPool = function() {
          if (priceInBounds) {
            return {
              lastUpdateBlock: pool.lastUpdateBlock,
              leverageState: pool.leverageState,
              liquidity: pool.liquidity,
              offering: pool.offering,
              protocolFee: pool.protocolFee,
              token0: pool.token0,
              token1: pool.token1,
              totalValue: pool.totalValue,
              unlocked: pool.unlocked,
              sqrtPriceX96: newSqrtPriceX96,
              tick: newTick,
              feeGrowthGlobal0X128: pool.feeGrowthGlobal0X128 + feeAmount,
              feeGrowthGlobal1X128: pool.feeGrowthGlobal1X128 + feeAmount
            };
          }
          ;
          return pool;
        }();
        var finalPool = updatePoolValue(updatedPool)(currentBlock);
        return {
          result: finalResult,
          updatedPool: finalPool
        };
      };
    };
  };
  var calculateAmount1ForLiquidity = function(sqrtPrice) {
    return function(tickLower) {
      return function(tickUpper) {
        return function(liquidity) {
          var sqrtPriceUpper = sqrt(pow(1.0001)(toNumber2(tickUpper)));
          var sqrtPriceLower = sqrt(pow(1.0001)(toNumber2(tickLower)));
          var priceRange = function() {
            var $76 = sqrtPrice < sqrtPriceLower;
            if ($76) {
              return {
                sqrtPriceA: sqrtPriceLower,
                sqrtPriceB: sqrtPriceLower
              };
            }
            ;
            var $77 = sqrtPrice > sqrtPriceUpper;
            if ($77) {
              return {
                sqrtPriceA: sqrtPriceLower,
                sqrtPriceB: sqrtPriceUpper
              };
            }
            ;
            return {
              sqrtPriceA: sqrtPriceLower,
              sqrtPriceB: sqrtPrice
            };
          }();
          var amount = function() {
            var $78 = priceRange.sqrtPriceA === priceRange.sqrtPriceB;
            if ($78) {
              return 0;
            }
            ;
            return liquidity * abs3(priceRange.sqrtPriceB - priceRange.sqrtPriceA);
          }();
          return amount;
        };
      };
    };
  };
  var calculateAmount0ForLiquidity = function(sqrtPrice) {
    return function(tickLower) {
      return function(tickUpper) {
        return function(liquidity) {
          var sqrtPriceUpper = sqrt(pow(1.0001)(toNumber2(tickUpper)));
          var sqrtPriceLower = sqrt(pow(1.0001)(toNumber2(tickLower)));
          var priceRange = function() {
            var $79 = sqrtPrice < sqrtPriceLower;
            if ($79) {
              return {
                sqrtPriceA: sqrtPriceLower,
                sqrtPriceB: sqrtPriceUpper
              };
            }
            ;
            var $80 = sqrtPrice > sqrtPriceUpper;
            if ($80) {
              return {
                sqrtPriceA: sqrtPriceUpper,
                sqrtPriceB: sqrtPriceUpper
              };
            }
            ;
            return {
              sqrtPriceA: sqrtPrice,
              sqrtPriceB: sqrtPriceUpper
            };
          }();
          var amount = function() {
            var $81 = priceRange.sqrtPriceA === priceRange.sqrtPriceB;
            if ($81) {
              return 0;
            }
            ;
            return liquidity * abs3(1 / priceRange.sqrtPriceA - 1 / priceRange.sqrtPriceB);
          }();
          return amount;
        };
      };
    };
  };
  var addLiquidity = function(pool) {
    return function(params) {
      return function(leverage) {
        return function(currentBlock) {
          var positionId = abs1((params.tickLower * 1e3 | 0) + params.tickUpper | 0);
          var leverageMultiplier$prime = leverageMultiplier(leverage);
          var shares = params.amount * leverageMultiplier$prime;
          var leverageIdx = function() {
            var $82 = eq12(leverage)(Senior.value);
            if ($82) {
              return 0;
            }
            ;
            return 1;
          }();
          var updatedLeverageGroups = function() {
            var updateGroup = function(idx) {
              return function(group4) {
                var $83 = idx === leverageIdx;
                if ($83) {
                  return {
                    leverage: group4.leverage,
                    value: group4.value + params.amount,
                    shares: group4.shares + shares
                  };
                }
                ;
                return group4;
              };
            };
            return mapWithIndex2(updateGroup)(pool.leverageState.leverageGroups);
          }();
          var updatedPool = {
            feeGrowthGlobal0X128: pool.feeGrowthGlobal0X128,
            feeGrowthGlobal1X128: pool.feeGrowthGlobal1X128,
            offering: pool.offering,
            protocolFee: pool.protocolFee,
            sqrtPriceX96: pool.sqrtPriceX96,
            tick: pool.tick,
            token0: pool.token0,
            token1: pool.token1,
            unlocked: pool.unlocked,
            liquidity: pool.liquidity + params.amount,
            leverageState: {
              leverageGroups: updatedLeverageGroups,
              totalValue: pool.leverageState.totalValue + params.amount
            },
            totalValue: pool.totalValue + params.amount,
            lastUpdateBlock: currentBlock
          };
          var amount1 = calculateAmount1ForLiquidity(pool.sqrtPriceX96)(params.tickLower)(params.tickUpper)(params.amount);
          var amount0 = calculateAmount0ForLiquidity(pool.sqrtPriceX96)(params.tickLower)(params.tickUpper)(params.amount);
          var result = {
            positionId,
            liquidity: params.amount,
            amount0,
            amount1
          };
          return {
            result,
            updatedPool
          };
        };
      };
    };
  };

  // output/Protocol.Offering/index.js
  var abs4 = /* @__PURE__ */ abs(ordNumber)(ringNumber);
  var pure17 = /* @__PURE__ */ pure(applicativeEffect);
  var sum7 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var map30 = /* @__PURE__ */ map(functorArray);
  var show7 = /* @__PURE__ */ show(showNumber);
  var find3 = /* @__PURE__ */ find(foldableArray);
  var MonthlyPhase = /* @__PURE__ */ function() {
    function MonthlyPhase2() {
    }
    ;
    MonthlyPhase2.value = new MonthlyPhase2();
    return MonthlyPhase2;
  }();
  var SpotPhase = /* @__PURE__ */ function() {
    function SpotPhase2() {
    }
    ;
    SpotPhase2.value = new SpotPhase2();
    return SpotPhase2;
  }();
  var Completed = /* @__PURE__ */ function() {
    function Completed2() {
    }
    ;
    Completed2.value = new Completed2();
    return Completed2;
  }();
  var showOfferingPhase = {
    show: function(v) {
      if (v instanceof MonthlyPhase) {
        return "Monthly";
      }
      ;
      if (v instanceof SpotPhase) {
        return "Spot";
      }
      ;
      if (v instanceof Completed) {
        return "Completed";
      }
      ;
      throw new Error("Failed pattern match at Protocol.Offering (line 73, column 1 - line 76, column 31): " + [v.constructor.name]);
    }
  };
  var initOffering = function(config) {
    var phaseTotal = sum7(map30(function(v) {
      return v.tokenAmount;
    })(config.phases));
    var $33 = abs4(phaseTotal - config.totalTokens) > 0.01;
    if ($33) {
      return pure17(new Left(new InvalidCommandError("Phase token amounts (" + (show7(phaseTotal) + (") don't match total tokens (" + (show7(config.totalTokens) + ")"))))));
    }
    ;
    return function __do7() {
      var offeringState = $$new({
        config,
        currentPhase: MonthlyPhase.value,
        tokensRemaining: 0,
        totalRaised: 0,
        phaseStartBlock: 0,
        isActive: false
      })();
      return new Right(offeringState);
    };
  };
  var getOfferingStatus = function(offeringRef) {
    return function __do7() {
      var offering = read(offeringRef)();
      var currentPrice = function() {
        if (offering.currentPhase instanceof MonthlyPhase) {
          return 0.15;
        }
        ;
        if (offering.currentPhase instanceof SpotPhase) {
          return 0.3;
        }
        ;
        if (offering.currentPhase instanceof Completed) {
          return 0.4;
        }
        ;
        throw new Error("Failed pattern match at Protocol.Offering (line 275, column 22 - line 278, column 26): " + [offering.currentPhase.constructor.name]);
      }();
      return {
        phase: offering.currentPhase,
        tokensRemaining: offering.tokensRemaining,
        currentPrice,
        totalRaised: offering.totalRaised
      };
    };
  };
  var eqOfferingPhase = {
    eq: function(x) {
      return function(y) {
        if (x instanceof MonthlyPhase && y instanceof MonthlyPhase) {
          return true;
        }
        ;
        if (x instanceof SpotPhase && y instanceof SpotPhase) {
          return true;
        }
        ;
        if (x instanceof Completed && y instanceof Completed) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eq4 = /* @__PURE__ */ eq(eqOfferingPhase);
  var findPhaseConfig = function(config) {
    return function(phase) {
      return find3(function(p2) {
        return eq4(p2.phase)(phase);
      })(config.phases);
    };
  };
  var startPhase = function(offeringRef) {
    return function(pool) {
      return function(currentBlock) {
        return function __do7() {
          var offering = read(offeringRef)();
          if (offering.isActive) {
            return new Left(new InvalidCommandError("Phase already active"));
          }
          ;
          var v = findPhaseConfig(offering.config)(offering.currentPhase);
          if (v instanceof Nothing) {
            return new Left(new InvalidCommandError("Invalid phase"));
          }
          ;
          if (v instanceof Just) {
            var liquidityParams = {
              tickLower: v.value0.tickLower,
              tickUpper: v.value0.tickUpper,
              amount: v.value0.tokenAmount,
              recipient: "offering-contract"
            };
            var liquidityResult = addLiquidity(pool)(liquidityParams);
            modify_(function(s) {
              return {
                config: s.config,
                currentPhase: s.currentPhase,
                totalRaised: s.totalRaised,
                isActive: true,
                tokensRemaining: v.value0.tokenAmount,
                phaseStartBlock: currentBlock
              };
            })(offeringRef)();
            return new Right(unit);
          }
          ;
          throw new Error("Failed pattern match at Protocol.Offering (line 192, column 7 - line 214, column 28): " + [v.constructor.name]);
        };
      };
    };
  };
  var completeOffering = function(offeringRef) {
    return function __do7() {
      var offering = read(offeringRef)();
      var $42 = !offering.isActive;
      if ($42) {
        return new Left(new InvalidCommandError("No active phase"));
      }
      ;
      var nextPhase = function() {
        if (offering.currentPhase instanceof MonthlyPhase) {
          return SpotPhase.value;
        }
        ;
        if (offering.currentPhase instanceof SpotPhase) {
          return Completed.value;
        }
        ;
        if (offering.currentPhase instanceof Completed) {
          return Completed.value;
        }
        ;
        throw new Error("Failed pattern match at Protocol.Offering (line 234, column 23 - line 237, column 35): " + [offering.currentPhase.constructor.name]);
      }();
      modify_(function(s) {
        return {
          config: s.config,
          phaseStartBlock: s.phaseStartBlock,
          totalRaised: s.totalRaised,
          currentPhase: nextPhase,
          isActive: false,
          tokensRemaining: 0
        };
      })(offeringRef)();
      return new Right(nextPhase);
    };
  };
  var checkPhaseComplete = function(offeringRef) {
    return function(_pool) {
      return function __do7() {
        var offering = read(offeringRef)();
        return offering.isActive && offering.tokensRemaining <= 0.01;
      };
    };
  };

  // output/UI.Action.AccountActions/index.js
  var pure18 = /* @__PURE__ */ pure(applicativeEffect);
  var show8 = /* @__PURE__ */ show(showNumber);
  var transfer = function(from2) {
    return function(to) {
      return function(token) {
        return function(amount) {
          return function(state3) {
            var $8 = amount <= 0;
            if ($8) {
              return pure18(new Left(new InvalidAmountError(amount)));
            }
            ;
            return function __do7() {
              var fromBalance = getFeelsAccountBalance(state3.accounts)(from2)(token)();
              var $9 = fromBalance < amount;
              if ($9) {
                return new Left(new InsufficientBalanceError("Required: " + (show8(amount) + (", Available: " + show8(fromBalance)))));
              }
              ;
              updateFeelsAccountBalance(state3.accounts)(from2)(token)(fromBalance - amount)();
              var toBalance = getFeelsAccountBalance(state3.accounts)(to)(token)();
              updateFeelsAccountBalance(state3.accounts)(to)(token)(toBalance + amount)();
              return new Right(unit);
            };
          };
        };
      };
    };
  };
  var transferTokens = transfer;

  // output/UI.Action.FeelsSOLActions/index.js
  var show9 = /* @__PURE__ */ show(showNumber);
  var exitFeelsSOL = function(user) {
    return function(feelsAmount) {
      return function(state3) {
        return function __do7() {
          var feelsBalance = getFeelsAccountBalance(state3.accounts)(user)(FeelsSOL.value)();
          var $8 = feelsBalance < feelsAmount;
          if ($8) {
            return new Left(new InsufficientBalanceError("Insufficient FeelsSOL balance. Required: " + (show9(feelsAmount) + (", Available: " + show9(feelsBalance)))));
          }
          ;
          var result = exitSystem(state3.feelsSOL)(user)(feelsAmount)();
          if (result instanceof Left) {
            return new Left(result.value0);
          }
          ;
          if (result instanceof Right) {
            var withdrawResult = withdrawToChain(state3.accounts)(user)(result.value0.jitoSOLReleased)();
            if (withdrawResult instanceof Left) {
              return new Left(new InvalidCommandError(withdrawResult.value0));
            }
            ;
            if (withdrawResult instanceof Right) {
              var polContribution = result.value0.fee * state3.feelsSOL.polAllocationRate;
              contribute(state3.polState)(polContribution)();
              return new Right({
                user,
                jitoSOLReceived: result.value0.jitoSOLReleased
              });
            }
            ;
            throw new Error("Failed pattern match at UI.Action.FeelsSOLActions (line 66, column 11 - line 73, column 79): " + [withdrawResult.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at UI.Action.FeelsSOLActions (line 61, column 7 - line 73, column 79): " + [result.constructor.name]);
        };
      };
    };
  };
  var enterFeelsSOL = function(user) {
    return function(jitoAmount) {
      return function(state3) {
        return function __do7() {
          var jitoBalance = getChainAccountBalance(state3.accounts)(user)();
          var $15 = jitoBalance < jitoAmount;
          if ($15) {
            return new Left(new InsufficientBalanceError("Insufficient JitoSOL balance. Required: " + (show9(jitoAmount) + (", Available: " + show9(jitoBalance)))));
          }
          ;
          var result = enterSystem(state3.feelsSOL)(user)(jitoAmount)();
          if (result instanceof Left) {
            return new Left(result.value0);
          }
          ;
          if (result instanceof Right) {
            var depositResult = depositFromChain(state3.accounts)(user)(jitoAmount)();
            if (depositResult instanceof Left) {
              return new Left(new InvalidCommandError(depositResult.value0));
            }
            ;
            if (depositResult instanceof Right) {
              var polContribution = result.value0.fee * state3.feelsSOL.polAllocationRate;
              contribute(state3.polState)(polContribution)();
              return new Right({
                user,
                feelsSOLMinted: result.value0.feelsSOLMinted
              });
            }
            ;
            throw new Error("Failed pattern match at UI.Action.FeelsSOLActions (line 41, column 11 - line 48, column 77): " + [depositResult.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at UI.Action.FeelsSOLActions (line 36, column 7 - line 48, column 77): " + [result.constructor.name]);
        };
      };
    };
  };

  // output/UI.Action.PositionActions/index.js
  var pure19 = /* @__PURE__ */ pure(applicativeEffect);
  var show10 = /* @__PURE__ */ show(showInt);
  var isSpotPosition = isSpot;
  var createPosition2 = function(user) {
    return function(_lendAsset) {
      return function(amount) {
        return function(_collateralAsset) {
          return function(_collateralAmount) {
            return function(duration2) {
              return function(leverage) {
                return function(rollover) {
                  return function(targetToken) {
                    return function(state3) {
                      var $18 = amount <= 0;
                      if ($18) {
                        return pure19(new Left(new InvalidAmountError(amount)));
                      }
                      ;
                      return function __do7() {
                        var nextId = getNextPositionId(state3.poolRegistry)();
                        var shares = amount * leverageMultiplier(leverage);
                        var position2 = createPosition(nextId)(user)(amount)(1)(duration2)(leverage)(rollover)(shares)(state3.currentBlock);
                        addPosition(position2)(state3.poolRegistry)();
                        var newMapping = function() {
                          if (targetToken instanceof Just) {
                            return cons({
                              positionId: position2.id,
                              tokenTicker: targetToken.value0
                            })(state3.positionTokenMap);
                          }
                          ;
                          if (targetToken instanceof Nothing) {
                            return state3.positionTokenMap;
                          }
                          ;
                          throw new Error("Failed pattern match at UI.Action.PositionActions (line 87, column 24 - line 89, column 46): " + [targetToken.constructor.name]);
                        }();
                        return new Right({
                          position: position2,
                          positionTokenMap: newMapping
                        });
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
  var closePosition = function(user) {
    return function(positionId) {
      return function(state3) {
        return function __do7() {
          var maybePosition = getPosition(positionId)(state3.poolRegistry)();
          if (maybePosition instanceof Nothing) {
            return new Left(new InvalidCommandError("Position " + (show10(positionId) + " not found")));
          }
          ;
          if (maybePosition instanceof Just) {
            var $22 = maybePosition.value0.owner !== user;
            if ($22) {
              return new Left(new InvalidCommandError("User " + (user + (" does not own position " + show10(positionId)))));
            }
            ;
            var $23 = !isSpotPosition(maybePosition.value0) && !isExpired(state3.currentBlock)(maybePosition.value0);
            if ($23) {
              return new Left(new InvalidCommandError("Position " + (show10(positionId) + " has not expired yet")));
            }
            ;
            var currentBalance = getFeelsAccountBalance(state3.accounts)(user)(FeelsSOL.value)();
            var updateResult = updateFeelsAccountBalance(state3.accounts)(user)(FeelsSOL.value)(currentBalance + maybePosition.value0.amount)();
            if (updateResult instanceof Left) {
              return new Left(new InvalidCommandError(updateResult.value0));
            }
            ;
            if (updateResult instanceof Right) {
              removePosition(positionId)(state3.poolRegistry)();
              return new Right(unit);
            }
            ;
            throw new Error("Failed pattern match at UI.Action.PositionActions (line 120, column 15 - line 125, column 36): " + [updateResult.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at UI.Action.PositionActions (line 102, column 3 - line 125, column 36): " + [maybePosition.constructor.name]);
        };
      };
    };
  };
  var initiateUnbonding = function(user) {
    return function(positionId) {
      return function(state3) {
        return function __do7() {
          var maybePosition = getPosition(positionId)(state3.poolRegistry)();
          if (maybePosition instanceof Nothing) {
            return new Left(new InvalidCommandError("Position " + (show10(positionId) + " not found")));
          }
          ;
          if (maybePosition instanceof Just) {
            var $29 = maybePosition.value0.owner !== user;
            if ($29) {
              return new Left(new InvalidCommandError("User " + (user + (" does not own position " + show10(positionId)))));
            }
            ;
            var $30 = isSpotPosition(maybePosition.value0);
            if ($30) {
              return closePosition(user)(positionId)(state3)();
            }
            ;
            return new Right(unit);
          }
          ;
          throw new Error("Failed pattern match at UI.Action.PositionActions (line 162, column 3 - line 179, column 32): " + [maybePosition.constructor.name]);
        };
      };
    };
  };
  var withdrawPosition = function(user) {
    return function(positionId) {
      return function(state3) {
        return function __do7() {
          var maybePosition = getPosition(positionId)(state3.poolRegistry)();
          if (maybePosition instanceof Nothing) {
            return new Left(new InvalidCommandError("Position " + (show10(positionId) + " not found")));
          }
          ;
          if (maybePosition instanceof Just) {
            var $33 = maybePosition.value0.owner !== user;
            if ($33) {
              return new Left(new InvalidCommandError("User " + (user + (" does not own position " + show10(positionId)))));
            }
            ;
            var $34 = !isSpotPosition(maybePosition.value0) && !isExpired(state3.currentBlock)(maybePosition.value0);
            if ($34) {
              return new Left(new InvalidCommandError("Position " + (show10(positionId) + " has not expired yet")));
            }
            ;
            return closePosition(user)(positionId)(state3)();
          }
          ;
          throw new Error("Failed pattern match at UI.Action.PositionActions (line 186, column 3 - line 199, column 50): " + [maybePosition.constructor.name]);
        };
      };
    };
  };

  // output/UI.TokenRegistry/index.js
  var registerToken = function(registry) {
    return function(token) {
      return modify_(function(tokens) {
        return cons(token)(tokens);
      })(registry);
    };
  };
  var getSystemTokens = /* @__PURE__ */ function() {
    return [{
      id: 1,
      ticker: "JitoSOL",
      name: "Jito Staked SOL",
      tokenType: JitoSOL.value,
      totalSupply: 1e6,
      creator: "system",
      createdAt: 0,
      live: true
    }, {
      id: 2,
      ticker: "FeelsSOL",
      name: "Feels SOL",
      tokenType: FeelsSOL.value,
      totalSupply: 1e6,
      creator: "system",
      createdAt: 0,
      live: true
    }];
  }();
  var initTokenRegistry = /* @__PURE__ */ $$new(getSystemTokens);
  var getAllTokens = function(registry) {
    return read(registry);
  };
  var createAndRegisterToken = function(registry) {
    return function(params) {
      return function __do7() {
        var token = createToken(params)();
        registerToken(registry)(token)();
        return token;
      };
    };
  };

  // output/UI.Action.TokenActions/index.js
  var pure20 = /* @__PURE__ */ pure(applicativeEffect);
  var validateTokenParams = function(ticker) {
    return function(name16) {
      return function(tokenRegistry) {
        var $9 = ticker === "" || name16 === "";
        if ($9) {
          return pure20(new Left(new InvalidCommandError("Ticker and name cannot be empty")));
        }
        ;
        return function __do7() {
          var existingTokens = getAllTokens(tokenRegistry)();
          var duplicateTicker = find2(function(t) {
            return t.ticker === ticker;
          })(existingTokens);
          var duplicateName = find2(function(t) {
            return t.name === name16;
          })(existingTokens);
          if (duplicateTicker instanceof Just) {
            return new Left(new InvalidCommandError("Token with ticker '" + (ticker + "' already exists")));
          }
          ;
          if (duplicateTicker instanceof Nothing) {
            if (duplicateName instanceof Just) {
              return new Left(new InvalidCommandError("Token with name '" + (name16 + "' already exists")));
            }
            ;
            if (duplicateName instanceof Nothing) {
              return new Right(unit);
            }
            ;
            throw new Error("Failed pattern match at UI.Action.TokenActions (line 40, column 20 - line 42, column 39): " + [duplicateName.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at UI.Action.TokenActions (line 38, column 7 - line 42, column 39): " + [duplicateTicker.constructor.name]);
        };
      };
    };
  };
  var createToken2 = function(creator) {
    return function(ticker) {
      return function(name16) {
        return function(state3) {
          return function __do7() {
            var validationResult = validateTokenParams(ticker)(name16)(state3.tokenRegistry)();
            if (validationResult instanceof Left) {
              return new Left(validationResult.value0);
            }
            ;
            if (validationResult instanceof Right) {
              var tokenParams = {
                ticker,
                name: name16,
                creator
              };
              var newToken = createAndRegisterToken(state3.tokenRegistry)(tokenParams)();
              var poolId = ticker + "/FeelsSOL";
              var poolConfig = {
                fee: 3e-3,
                tickSpacing: 10,
                maxLiquidityPerTick: 1e6
              };
              var newPool = initializePool(new Token(ticker))(FeelsSOL.value)(1)(poolConfig);
              var initialFeelsSOLAmount = 1e4 * 1;
              var initialLiquidity = sqrt2(1e4 * initialFeelsSOLAmount);
              var poolWithLiquidity = {
                feeGrowthGlobal0X128: newPool.feeGrowthGlobal0X128,
                feeGrowthGlobal1X128: newPool.feeGrowthGlobal1X128,
                lastUpdateBlock: newPool.lastUpdateBlock,
                leverageState: newPool.leverageState,
                offering: newPool.offering,
                protocolFee: newPool.protocolFee,
                sqrtPriceX96: newPool.sqrtPriceX96,
                tick: newPool.tick,
                token0: newPool.token0,
                token1: newPool.token1,
                totalValue: newPool.totalValue,
                unlocked: newPool.unlocked,
                liquidity: initialLiquidity
              };
              addPool(poolId)(poolWithLiquidity)(state3.poolRegistry)();
              return new Right(newToken);
            }
            ;
            throw new Error("Failed pattern match at UI.Action.TokenActions (line 49, column 3 - line 78, column 28): " + [validationResult.constructor.name]);
          };
        };
      };
    };
  };

  // output/Protocol.Incentive/index.js
  var initMarketDynamics = function(oracle) {
    return function(polState) {
      return function __do7() {
        var config = $$new({
          baseRate: 0.05,
          spread: 0.01
        })();
        return {
          config,
          oracle,
          polState
        };
      };
    };
  };

  // output/UI.ProtocolState/index.js
  var show11 = /* @__PURE__ */ show(showNumber);
  var pure21 = /* @__PURE__ */ pure(applicativeEffect);
  var CreateToken2 = /* @__PURE__ */ function() {
    function CreateToken3(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    CreateToken3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new CreateToken3(value0, value1, value22);
        };
      };
    };
    return CreateToken3;
  }();
  var CreatePosition = /* @__PURE__ */ function() {
    function CreatePosition2(value0, value1, value22, value32, value42, value52, value62, value72, value82) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
      this.value7 = value72;
      this.value8 = value82;
    }
    ;
    CreatePosition2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return function(value72) {
                    return function(value82) {
                      return new CreatePosition2(value0, value1, value22, value32, value42, value52, value62, value72, value82);
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
    return CreatePosition2;
  }();
  var TransferTokens = /* @__PURE__ */ function() {
    function TransferTokens2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    TransferTokens2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new TransferTokens2(value0, value1, value22, value32);
          };
        };
      };
    };
    return TransferTokens2;
  }();
  var EnterFeelsSOL = /* @__PURE__ */ function() {
    function EnterFeelsSOL2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    EnterFeelsSOL2.create = function(value0) {
      return function(value1) {
        return new EnterFeelsSOL2(value0, value1);
      };
    };
    return EnterFeelsSOL2;
  }();
  var ExitFeelsSOL = /* @__PURE__ */ function() {
    function ExitFeelsSOL2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ExitFeelsSOL2.create = function(value0) {
      return function(value1) {
        return new ExitFeelsSOL2(value0, value1);
      };
    };
    return ExitFeelsSOL2;
  }();
  var InitiateUnbonding = /* @__PURE__ */ function() {
    function InitiateUnbonding2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    InitiateUnbonding2.create = function(value0) {
      return function(value1) {
        return new InitiateUnbonding2(value0, value1);
      };
    };
    return InitiateUnbonding2;
  }();
  var WithdrawPosition = /* @__PURE__ */ function() {
    function WithdrawPosition2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    WithdrawPosition2.create = function(value0) {
      return function(value1) {
        return new WithdrawPosition2(value0, value1);
      };
    };
    return WithdrawPosition2;
  }();
  var CreateOffering = /* @__PURE__ */ function() {
    function CreateOffering2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    CreateOffering2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new CreateOffering2(value0, value1, value22);
        };
      };
    };
    return CreateOffering2;
  }();
  var StartOfferingPhase = /* @__PURE__ */ function() {
    function StartOfferingPhase2(value0) {
      this.value0 = value0;
    }
    ;
    StartOfferingPhase2.create = function(value0) {
      return new StartOfferingPhase2(value0);
    };
    return StartOfferingPhase2;
  }();
  var CompleteOfferingPhase = /* @__PURE__ */ function() {
    function CompleteOfferingPhase2(value0) {
      this.value0 = value0;
    }
    ;
    CompleteOfferingPhase2.create = function(value0) {
      return new CompleteOfferingPhase2(value0);
    };
    return CompleteOfferingPhase2;
  }();
  var GetUserTokens = /* @__PURE__ */ function() {
    function GetUserTokens2(value0) {
      this.value0 = value0;
    }
    ;
    GetUserTokens2.create = function(value0) {
      return new GetUserTokens2(value0);
    };
    return GetUserTokens2;
  }();
  var GetAllTokens = /* @__PURE__ */ function() {
    function GetAllTokens2() {
    }
    ;
    GetAllTokens2.value = new GetAllTokens2();
    return GetAllTokens2;
  }();
  var GetUserPositions = /* @__PURE__ */ function() {
    function GetUserPositions2(value0) {
      this.value0 = value0;
    }
    ;
    GetUserPositions2.create = function(value0) {
      return new GetUserPositions2(value0);
    };
    return GetUserPositions2;
  }();
  var GetUserBalance = /* @__PURE__ */ function() {
    function GetUserBalance2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetUserBalance2.create = function(value0) {
      return function(value1) {
        return new GetUserBalance2(value0, value1);
      };
    };
    return GetUserBalance2;
  }();
  var GetTokenByTicker = /* @__PURE__ */ function() {
    function GetTokenByTicker2(value0) {
      this.value0 = value0;
    }
    ;
    GetTokenByTicker2.create = function(value0) {
      return new GetTokenByTicker2(value0);
    };
    return GetTokenByTicker2;
  }();
  var GetLenderOffers = /* @__PURE__ */ function() {
    function GetLenderOffers2() {
    }
    ;
    GetLenderOffers2.value = new GetLenderOffers2();
    return GetLenderOffers2;
  }();
  var GetSystemStats = /* @__PURE__ */ function() {
    function GetSystemStats2() {
    }
    ;
    GetSystemStats2.value = new GetSystemStats2();
    return GetSystemStats2;
  }();
  var GetPOLMetrics = /* @__PURE__ */ function() {
    function GetPOLMetrics2() {
    }
    ;
    GetPOLMetrics2.value = new GetPOLMetrics2();
    return GetPOLMetrics2;
  }();
  var GetPositionTargetToken = /* @__PURE__ */ function() {
    function GetPositionTargetToken2(value0) {
      this.value0 = value0;
    }
    ;
    GetPositionTargetToken2.create = function(value0) {
      return new GetPositionTargetToken2(value0);
    };
    return GetPositionTargetToken2;
  }();
  var GetActiveOfferings = /* @__PURE__ */ function() {
    function GetActiveOfferings2() {
    }
    ;
    GetActiveOfferings2.value = new GetActiveOfferings2();
    return GetActiveOfferings2;
  }();
  var GetOfferingStatus = /* @__PURE__ */ function() {
    function GetOfferingStatus2(value0) {
      this.value0 = value0;
    }
    ;
    GetOfferingStatus2.create = function(value0) {
      return new GetOfferingStatus2(value0);
    };
    return GetOfferingStatus2;
  }();
  var initStateImpl = function __do4() {
    var v = initTokenRegistry();
    var v1 = initPoolRegistry();
    var v2 = initAccountRegistry();
    var oracleInit = initOracle(1.05);
    var v3 = oracleInit();
    var v4 = initPOL();
    var polStateData = read(v4)();
    log("POL initialized with totalPOL: " + show11(polStateData.totalPOL))();
    var v5 = initMarketDynamics(v3)(v4)();
    var v6 = initFeelsSOL(pure21(1.05))(1e-3)(2e-3)();
    var timestamp = currentTime();
    var initialState = {
      tokenRegistry: v,
      poolRegistry: v1,
      feelsSOL: v6,
      polState: v4,
      oracle: v3,
      marketDynamics: v5,
      accounts: v2,
      offerings: empty2,
      positionTokenMap: [],
      currentUser: "demo-user",
      currentBlock: 1e3,
      timestamp,
      lastJitoSOLPrice: 1.05,
      priceHistory: []
    };
    addPool("FeelsSOL/BONK")({
      token0: FeelsSOL.value,
      token1: new Token("BONK"),
      sqrtPriceX96: 7922816251426434e13,
      liquidity: 1e4,
      tick: 0,
      feeGrowthGlobal0X128: 0,
      feeGrowthGlobal1X128: 0,
      protocolFee: 30,
      unlocked: true,
      offering: Nothing.value,
      leverageState: {
        totalValue: 1e4,
        leverageGroups: [{
          leverage: 1,
          value: 0,
          shares: 0
        }, {
          leverage: 3,
          value: 0,
          shares: 0
        }]
      },
      totalValue: 1e4,
      lastUpdateBlock: 0
    })(v1)();
    updateChainAccountBalance(v2)("main-user")(1e3)();
    updateChainAccountBalance(v2)("demo-user")(1e3)();
    var stateRef = $$new(initialState)();
    var listenersRef = $$new([])();
    var listenerIdRef = $$new(0)();
    return {
      state: stateRef,
      listeners: listenersRef,
      nextListenerId: listenerIdRef
    };
  };
  var initState = initStateImpl;

  // output/UI.Command/index.js
  var pure23 = /* @__PURE__ */ pure(applicativeEffect);
  var lookup7 = /* @__PURE__ */ lookup(ordString);
  var show14 = /* @__PURE__ */ show(showOfferingPhase);
  var map31 = /* @__PURE__ */ map(functorArray);
  var insert7 = /* @__PURE__ */ insert(ordString);
  var lookupPool = function(poolId) {
    return function(registry) {
      return getPool(poolId)(registry);
    };
  };
  var handleWithdrawPosition = function(user) {
    return function(positionId) {
      return function(state3) {
        return function __do7() {
          var result = withdrawPosition(user)(positionId)(state3)();
          if (result instanceof Left) {
            return new Left(result.value0);
          }
          ;
          if (result instanceof Right) {
            var timestamp = currentTime();
            var newState = {
              accounts: state3.accounts,
              currentBlock: state3.currentBlock,
              currentUser: state3.currentUser,
              feelsSOL: state3.feelsSOL,
              lastJitoSOLPrice: state3.lastJitoSOLPrice,
              marketDynamics: state3.marketDynamics,
              offerings: state3.offerings,
              oracle: state3.oracle,
              polState: state3.polState,
              poolRegistry: state3.poolRegistry,
              positionTokenMap: state3.positionTokenMap,
              priceHistory: state3.priceHistory,
              tokenRegistry: state3.tokenRegistry,
              timestamp
            };
            return new Right(new Tuple(newState, new PositionWithdrawn(positionId)));
          }
          ;
          throw new Error("Failed pattern match at UI.Command (line 181, column 3 - line 186, column 67): " + [result.constructor.name]);
        };
      };
    };
  };
  var handleTransferTokens = function(from2) {
    return function(to) {
      return function(token) {
        return function(amount) {
          return function(state3) {
            return function __do7() {
              var result = transferTokens(from2)(to)(token)(amount)(state3)();
              if (result instanceof Left) {
                return new Left(result.value0);
              }
              ;
              if (result instanceof Right) {
                var timestamp = currentTime();
                var newState = {
                  accounts: state3.accounts,
                  currentBlock: state3.currentBlock,
                  currentUser: state3.currentUser,
                  feelsSOL: state3.feelsSOL,
                  lastJitoSOLPrice: state3.lastJitoSOLPrice,
                  marketDynamics: state3.marketDynamics,
                  offerings: state3.offerings,
                  oracle: state3.oracle,
                  polState: state3.polState,
                  poolRegistry: state3.poolRegistry,
                  positionTokenMap: state3.positionTokenMap,
                  priceHistory: state3.priceHistory,
                  tokenRegistry: state3.tokenRegistry,
                  timestamp
                };
                return new Right(new Tuple(newState, new TokensTransferred({
                  from: from2,
                  to,
                  token,
                  amount
                })));
              }
              ;
              throw new Error("Failed pattern match at UI.Command (line 135, column 3 - line 140, column 84): " + [result.constructor.name]);
            };
          };
        };
      };
    };
  };
  var handleStartOfferingPhase = function(poolId) {
    return function(state3) {
      var v = lookup7(poolId)(state3.offerings);
      if (v instanceof Nothing) {
        return pure23(new Left(new InvalidCommandError("Offering not found")));
      }
      ;
      if (v instanceof Just) {
        return function __do7() {
          var pool = lookupPool(poolId)(state3.poolRegistry)();
          if (pool instanceof Nothing) {
            return new Left(new InvalidCommandError("Pool not found"));
          }
          ;
          if (pool instanceof Just) {
            var result = startPhase(v.value0)(pool.value0)(state3.currentBlock)();
            if (result instanceof Left) {
              return new Left(result.value0);
            }
            ;
            if (result instanceof Right) {
              var offering = read(v.value0)();
              return new Right(new Tuple(state3, new OfferingPhaseStarted(poolId, show14(offering.currentPhase))));
            }
            ;
            throw new Error("Failed pattern match at UI.Command (line 233, column 11 - line 237, column 100): " + [result.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at UI.Command (line 229, column 7 - line 237, column 100): " + [pool.constructor.name]);
        };
      }
      ;
      throw new Error("Failed pattern match at UI.Command (line 224, column 3 - line 237, column 100): " + [v.constructor.name]);
    };
  };
  var handleInitiateUnbonding = function(user) {
    return function(positionId) {
      return function(state3) {
        return function __do7() {
          var result = initiateUnbonding(user)(positionId)(state3)();
          if (result instanceof Left) {
            return new Left(result.value0);
          }
          ;
          if (result instanceof Right) {
            var timestamp = currentTime();
            var newState = {
              accounts: state3.accounts,
              currentBlock: state3.currentBlock,
              currentUser: state3.currentUser,
              feelsSOL: state3.feelsSOL,
              lastJitoSOLPrice: state3.lastJitoSOLPrice,
              marketDynamics: state3.marketDynamics,
              offerings: state3.offerings,
              oracle: state3.oracle,
              polState: state3.polState,
              poolRegistry: state3.poolRegistry,
              positionTokenMap: state3.positionTokenMap,
              priceHistory: state3.priceHistory,
              tokenRegistry: state3.tokenRegistry,
              timestamp
            };
            return new Right(new Tuple(newState, new UnbondingInitiated(positionId)));
          }
          ;
          throw new Error("Failed pattern match at UI.Command (line 170, column 3 - line 175, column 68): " + [result.constructor.name]);
        };
      };
    };
  };
  var handleExitFeelsSOL = function(user) {
    return function(feelsAmount) {
      return function(state3) {
        return function __do7() {
          var result = exitFeelsSOL(user)(feelsAmount)(state3)();
          if (result instanceof Left) {
            return new Left(result.value0);
          }
          ;
          if (result instanceof Right) {
            var timestamp = currentTime();
            var newState = {
              accounts: state3.accounts,
              currentBlock: state3.currentBlock,
              currentUser: state3.currentUser,
              feelsSOL: state3.feelsSOL,
              lastJitoSOLPrice: state3.lastJitoSOLPrice,
              marketDynamics: state3.marketDynamics,
              offerings: state3.offerings,
              oracle: state3.oracle,
              polState: state3.polState,
              poolRegistry: state3.poolRegistry,
              positionTokenMap: state3.positionTokenMap,
              priceHistory: state3.priceHistory,
              tokenRegistry: state3.tokenRegistry,
              timestamp
            };
            return new Right(new Tuple(newState, new FeelsSOLBurned(result.value0)));
          }
          ;
          throw new Error("Failed pattern match at UI.Command (line 158, column 3 - line 164, column 64): " + [result.constructor.name]);
        };
      };
    };
  };
  var handleEnterFeelsSOL = function(user) {
    return function(jitoAmount) {
      return function(state3) {
        return function __do7() {
          var result = enterFeelsSOL(user)(jitoAmount)(state3)();
          if (result instanceof Left) {
            return new Left(result.value0);
          }
          ;
          if (result instanceof Right) {
            var timestamp = currentTime();
            var newState = {
              accounts: state3.accounts,
              currentBlock: state3.currentBlock,
              currentUser: state3.currentUser,
              feelsSOL: state3.feelsSOL,
              lastJitoSOLPrice: state3.lastJitoSOLPrice,
              marketDynamics: state3.marketDynamics,
              offerings: state3.offerings,
              oracle: state3.oracle,
              polState: state3.polState,
              poolRegistry: state3.poolRegistry,
              positionTokenMap: state3.positionTokenMap,
              priceHistory: state3.priceHistory,
              tokenRegistry: state3.tokenRegistry,
              timestamp
            };
            return new Right(new Tuple(newState, new FeelsSOLMinted(result.value0)));
          }
          ;
          throw new Error("Failed pattern match at UI.Command (line 146, column 3 - line 152, column 64): " + [result.constructor.name]);
        };
      };
    };
  };
  var handleCreateToken = function(creator) {
    return function(ticker) {
      return function(name16) {
        return function(state3) {
          return function __do7() {
            var result = createToken2(creator)(ticker)(name16)(state3)();
            if (result instanceof Left) {
              return new Left(result.value0);
            }
            ;
            if (result instanceof Right) {
              var timestamp = currentTime();
              var newState = {
                accounts: state3.accounts,
                currentBlock: state3.currentBlock,
                currentUser: state3.currentUser,
                feelsSOL: state3.feelsSOL,
                lastJitoSOLPrice: state3.lastJitoSOLPrice,
                marketDynamics: state3.marketDynamics,
                offerings: state3.offerings,
                oracle: state3.oracle,
                polState: state3.polState,
                poolRegistry: state3.poolRegistry,
                positionTokenMap: state3.positionTokenMap,
                priceHistory: state3.priceHistory,
                tokenRegistry: state3.tokenRegistry,
                timestamp
              };
              return new Right(new Tuple(newState, new TokenCreated(result.value0)));
            }
            ;
            throw new Error("Failed pattern match at UI.Command (line 104, column 3 - line 111, column 66): " + [result.constructor.name]);
          };
        };
      };
    };
  };
  var handleCreatePosition = function(user) {
    return function(lendAsset) {
      return function(amount) {
        return function(collateralAsset) {
          return function(collateralAmount) {
            return function(term) {
              return function(leverage) {
                return function(rollover) {
                  return function(targetToken) {
                    return function(state3) {
                      return function __do7() {
                        var result = createPosition2(user)(lendAsset)(amount)(collateralAsset)(collateralAmount)(term)(leverage)(rollover)(targetToken)(state3)();
                        if (result instanceof Left) {
                          return new Left(result.value0);
                        }
                        ;
                        if (result instanceof Right) {
                          var timestamp = currentTime();
                          var newState = {
                            accounts: state3.accounts,
                            currentBlock: state3.currentBlock,
                            currentUser: state3.currentUser,
                            feelsSOL: state3.feelsSOL,
                            lastJitoSOLPrice: state3.lastJitoSOLPrice,
                            marketDynamics: state3.marketDynamics,
                            offerings: state3.offerings,
                            oracle: state3.oracle,
                            polState: state3.polState,
                            poolRegistry: state3.poolRegistry,
                            priceHistory: state3.priceHistory,
                            tokenRegistry: state3.tokenRegistry,
                            positionTokenMap: result.value0.positionTokenMap,
                            timestamp
                          };
                          return new Right(new Tuple(newState, new PositionCreated(result.value0.position)));
                        }
                        ;
                        throw new Error("Failed pattern match at UI.Command (line 117, column 3 - line 129, column 72): " + [result.constructor.name]);
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
  var handleCreateOffering = function(ticker) {
    return function(totalTokens) {
      return function(phases) {
        return function(state3) {
          var priceToTick2 = function(price) {
            return floor(log2(price) / log2(1.0001));
          };
          var parsePhase = function(v) {
            if (v === "Monthly") {
              return MonthlyPhase.value;
            }
            ;
            return SpotPhase.value;
          };
          var convertPhase = function(p2) {
            return {
              phase: parsePhase(p2.phase),
              tokenAmount: p2.tokens,
              priceRangeLower: p2.priceLower,
              priceRangeUpper: p2.priceUpper,
              tickLower: priceToTick2(p2.priceLower),
              tickUpper: priceToTick2(p2.priceUpper)
            };
          };
          var phaseConfigs = map31(convertPhase)(phases);
          var config = {
            tokenTicker: ticker,
            totalTokens,
            phases: phaseConfigs,
            treasuryAddress: "treasury"
          };
          return function __do7() {
            var offeringResult = initOffering(config)();
            if (offeringResult instanceof Left) {
              return new Left(offeringResult.value0);
            }
            ;
            if (offeringResult instanceof Right) {
              var poolId = ticker + "/FeelsSOL";
              var newState = {
                accounts: state3.accounts,
                currentBlock: state3.currentBlock,
                currentUser: state3.currentUser,
                feelsSOL: state3.feelsSOL,
                lastJitoSOLPrice: state3.lastJitoSOLPrice,
                marketDynamics: state3.marketDynamics,
                oracle: state3.oracle,
                polState: state3.polState,
                poolRegistry: state3.poolRegistry,
                positionTokenMap: state3.positionTokenMap,
                priceHistory: state3.priceHistory,
                tokenRegistry: state3.tokenRegistry,
                offerings: insert7(poolId)(offeringResult.value0)(state3.offerings),
                timestamp: state3.timestamp
              };
              return new Right(new Tuple(newState, new OfferingCreated(poolId, ticker)));
            }
            ;
            throw new Error("Failed pattern match at UI.Command (line 200, column 3 - line 207, column 68): " + [offeringResult.constructor.name]);
          };
        };
      };
    };
  };
  var handleCompleteOfferingPhase = function(poolId) {
    return function(state3) {
      var v = lookup7(poolId)(state3.offerings);
      if (v instanceof Nothing) {
        return pure23(new Left(new InvalidCommandError("Offering not found")));
      }
      ;
      if (v instanceof Just) {
        return function __do7() {
          var pool = lookupPool(poolId)(state3.poolRegistry)();
          if (pool instanceof Nothing) {
            return new Left(new InvalidCommandError("Pool not found"));
          }
          ;
          if (pool instanceof Just) {
            var isComplete = checkPhaseComplete(v.value0)(pool.value0)();
            var $68 = !isComplete;
            if ($68) {
              return new Left(new InvalidCommandError("Phase not complete"));
            }
            ;
            var result = completeOffering(v.value0)();
            if (result instanceof Left) {
              return new Left(result.value0);
            }
            ;
            if (result instanceof Right) {
              return new Right(new Tuple(state3, new OfferingPhaseCompleted(poolId, show14(result.value0))));
            }
            ;
            throw new Error("Failed pattern match at UI.Command (line 255, column 15 - line 257, column 111): " + [result.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at UI.Command (line 246, column 7 - line 257, column 111): " + [pool.constructor.name]);
        };
      }
      ;
      throw new Error("Failed pattern match at UI.Command (line 242, column 3 - line 257, column 111): " + [v.constructor.name]);
    };
  };
  var executeCommand = function(cmd) {
    return function(state3) {
      if (cmd instanceof CreateToken2) {
        return handleCreateToken(cmd.value0)(cmd.value1)(cmd.value2)(state3);
      }
      ;
      if (cmd instanceof CreatePosition) {
        return handleCreatePosition(cmd.value0)(cmd.value1)(cmd.value2)(cmd.value3)(cmd.value4)(cmd.value5)(cmd.value6)(cmd.value7)(cmd.value8)(state3);
      }
      ;
      if (cmd instanceof TransferTokens) {
        return handleTransferTokens(cmd.value0)(cmd.value1)(cmd.value2)(cmd.value3)(state3);
      }
      ;
      if (cmd instanceof EnterFeelsSOL) {
        return handleEnterFeelsSOL(cmd.value0)(cmd.value1)(state3);
      }
      ;
      if (cmd instanceof ExitFeelsSOL) {
        return handleExitFeelsSOL(cmd.value0)(cmd.value1)(state3);
      }
      ;
      if (cmd instanceof InitiateUnbonding) {
        return handleInitiateUnbonding(cmd.value0)(cmd.value1)(state3);
      }
      ;
      if (cmd instanceof WithdrawPosition) {
        return handleWithdrawPosition(cmd.value0)(cmd.value1)(state3);
      }
      ;
      if (cmd instanceof CreateOffering) {
        return handleCreateOffering(cmd.value0)(cmd.value1)(cmd.value2)(state3);
      }
      ;
      if (cmd instanceof StartOfferingPhase) {
        return handleStartOfferingPhase(cmd.value0)(state3);
      }
      ;
      if (cmd instanceof CompleteOfferingPhase) {
        return handleCompleteOfferingPhase(cmd.value0)(state3);
      }
      ;
      throw new Error("Failed pattern match at UI.Command (line 269, column 28 - line 289, column 45): " + [cmd.constructor.name]);
    };
  };

  // output/UI.Query/index.js
  var pure24 = /* @__PURE__ */ pure(applicativeEffect);
  var map32 = /* @__PURE__ */ map(functorArray);
  var sum8 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var nub3 = /* @__PURE__ */ nub(ordString);
  var lookup8 = /* @__PURE__ */ lookup(ordString);
  var toUnfoldable4 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var traverse3 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
  var show15 = /* @__PURE__ */ show(showOfferingPhase);
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var handleGetUserTokens = function(user) {
    return function(state3) {
      return function __do7() {
        var allTokens = getAllTokens(state3.tokenRegistry)();
        var userTokens = filter(function(t) {
          return t.creator === user;
        })(allTokens);
        return new Right(new TokenList(userTokens));
      };
    };
  };
  var handleGetUserPositions = function(user) {
    return function(state3) {
      return function __do7() {
        var positions = getUserPositions(user)(state3.poolRegistry)();
        var poolResult = getPool("FeelsSOL/DEFAULT")(state3.poolRegistry)();
        var syncedPositions = function() {
          if (poolResult instanceof Just) {
            return map32(function(pos) {
              return syncPositionValue(pos)(poolResult.value0)(state3.currentBlock);
            })(positions);
          }
          ;
          if (poolResult instanceof Nothing) {
            return positions;
          }
          ;
          throw new Error("Failed pattern match at UI.Query (line 98, column 25 - line 100, column 29): " + [poolResult.constructor.name]);
        }();
        return new Right(new PositionList(syncedPositions));
      };
    };
  };
  var handleGetUserBalance = function(user) {
    return function(tokenType) {
      return function(state3) {
        return function __do7() {
          var balance = getFeelsAccountBalance(state3.accounts)(user)(tokenType)();
          return new Right(new Balance(balance));
        };
      };
    };
  };
  var handleGetTokenByTicker = function(ticker) {
    return function(state3) {
      return function __do7() {
        var allTokens = getAllTokens(state3.tokenRegistry)();
        var maybeToken = find2(function(t) {
          return t.ticker === ticker;
        })(allTokens);
        return new Right(new TokenInfo(maybeToken));
      };
    };
  };
  var handleGetSystemStats = function(state3) {
    return function __do7() {
      var activePositions = getAllPositions(state3.poolRegistry)();
      var totalValueLocked = sum8(map32(function(r) {
        return r.amount;
      })(activePositions));
      var uniqueUsers = nub3(map32(function(r) {
        return r.owner;
      })(activePositions));
      var userCount = function() {
        var $18 = length3(uniqueUsers) === 0;
        if ($18) {
          return 1;
        }
        ;
        return length3(uniqueUsers);
      }();
      var tokenList = getAllTokens(state3.tokenRegistry)();
      var liveCount = length3(filter(function(t) {
        return t.live;
      })(tokenList));
      var polBalance = getTotalPOL(state3.polState)();
      var feelsSOLSupply = getTotalTokenBalance(state3.accounts)(FeelsSOL.value)();
      var jitoSOLLocked = getTotalTokenBalance(state3.accounts)(JitoSOL.value)();
      return new Right(new SystemStatsResult({
        totalValueLocked,
        totalUsers: userCount,
        activePositions: length3(activePositions),
        liveTokens: liveCount,
        totalLenderOffers: length3(activePositions),
        polBalance,
        feelsSOLSupply,
        jitoSOLLocked
      }));
    };
  };
  var handleGetPositionTargetToken = function(positionId) {
    return function(state3) {
      var maybeTarget = find2(function(m) {
        return m.positionId === positionId;
      })(state3.positionTokenMap);
      if (maybeTarget instanceof Just) {
        return pure24(new Right(new TargetTokenInfo(new Just(maybeTarget.value0.tokenTicker))));
      }
      ;
      if (maybeTarget instanceof Nothing) {
        return pure24(new Right(new TargetTokenInfo(Nothing.value)));
      }
      ;
      throw new Error("Failed pattern match at UI.Query (line 173, column 3 - line 175, column 54): " + [maybeTarget.constructor.name]);
    };
  };
  var handleGetPOLMetrics = function(state3) {
    return function __do7() {
      var balance = getTotalPOL(state3.polState)();
      var _metrics = getPOLMetrics(state3.polState)();
      var growthRate = calculateGrowthRate24h(state3.polState)();
      return new Right(new POLMetricsResult({
        balance,
        growthRate24h: growthRate
      }));
    };
  };
  var handleGetOfferingStatus = function(poolId) {
    return function(state3) {
      var v = lookup8(poolId)(state3.offerings);
      if (v instanceof Nothing) {
        return pure24(new Right(new OfferingStatusResult(Nothing.value)));
      }
      ;
      if (v instanceof Just) {
        return function __do7() {
          var result = getOfferingStatus(v.value0)();
          return new Right(new OfferingStatusResult(new Just(result)));
        };
      }
      ;
      throw new Error("Failed pattern match at UI.Query (line 195, column 3 - line 201, column 65): " + [v.constructor.name]);
    };
  };
  var handleGetLenderOffers = function(state3) {
    return function __do7() {
      var offers = getAllPositions(state3.poolRegistry)();
      return new Right(new LenderOfferList(offers));
    };
  };
  var handleGetAllTokens = function(state3) {
    return function __do7() {
      var allTokens = getAllTokens(state3.tokenRegistry)();
      return new Right(new TokenList(allTokens));
    };
  };
  var handleGetActiveOfferings = function(state3) {
    var offeringPairs = toUnfoldable4(state3.offerings);
    return function __do7() {
      var activeOfferings = traverse3(function(v) {
        return function __do8() {
          var offering = read(v.value1)();
          if (offering.isActive) {
            return new Just({
              poolId: v.value0,
              phase: show15(offering.currentPhase)
            });
          }
          ;
          return Nothing.value;
        };
      })(offeringPairs)();
      var filtered = mapMaybe(identity9)(activeOfferings);
      return new Right(new ActiveOfferingsList(filtered));
    };
  };
  var executeQuery = function(query2) {
    return function(state3) {
      if (query2 instanceof GetUserTokens) {
        return handleGetUserTokens(query2.value0)(state3);
      }
      ;
      if (query2 instanceof GetAllTokens) {
        return handleGetAllTokens(state3);
      }
      ;
      if (query2 instanceof GetUserPositions) {
        return handleGetUserPositions(query2.value0)(state3);
      }
      ;
      if (query2 instanceof GetUserBalance) {
        return handleGetUserBalance(query2.value0)(query2.value1)(state3);
      }
      ;
      if (query2 instanceof GetTokenByTicker) {
        return handleGetTokenByTicker(query2.value0)(state3);
      }
      ;
      if (query2 instanceof GetLenderOffers) {
        return handleGetLenderOffers(state3);
      }
      ;
      if (query2 instanceof GetSystemStats) {
        return handleGetSystemStats(state3);
      }
      ;
      if (query2 instanceof GetPOLMetrics) {
        return handleGetPOLMetrics(state3);
      }
      ;
      if (query2 instanceof GetPositionTargetToken) {
        return handleGetPositionTargetToken(query2.value0)(state3);
      }
      ;
      if (query2 instanceof GetActiveOfferings) {
        return handleGetActiveOfferings(state3);
      }
      ;
      if (query2 instanceof GetOfferingStatus) {
        return handleGetOfferingStatus(query2.value0)(state3);
      }
      ;
      throw new Error("Failed pattern match at UI.Query (line 55, column 28 - line 66, column 67): " + [query2.constructor.name]);
    };
  };

  // output/Simulation.ProtocolEngine/index.js
  var traverse_7 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray);
  var min11 = /* @__PURE__ */ min(ordNumber);
  var max10 = /* @__PURE__ */ max(ordNumber);
  var when5 = /* @__PURE__ */ when(applicativeEffect);
  var abs5 = /* @__PURE__ */ abs(ordNumber)(ringNumber);
  var show16 = /* @__PURE__ */ show(showNumber);
  var mod4 = /* @__PURE__ */ mod(euclideanRingInt);
  var show17 = /* @__PURE__ */ show(showInt);
  var map33 = /* @__PURE__ */ map(functorArray);
  var sum9 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var show22 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(/* @__PURE__ */ showTuple(showString)(showNumber)));
  var show32 = /* @__PURE__ */ show(showProtocolError);
  var foldM3 = /* @__PURE__ */ foldM(monadEffect);
  var fromFoldable7 = /* @__PURE__ */ fromFoldable(ordString)(foldableArray);
  var append12 = /* @__PURE__ */ append(semigroupArray);
  var updatePoolHealthMetrics = function(state3) {
    return function __do7() {
      var pools = getAllPools(state3.poolRegistry)();
      return traverse_7(function(v) {
        var volume2 = v.value1.feeGrowthGlobal0X128 + v.value1.feeGrowthGlobal1X128;
        var volumeScore = min11(1)(volume2 / 100);
        var liquidityScore = min11(1)(v.value1.liquidity / 1e4);
        var healthScore = (liquidityScore + volumeScore) / 2;
        var adjustedFee = function() {
          var $53 = healthScore > 0.8;
          if ($53) {
            return max10(10)(v.value1.protocolFee * 0.8);
          }
          ;
          var $54 = healthScore < 0.3;
          if ($54) {
            return min11(50)(v.value1.protocolFee * 1.5);
          }
          ;
          return v.value1.protocolFee;
        }();
        return when5(abs5(adjustedFee - v.value1.protocolFee) > 1)(function() {
          var updatedPool = {
            feeGrowthGlobal0X128: v.value1.feeGrowthGlobal0X128,
            feeGrowthGlobal1X128: v.value1.feeGrowthGlobal1X128,
            liquidity: v.value1.liquidity,
            lastUpdateBlock: v.value1.lastUpdateBlock,
            leverageState: v.value1.leverageState,
            offering: v.value1.offering,
            sqrtPriceX96: v.value1.sqrtPriceX96,
            tick: v.value1.tick,
            token0: v.value1.token0,
            token1: v.value1.token1,
            totalValue: v.value1.totalValue,
            unlocked: v.value1.unlocked,
            protocolFee: adjustedFee
          };
          return function __do8() {
            updatePool(v.value0)(updatedPool)(state3.poolRegistry)();
            return log("Updated pool " + (v.value0 + (" health score: " + (show16(healthScore) + (", fee: " + (show16(v.value1.protocolFee) + (" -> " + show16(adjustedFee))))))))();
          };
        }());
      })(pools)();
    };
  };
  var updatePOLDistribution = function(state3) {
    return function(scenario) {
      return function(blockNum) {
        var shouldDistribute = mod4(blockNum)(10) === 0;
        return when5(shouldDistribute)(function __do7() {
          log("POL Distribution check at block " + show17(blockNum))();
          var unallocated = getUnallocatedPOL(state3.polState)();
          log("Unallocated POL: " + show16(unallocated))();
          return when5(unallocated > 100)(function __do8() {
            var pools = getAllPools(state3.poolRegistry)();
            log("Found " + (show17(length3(pools)) + " pools for POL distribution"))();
            var baseAllocation = function() {
              if (scenario instanceof VolatileMarket) {
                return unallocated * 0.3;
              }
              ;
              if (scenario instanceof CrashScenario) {
                return unallocated * 0.4;
              }
              ;
              if (scenario instanceof BearMarket) {
                return unallocated * 0.2;
              }
              ;
              if (scenario instanceof RecoveryMarket) {
                return unallocated * 0.25;
              }
              ;
              return unallocated * 0.1;
            }();
            return when5(length3(pools) > 0)(function() {
              var poolsWithMetrics = map33(function(v) {
                var volume2 = v.value1.feeGrowthGlobal0X128 + v.value1.feeGrowthGlobal1X128;
                var score = max10(1)(v.value1.liquidity * (1 + min11(volume2)(1)));
                return new Tuple(v.value0, score);
              })(pools);
              var totalScore = sum9(map33(snd)(poolsWithMetrics));
              return function __do9() {
                log("POL allocation scores: " + show22(poolsWithMetrics))();
                log("Total score: " + show16(totalScore))();
                return traverse_7(function(v) {
                  var proportion = function() {
                    var $62 = totalScore > 0;
                    if ($62) {
                      return v.value1 / totalScore;
                    }
                    ;
                    return 0;
                  }();
                  var allocationAmount = baseAllocation * proportion;
                  return when5(allocationAmount > 10)(function __do10() {
                    var success = allocateToPool(state3.polState)(v.value0)(allocationAmount)();
                    return when5(success)(log("Allocated " + (show16(allocationAmount) + (" POL to pool " + v.value0))))();
                  });
                })(poolsWithMetrics)();
              };
            }())();
          })();
        });
      };
    };
  };
  var updateAccountBalances = function(accounts) {
    return function(userId) {
      return function(amount) {
        return function(token) {
          var updateAccount = function(acc) {
            if (acc.id === userId) {
              if (token instanceof JitoSOL) {
                return {
                  id: acc.id,
                  feelsSOLBalance: acc.feelsSOLBalance,
                  activePositions: acc.activePositions,
                  netPnL: acc.netPnL,
                  profile: acc.profile,
                  totalDeposited: acc.totalDeposited,
                  totalWithdrawn: acc.totalWithdrawn,
                  jitoSOLBalance: max10(0)(acc.jitoSOLBalance + amount)
                };
              }
              ;
              if (token instanceof FeelsSOL) {
                return {
                  id: acc.id,
                  jitoSOLBalance: acc.jitoSOLBalance,
                  activePositions: acc.activePositions,
                  netPnL: acc.netPnL,
                  profile: acc.profile,
                  totalDeposited: acc.totalDeposited,
                  totalWithdrawn: acc.totalWithdrawn,
                  feelsSOLBalance: max10(0)(acc.feelsSOLBalance + amount)
                };
              }
              ;
              if (token instanceof Token) {
                return acc;
              }
              ;
              throw new Error("Failed pattern match at Simulation.ProtocolEngine (line 301, column 28 - line 304, column 25): " + [token.constructor.name]);
            }
            ;
            if (otherwise) {
              return acc;
            }
            ;
            throw new Error("Failed pattern match at Simulation.ProtocolEngine (line 300, column 5 - line 305, column 24): " + [acc.constructor.name]);
          };
          return map33(updateAccount)(accounts);
        };
      };
    };
  };
  var getProtocolMetrics = function(state3) {
    return function __do7() {
      var polState = read(state3.polState)();
      log("DEBUG getProtocolMetrics - polState.totalPOL: " + show16(polState.totalPOL))();
      log("DEBUG getProtocolMetrics - polState.unallocated: " + show16(polState.unallocated))();
      var tokensResult = executeQuery(GetAllTokens.value)(state3)();
      var tokens = function() {
        if (tokensResult instanceof Right && tokensResult.value0 instanceof TokenList) {
          return tokensResult.value0.value0;
        }
        ;
        return [];
      }();
      var tokenMetrics = map33(function(foreignToken) {
        var tokenShare = foreignToken.supply / 1e6;
        var polFloor = function() {
          var $71 = foreignToken.supply > 0;
          if ($71) {
            return polState.totalPOL * tokenShare / foreignToken.supply;
          }
          ;
          return 0;
        }();
        return {
          ticker: foreignToken.ticker,
          supply: foreignToken.supply,
          polFloor
        };
      })(tokens);
      var tvl = polState.totalPOL * 10;
      var totalFees = function() {
        if (polState.contributionHistory.length === 0) {
          return 0;
        }
        ;
        return polState.totalPOL;
      }();
      return {
        totalValueLocked: tvl,
        polReserves: polState.totalPOL,
        totalFeesCollected: totalFees,
        tokenMetrics
      };
    };
  };
  var executeProtocolAction = function(protocolRef) {
    return function(simState) {
      return function(action2) {
        return function __do7() {
          var protocolState = read(protocolRef)();
          var executeAndUpdate = function(commandEffect) {
            return function __do8() {
              var result2 = commandEffect();
              if (result2 instanceof Right) {
                write(result2.value0.value0)(protocolRef)();
                return simState;
              }
              ;
              if (result2 instanceof Left) {
                log("Protocol error: " + show32(result2.value0))();
                return simState;
              }
              ;
              throw new Error("Failed pattern match at Simulation.ProtocolEngine (line 172, column 9 - line 178, column 26): " + [result2.constructor.name]);
            };
          };
          if (action2 instanceof EnterProtocol) {
            var result = executeCommand(new EnterFeelsSOL(action2.value0, action2.value1))(protocolState)();
            if (result instanceof Right && result.value0.value1 instanceof FeelsSOLMinted) {
              write(result.value0.value0)(protocolRef)();
              var updatedAccounts1 = updateAccountBalances(simState.accounts)(action2.value0)(-action2.value1)(JitoSOL.value);
              var updatedAccounts2 = updateAccountBalances(updatedAccounts1)(action2.value0)(action2.value1)(FeelsSOL.value);
              return {
                actionHistory: simState.actionHistory,
                currentBlock: simState.currentBlock,
                currentPrice: simState.currentPrice,
                feelsSOL: simState.feelsSOL,
                nextPositionId: simState.nextPositionId,
                oracle: simState.oracle,
                polAllocationHistory: simState.polAllocationHistory,
                poolRegistry: simState.poolRegistry,
                priceHistory: simState.priceHistory,
                accounts: updatedAccounts2
              };
            }
            ;
            log("Failed to enter protocol for user " + action2.value0)();
            return simState;
          }
          ;
          if (action2 instanceof ExitProtocol) {
            var result = executeCommand(new ExitFeelsSOL(action2.value0, action2.value1))(protocolState)();
            if (result instanceof Right && result.value0.value1 instanceof FeelsSOLBurned) {
              write(result.value0.value0)(protocolRef)();
              var updatedAccounts1 = updateAccountBalances(simState.accounts)(action2.value0)(-action2.value1)(FeelsSOL.value);
              var jitoAmount = action2.value1 * 0.998;
              var updatedAccounts2 = updateAccountBalances(updatedAccounts1)(action2.value0)(jitoAmount)(JitoSOL.value);
              return {
                actionHistory: simState.actionHistory,
                currentBlock: simState.currentBlock,
                currentPrice: simState.currentPrice,
                feelsSOL: simState.feelsSOL,
                nextPositionId: simState.nextPositionId,
                oracle: simState.oracle,
                polAllocationHistory: simState.polAllocationHistory,
                poolRegistry: simState.poolRegistry,
                priceHistory: simState.priceHistory,
                accounts: updatedAccounts2
              };
            }
            ;
            log("Failed to exit protocol for user " + action2.value0)();
            return simState;
          }
          ;
          if (action2 instanceof CreateLendOffer) {
            var result = executeCommand(new CreatePosition(action2.value0, action2.value1, action2.value2, action2.value3, action2.value4, action2.value5, Senior.value, false, Nothing.value))(protocolState)();
            if (result instanceof Right && result.value0.value1 instanceof PositionCreated) {
              write(result.value0.value0)(protocolRef)();
              log("Created position for " + (action2.value0 + ", fees collected to POL"))();
              var updatedAccounts = updateAccountBalances(simState.accounts)(action2.value0)(-action2.value2)(action2.value1);
              return {
                actionHistory: simState.actionHistory,
                currentBlock: simState.currentBlock,
                currentPrice: simState.currentPrice,
                feelsSOL: simState.feelsSOL,
                nextPositionId: simState.nextPositionId,
                oracle: simState.oracle,
                polAllocationHistory: simState.polAllocationHistory,
                poolRegistry: simState.poolRegistry,
                priceHistory: simState.priceHistory,
                accounts: updatedAccounts
              };
            }
            ;
            log("Failed to create position for user " + action2.value0)();
            return simState;
          }
          ;
          if (action2 instanceof TakeLoan) {
            var poolId = function() {
              var v = new Tuple(action2.value1, action2.value3);
              if (v.value0 instanceof Token && v.value1 instanceof FeelsSOL) {
                return v.value0.value0 + "/FeelsSOL";
              }
              ;
              if (v.value0 instanceof FeelsSOL && v.value1 instanceof Token) {
                return v.value1.value0 + "/FeelsSOL";
              }
              ;
              return "Unknown/FeelsSOL";
            }();
            var maybePool = getPool(poolId)(protocolState.poolRegistry)();
            if (maybePool instanceof Just) {
              var zeroForOne = function() {
                if (action2.value1 instanceof FeelsSOL) {
                  return false;
                }
                ;
                return true;
              }();
              var swapParams = {
                zeroForOne,
                amountSpecified: action2.value2,
                sqrtPriceLimitX96: 0
              };
              var protocolState1 = read(protocolRef)();
              var currentBlock = protocolState1.currentBlock + 1 | 0;
              var swapResult = swap(maybePool.value0)(swapParams)(currentBlock);
              updatePool(poolId)(swapResult.updatedPool)(protocolState1.poolRegistry)();
              log("Executed swap in pool " + (poolId + (" for user " + (action2.value0 + (", fee collected: ~" + (show16(action2.value2 * maybePool.value0.protocolFee / 1e4) + " FeelsSOL"))))))();
              var updatedAccounts1 = updateAccountBalances(simState.accounts)(action2.value0)(-action2.value2)(action2.value3);
              var amountOut = function() {
                if (zeroForOne) {
                  return abs5(swapResult.result.amount1);
                }
                ;
                return abs5(swapResult.result.amount0);
              }();
              var updatedAccounts2 = updateAccountBalances(updatedAccounts1)(action2.value0)(amountOut)(action2.value1);
              return {
                actionHistory: simState.actionHistory,
                currentBlock: simState.currentBlock,
                currentPrice: simState.currentPrice,
                feelsSOL: simState.feelsSOL,
                nextPositionId: simState.nextPositionId,
                oracle: simState.oracle,
                polAllocationHistory: simState.polAllocationHistory,
                poolRegistry: simState.poolRegistry,
                priceHistory: simState.priceHistory,
                accounts: updatedAccounts2
              };
            }
            ;
            if (maybePool instanceof Nothing) {
              log("Pool " + (poolId + " not found, cannot execute swap"))();
              return simState;
            }
            ;
            throw new Error("Failed pattern match at Simulation.ProtocolEngine (line 242, column 7 - line 278, column 24): " + [maybePool.constructor.name]);
          }
          ;
          if (action2 instanceof CreateToken) {
            var result = executeCommand(new CreateToken2(action2.value0, action2.value1, action2.value2))(protocolState)();
            if (result instanceof Right && result.value0.value1 instanceof TokenCreated) {
              write(result.value0.value0)(protocolRef)();
              log("Created token " + action2.value1)();
              return simState;
            }
            ;
            log("Failed to create token " + action2.value1)();
            return simState;
          }
          ;
          return simState;
        };
      };
    };
  };
  var collectAndContributePoolFees = function(state3) {
    var collectPoolFees = function(accFees) {
      return function(v) {
        var totalFeeGrowth = v.value1.feeGrowthGlobal0X128 + v.value1.feeGrowthGlobal1X128;
        var collectedFees = function() {
          var $134 = v.value1.liquidity > 0 && totalFeeGrowth > 0;
          if ($134) {
            return min11(totalFeeGrowth * v.value1.liquidity / 1e6)(v.value1.liquidity * 0.01);
          }
          ;
          return 0;
        }();
        return function __do7() {
          when5(collectedFees > 0)(log("Pool " + (v.value0 + (" collected " + (show16(collectedFees) + " FeelsSOL in fees")))))();
          return accFees + collectedFees;
        };
      };
    };
    return function __do7() {
      var pools = getAllPools(state3.poolRegistry)();
      var totalFees = foldM3(collectPoolFees)(0)(pools)();
      return when5(totalFees > 0)(function __do8() {
        log("Collecting " + (show16(totalFees) + " FeelsSOL in swap fees for POL"))();
        return contribute(state3.polState)(totalFees)();
      })();
    };
  };
  var executeSimulationBlock = function(protocolRef) {
    return function(config) {
      return function(simState) {
        return function(blockNum) {
          return function __do7() {
            when5(blockNum === 1)(function __do8() {
              log("TRACE: executeSimulationBlock called for Block 1 - WHO IS CALLING ME?")();
              return log("If executeSimulationWithProtocol was called, we should have seen chain account logs")();
            })();
            log("=== Block " + (show17(blockNum) + " ==="))();
            var priceMovement = generateMarketScenario(config)(blockNum)();
            var newPrice = simState.currentPrice * (1 + priceMovement);
            var baseTime = currentTime();
            var simulatedTimestamp = baseTime + toNumber2(blockNum) * 5e3;
            updatePriceWithTimestamp(newPrice)(simulatedTimestamp)(simState.oracle)();
            log("Updated oracle price to: " + (show16(newPrice) + " at simulated time"))();
            var protocolState = read(protocolRef)();
            updatePOLDistribution(protocolState)(config.scenario)(blockNum)();
            var polAllocations = getAllAllocations(protocolState.polState)();
            var polAllocationMap = fromFoldable7(map33(function(alloc) {
              return new Tuple(alloc.poolId, alloc.permanentAllocated);
            })(polAllocations));
            var polSnapshot = {
              block: blockNum,
              timestamp: simulatedTimestamp,
              allocations: polAllocationMap
            };
            updatePoolHealthMetrics(protocolState)();
            var tradingActions = generateTradingSequence(config)({
              accounts: simState.accounts,
              currentBlock: blockNum,
              actionHistory: simState.actionHistory,
              oracle: simState.oracle
            })();
            log("Generated " + (show17(length3(tradingActions)) + (" orders for block " + show17(blockNum))))();
            var updatedSimState = foldM3(executeProtocolAction(protocolRef))(simState)(tradingActions)();
            var protocolState$prime = read(protocolRef)();
            collectAndContributePoolFees(protocolState$prime)();
            var marketSnapshot = takeMarketSnapshot(updatedSimState.oracle)();
            var finalProtocolState = read(protocolRef)();
            var metrics = getProtocolMetrics(finalProtocolState)();
            return {
              accounts: updatedSimState.accounts,
              feelsSOL: updatedSimState.feelsSOL,
              nextPositionId: updatedSimState.nextPositionId,
              oracle: updatedSimState.oracle,
              poolRegistry: updatedSimState.poolRegistry,
              currentBlock: blockNum,
              currentPrice: marketSnapshot.spot,
              priceHistory: cons({
                price: marketSnapshot.spot,
                timestamp: marketSnapshot.timestamp
              })(updatedSimState.priceHistory),
              actionHistory: append12(updatedSimState.actionHistory)(tradingActions),
              polAllocationHistory: cons(polSnapshot)(updatedSimState.polAllocationHistory)
            };
          };
        };
      };
    };
  };
  var executeSimulationWithProtocol = function(protocolRef) {
    return function(config) {
      return function(initialSimState) {
        return function __do7() {
          log("executeSimulationWithProtocol called - ENTERING FUNCTION")();
          var initialProtocolState = read(protocolRef)();
          log("Read initial protocol state")();
          log("Creating chain accounts for " + (show17(length3(initialSimState.accounts)) + " simulated agents..."))();
          traverse_7(function(account) {
            return function __do8() {
              log("Processing account " + (account.id + (" with balance " + show16(account.jitoSOLBalance))))();
              var result = updateChainAccountBalance(initialProtocolState.accounts)(account.id)(account.jitoSOLBalance)();
              if (result instanceof Right) {
                var balance = getChainAccountBalance(initialProtocolState.accounts)(account.id)();
                return log("Created chain account for " + (account.id + (" with " + (show16(balance) + " JitoSOL"))))();
              }
              ;
              if (result instanceof Left) {
                return log("Failed to create chain account for " + (account.id + (": " + result.value0)))();
              }
              ;
              throw new Error("Failed pattern match at Simulation.ProtocolEngine (line 71, column 5 - line 76, column 91): " + [result.constructor.name]);
            };
          })(initialSimState.accounts)();
          var finalState = foldM3(executeSimulationBlock(protocolRef)(config))(initialSimState)(range2(1)(config.simulationBlocks))();
          var finalProtocolState = read(protocolRef)();
          return {
            finalSimState: finalState,
            finalProtocolState
          };
        };
      };
    };
  };

  // output/Data.Char/index.js
  var toCharCode2 = /* @__PURE__ */ fromEnum(boundedEnumChar);

  // output/UI.Integration/index.js
  var show18 = /* @__PURE__ */ show(showInt);
  var show19 = /* @__PURE__ */ show(showProtocolError);
  var pure25 = /* @__PURE__ */ pure(applicativeEffect);
  var show23 = /* @__PURE__ */ show(showNumber);
  var traverse4 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
  var map34 = /* @__PURE__ */ map(functorArray);
  var show33 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(showString));
  var traverse_8 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray);
  var toUnfoldable5 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var compare5 = /* @__PURE__ */ compare(ordNumber);
  var show42 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(showNumber));
  var lookup9 = /* @__PURE__ */ lookup(ordString);
  var max11 = /* @__PURE__ */ max(ordNumber);
  var mod5 = /* @__PURE__ */ mod(euclideanRingInt);
  var $$void8 = /* @__PURE__ */ $$void(functorEffect);
  var show52 = /* @__PURE__ */ show(showBoolean);
  var refreshProtocolData = function(protocol2) {
    return function(currentUser) {
      return function __do7() {
        log("Refreshing protocol data...")();
        var state3 = read(protocol2.state)();
        var posResult = executeQuery(new GetUserPositions(currentUser))(state3)();
        (function() {
          if (posResult instanceof Right && posResult.value0 instanceof PositionList) {
            return log("Found " + (show18(length3(posResult.value0.value0)) + " user positions"))();
          }
          ;
          if (posResult instanceof Left) {
            return log("Failed to get user positions: " + show19(posResult.value0))();
          }
          ;
          return unit;
        })();
        var offersResult = executeQuery(GetLenderOffers.value)(state3)();
        (function() {
          if (offersResult instanceof Right && offersResult.value0 instanceof LenderOfferList) {
            return log("Found " + (show18(length3(offersResult.value0.value0)) + " lender offers"))();
          }
          ;
          if (offersResult instanceof Left) {
            return log("Failed to get lender offers: " + show19(offersResult.value0))();
          }
          ;
          return unit;
        })();
        var statsResult = executeQuery(GetSystemStats.value)(state3)();
        if (statsResult instanceof Right && statsResult.value0 instanceof SystemStatsResult) {
          return log("Updated protocol stats - TVL: " + (show23(statsResult.value0.value0.totalValueLocked) + (", Users: " + show18(statsResult.value0.value0.totalUsers))))();
        }
        ;
        if (statsResult instanceof Left) {
          return log("Failed to get protocol stats: " + show19(statsResult.value0))();
        }
        ;
        return unit;
      };
    };
  };
  var processSimulationResults = function(protocol2) {
    return function(finalState) {
      return function(_results) {
        var processTokenCreation = function(_protocol) {
          return function(action2) {
            if (action2 instanceof CreateToken) {
              return function __do7() {
                log("Creating token: " + (action2.value1 + (" for user: " + action2.value0)))();
                var state3 = read(protocol2.state)();
                var result = executeCommand(new CreateToken2(action2.value0, action2.value1, action2.value2))(state3)();
                if (result instanceof Right && result.value0.value1 instanceof TokenCreated) {
                  write(result.value0.value0)(protocol2.state)();
                  log("Successfully created token: " + action2.value1)();
                  var verifyResult = executeQuery(GetAllTokens.value)(result.value0.value0)();
                  if (verifyResult instanceof Right && verifyResult.value0 instanceof TokenList) {
                    return log("Token list after creation has " + (show18(length3(verifyResult.value0.value0)) + " tokens"))();
                  }
                  ;
                  return log("Failed to verify token creation")();
                }
                ;
                if (result instanceof Right) {
                  write(result.value0.value0)(protocol2.state)();
                  return log("Token created but unexpected response type")();
                }
                ;
                if (result instanceof Left) {
                  return log("Failed to create token: " + show19(result.value0))();
                }
                ;
                throw new Error("Failed pattern match at UI.Integration (line 362, column 9 - line 375, column 67): " + [result.constructor.name]);
              };
            }
            ;
            return pure25(unit);
          };
        };
        var isCreateTokenAction = function(action2) {
          if (action2 instanceof CreateToken) {
            return true;
          }
          ;
          return false;
        };
        var tokenCreationActions = filter(isCreateTokenAction)(finalState.actionHistory);
        return function __do7() {
          log("Processing " + (show18(length3(tokenCreationActions)) + " token creation actions"))();
          traverse4(processTokenCreation(protocol2))(tokenCreationActions)();
          var state22 = read(protocol2.state)();
          var allTokensResult = executeQuery(GetAllTokens.value)(state22)();
          var allTokens = function() {
            if (allTokensResult instanceof Right && allTokensResult.value0 instanceof TokenList) {
              return allTokensResult.value0.value0;
            }
            ;
            return [];
          }();
          log("Found " + (show18(length3(allTokens)) + " total tokens"))();
          var tokenTickers = map34(function(token) {
            return token.ticker;
          })(allTokens);
          log("Token tickers: " + show33(tokenTickers))();
          var protocolState = read(protocol2.state)();
          var oracleState = read(protocolState.oracle)();
          log("Oracle has " + (show18(length3(oracleState.priceHistory)) + " price observations"))();
          var polState = read(protocolState.polState)();
          log("Current POL reserves: " + (show23(polState.totalPOL) + " FeelsSOL"))();
          log("POL state details - unallocated: " + (show23(polState.unallocated) + (", allocations: " + show18(size3(polState.poolAllocations)))))();
          log("POL allocation history has " + (show18(length3(finalState.polAllocationHistory)) + " snapshots"))();
          (function() {
            var v = head(finalState.polAllocationHistory);
            if (v instanceof Just) {
              log("Final POL allocations at block " + (show18(v.value0.block) + ":"))();
              return traverse_8(function(v1) {
                return log("  Pool " + (v1.value0 + (": " + (show23(v1.value1) + " POL"))));
              })(toUnfoldable5(v.value0.allocations))();
            }
            ;
            if (v instanceof Nothing) {
              return log("No POL allocation history found")();
            }
            ;
            throw new Error("Failed pattern match at UI.Integration (line 197, column 3 - line 203, column 53): " + [v.constructor.name]);
          })();
          var sortedHistory = sortBy(function(a2) {
            return function(b2) {
              return compare5(a2.timestamp)(b2.timestamp);
            };
          })(oracleState.priceHistory);
          var tokenCreationMap = foldl2(function(acc) {
            return function(v) {
              if (v.value1 instanceof CreateToken) {
                var creationTime = function() {
                  var v1 = head(sortedHistory);
                  if (v1 instanceof Just) {
                    return v1.value0.timestamp + toNumber2(v.value0) * 5e3;
                  }
                  ;
                  if (v1 instanceof Nothing) {
                    return 0;
                  }
                  ;
                  throw new Error("Failed pattern match at UI.Integration (line 214, column 32 - line 216, column 33): " + [v1.constructor.name]);
                }();
                return cons(new Tuple(v.value1.value1, creationTime))(acc);
              }
              ;
              return acc;
            };
          })([])(zip(range2(0)(length3(tokenCreationActions) - 1 | 0))(tokenCreationActions));
          var timestamps = map34(function(v) {
            return v.timestamp;
          })(sortedHistory);
          log("Sorted history timestamps: " + (show42(take(20)(timestamps)) + (" ... " + show42(drop(length3(timestamps) - 5 | 0)(timestamps)))))();
          var _findFirstGap = function(nums) {
            if (nums.length === 0) {
              return Nothing.value;
            }
            ;
            if (nums.length === 1) {
              return Nothing.value;
            }
            ;
            var pairs = zip(nums)(drop(1)(nums));
            var gapPairs = filter(function(v2) {
              return (v2.value1 - v2.value0 | 0) > 1;
            })(pairs);
            var v = head(gapPairs);
            if (v instanceof Just) {
              return new Just("Gap from block " + (show18(v.value0.value0) + (" to " + show18(v.value0.value1))));
            }
            ;
            if (v instanceof Nothing) {
              return Nothing.value;
            }
            ;
            throw new Error("Failed pattern match at UI.Integration (line 234, column 14 - line 236, column 34): " + [v.constructor.name]);
          };
          (function() {
            if (Nothing.value instanceof Just) {
              return log("WARNING: Found gap in block sequence: " + Nothing.value.value0)();
            }
            ;
            if (Nothing.value instanceof Nothing) {
              return unit;
            }
            ;
            throw new Error("Failed pattern match at UI.Integration (line 238, column 3 - line 240, column 25): " + [Nothing.value.constructor.name]);
          })();
          var convertedPriceHistory = map34(function(obs) {
            var existingTokensAtTime = filter(function(token) {
              var wasCreated = function() {
                var v = find2(function(action2) {
                  if (action2 instanceof CreateToken) {
                    return action2.value1 === token.ticker;
                  }
                  ;
                  return false;
                })(tokenCreationActions);
                if (v instanceof Just) {
                  return true;
                }
                ;
                if (v instanceof Nothing) {
                  return false;
                }
                ;
                throw new Error("Failed pattern match at UI.Integration (line 258, column 32 - line 264, column 37): " + [v.constructor.name]);
              }();
              return wasCreated;
            })(allTokens);
            var tokenPrices = map34(function(token) {
              var poolId = token.ticker + "/FeelsSOL";
              var polAllocationAtTime = function() {
                var v2 = find2(function(snapshot) {
                  return snapshot.timestamp <= obs.timestamp;
                })(finalState.polAllocationHistory);
                if (v2 instanceof Just) {
                  return fromMaybe(0)(lookup9(poolId)(v2.value0.allocations));
                }
                ;
                if (v2 instanceof Nothing) {
                  return 0;
                }
                ;
                throw new Error("Failed pattern match at UI.Integration (line 278, column 41 - line 280, column 35): " + [v2.constructor.name]);
              }();
              var v = unsafePerformEffect(function __do8() {
                log("Looking up pool " + (poolId + (" at timestamp " + show23(obs.timestamp))))();
                return log("  POL allocation at this time: " + show23(polAllocationAtTime))();
              });
              var tickerSum = (length7(token.ticker) * 100 | 0) + foldl2(function(sum10) {
                return function(i2) {
                  return sum10 + (i2 * 17 | 0) | 0;
                };
              })(0)(range2(0)(length7(token.ticker) - 1 | 0)) | 0;
              var polFloorPrice = max11(0.01)(polAllocationAtTime / 1e5);
              var marketTrend = function() {
                var v12 = find2(function(v2) {
                  return v2.value0 === token.ticker;
                })(tokenCreationMap);
                if (v12 instanceof Just) {
                  var tickerHash = foldl2(function(h) {
                    return function(c) {
                      return (h * 31 | 0) + toCharCode2(c) | 0;
                    };
                  })(0)(toCharArray(token.ticker));
                  var phase = toNumber2(mod5(tickerHash)(360)) * 0.0174533;
                  var frequency = 0.05 + toNumber2(mod5(tickerHash)(20)) * 0.01;
                  var age = (obs.timestamp - v12.value0.value1) / 1e6;
                  var volatility2 = max11(0.05)(0.3 * exp(-age / 100));
                  var movement = sin(age * frequency + phase) * volatility2;
                  return 1 + movement;
                }
                ;
                if (v12 instanceof Nothing) {
                  return 1;
                }
                ;
                throw new Error("Failed pattern match at UI.Integration (line 296, column 33 - line 310, column 35): " + [v12.constructor.name]);
              }();
              var basePrice = 0.8 + toNumber2(mod5(tickerSum)(40)) * 0.01;
              var v1 = unsafePerformEffect(log("  Floor price for " + (token.ticker + (" at block " + (show18(length3(finalState.polAllocationHistory) - length3(filter(function(s) {
                return s.timestamp > obs.timestamp;
              })(finalState.polAllocationHistory)) | 0) + (": " + (show23(polAllocationAtTime) + (" / " + (show23(1e5) + (" = " + (show23(polAllocationAtTime / 1e5) + (" -> " + show23(polFloorPrice)))))))))))));
              var rawPrice = basePrice * marketTrend * obs.price;
              var currentPrice = max11(polFloorPrice)(rawPrice * 1.1);
              return {
                ticker: token.ticker,
                price: currentPrice,
                polFloor: polFloorPrice,
                live: token.live
              };
            })(existingTokensAtTime);
            var estimatedPOL = function() {
              var $131 = polState.totalPOL > 0;
              if ($131) {
                return polState.totalPOL;
              }
              ;
              return 1e4;
            }();
            return {
              timestamp: obs.timestamp,
              block: 0,
              price: obs.price,
              polValue: estimatedPOL,
              tokens: tokenPrices
            };
          })(sortedHistory);
          log("Converted " + (show18(length3(convertedPriceHistory)) + " price observations to chart format"))();
          log("Simulation used shared lending book - protocol state automatically updated")();
          return convertedPriceHistory;
        };
      };
    };
  };
  var initializeRemoteActions = function __do5() {
    var _ref = $$new(Nothing.value)();
    registerRemoteAction("runSimulation")(function(_params) {
      return function __do7() {
        log("Remote action: runSimulation triggered via WebSocket")();
        var button2 = getElementById("run-simulation-btn")();
        var v = toMaybe(button2);
        if (v instanceof Just) {
          log("Found simulation button, clicking...")();
          $$void8(setTimeout2(function __do8() {
            var success = triggerUIAction("runSimulation")();
            log("Trigger result: " + show52(success))();
            return unit;
          })(100))();
          return {
            status: "success",
            message: "Simulation button click scheduled"
          };
        }
        ;
        if (v instanceof Nothing) {
          log("ERROR: Simulation button not found!")();
          return {
            status: "error",
            message: "Simulation button not found"
          };
        }
        ;
        throw new Error("Failed pattern match at UI.Integration (line 64, column 5 - line 76, column 73): " + [v.constructor.name]);
      };
    })();
    registerRemoteAction("refreshData")(function(v) {
      return function __do7() {
        log("Remote action: refreshData triggered")();
        $$void8(triggerUIAction("refreshData"))();
        return unit;
      };
    })();
    registerRemoteAction("debugChartElements")(function(v) {
      return function __do7() {
        log("Remote action: debugChartElements triggered")();
        var canvas2 = getElementById("price-chart")();
        (function() {
          var v1 = toMaybe(canvas2);
          if (v1 instanceof Just) {
            return log("Canvas element found")();
          }
          ;
          if (v1 instanceof Nothing) {
            return log("Canvas element NOT found")();
          }
          ;
          throw new Error("Failed pattern match at UI.Integration (line 89, column 5 - line 91, column 48): " + [v1.constructor.name]);
        })();
        var dataEl = getElementById("chart-data-hidden")();
        (function() {
          var v1 = toMaybe(dataEl);
          if (v1 instanceof Just) {
            var content3 = getTextContent(v1.value0)();
            log("Data element found, length: " + show18(length7(content3)))();
            return log("Data preview: " + take4(100)(content3))();
          }
          ;
          if (v1 instanceof Nothing) {
            return log("Data element NOT found")();
          }
          ;
          throw new Error("Failed pattern match at UI.Integration (line 94, column 5 - line 99, column 46): " + [v1.constructor.name]);
        })();
        return unit;
      };
    })();
    return registerRemoteAction("testTokenValidation")(function(v) {
      return function __do7() {
        log("Remote action: testTokenValidation triggered")();
        $$void8(triggerUIAction("testTokenValidation"))();
        return unit;
      };
    })();
  };

  // output/UI.State/index.js
  var Initialize2 = /* @__PURE__ */ function() {
    function Initialize3() {
    }
    ;
    Initialize3.value = new Initialize3();
    return Initialize3;
  }();
  var RefreshData = /* @__PURE__ */ function() {
    function RefreshData2() {
    }
    ;
    RefreshData2.value = new RefreshData2();
    return RefreshData2;
  }();
  var RenderChart = /* @__PURE__ */ function() {
    function RenderChart2() {
    }
    ;
    RenderChart2.value = new RenderChart2();
    return RenderChart2;
  }();
  var UpdateInputAmount = /* @__PURE__ */ function() {
    function UpdateInputAmount2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateInputAmount2.create = function(value0) {
      return new UpdateInputAmount2(value0);
    };
    return UpdateInputAmount2;
  }();
  var SelectFromAsset = /* @__PURE__ */ function() {
    function SelectFromAsset2(value0) {
      this.value0 = value0;
    }
    ;
    SelectFromAsset2.create = function(value0) {
      return new SelectFromAsset2(value0);
    };
    return SelectFromAsset2;
  }();
  var SelectToAsset = /* @__PURE__ */ function() {
    function SelectToAsset2(value0) {
      this.value0 = value0;
    }
    ;
    SelectToAsset2.create = function(value0) {
      return new SelectToAsset2(value0);
    };
    return SelectToAsset2;
  }();
  var SelectTargetToken = /* @__PURE__ */ function() {
    function SelectTargetToken2(value0) {
      this.value0 = value0;
    }
    ;
    SelectTargetToken2.create = function(value0) {
      return new SelectTargetToken2(value0);
    };
    return SelectTargetToken2;
  }();
  var SetTermType = /* @__PURE__ */ function() {
    function SetTermType2(value0) {
      this.value0 = value0;
    }
    ;
    SetTermType2.create = function(value0) {
      return new SetTermType2(value0);
    };
    return SetTermType2;
  }();
  var SetLeverageType = /* @__PURE__ */ function() {
    function SetLeverageType2(value0) {
      this.value0 = value0;
    }
    ;
    SetLeverageType2.create = function(value0) {
      return new SetLeverageType2(value0);
    };
    return SetLeverageType2;
  }();
  var ExecuteExchange = /* @__PURE__ */ function() {
    function ExecuteExchange2() {
    }
    ;
    ExecuteExchange2.value = new ExecuteExchange2();
    return ExecuteExchange2;
  }();
  var CreateTokenUI = /* @__PURE__ */ function() {
    function CreateTokenUI2() {
    }
    ;
    CreateTokenUI2.value = new CreateTokenUI2();
    return CreateTokenUI2;
  }();
  var UpdateTokenTicker = /* @__PURE__ */ function() {
    function UpdateTokenTicker2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateTokenTicker2.create = function(value0) {
      return new UpdateTokenTicker2(value0);
    };
    return UpdateTokenTicker2;
  }();
  var UpdateTokenName = /* @__PURE__ */ function() {
    function UpdateTokenName2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateTokenName2.create = function(value0) {
      return new UpdateTokenName2(value0);
    };
    return UpdateTokenName2;
  }();
  var SelectLaunch = /* @__PURE__ */ function() {
    function SelectLaunch2(value0) {
      this.value0 = value0;
    }
    ;
    SelectLaunch2.create = function(value0) {
      return new SelectLaunch2(value0);
    };
    return SelectLaunch2;
  }();
  var UpdateLaunchBidAmount = /* @__PURE__ */ function() {
    function UpdateLaunchBidAmount2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateLaunchBidAmount2.create = function(value0) {
      return new UpdateLaunchBidAmount2(value0);
    };
    return UpdateLaunchBidAmount2;
  }();
  var UpdateLaunchPriorityFee = /* @__PURE__ */ function() {
    function UpdateLaunchPriorityFee2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateLaunchPriorityFee2.create = function(value0) {
      return new UpdateLaunchPriorityFee2(value0);
    };
    return UpdateLaunchPriorityFee2;
  }();
  var SubmitLaunchBid = /* @__PURE__ */ function() {
    function SubmitLaunchBid2() {
    }
    ;
    SubmitLaunchBid2.value = new SubmitLaunchBid2();
    return SubmitLaunchBid2;
  }();
  var RefreshLaunches = /* @__PURE__ */ function() {
    function RefreshLaunches2() {
    }
    ;
    RefreshLaunches2.value = new RefreshLaunches2();
    return RefreshLaunches2;
  }();
  var ProcessLaunchBatch = /* @__PURE__ */ function() {
    function ProcessLaunchBatch2(value0) {
      this.value0 = value0;
    }
    ;
    ProcessLaunchBatch2.create = function(value0) {
      return new ProcessLaunchBatch2(value0);
    };
    return ProcessLaunchBatch2;
  }();
  var UpdateSimulationConfig = /* @__PURE__ */ function() {
    function UpdateSimulationConfig2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateSimulationConfig2.create = function(value0) {
      return new UpdateSimulationConfig2(value0);
    };
    return UpdateSimulationConfig2;
  }();
  var RunSimulation = /* @__PURE__ */ function() {
    function RunSimulation2() {
    }
    ;
    RunSimulation2.value = new RunSimulation2();
    return RunSimulation2;
  }();
  var defaultSimulationConfig = /* @__PURE__ */ function() {
    return {
      scenario: BullMarket.value,
      numAccounts: 5,
      simulationBlocks: 100,
      initialJitoSOLPrice: 1.22,
      priceVolatility: 0.02,
      accountProfiles: [Whale.value, Aggressive.value, Conservative.value],
      actionFrequency: 1,
      juniorTranchePreference: 0.5
    };
  }();
  var initialUIState = /* @__PURE__ */ function() {
    return {
      currentUser: "main-user",
      api: Nothing.value,
      inputAmount: 100,
      selectedFromAsset: "jitosol",
      selectedToAsset: "position-spot",
      selectedTermType: "spot",
      selectedLeverage: "senior",
      selectedTargetToken: Nothing.value,
      simulationConfig: defaultSimulationConfig,
      simulationResults: Nothing.value,
      simulationRunning: false,
      tokenTicker: "",
      tokenName: "",
      tokenValidationErrors: [],
      selectedLaunchId: Nothing.value,
      launchBidAmount: 100,
      launchPriorityFeePercent: 10,
      activeLaunches: [],
      loading: true,
      error: Nothing.value,
      userPositions: [],
      lenderOffers: [],
      protocolStats: Nothing.value,
      userTokens: [],
      jitoBalance: 1e3,
      feelsBalance: 0,
      priceHistory: []
    };
  }();

  // output/UI.Action/index.js
  var append13 = /* @__PURE__ */ append(semigroupArray);
  var bind5 = /* @__PURE__ */ bind(bindHalogenM);
  var get2 = /* @__PURE__ */ get(monadStateHalogenM);
  var pure26 = /* @__PURE__ */ pure(applicativeHalogenM);
  var discard5 = /* @__PURE__ */ discard(discardUnit);
  var discard12 = /* @__PURE__ */ discard5(bindHalogenM);
  var modify_3 = /* @__PURE__ */ modify_2(monadStateHalogenM);
  var when6 = /* @__PURE__ */ when(applicativeHalogenM);
  var unless2 = /* @__PURE__ */ unless(applicativeHalogenM);
  var show20 = /* @__PURE__ */ show(showProtocolError);
  var pure110 = /* @__PURE__ */ pure(applicativeEffect);
  var show110 = /* @__PURE__ */ show(showInt);
  var show24 = /* @__PURE__ */ show(showNumber);
  var $$void9 = /* @__PURE__ */ $$void(functorHalogenM);
  var void1 = /* @__PURE__ */ $$void(functorEffect);
  var traverse_9 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray);
  var validateTokenInput = function(ticker) {
    return function(name16) {
      return function(existingTokens) {
        var trimmedTicker = trim(ticker);
        var trimmedName = trim(name16);
        var tickerLengthErrors = function() {
          var $77 = trimmedTicker === "";
          if ($77) {
            return [];
          }
          ;
          var $78 = length7(trimmedTicker) < 3;
          if ($78) {
            return ["Ticker must be at least 3 characters"];
          }
          ;
          var $79 = length7(trimmedTicker) > 10;
          if ($79) {
            return ["Ticker must be at most 10 characters"];
          }
          ;
          return [];
        }();
        var reservedTickerErrors = function() {
          var $80 = trimmedTicker === "";
          if ($80) {
            return [];
          }
          ;
          var $81 = toUpper(trimmedTicker) === "SOL" || (toUpper(trimmedTicker) === "JITO" || (toUpper(trimmedTicker) === "FEELSSOL" || toUpper(trimmedTicker) === "JITOSOL"));
          if ($81) {
            return ["Ticker '" + (trimmedTicker + "' is reserved")];
          }
          ;
          return [];
        }();
        var duplicateTickerErrors = [];
        var duplicateNameErrors = [];
        return append13(tickerLengthErrors)(append13(duplicateTickerErrors)(append13(duplicateNameErrors)(reservedTickerErrors)));
      };
    };
  };
  var handleExecuteExchange = function(dictMonadAff) {
    var liftEffect8 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    return bind5(get2)(function(state3) {
      if (state3.api instanceof Nothing) {
        return pure26(unit);
      }
      ;
      if (state3.api instanceof Just) {
        return bind5(liftEffect8(read(state3.api.value0.state)))(function(protocolState) {
          var $83 = {
            from: state3.selectedFromAsset,
            to: state3.selectedToAsset
          };
          if ($83.from === "jitosol" && $83.to === "position-spot") {
            return discard12(liftEffect8(log("Exchange: JitoSOL -> Spot Position")))(function() {
              return modify_3(function(v) {
                var $84 = {};
                for (var $85 in v) {
                  if ({}.hasOwnProperty.call(v, $85)) {
                    $84[$85] = v[$85];
                  }
                  ;
                }
                ;
                $84.error = new Just("JitoSOL to Position exchange coming soon!");
                return $84;
              });
            });
          }
          ;
          if ($83.from === "jitosol" && $83.to === "position-term") {
            return discard12(liftEffect8(log("Exchange: JitoSOL -> Term Position")))(function() {
              return modify_3(function(v) {
                var $89 = {};
                for (var $90 in v) {
                  if ({}.hasOwnProperty.call(v, $90)) {
                    $89[$90] = v[$90];
                  }
                  ;
                }
                ;
                $89.error = new Just("JitoSOL to Term Position exchange coming soon!");
                return $89;
              });
            });
          }
          ;
          return modify_3(function(v) {
            var $94 = {};
            for (var $95 in v) {
              if ({}.hasOwnProperty.call(v, $95)) {
                $94[$95] = v[$95];
              }
              ;
            }
            ;
            $94.error = new Just("This exchange route is not yet implemented");
            return $94;
          });
        });
      }
      ;
      throw new Error("Failed pattern match at UI.Action (line 250, column 3 - line 272, column 84): " + [state3.api.constructor.name]);
    });
  };
  var handleRunSimulation = function(dictMonadAff) {
    var liftEffect8 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    return bind5(get2)(function(currentState) {
      return discard12(when6(currentState.simulationRunning)(discard12(liftEffect8(log("Simulation already running, skipping...")))(function() {
        return pure26(unit);
      })))(function() {
        return unless2(currentState.simulationRunning)(discard12(liftEffect8(log("Starting simulation...")))(function() {
          return discard12(modify_3(function(v) {
            var $98 = {};
            for (var $99 in v) {
              if ({}.hasOwnProperty.call(v, $99)) {
                $98[$99] = v[$99];
              }
              ;
            }
            ;
            $98.simulationRunning = true;
            $98.error = Nothing.value;
            return $98;
          }))(function() {
            return bind5(get2)(function(state3) {
              if (state3.api instanceof Nothing) {
                return modify_3(function(v) {
                  var $102 = {};
                  for (var $103 in v) {
                    if ({}.hasOwnProperty.call(v, $103)) {
                      $102[$103] = v[$103];
                    }
                    ;
                  }
                  ;
                  $102.error = new Just("Protocol not initialized");
                  return $102;
                });
              }
              ;
              if (state3.api instanceof Just) {
                return bind5(liftEffect8(read(state3.api.value0.state)))(function(protocolState) {
                  return bind5(liftEffect8(initSimulationWithPoolRegistry(state3.simulationConfig)(protocolState.poolRegistry)(protocolState.oracle)))(function(simState) {
                    return discard12(liftEffect8(log("Seeding initial JitoSOL/FeelsSOL liquidity...")))(function() {
                      return bind5(liftEffect8(function __do7() {
                        log("Creating first liquidity position...")();
                        var state1 = read(state3.api.value0.state)();
                        var result1 = executeCommand(new CreatePosition("liquidity-bot", JitoSOL.value, 500, FeelsSOL.value, 610, spotDuration, Senior.value, false, Nothing.value))(state1)();
                        (function() {
                          if (result1 instanceof Right) {
                            write(result1.value0.value0)(state3.api.value0.state)();
                            return log("First liquidity position created")();
                          }
                          ;
                          if (result1 instanceof Left) {
                            return log("Failed to create first position: " + show20(result1.value0))();
                          }
                          ;
                          throw new Error("Failed pattern match at UI.Action (line 311, column 16 - line 315, column 78): " + [result1.constructor.name]);
                        })();
                        log("Creating second liquidity position...")();
                        var state22 = read(state3.api.value0.state)();
                        var result2 = executeCommand(new CreatePosition("liquidity-bot", FeelsSOL.value, 500, JitoSOL.value, 410, spotDuration, Senior.value, false, Nothing.value))(state22)();
                        (function() {
                          if (result2 instanceof Right) {
                            write(result2.value0.value0)(state3.api.value0.state)();
                            return log("Second liquidity position created")();
                          }
                          ;
                          if (result2 instanceof Left) {
                            return log("Failed to create second position: " + show20(result2.value0))();
                          }
                          ;
                          throw new Error("Failed pattern match at UI.Action (line 320, column 16 - line 324, column 79): " + [result2.constructor.name]);
                        })();
                        log("Finished seeding liquidity")();
                        return unit;
                      }))(function() {
                        return discard12(liftEffect8(log("About to call executeSimulationWithProtocol...")))(function() {
                          return bind5(liftEffect8(executeSimulationWithProtocol(state3.api.value0.state)(state3.simulationConfig)(simState)))(function(executionResult) {
                            return bind5(liftEffect8(calculateResults(state3.simulationConfig)(executionResult.finalSimState)))(function(results) {
                              return bind5(liftEffect8(getProtocolMetrics(executionResult.finalProtocolState)))(function(protocolMetrics) {
                                return discard12(liftEffect8(log("Simulation completed with " + (show110(results.totalUsers) + " users"))))(function() {
                                  return discard12(liftEffect8(log("POL reserves: " + show24(protocolMetrics.polReserves))))(function() {
                                    return discard12(liftEffect8(log("Total fees collected: " + show24(protocolMetrics.totalFeesCollected))))(function() {
                                      return bind5(liftEffect8(processSimulationResults(state3.api.value0)(executionResult.finalSimState)(results)))(function(priceHistory) {
                                        return discard12(handleAction(dictMonadAff)(RefreshData.value))(function() {
                                          return discard12(modify_3(function(v) {
                                            var $115 = {};
                                            for (var $116 in v) {
                                              if ({}.hasOwnProperty.call(v, $116)) {
                                                $115[$116] = v[$116];
                                              }
                                              ;
                                            }
                                            ;
                                            $115.simulationRunning = false;
                                            $115.simulationResults = new Just(results);
                                            $115.priceHistory = priceHistory;
                                            return $115;
                                          }))(function() {
                                            return discard12(liftEffect8(function __do7() {
                                              log("Setting chart data with " + (show110(length3(priceHistory)) + " points"))();
                                              return setChartData(priceHistory)();
                                            }))(function() {
                                              return discard12(liftEffect8(log("State updated, scheduling chart render...")))(function() {
                                                return $$void9(fork(discard12(liftEffect8(void1(setTimeout2(pure110(unit))(200))))(function() {
                                                  return handleAction(dictMonadAff)(RenderChart.value);
                                                })));
                                              });
                                            });
                                          });
                                        });
                                      });
                                    });
                                  });
                                });
                              });
                            });
                          });
                        });
                      });
                    });
                  });
                });
              }
              ;
              throw new Error("Failed pattern match at UI.Action (line 291, column 5 - line 372, column 35): " + [state3.api.constructor.name]);
            });
          });
        }));
      });
    });
  };
  var handleCreateToken2 = function(dictMonadAff) {
    var liftEffect8 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    return bind5(get2)(function(state3) {
      if (state3.api instanceof Nothing) {
        return pure26(unit);
      }
      ;
      if (state3.api instanceof Just) {
        var ticker = trim(state3.tokenTicker);
        var name16 = trim(state3.tokenName);
        var errors = validateTokenInput(ticker)(name16)(state3.userTokens);
        return discard12(modify_3(function(v) {
          var $120 = {};
          for (var $121 in v) {
            if ({}.hasOwnProperty.call(v, $121)) {
              $120[$121] = v[$121];
            }
            ;
          }
          ;
          $120.tokenValidationErrors = errors;
          return $120;
        }))(function() {
          var $123 = $$null(errors) && (ticker !== "" && name16 !== "");
          if ($123) {
            return bind5(liftEffect8(read(state3.api.value0.state)))(function(protocolState) {
              return bind5(liftEffect8(executeCommand(new CreateToken2(state3.currentUser, ticker, name16))(protocolState)))(function(result) {
                if (result instanceof Right && result.value0.value1 instanceof TokenCreated) {
                  return discard12(liftEffect8(write(result.value0.value0)(state3.api.value0.state)))(function() {
                    return discard12(liftEffect8(log("Token created successfully")))(function() {
                      return discard12(modify_3(function(s) {
                        var $125 = {};
                        for (var $126 in s) {
                          if ({}.hasOwnProperty.call(s, $126)) {
                            $125[$126] = s[$126];
                          }
                          ;
                        }
                        ;
                        $125.tokenTicker = "";
                        $125.tokenName = "";
                        $125.tokenValidationErrors = [];
                        $125.error = Nothing.value;
                        return $125;
                      }))(function() {
                        return handleAction(dictMonadAff)(RefreshData.value);
                      });
                    });
                  });
                }
                ;
                if (result instanceof Right) {
                  return modify_3(function(v) {
                    var $132 = {};
                    for (var $133 in v) {
                      if ({}.hasOwnProperty.call(v, $133)) {
                        $132[$133] = v[$133];
                      }
                      ;
                    }
                    ;
                    $132.error = new Just("Unexpected result from token creation");
                    return $132;
                  });
                }
                ;
                if (result instanceof Left) {
                  return modify_3(function(v) {
                    var $136 = {};
                    for (var $137 in v) {
                      if ({}.hasOwnProperty.call(v, $137)) {
                        $136[$137] = v[$137];
                      }
                      ;
                    }
                    ;
                    $136.error = new Just(show20(result.value0));
                    return $136;
                  });
                }
                ;
                throw new Error("Failed pattern match at UI.Action (line 219, column 11 - line 235, column 54): " + [result.constructor.name]);
              });
            });
          }
          ;
          var emptyFieldErrors = append13(function() {
            var $140 = ticker === "";
            if ($140) {
              return ["Token ticker is required"];
            }
            ;
            return [];
          }())(function() {
            var $141 = name16 === "";
            if ($141) {
              return ["Token name is required"];
            }
            ;
            return [];
          }());
          return modify_3(function(s) {
            var $142 = {};
            for (var $143 in s) {
              if ({}.hasOwnProperty.call(s, $143)) {
                $142[$143] = s[$143];
              }
              ;
            }
            ;
            $142.tokenValidationErrors = append13(s.tokenValidationErrors)(emptyFieldErrors);
            return $142;
          });
        });
      }
      ;
      throw new Error("Failed pattern match at UI.Action (line 200, column 3 - line 241, column 100): " + [state3.api.constructor.name]);
    });
  };
  var handleAction = function(dictMonadAff) {
    var liftEffect8 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    var handleExecuteExchange1 = handleExecuteExchange(dictMonadAff);
    return function(v) {
      if (v instanceof Initialize2) {
        return bind5(liftEffect8(initState))(function(protocol2) {
          return discard12(modify_3(function(v1) {
            var $147 = {};
            for (var $148 in v1) {
              if ({}.hasOwnProperty.call(v1, $148)) {
                $147[$148] = v1[$148];
              }
              ;
            }
            ;
            $147.api = new Just(protocol2);
            $147.loading = false;
            return $147;
          }))(function() {
            return discard12(liftEffect8(initializeRemoteActions))(function() {
              return handleAction(dictMonadAff)(RefreshData.value);
            });
          });
        });
      }
      ;
      if (v instanceof RefreshData) {
        return bind5(get2)(function(state3) {
          if (state3.api instanceof Nothing) {
            return pure26(unit);
          }
          ;
          if (state3.api instanceof Just) {
            return discard12(liftEffect8(refreshProtocolData(state3.api.value0)(state3.currentUser)))(function() {
              return bind5(liftEffect8(read(state3.api.value0.state)))(function(protocolState) {
                return bind5(liftEffect8(executeQuery(new GetUserPositions(state3.currentUser))(protocolState)))(function(posResult) {
                  return discard12(function() {
                    if (posResult instanceof Right && posResult.value0 instanceof PositionList) {
                      return modify_3(function(v1) {
                        var $152 = {};
                        for (var $153 in v1) {
                          if ({}.hasOwnProperty.call(v1, $153)) {
                            $152[$153] = v1[$153];
                          }
                          ;
                        }
                        ;
                        $152.userPositions = posResult.value0.value0;
                        return $152;
                      });
                    }
                    ;
                    return pure26(unit);
                  }())(function() {
                    return bind5(liftEffect8(executeQuery(GetLenderOffers.value)(protocolState)))(function(offersResult) {
                      return discard12(function() {
                        if (offersResult instanceof Right && offersResult.value0 instanceof LenderOfferList) {
                          return discard12(liftEffect8(log("RefreshData: Found " + (show110(length3(offersResult.value0.value0)) + " lender offers"))))(function() {
                            return modify_3(function(v1) {
                              var $158 = {};
                              for (var $159 in v1) {
                                if ({}.hasOwnProperty.call(v1, $159)) {
                                  $158[$159] = v1[$159];
                                }
                                ;
                              }
                              ;
                              $158.lenderOffers = offersResult.value0.value0;
                              return $158;
                            });
                          });
                        }
                        ;
                        if (offersResult instanceof Left) {
                          return liftEffect8(log("RefreshData: Failed to get lender offers: " + show20(offersResult.value0)));
                        }
                        ;
                        return pure26(unit);
                      }())(function() {
                        return bind5(liftEffect8(executeQuery(GetSystemStats.value)(protocolState)))(function(statsResult) {
                          return discard12(function() {
                            if (statsResult instanceof Right && statsResult.value0 instanceof SystemStatsResult) {
                              return discard12(liftEffect8(log("RefreshData: Updated protocol stats - TVL: " + (show24(statsResult.value0.value0.totalValueLocked) + (", Users: " + show110(statsResult.value0.value0.totalUsers))))))(function() {
                                return modify_3(function(v1) {
                                  var $165 = {};
                                  for (var $166 in v1) {
                                    if ({}.hasOwnProperty.call(v1, $166)) {
                                      $165[$166] = v1[$166];
                                    }
                                    ;
                                  }
                                  ;
                                  $165.protocolStats = new Just(statsResult.value0.value0);
                                  return $165;
                                });
                              });
                            }
                            ;
                            if (statsResult instanceof Left) {
                              return liftEffect8(log("RefreshData: Failed to get protocol stats: " + show20(statsResult.value0)));
                            }
                            ;
                            return pure26(unit);
                          }())(function() {
                            return bind5(liftEffect8(executeQuery(new GetUserBalance(state3.currentUser, JitoSOL.value))(protocolState)))(function(jitoBalanceResult) {
                              return discard12(function() {
                                if (jitoBalanceResult instanceof Right && jitoBalanceResult.value0 instanceof Balance) {
                                  return modify_3(function(v1) {
                                    var $172 = {};
                                    for (var $173 in v1) {
                                      if ({}.hasOwnProperty.call(v1, $173)) {
                                        $172[$173] = v1[$173];
                                      }
                                      ;
                                    }
                                    ;
                                    $172.jitoBalance = jitoBalanceResult.value0.value0;
                                    return $172;
                                  });
                                }
                                ;
                                return pure26(unit);
                              }())(function() {
                                return bind5(liftEffect8(executeQuery(new GetUserBalance(state3.currentUser, FeelsSOL.value))(protocolState)))(function(feelsBalanceResult) {
                                  return discard12(function() {
                                    if (feelsBalanceResult instanceof Right && feelsBalanceResult.value0 instanceof Balance) {
                                      return modify_3(function(v1) {
                                        var $178 = {};
                                        for (var $179 in v1) {
                                          if ({}.hasOwnProperty.call(v1, $179)) {
                                            $178[$179] = v1[$179];
                                          }
                                          ;
                                        }
                                        ;
                                        $178.feelsBalance = feelsBalanceResult.value0.value0;
                                        return $178;
                                      });
                                    }
                                    ;
                                    return pure26(unit);
                                  }())(function() {
                                    return modify_3(function(v1) {
                                      var $183 = {};
                                      for (var $184 in v1) {
                                        if ({}.hasOwnProperty.call(v1, $184)) {
                                          $183[$184] = v1[$184];
                                        }
                                        ;
                                      }
                                      ;
                                      $183.error = Nothing.value;
                                      return $183;
                                    });
                                  });
                                });
                              });
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          }
          ;
          throw new Error("Failed pattern match at UI.Action (line 64, column 5 - line 111, column 40): " + [state3.api.constructor.name]);
        });
      }
      ;
      if (v instanceof RenderChart) {
        return discard12(liftEffect8(log("Rendering price chart...")))(function() {
          return bind5(get2)(function(currentState) {
            return discard12(liftEffect8(log("Price history length: " + show110(length3(currentState.priceHistory)))))(function() {
              return discard12(liftEffect8(log("UI: About to schedule checkAndInitializeChart")))(function() {
                return discard12(liftEffect8(void1(setTimeout2(checkAndInitializeChart)(250))))(function() {
                  return liftEffect8(log("UI: Scheduled checkAndInitializeChart for 250ms delay"));
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof UpdateInputAmount) {
        return modify_3(function(v1) {
          var $187 = {};
          for (var $188 in v1) {
            if ({}.hasOwnProperty.call(v1, $188)) {
              $187[$188] = v1[$188];
            }
            ;
          }
          ;
          $187.inputAmount = v.value0;
          return $187;
        });
      }
      ;
      if (v instanceof SelectFromAsset) {
        return modify_3(function(v1) {
          var $191 = {};
          for (var $192 in v1) {
            if ({}.hasOwnProperty.call(v1, $192)) {
              $191[$192] = v1[$192];
            }
            ;
          }
          ;
          $191.selectedFromAsset = v.value0;
          return $191;
        });
      }
      ;
      if (v instanceof SelectToAsset) {
        return modify_3(function(v1) {
          var $195 = {};
          for (var $196 in v1) {
            if ({}.hasOwnProperty.call(v1, $196)) {
              $195[$196] = v1[$196];
            }
            ;
          }
          ;
          $195.selectedToAsset = v.value0;
          return $195;
        });
      }
      ;
      if (v instanceof SelectTargetToken) {
        return modify_3(function(v1) {
          var $199 = {};
          for (var $200 in v1) {
            if ({}.hasOwnProperty.call(v1, $200)) {
              $199[$200] = v1[$200];
            }
            ;
          }
          ;
          $199.selectedTargetToken = v.value0;
          return $199;
        });
      }
      ;
      if (v instanceof SetTermType) {
        return modify_3(function(v1) {
          var $203 = {};
          for (var $204 in v1) {
            if ({}.hasOwnProperty.call(v1, $204)) {
              $203[$204] = v1[$204];
            }
            ;
          }
          ;
          $203.selectedTermType = v.value0;
          return $203;
        });
      }
      ;
      if (v instanceof SetLeverageType) {
        return modify_3(function(v1) {
          var $207 = {};
          for (var $208 in v1) {
            if ({}.hasOwnProperty.call(v1, $208)) {
              $207[$208] = v1[$208];
            }
            ;
          }
          ;
          $207.selectedLeverage = v.value0;
          return $207;
        });
      }
      ;
      if (v instanceof ExecuteExchange) {
        return handleExecuteExchange1;
      }
      ;
      if (v instanceof UpdateTokenTicker) {
        return bind5(get2)(function(state3) {
          var errors = validateTokenInput(v.value0)(state3.tokenName)(state3.userTokens);
          return discard12(liftEffect8(function __do7() {
            log("Ticker validation: " + (v.value0 + (" -> errors: " + show110(length3(errors)))))();
            return traverse_9(function($236) {
              return log(function(v1) {
                return "  - " + v1;
              }($236));
            })(errors)();
          }))(function() {
            return modify_3(function(v1) {
              var $211 = {};
              for (var $212 in v1) {
                if ({}.hasOwnProperty.call(v1, $212)) {
                  $211[$212] = v1[$212];
                }
                ;
              }
              ;
              $211.tokenTicker = v.value0;
              $211.tokenValidationErrors = errors;
              return $211;
            });
          });
        });
      }
      ;
      if (v instanceof UpdateTokenName) {
        return bind5(get2)(function(state3) {
          var errors = validateTokenInput(state3.tokenTicker)(v.value0)(state3.userTokens);
          return discard12(liftEffect8(function __do7() {
            log("Name validation: " + (v.value0 + (" -> errors: " + show110(length3(errors)))))();
            return traverse_9(function($237) {
              return log(function(v1) {
                return "  - " + v1;
              }($237));
            })(errors)();
          }))(function() {
            return modify_3(function(v1) {
              var $215 = {};
              for (var $216 in v1) {
                if ({}.hasOwnProperty.call(v1, $216)) {
                  $215[$216] = v1[$216];
                }
                ;
              }
              ;
              $215.tokenName = v.value0;
              $215.tokenValidationErrors = errors;
              return $215;
            });
          });
        });
      }
      ;
      if (v instanceof CreateTokenUI) {
        return handleCreateToken2(dictMonadAff);
      }
      ;
      if (v instanceof UpdateSimulationConfig) {
        return modify_3(function(s) {
          var $219 = {};
          for (var $220 in s) {
            if ({}.hasOwnProperty.call(s, $220)) {
              $219[$220] = s[$220];
            }
            ;
          }
          ;
          $219.simulationConfig = v.value0(s.simulationConfig);
          return $219;
        });
      }
      ;
      if (v instanceof RunSimulation) {
        return handleRunSimulation(dictMonadAff);
      }
      ;
      if (v instanceof SelectLaunch) {
        return modify_3(function(v1) {
          var $223 = {};
          for (var $224 in v1) {
            if ({}.hasOwnProperty.call(v1, $224)) {
              $223[$224] = v1[$224];
            }
            ;
          }
          ;
          $223.selectedLaunchId = new Just(v.value0);
          return $223;
        });
      }
      ;
      if (v instanceof UpdateLaunchBidAmount) {
        return modify_3(function(v1) {
          var $227 = {};
          for (var $228 in v1) {
            if ({}.hasOwnProperty.call(v1, $228)) {
              $227[$228] = v1[$228];
            }
            ;
          }
          ;
          $227.launchBidAmount = v.value0;
          return $227;
        });
      }
      ;
      if (v instanceof UpdateLaunchPriorityFee) {
        return modify_3(function(v1) {
          var $231 = {};
          for (var $232 in v1) {
            if ({}.hasOwnProperty.call(v1, $232)) {
              $231[$232] = v1[$232];
            }
            ;
          }
          ;
          $231.launchPriorityFeePercent = v.value0;
          return $231;
        });
      }
      ;
      if (v instanceof SubmitLaunchBid) {
        return liftEffect8(log("Launch bid submission not yet implemented"));
      }
      ;
      if (v instanceof RefreshLaunches) {
        return liftEffect8(log("Launch refresh not yet implemented"));
      }
      ;
      if (v instanceof ProcessLaunchBatch) {
        return liftEffect8(log("Processing batch for launch: " + v.value0));
      }
      ;
      throw new Error("Failed pattern match at UI.Action (line 44, column 16 - line 191, column 69): " + [v.constructor.name]);
    };
  };

  // output/Control.Monad.Except/index.js
  var unwrap4 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap4(runExceptT($3));
  };

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value14) {
    return value14 == null ? f : s(value14[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail2 = fail(dictMonad);
    var pure27 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value14) {
        return unsafeReadPropImpl(fail2(new TypeMismatch("object", typeOf(value14))), pure27, k, value14);
      };
    };
  };
  var readProp = function(dictMonad) {
    return unsafeReadProp(dictMonad);
  };

  // output/Web.Event.Event/foreign.js
  function _currentTarget(e) {
    return e.currentTarget;
  }

  // output/Web.Event.Event/index.js
  var currentTarget = function($5) {
    return toMaybe(_currentTarget($5));
  };

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var click2 = "click";

  // output/Halogen.HTML.Events/index.js
  var map35 = /* @__PURE__ */ map(functorMaybe);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindMaybe);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var mouseHandler = unsafeCoerce2;
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map35(Action.create)(f(ev));
      });
    };
  };
  var handler2 = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return new Just(new Action(f(ev)));
      });
    };
  };
  var onClick = /* @__PURE__ */ function() {
    var $15 = handler2(click2);
    return function($16) {
      return $15(mouseHandler($16));
    };
  }();
  var addForeignPropHandler = function(key) {
    return function(prop5) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped3(reader)(readProp2(prop5))(unsafeToForeign(a2));
          };
          return handler$prime(key)(composeKleisli2(currentTarget)(function(e) {
            return either($$const(Nothing.value))(function($85) {
              return Just.create(f($85));
            })(runExcept(go2(e)));
          }));
        };
      };
    };
  };
  var onValueChange = /* @__PURE__ */ addForeignPropHandler(change)("value")(readString2);

  // output/UI.Component/index.js
  var show21 = /* @__PURE__ */ show(showInt);
  var map36 = /* @__PURE__ */ map(functorArray);
  var type_19 = /* @__PURE__ */ type_17(isPropInputType);
  var value13 = /* @__PURE__ */ value12(isPropString);
  var div1 = /* @__PURE__ */ div(euclideanRingInt);
  var show111 = /* @__PURE__ */ show(showString);
  var show25 = /* @__PURE__ */ show(showNumber);
  var mod6 = /* @__PURE__ */ mod(euclideanRingInt);
  var renderWalletPanel = function(state3) {
    var renderToken = function(foreignToken) {
      return div2([class_("list-item")])([div2([class_("list-item__header")])([strong_([text5(foreignToken.ticker)]), text5(" " + foreignToken.name)]), div2([class_("token-status")])([function() {
        if (foreignToken.live) {
          return span3([class_("status-live")])([text5("Live")]);
        }
        ;
        return span3([class_("status-pending")])([text5("Pending")]);
      }()])]);
    };
    var renderBalance = function(label5) {
      return function(amount) {
        return div2([class_("balance-line"), style("margin: 0; padding: 2px 0; line-height: 1.2;")])([span3([class_("balance-label")])([text5(label5 + ": ")]), span3([class_("balance-value")])([text5(formatAmount(amount))])]);
      };
    };
    var formatPrice = function(p2) {
      return formatAmount(p2);
    };
    var renderPosition = function(foreignPos) {
      return div2([class_("list-item")])([div2([class_("list-item__header")])([text5("Position #" + show21(foreignPos.id)), span3([class_("status status-active")])([text5("Active")])]), div2([class_("list-item__content")])([div_([text5("Initial: " + (formatAmount(foreignPos.amount) + (" @ " + formatPrice(foreignPos.price))))]), div_([text5("Current Value: " + formatAmount(foreignPos.value))]), div_([text5("Yield Earned: " + formatAmount(foreignPos.accumulatedYield))]), div_([text5("Leverage: " + (foreignPos.leverage + (" | Duration: " + foreignPos.duration)))]), div_([text5("Shares: " + formatAmount(foreignPos.shares))])])]);
    };
    return div2([class_("panel wallet-panel")])([h2_([text5("Your Wallet")]), div2([class_("wallet-section")])([h3_([text5("Balances")]), div2([class_("balance-compact")])([renderBalance("JitoSOL")(state3.jitoBalance), renderBalance("FeelsSOL")(state3.feelsBalance)])]), div2([class_("wallet-section")])([h3_([text5("Your Tokens")]), function() {
      var $30 = length3(state3.userTokens) === 0;
      if ($30) {
        return p([class_("empty-state")])([text5("No tokens created yet")]);
      }
      ;
      return div2([class_("token-list")])(map36(renderToken)(state3.userTokens));
    }()]), div2([class_("wallet-section")])([h3_([text5("Your Positions")]), function() {
      var $31 = length3(state3.userPositions) === 0;
      if ($31) {
        return p([class_("empty-state")])([text5("No active positions")]);
      }
      ;
      return div2([class_("position-list")])(map36(renderPosition)(state3.userPositions));
    }()])]);
  };
  var renderTokenCreatorPanel = function(state3) {
    var renderWarning = function(error4) {
      return div2([class_("warning-message")])([text5(error4)]);
    };
    var renderValidationWarnings = function(errors) {
      var $32 = $$null(errors);
      if ($32) {
        return text5("");
      }
      ;
      return div2([class_("validation-warnings")])(map36(renderWarning)(errors));
    };
    return div2([class_("panel")])([h2_([text5("Create Feels Token")]), div2([class_("form-group")])([label4([$$for("create-token-ticker")])([text5("Token Ticker:")]), input([type_19(InputText.value), id2("create-token-ticker"), name15("create-token-ticker"), placeholder3("e.g., ALPHA"), class_("form__input"), value13(state3.tokenTicker), onValueChange(function(v) {
      return new UpdateTokenTicker(trim(v));
    })]), label4([$$for("create-token-name")])([text5("Token Name:")]), input([type_19(InputText.value), id2("create-token-name"), name15("create-token-name"), placeholder3("e.g., Alpha Protocol Token"), class_("form__input"), value13(state3.tokenName), onValueChange(function(v) {
      return new UpdateTokenName(trim(v));
    })]), renderValidationWarnings(state3.tokenValidationErrors), button([onClick(function(v) {
      return CreateTokenUI.value;
    }), class_("btn btn--primary")])([text5("Create Token")])]), p([class_("info-text")])([text5("Tokens launch through the batch auction system")])]);
  };
  var renderSystemPanel = function(state3) {
    var renderSupply = function(label5) {
      return function(value1) {
        return div2([class_("supply-item")])([span3([class_("supply-label")])([text5(label5 + ":")]), span3([class_("supply-value")])([text5(value1)])]);
      };
    };
    var renderPrimaryMetric = function(label5) {
      return function(value1) {
        return function(unit2) {
          return div2([class_("primary-metric")])([div2([class_("metric-label")])([text5(label5)]), div2([class_("metric-value-large")])([span3([class_("value")])([text5(value1)]), function() {
            var $33 = unit2 !== "";
            if ($33) {
              return span3([class_("unit")])([text5(" " + unit2)]);
            }
            ;
            return text5("");
          }()])]);
        };
      };
    };
    var renderMetric = function(label5) {
      return function(value1) {
        return div2([class_("metric-item")])([div2([class_("metric-label")])([text5(label5)]), div2([class_("metric-value")])([text5(value1)])]);
      };
    };
    var formatCompactNumber = function(n) {
      var $34 = n >= 1e6;
      if ($34) {
        return formatAmount(n / 1e6) + "M";
      }
      ;
      var $35 = n >= 1e3;
      if ($35) {
        return formatAmount(n / 1e3) + "K";
      }
      ;
      return formatAmount(n);
    };
    var addCommas = function(s) {
      var len = length7(s);
      var $36 = len <= 3;
      if ($36) {
        return s;
      }
      ;
      var $37 = len <= 6;
      if ($37) {
        return take4(len - 3 | 0)(s) + ("," + drop4(len - 3 | 0)(s));
      }
      ;
      var $38 = len <= 9;
      if ($38) {
        return take4(len - 6 | 0)(s) + ("," + (take4(3)(drop4(len - 6 | 0)(s)) + ("," + drop4(len - 3 | 0)(s))));
      }
      ;
      return take4(len - 9 | 0)(s) + ("," + (take4(3)(drop4(len - 9 | 0)(s)) + ("," + (take4(3)(drop4(len - 6 | 0)(s)) + ("," + drop4(len - 3 | 0)(s))))));
    };
    var formatIntWithCommas = function(n) {
      return addCommas(show21(n));
    };
    var formatLargeNumber = function(n) {
      var str = formatAmount(n);
      var parts = split(".")(str);
      if (parts.length === 2) {
        return addCommas(parts[0]) + ("." + parts[1]);
      }
      ;
      if (parts.length === 1) {
        return addCommas(parts[0]);
      }
      ;
      return str;
    };
    return div2([class_("panel system-metrics-panel")])([h2_([text5("System Metrics")]), function() {
      if (state3.protocolStats instanceof Nothing) {
        return div2([class_("loading-state")])([text5("Loading system metrics...")]);
      }
      ;
      if (state3.protocolStats instanceof Just) {
        return div_([div2([class_("primary-metrics")])([renderPrimaryMetric("Total Value Locked")(formatLargeNumber(state3.protocolStats.value0.totalValueLocked))("FeelsSOL"), renderPrimaryMetric("Active Positions")(formatIntWithCommas(state3.protocolStats.value0.activePositions))("")]), div2([class_("metrics-grid")])([renderMetric("Total Users")(formatIntWithCommas(state3.protocolStats.value0.totalUsers)), renderMetric("Live Tokens")(formatIntWithCommas(state3.protocolStats.value0.liveTokens)), renderMetric("Lender Offers")(formatIntWithCommas(state3.protocolStats.value0.totalLenderOffers)), renderMetric("POL Balance")(formatCompactNumber(state3.protocolStats.value0.polBalance) + " FeelsSOL")]), div2([class_("supplies-section")])([h4_([text5("Token Supplies")]), div2([class_("supplies-grid")])([renderSupply("FeelsSOL Supply")(formatLargeNumber(state3.protocolStats.value0.feelsSOLSupply)), renderSupply("JitoSOL Locked")(formatLargeNumber(state3.protocolStats.value0.jitoSOLLocked))])])]);
      }
      ;
      throw new Error("Failed pattern match at UI.Component (line 44, column 7 - line 77, column 12): " + [state3.protocolStats.constructor.name]);
    }()]);
  };
  var renderSimulationResults = function(results) {
    var renderResultMetric = function(label5) {
      return function(value1) {
        return div2([class_("result-metric")])([div2([class_("metric-label")])([text5(label5)]), div2([class_("metric-value")])([text5(value1)])]);
      };
    };
    var formatPercentage = function(change2) {
      var percent = change2 * 100;
      var rounded = round2(percent);
      var sign2 = function() {
        var $45 = percent >= 0;
        if ($45) {
          return "+";
        }
        ;
        return "";
      }();
      return sign2 + (show21(rounded) + "%");
    };
    return div2([class_("simulation-results")])([h3_([text5("Simulation Results")]), div2([class_("results-grid")])([renderResultMetric("Total Users")(show21(results.totalUsers)), renderResultMetric("Active Positions")(show21(results.activePositions)), renderResultMetric("Total Volume")(formatAmount(results.totalVolume)), renderResultMetric("Price Change")(formatPercentage(results.priceChange))])]);
  };
  var renderSimulationPanel = function(state3) {
    var parseMarketScenario = function(v) {
      if (v === "BullMarket") {
        return BullMarket.value;
      }
      ;
      if (v === "BearMarket") {
        return BearMarket.value;
      }
      ;
      if (v === "SidewaysMarket") {
        return SidewaysMarket.value;
      }
      ;
      if (v === "VolatileMarket") {
        return VolatileMarket.value;
      }
      ;
      return state3.simulationConfig.scenario;
    };
    return div2([class_("panel")])([h2_([text5("Market Simulation")]), div2([class_("form-group")])([label4([$$for("simulation-blocks")])([text5("Simulation Blocks:")]), input([type_19(InputNumber.value), id2("simulation-blocks"), name15("simulation-blocks"), value13(show21(state3.simulationConfig.simulationBlocks)), onValueChange(function(v) {
      return new UpdateSimulationConfig(function(config) {
        return {
          scenario: config.scenario,
          numAccounts: config.numAccounts,
          initialJitoSOLPrice: config.initialJitoSOLPrice,
          priceVolatility: config.priceVolatility,
          accountProfiles: config.accountProfiles,
          actionFrequency: config.actionFrequency,
          juniorTranchePreference: config.juniorTranchePreference,
          simulationBlocks: fromMaybe(config.simulationBlocks)(fromString2(trim(v)))
        };
      });
    }), class_("form__input"), min5(10), max6(1e3)])]), div2([class_("form-group")])([label4([$$for("num-accounts")])([text5("Number of Accounts:")]), input([type_19(InputNumber.value), id2("num-accounts"), name15("num-accounts"), value13(show21(state3.simulationConfig.numAccounts)), onValueChange(function(v) {
      return new UpdateSimulationConfig(function(config) {
        return {
          scenario: config.scenario,
          simulationBlocks: config.simulationBlocks,
          initialJitoSOLPrice: config.initialJitoSOLPrice,
          priceVolatility: config.priceVolatility,
          accountProfiles: config.accountProfiles,
          actionFrequency: config.actionFrequency,
          juniorTranchePreference: config.juniorTranchePreference,
          numAccounts: fromMaybe(config.numAccounts)(fromString2(v))
        };
      });
    }), class_("form__input"), min5(3), max6(100)])]), div2([class_("form-group")])([label4([$$for("market-scenario")])([text5("Market Scenario:")]), select3([id2("market-scenario"), name15("market-scenario"), onValueChange(function(v) {
      return new UpdateSimulationConfig(function(config) {
        return {
          numAccounts: config.numAccounts,
          simulationBlocks: config.simulationBlocks,
          initialJitoSOLPrice: config.initialJitoSOLPrice,
          priceVolatility: config.priceVolatility,
          accountProfiles: config.accountProfiles,
          actionFrequency: config.actionFrequency,
          juniorTranchePreference: config.juniorTranchePreference,
          scenario: parseMarketScenario(v)
        };
      });
    }), class_("form__select")])([option([value13("BullMarket")])([text5("Bull Market")]), option([value13("BearMarket")])([text5("Bear Market")]), option([value13("SidewaysMarket")])([text5("Sideways Market")]), option([value13("VolatileMarket")])([text5("Volatile Market")])])]), function() {
      if (state3.simulationRunning) {
        return div2([class_("simulation-status")])([text5("Running simulation...")]);
      }
      ;
      return button([onClick(function(v) {
        return RunSimulation.value;
      }), class_("btn btn--primary btn--large"), id2("run-simulation-btn")])([text5("Run Market Simulation")]);
    }(), function() {
      if (state3.simulationResults instanceof Nothing) {
        return text5("");
      }
      ;
      if (state3.simulationResults instanceof Just) {
        return renderSimulationResults(state3.simulationResults.value0);
      }
      ;
      throw new Error("Failed pattern match at UI.Component (line 667, column 7 - line 669, column 56): " + [state3.simulationResults.constructor.name]);
    }()]);
  };
  var renderLoanBookPanel = function(offers) {
    var renderOfferStatus = function(offer) {
      var available = offer.amount - offer.lockedAmount;
      var percentFilled = function() {
        var $50 = offer.amount > 0;
        if ($50) {
          return (offer.amount - available) / offer.amount * 100;
        }
        ;
        return 0;
      }();
      var $51 = available <= 0;
      if ($51) {
        return span3([class_("status-filled")])([text5("Filled")]);
      }
      ;
      var $52 = percentFilled > 0;
      if ($52) {
        return span3([class_("status-partial")])([text5(show21(floor3(percentFilled)) + "% Filled")]);
      }
      ;
      return span3([class_("status-available")])([text5("Available")]);
    };
    var formatPrice = function(p2) {
      return formatAmount(p2);
    };
    var formatPositionTerms = function(position2) {
      return "Spot";
    };
    var formatDuration = function(d) {
      var $53 = d === 0;
      if ($53) {
        return "Spot";
      }
      ;
      return show21(div1(d)(40320)) + " months";
    };
    var formatAddress = function(addr) {
      var $54 = length7(addr) > 12;
      if ($54) {
        return take4(6)(addr) + ("..." + drop4(length7(addr) - 4 | 0)(addr));
      }
      ;
      return addr;
    };
    var renderOffer = function(foreignOffer) {
      return div2([class_("offer__item")])([div2([class_("offer__id")])([text5("#" + show21(foreignOffer.id))]), div2([class_("offer__owner")])([text5(formatAddress(show111(foreignOffer.owner)))]), div2([class_("offer__asset")])([text5(foreignOffer.leverage)]), div2([class_("offer__amount")])([text5(formatAmount(foreignOffer.amount) + " shares")]), div2([class_("offer__collateral")])([text5("Locked: " + formatAmount(foreignOffer.lockedAmount))]), div2([class_("offer__terms")])([text5("Spot")]), div2([class_("offer__status")])([renderOfferStatus(foreignOffer)])]);
    };
    var _formatCollateral = function(offer) {
      return formatAmount(offer.lockedAmount);
    };
    return div2([class_("panel")])([h2_([text5("Available Offers")]), function() {
      var $55 = length3(offers) === 0;
      if ($55) {
        return p_([text5("No lending offers available")]);
      }
      ;
      return div2([class_("offer-list")])([div2([class_("offer__header")])([div_([text5("ID")]), div_([text5("Lender")]), div_([text5("Asset")]), div_([text5("Amount (Avail/Total)")]), div_([text5("Collateral Required")]), div_([text5("Terms")]), div_([text5("Status")])]), div_(map36(renderOffer)(offers))]);
    }()]);
  };
  var renderLendOptions = function(state3) {
    var renderAPY = function(state$prime) {
      var leverageMultiplier2 = function() {
        var $56 = state$prime.selectedLeverage === "junior";
        if ($56) {
          return 3;
        }
        ;
        return 1;
      }();
      var leverage = function() {
        if (state$prime.selectedLeverage === "junior") {
          return "Junior (3x)";
        }
        ;
        return "Senior (1x)";
      }();
      var durationMultiplier = function() {
        var $58 = state$prime.selectedTermType === "monthly";
        if ($58) {
          return 1.2;
        }
        ;
        return 1;
      }();
      var duration2 = function() {
        if (state$prime.selectedTermType === "monthly") {
          return "Monthly Term";
        }
        ;
        return "Spot";
      }();
      var totalAPY = 5 * leverageMultiplier2 * durationMultiplier;
      return div_([div2([class_("apy-row")])([span_([text5("Duration: " + duration2)]), span_([text5("Leverage: " + leverage)])]), div2([class_("apy-total")])([text5(show25(totalAPY) + "% APY")]), div2([class_("apy-breakdown")])([text5("(Base 5% \xD7 Leverage \xD7 Duration bonus)")])]);
    };
    var calculateTermInfo = function(state$prime) {
      if (state$prime.api instanceof Nothing) {
        return "Term information unavailable";
      }
      ;
      if (state$prime.api instanceof Just) {
        var currentTermNumber = floor3(toNumber2(1e3) / toNumber2(201600)) + 1 | 0;
        var blocksIntoTerm = mod6(1e3)(201600);
        var blocksRemaining = 201600 - blocksIntoTerm | 0;
        var daysRemaining = floor3(toNumber2(blocksRemaining) / 7200);
        return "Current term #" + (show21(currentTermNumber) + (" - " + (show21(daysRemaining) + " days remaining")));
      }
      ;
      throw new Error("Failed pattern match at UI.Component (line 467, column 7 - line 478, column 108): " + [state$prime.api.constructor.name]);
    };
    var renderDurationInfo = function(state$prime) {
      return div2([class_("duration-info")])([function() {
        if (state$prime.selectedTermType === "monthly") {
          var termInfo = calculateTermInfo(state$prime);
          return div_([p_([text5("Monthly Term: Join the current 28-day cycle. Your position will mature with all other monthly positions at the end of this term.")]), p([class_("term-status")])([text5(termInfo)])]);
        }
        ;
        return p_([text5("Spot: No term commitment. Withdraw anytime with immediate liquidity. Lower yields than term positions.")]);
      }()]);
    };
    return div_([div2([class_("form-group")])([label4([$$for("term-type")])([text5("Term Type:")]), select3([id2("term-type"), name15("term-type"), onValueChange(SetTermType.create), class_("form__select")])([option([value13("spot"), selected2(true)])([text5("Spot (Flexible)")]), option([value13("monthly")])([text5("Monthly Term (Join Current Cycle)")])])]), div2([class_("form-group")])([label4([$$for("leverage-tier")])([text5("Risk Level:")]), select3([id2("leverage-tier"), name15("leverage-tier"), onValueChange(SetLeverageType.create), class_("form__select")])([option([value13("senior"), selected2(true)])([text5("Senior (1x - Protected)")]), option([value13("junior")])([text5("Junior (3x - Higher Risk/Reward)")])])]), renderDurationInfo(state3), renderAPY(state3)]);
  };
  var renderExchangePanel = function(state3) {
    var renderPositionOptions = function(state$prime) {
      var $63 = contains("position")(state$prime.selectedToAsset);
      if ($63) {
        return renderLendOptions(state$prime);
      }
      ;
      return div_([]);
    };
    var generateRouteText = function(state$prime) {
      var to = function() {
        if (state$prime.selectedToAsset === "position-spot") {
          return "Spot Position";
        }
        ;
        if (state$prime.selectedToAsset === "position-term") {
          return "Term Position";
        }
        ;
        if (state$prime.selectedToAsset === "jitosol") {
          return "JitoSOL";
        }
        ;
        if (state$prime.selectedToAsset === "feelssol") {
          return "FeelsSOL";
        }
        ;
        return "Asset";
      }();
      var from2 = function() {
        if (state$prime.selectedFromAsset === "jitosol") {
          return "JitoSOL";
        }
        ;
        if (state$prime.selectedFromAsset === "feelssol") {
          return "FeelsSOL";
        }
        ;
        if (state$prime.selectedFromAsset === "position") {
          return "Position";
        }
        ;
        return "Asset";
      }();
      return from2 + (" \u2192 " + (to + " (Atomic transaction)"));
    };
    return div2([class_("panel")])([h2_([text5("Exchange")]), div2([class_("exchange-form")])([div2([class_("exchange-section")])([h3_([text5("From")]), div2([class_("form-group")])([label4([$$for("from-asset")])([text5("Asset:")]), select3([id2("from-asset"), name15("from-asset"), onValueChange(function(v) {
      return new SelectFromAsset(v);
    }), class_("form__select")])([option([value13("jitosol")])([text5("JitoSOL")]), option([value13("feelssol")])([text5("FeelsSOL")]), option([value13("position")])([text5("Existing Position")])])]), div2([class_("form-group")])([label4([$$for("from-amount")])([text5("Amount:")]), input([type_19(InputNumber.value), id2("from-amount"), name15("from-amount"), value13(formatAmount(state3.inputAmount)), onValueChange(function(v) {
      return new UpdateInputAmount(fromMaybe(0)(fromString(v)));
    }), class_("form__input"), placeholder3("Enter amount")])])]), div2([class_("exchange-arrow")])([]), div2([class_("exchange-section")])([h3_([text5("To")]), div2([class_("form-group")])([label4([$$for("to-asset")])([text5("Asset/Position:")]), select3([id2("to-asset"), name15("to-asset"), onValueChange(function(v) {
      return new SelectToAsset(v);
    }), class_("form__select")])([option([value13("position-spot")])([text5("Spot Position")]), option([value13("position-term")])([text5("Term Position")]), option([value13("jitosol")])([text5("JitoSOL")]), option([value13("feelssol")])([text5("FeelsSOL")])])]), renderPositionOptions(state3)]), div2([class_("route-preview")])([h4_([text5("Route Preview")]), p([class_("route-text")])([text5(generateRouteText(state3))])]), button([onClick(function(v) {
      return ExecuteExchange.value;
    }), class_("btn btn--primary btn--large")])([text5("Execute Exchange")])])]);
  };

  // output/UI.App/index.js
  var $runtime_lazy11 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var discard6 = /* @__PURE__ */ discard(discardUnit);
  var bind6 = /* @__PURE__ */ bind(bindAff);
  var discard23 = /* @__PURE__ */ discard6(bindAff);
  var liftEffect7 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var render = function(state3) {
    return div2([class_("app")])([function() {
      if (state3.loading) {
        return div2([class_("loading")])([text5("Initializing protocol...")]);
      }
      ;
      return div2([class_("main-layout two-column")])([div2([class_("left-column")])([renderSimulationPanel(state3), renderWalletPanel(state3), renderTokenCreatorPanel(state3), renderExchangePanel(state3), renderSystemPanel(state3)]), div2([class_("right-column")])([div2([id2("chart-data-hidden"), style("display: none;")])([text5("")]), div2([class_("panel")])([h2_([text5("Price Chart")]), canvas([id2("price-chart"), width8(600), height8(400)])]), renderLoanBookPanel(state3.lenderOffers)])]);
    }()]);
  };
  var component = function(dictMonadAff) {
    return mkComponent({
      initialState: function(v) {
        return initialUIState;
      },
      render,
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction(dictMonadAff),
        initialize: new Just(Initialize2.value)
      })
    });
  };
  var component1 = /* @__PURE__ */ component(monadAffAff);
  var renderUI = /* @__PURE__ */ function() {
    var $lazy_initState = $runtime_lazy11("initState", "UI.App", function() {
      return function __do7() {
        log("Initializing protocol state...")();
        return $lazy_initState(114)();
      };
    });
    var initState2 = $lazy_initState(112);
    return function __do7() {
      log("Starting Feels Protocol UI...")();
      return runHalogenAff(bind6(selectElement("#app"))(function(appElement) {
        if (appElement instanceof Nothing) {
          return discard23(liftEffect7(log("Could not find #app element")))(function() {
            return bind6(awaitBody)(runUI2(component1)(unit));
          });
        }
        ;
        if (appElement instanceof Just) {
          return discard23(liftEffect7(log("Mounting UI component")))(function() {
            return runUI2(component1)(unit)(appElement.value0);
          });
        }
        ;
        throw new Error("Failed pattern match at UI.App (line 103, column 5 - line 109, column 37): " + [appElement.constructor.name]);
      }))();
    };
  }();

  // output/Main/index.js
  var main2 = function __do6() {
    log("Starting Feels Protocol")();
    return onDOMReady(function __do7() {
      log("DOM ready, initializing Halogen application")();
      renderUI();
      return log("Halogen application initialized successfully")();
    })();
  };

  // <stdin>
  main2();
})();
