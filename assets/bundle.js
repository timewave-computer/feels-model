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

  // output/Data.Show/index.js
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
  };

  // output/FFI/foreign.js
  var positionIdCounter = 1e3;
  var sqrt = (x) => Math.sqrt(x);
  var currentTime = () => Date.now();
  var generateId = (timestamp) => Math.floor(timestamp * Math.random());
  var getValue = function(element3) {
    return function() {
      return element3.value;
    };
  };
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
  var generateRecordId = function() {
    return function() {
      return positionIdCounter++;
    };
  };
  var chartInstance = null;
  var checkAndInitializeChart = function() {
    console.log("checkAndInitializeChart wrapper called");
    return function() {
      console.log("=== CHECKING CHART READINESS ===");
      console.log("Document ready state:", document.readyState);
      if (typeof Chart === "undefined") {
        console.error("Chart.js is not loaded! Retrying in 500ms...");
        window.setTimeout(() => checkAndInitializeChart()(), 500);
        return;
      }
      console.log("Chart.js version:", Chart.version);
      const canvas2 = document.getElementById("price-chart");
      const dataElement = document.querySelector(".chart-data");
      console.log("Canvas found:", !!canvas2);
      console.log("Data element found:", !!dataElement);
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
      const dataElement = document.querySelector(".chart-data");
      console.log("Canvas found:", !!canvas2);
      console.log("Data element found:", !!dataElement);
      console.log("Canvas dimensions:", canvas2?.width, "x", canvas2?.height);
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
        if (!rawData || rawData.trim() === "" || rawData === "[]") {
          console.log("No chart data available yet");
          const ctx = canvas2.getContext("2d");
          ctx.fillStyle = "#1a1a1a";
          ctx.fillRect(0, 0, canvas2.width, canvas2.height);
          ctx.fillStyle = "#666";
          ctx.font = "14px sans-serif";
          ctx.fillText("No data - run simulation first", 10, canvas2.height / 2);
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
    if (chartInstance) {
      chartInstance.destroy();
      chartInstance = null;
    }
    const ctx = canvas2.getContext("2d");
    const isMultiToken = data.length > 0 && data[0].tokens;
    if (isMultiToken) {
      renderMultiTokenChart(ctx, data);
    } else {
      renderSingleTokenChart(ctx, data);
    }
  }
  function renderSingleTokenChart(ctx, data) {
    console.log("renderSingleTokenChart called with", data.length, "data points");
    try {
      const priceData = data.map((point, index4) => {
        const timestamp = new Date(point.timestamp);
        const price = point.price;
        const variation = 0.01;
        const open = index4 === 0 ? price : data[index4 - 1].price;
        const close2 = price;
        const high2 = Math.max(open, close2) * (1 + variation * Math.random());
        const low2 = Math.min(open, close2) * (1 - variation * Math.random());
        return {
          x: timestamp.valueOf(),
          o: open,
          h: high2,
          l: low2,
          c: close2
        };
      });
      console.log("Price data sample:", priceData.slice(0, 3));
      const nfvData = data.map((point) => ({
        x: new Date(point.timestamp).valueOf(),
        y: point.nfvValue
      }));
      try {
        chartInstance = new Chart(ctx, {
          type: "candlestick",
          data: {
            datasets: [{
              label: "JitoSOL/FeelsSOL Price",
              data: priceData,
              borderColor: "#ffc0cb",
              backgroundColors: {
                up: "#ffc0cb",
                down: "#ef4444",
                unchanged: "#6b7280"
              }
            }, {
              label: "NFV Floor",
              type: "line",
              data: nfvData,
              borderColor: "#f59e0b",
              backgroundColor: "rgba(245, 158, 11, 0.1)",
              borderWidth: 2,
              pointRadius: 0,
              fill: true,
              yAxisID: "y2"
            }]
          },
          options: {
            responsive: true,
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
                    if (context.dataset.type === "candlestick") {
                      const point = context.raw;
                      if (!point || point.o == null || point.h == null || point.l == null || point.c == null) {
                        return "No data";
                      }
                      return [
                        `Open: ${point.o.toFixed(4)}`,
                        `High: ${point.h.toFixed(4)}`,
                        `Low: ${point.l.toFixed(4)}`,
                        `Close: ${point.c.toFixed(4)}`
                      ];
                    } else {
                      if (!context.parsed || context.parsed.y == null) {
                        return "No data";
                      }
                      return `${context.dataset.label}: ${context.parsed.y.toFixed(4)}`;
                    }
                  }
                }
              }
            },
            scales: {
              x: {
                type: "time",
                time: {
                  unit: "minute",
                  displayFormats: {
                    minute: "HH:mm"
                  }
                },
                grid: {
                  color: "#1f2937",
                  drawBorder: false
                },
                ticks: {
                  color: "#000000",
                  font: {
                    family: "monospace"
                  }
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
                  text: "NFV Floor",
                  color: "#f59e0b"
                }
              }
            }
          }
        });
        console.log("Chart.js chart created successfully");
      } catch (error4) {
        console.error("Error creating Chart.js chart:", error4);
        console.error("Error stack:", error4.stack);
      }
    } catch (error4) {
      console.error("Error in renderSingleTokenChart:", error4);
    }
  }
  function renderMultiTokenChart(ctx, data) {
    console.log("Rendering multi-token chart");
    const tokenTickers = data.length > 0 && data[0].tokens ? Object.keys(data[0].tokens).filter((ticker) => ticker !== "JitoSOL") : [];
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
    const jitoData = data.map((point, index4) => {
      const timestamp = new Date(point.timestamp);
      const price = point.price || 1;
      const variation = 0.01;
      const open = index4 === 0 ? price : data[index4 - 1].price || price;
      const close2 = price;
      const high2 = Math.max(open, close2) * (1 + variation * Math.random());
      const low2 = Math.min(open, close2) * (1 - variation * Math.random());
      return {
        x: timestamp.valueOf(),
        o: open,
        h: high2,
        l: low2,
        c: close2
      };
    });
    datasets.push({
      label: "JitoSOL/FeelsSOL",
      type: "candlestick",
      data: jitoData,
      borderColor: "#000000",
      backgroundColors: {
        up: "#10b981",
        down: "#ef4444",
        unchanged: "#6b7280"
      }
    });
    tokenTickers.forEach((ticker, index4) => {
      const tokenData = data.map((point) => ({
        x: new Date(point.timestamp).valueOf(),
        y: point.tokens && point.tokens[ticker] ? point.tokens[ticker].price : 0
      }));
      datasets.push({
        label: `${ticker}/FeelsSOL`,
        type: "line",
        data: tokenData,
        borderColor: colors[index4 % colors.length],
        backgroundColor: "transparent",
        borderWidth: 2,
        pointRadius: 0,
        tension: 0.1
      });
      const nfvData = data.map((point) => ({
        x: new Date(point.timestamp).valueOf(),
        y: point.tokens && point.tokens[ticker] ? point.tokens[ticker].nfvFloor : 0
      }));
      datasets.push({
        label: `${ticker} NFV Floor`,
        type: "line",
        data: nfvData,
        borderColor: colors[index4 % colors.length],
        backgroundColor: colors[index4 % colors.length] + "20",
        // 20% opacity
        borderWidth: 1,
        borderDash: [5, 5],
        pointRadius: 0,
        fill: true,
        tension: 0.1
      });
    });
    chartInstance = new Chart(ctx, {
      type: "candlestick",
      data: {
        datasets
      },
      options: {
        responsive: true,
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
                if (context.dataset.type === "candlestick") {
                  const point = context.raw;
                  if (!point || point.o == null || point.h == null || point.l == null || point.c == null) {
                    return "No data";
                  }
                  return [
                    `${context.dataset.label}`,
                    `Open: ${point.o.toFixed(4)}`,
                    `High: ${point.h.toFixed(4)}`,
                    `Low: ${point.l.toFixed(4)}`,
                    `Close: ${point.c.toFixed(4)}`
                  ];
                } else {
                  if (!context.parsed || context.parsed.y == null) {
                    return "No data";
                  }
                  return `${context.dataset.label}: ${context.parsed.y.toFixed(4)}`;
                }
              }
            }
          }
        },
        scales: {
          x: {
            type: "time",
            time: {
              unit: "minute",
              displayFormats: {
                minute: "HH:mm"
              }
            },
            grid: {
              color: "#1f2937",
              drawBorder: false
            },
            ticks: {
              color: "#000000",
              font: {
                family: "monospace",
                size: 11
              }
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
  }

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
    var map110 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map110($$const(x))(f);
      };
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map30 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map30($$const(identity2))(a2))(b2);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    var pure16 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply2(pure16(f))(a2);
      };
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

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
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

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq3) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq3 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
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
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Eq/index.js
  var eqString = {
    eq: eqStringImpl
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
    var eq3 = eq(dictEq);
    return function(x) {
      return function(y) {
        return eq2(eq3(x)(y))(false);
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
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
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
    var compare3 = compare(dictOrd);
    return function(f) {
      return function(x) {
        return function(y) {
          return compare3(f(x))(f(y));
        };
      };
    };
  };
  var max = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare3(x)(y);
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

  // output/Data.Monoid/index.js
  var mempty = function(dict) {
    return dict.mempty;
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
  var modify_ = function(dictMonadState) {
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
  var length = function(xs) {
    return xs.length;
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
  var reverse = function(l) {
    return l.slice().reverse();
  };
  var filterImpl = function(f, xs) {
    return xs.filter(f);
  };
  var sortByImpl = /* @__PURE__ */ function() {
    function mergeFromTo(compare3, fromOrdering, xs1, xs2, from2, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to - from2 >> 1);
      if (mid - from2 > 1) mergeFromTo(compare3, fromOrdering, xs2, xs1, from2, mid);
      if (to - mid > 1) mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to);
      i2 = from2;
      j = mid;
      k = from2;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare3(x)(y));
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
    return function(compare3, fromOrdering, xs) {
      var out;
      if (xs.length < 2) return xs;
      out = xs.slice(0);
      mergeFromTo(compare3, fromOrdering, out, xs.slice(0), 0, xs.length);
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

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind5 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind5(mb)(function(b2) {
          return unless2(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind5 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind5(f)(function(f$prime) {
          return bind5(a2)(function(a$prime) {
            return pure16(f$prime(a$prime));
          });
        });
      };
    };
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
  var map3 = /* @__PURE__ */ map(functorEither);
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
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map3(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };
  var bindEither = {
    bind: /* @__PURE__ */ either(function(e) {
      return function(v) {
        return new Left(e);
      };
    })(function(a2) {
      return function(f) {
        return f(a2);
      };
    }),
    Apply0: function() {
      return applyEither;
    }
  };
  var applicativeEither = /* @__PURE__ */ function() {
    return {
      pure: Right.create,
      Apply0: function() {
        return applyEither;
      }
    };
  }();

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

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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
  var modify_2 = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map4 = /* @__PURE__ */ map(functorEffect);
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
          return map4(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
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
  var $runtime_lazy2 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init2) {
      return function(xs) {
        var acc = init2;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init2) {
      return function(xs) {
        var acc = init2;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
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

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure16 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond2(f($454));
        })(pure16(unit));
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
      var mempty2 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty2;
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
      var append6 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append6(f(x))(acc);
          };
        })(mempty2);
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
      return function(map30) {
        return function(pure16) {
          return function(f) {
            return function(array) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure16([]);
                  case 1:
                    return map30(array1)(f(array[bot]));
                  case 2:
                    return apply2(map30(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply2(apply2(map30(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply2(map30(concat2)(go2(bot, pivot)))(go2(pivot, top3));
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

  // output/Data.Array/index.js
  var $$void3 = /* @__PURE__ */ $$void(functorST);
  var map5 = /* @__PURE__ */ map(functorMaybe);
  var map1 = /* @__PURE__ */ map(functorArray);
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
  var singleton2 = function(a2) {
    return [a2];
  };
  var range2 = /* @__PURE__ */ runFn2(rangeImpl);
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var index = /* @__PURE__ */ function() {
    return runFn4(indexImpl)(Just.create)(Nothing.value);
  }();
  var last = function(xs) {
    return index(xs)(length(xs) - 1 | 0);
  };
  var head = function(xs) {
    return index(xs)(0);
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
        return map1(snd)(sortWith1(fst)(function __do7() {
          var result = unsafeThaw(singleton2(v.value0))();
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
              return when2(notEq2(comp(lst)(v1.value1))(EQ.value))($$void3(push(v1)(result)))();
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
  var findIndex = /* @__PURE__ */ function() {
    return runFn4(findIndexImpl)(Just.create)(Nothing.value);
  }();
  var find2 = function(f) {
    return function(xs) {
      return map5(unsafeIndex1(xs))(findIndex(f)(xs));
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
      return slice(n)(length(xs))(xs);
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
  var floor3 = function($39) {
    return unsafeClamp(floor2($39));
  };

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

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

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

  // output/Data.String.CodeUnits/foreign.js
  var singleton3 = function(c) {
    return c;
  };
  var length2 = function(s) {
    return s.length;
  };
  var drop2 = function(n) {
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

  // output/Data.String.Common/foreign.js
  var trim = function(s) {
    return s.trim();
  };
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output/Data.String.CodePoints/index.js
  var $runtime_lazy3 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
      state3 = 2;
      return val;
    };
  };
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map6 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div2 = /* @__PURE__ */ div(euclideanRingInt);
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
  var uncons = function(s) {
    var v = length2(s);
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
        tail: drop2(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop2(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map6(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length2(s) > 1;
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
  var length3 = function($74) {
    return length(toCodePointArray($74));
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton3($75($76));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div2(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var singleton4 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(v) {
    return function(v1) {
      if (v < 1) {
        return "";
      }
      ;
      var v2 = uncons(v1);
      if (v2 instanceof Just) {
        return singleton4(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
      }
      ;
      return v1;
    };
  };
  var take2 = /* @__PURE__ */ _take(takeFallback);
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
  var drop3 = function(n) {
    return function(s) {
      return drop2(length2(take2(n)(s)))(s);
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
  var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy3("enumCodePoint", "Data.String.CodePoints", function() {
    return {
      succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      Ord0: function() {
        return ordCodePoint;
      }
    };
  });

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
      var size4 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size4 !== 0) {
          size4--;
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
          if (size4 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size4) % limit] = cb;
          size4++;
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
        var head3 = null;
        var tail = null;
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
              if (head3 === null) {
                break loop;
              }
              step3 = head3._2;
              if (tail === null) {
                head3 = null;
              } else {
                head3 = tail._1;
                tail = tail._2;
              }
              break;
            case MAP:
              step3 = step3._2;
              break;
            case APPLY:
            case ALT:
              if (head3) {
                tail = new Aff2(CONS, head3, tail);
              }
              head3 = step3;
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
      function join3(result, head3, tail) {
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
          if (head3 === null) {
            cb(fail2 || step3)();
            return;
          }
          if (head3._3 !== EMPTY) {
            return;
          }
          switch (head3.tag) {
            case MAP:
              if (fail2 === null) {
                head3._3 = util.right(head3._1(util.fromRight(step3)));
                step3 = head3._3;
              } else {
                head3._3 = fail2;
              }
              break;
            case APPLY:
              lhs = head3._1._3;
              rhs = head3._2._3;
              if (fail2) {
                head3._3 = fail2;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, fail2 === lhs ? head3._2 : head3._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join3(fail2, null, null);
                    } else {
                      join3(fail2, tail._1, tail._2);
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
                head3._3 = step3;
              }
              break;
            case ALT:
              lhs = head3._1._3;
              rhs = head3._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail2 = step3 === lhs ? rhs : lhs;
                step3 = null;
                head3._3 = fail2;
              } else {
                head3._3 = step3;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, step3 === lhs ? head3._2 : head3._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join3(step3, null, null);
                    } else {
                      join3(step3, tail._1, tail._2);
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
          if (tail === null) {
            head3 = null;
          } else {
            head3 = tail._1;
            tail = tail._2;
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
        var head3 = null;
        var tail = null;
        var tmp, fid;
        loop: while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step3.tag) {
                case MAP:
                  if (head3) {
                    tail = new Aff2(CONS, head3, tail);
                  }
                  head3 = new Aff2(MAP, step3._1, EMPTY, EMPTY);
                  step3 = step3._2;
                  break;
                case APPLY:
                  if (head3) {
                    tail = new Aff2(CONS, head3, tail);
                  }
                  head3 = new Aff2(APPLY, EMPTY, step3._2, EMPTY);
                  step3 = step3._1;
                  break;
                case ALT:
                  if (head3) {
                    tail = new Aff2(CONS, head3, tail);
                  }
                  head3 = new Aff2(ALT, EMPTY, step3._2, EMPTY);
                  step3 = step3._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step3;
                  step3 = new Aff2(FORKED, fid, new Aff2(CONS, head3, tail), EMPTY);
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
              if (head3 === null) {
                break loop;
              }
              if (head3._1 === EMPTY) {
                head3._1 = step3;
                status = CONTINUE;
                step3 = head3._2;
                head3._2 = EMPTY;
              } else {
                head3._2 = step3;
                step3 = head3;
                if (tail === null) {
                  head3 = null;
                } else {
                  head3 = tail._1;
                  tail = tail._2;
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
    var map30 = map(Monad0.Bind1().Apply0().Functor0());
    var pure16 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map30(Right.create)(a2))(function($52) {
        return pure16(Left.create($52));
      });
    };
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
  var map7 = /* @__PURE__ */ map(functorEither);
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
    var map110 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map110(map7(f)));
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
    var bind5 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind5(v)(either(function($193) {
            return pure16(Left.create($193));
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

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Control.Parallel/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential3 = sequential(dictParallel);
    var parallel4 = parallel(dictParallel);
    return function(dictApplicative) {
      var traverse_8 = traverse_(dictApplicative);
      return function(dictFoldable) {
        var traverse_14 = traverse_8(dictFoldable);
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
  var $runtime_lazy4 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
      state3 = 2;
      return val;
    };
  };
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void4 = /* @__PURE__ */ $$void(functorEffect);
  var map8 = /* @__PURE__ */ map(functorEffect);
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
  var map12 = /* @__PURE__ */ map(functorAff);
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
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy4("applyAff", "Effect.Aff", function() {
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
      return map8(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map12(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void4(v.kill(e, $$const(pure2(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map8(effectCanceler)(v.kill(e, k));
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
      return $$void4(runAff(k)(aff));
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
  var getEffProp = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
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

  // output/Web.DOM.ParentNode/index.js
  var map9 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map9(toMaybe);
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
  var map10 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = function(doc) {
    return map10(function() {
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
  var map11 = /* @__PURE__ */ map(functorEffect);
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
        var et = map11(toEventTarget)(windowImpl)();
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
  var singleton5 = function(dictPlus) {
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
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty2);
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
  var $runtime_lazy5 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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
  var singleton6 = function(k) {
    return function(v) {
      return new Node(1, 1, k, v, Leaf.value, Leaf.value);
    };
  };
  var unsafeBalancedNode = /* @__PURE__ */ function() {
    var height8 = function(v) {
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
      if (rl instanceof Node && rl.value0 > height8(rr)) {
        return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
      }
      ;
      return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
    };
    var rotateRight = function(k, v, lk, lv, ll, lr, r) {
      if (lr instanceof Node && height8(ll) <= lr.value0) {
        return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
      }
      ;
      return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
    };
    return function(k, v, l, r) {
      if (l instanceof Leaf) {
        if (r instanceof Leaf) {
          return singleton6(k)(v);
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
  var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy5("unsafeSplit", "Data.Map.Internal", function() {
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
  var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy5("unsafeSplitLast", "Data.Map.Internal", function() {
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
    var compare3 = compare(dictOrd);
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
            var v1 = compare3(k)(v.value2);
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
  var insert = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var go2 = function(v1) {
          if (v1 instanceof Leaf) {
            return singleton6(k)(v);
          }
          ;
          if (v1 instanceof Node) {
            var v2 = compare3(k)(v1.value2);
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
        var $lazy_go = $runtime_lazy5("go", "Data.Map.Internal", function() {
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
        var $lazy_go = $runtime_lazy5("go", "Data.Map.Internal", function() {
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
      var mempty2 = mempty(dictMonoid);
      var append15 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty2;
          }
          ;
          if (v instanceof Node) {
            return append15(go2(v.value4))(append15(f(v.value3))(go2(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 181, column 10 - line 184, column 28): " + [v.constructor.name]);
        };
        return go2;
      };
    }
  };
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var $$delete = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(k) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare3(k)(v.value2);
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
    var compare3 = compare(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = unsafeSplit(compare3, k, m);
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
    var traverse_8 = traverse_(dictApplicative)(foldableMap);
    return function(v) {
      return function(k) {
        return traverse_8(function($54) {
          return k($54);
        })(v);
      };
    };
  };
  var empty3 = empty2;

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
  var map13 = /* @__PURE__ */ map(functorArray);
  var map14 = /* @__PURE__ */ map(functorTuple);
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
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map13(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map13(map14(go2))(v2.value3));
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
  function createElement(ns, name15, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name15);
    } else {
      return doc.createElement(name15);
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
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.Element/index.js
  var toNode2 = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy6 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy6("patchWidget", "Halogen.VDom.DOM", function() {
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
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy6("patchText", "Halogen.VDom.DOM", function() {
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
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy6("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        var v1 = length(state3.children);
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
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy6("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
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
      length: length(ch1)
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
    var $lazy_build = $runtime_lazy6("build", "Halogen.VDom.DOM", function() {
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
  var $$null = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };

  // output/Data.List.NonEmpty/index.js
  var singleton7 = /* @__PURE__ */ function() {
    var $200 = singleton5(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  }();
  var cons2 = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
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
      return $153(singleton7($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure16 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value14) {
        if (tagOf(value14) === tag) {
          return pure16(unsafeFromForeign(value14));
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
  var $runtime_lazy7 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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
      var $lazy_patchProp = $runtime_lazy7("patchProp", "Halogen.VDom.DOM.Prop", function() {
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
    return function(name15) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name15, props, children2);
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
    var pure16 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure16(func.value0),
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
    var pure16 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure16(v.value1.value0.value0));
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
        return go2(new Tuple(Nil.value, singleton7(z)));
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
  var $$null2 = function(v) {
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
        var foldl4 = function($copy_v) {
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
                return foldl4(function(x) {
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
        var $66 = $$null2(v.value1);
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
  var $runtime_lazy8 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy8("freeApply", "Control.Monad.Free", function() {
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
    var map110 = map(Monad0.Bind1().Apply0().Functor0());
    var pure16 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map110(Done.create)(pure16(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map110(function($199) {
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
          modify_2(function(v) {
            return append5(v)([k]);
          })(subscribers)();
          return modify_2(deleteBy(unsafeRefEq)(k))(subscribers);
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
  var $runtime_lazy9 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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
    var $lazy_patchThunk = $runtime_lazy9("patchThunk", "Halogen.VDom.Thunk", function() {
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
  var map15 = /* @__PURE__ */ map(functorHalogenM);
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
          var $45 = map15(maybe(v.value1(unit))(g));
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
  var input = function(props) {
    return element2("input")(props)([]);
  };
  var label4 = /* @__PURE__ */ element2("label");
  var label_ = /* @__PURE__ */ label4([]);
  var option = /* @__PURE__ */ element2("option");
  var p = /* @__PURE__ */ element2("p");
  var p_ = /* @__PURE__ */ p([]);
  var select3 = /* @__PURE__ */ element2("select");
  var span3 = /* @__PURE__ */ element2("span");
  var strong = /* @__PURE__ */ element2("strong");
  var strong_ = /* @__PURE__ */ strong([]);
  var div3 = /* @__PURE__ */ element2("div");
  var div_ = /* @__PURE__ */ div3([]);
  var button = /* @__PURE__ */ element2("button");
  var br = function(props) {
    return element2("br")(props)([]);
  };
  var br_ = /* @__PURE__ */ br([]);

  // output/Control.Monad.Except/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap2(runExceptT($3));
  };

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value14) {
    return value14 == null ? f : s(value14[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail2 = fail(dictMonad);
    var pure16 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value14) {
        return unsafeReadPropImpl(fail2(new TypeMismatch("object", typeOf(value14))), pure16, k, value14);
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
  var map16 = /* @__PURE__ */ map(functorMaybe);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindMaybe);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var mouseHandler = unsafeCoerce2;
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map16(Action.create)(f(ev));
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
    return function(prop3) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped3(reader)(readProp2(prop3))(unsafeToForeign(a2));
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

  // output/Halogen.HTML.Properties/index.js
  var unwrap3 = /* @__PURE__ */ unwrap();
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop1 = /* @__PURE__ */ prop2(isPropBoolean);
  var prop22 = /* @__PURE__ */ prop2(isPropString);
  var prop4 = /* @__PURE__ */ prop2(isPropNumber);
  var selected2 = /* @__PURE__ */ prop1("selected");
  var type_18 = function(dictIsProp) {
    return prop2(dictIsProp)("type");
  };
  var value12 = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };
  var placeholder3 = /* @__PURE__ */ prop22("placeholder");
  var min5 = /* @__PURE__ */ prop4("min");
  var max6 = /* @__PURE__ */ prop4("max");
  var id2 = /* @__PURE__ */ prop22("id");
  var disabled10 = /* @__PURE__ */ prop1("disabled");
  var class_ = /* @__PURE__ */ function() {
    var $36 = prop22("className");
    return function($37) {
      return $36(unwrap3($37));
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
    var traverse_8 = traverse_(dictApplicative)(foldableMaybe);
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_8(f)(st.rendering);
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
  var map18 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel3 = /* @__PURE__ */ parallel(parallelAff);
  var map19 = /* @__PURE__ */ map(functorAff);
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
          return evalM(render2)(ref2)(v["component"]["eval"](new Query(map18(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
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
                return map19(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
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
                    return discard1(liftEffect4(modify_2(map23(insert3(sid)(finalize)))(v2.subscriptions)))(function() {
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
                      modify_2($$delete2(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render2)(ref2)(v1.value0))))(function(fiber) {
                      return discard1(liftEffect4(unlessM2(read(doneRef))(modify_2(insert1(fid)(fiber))(v2.forks))))(function() {
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
          return liftEffect4(flip(modify_2)(ref2)(mapDriverState(function(st) {
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
  var map20 = /* @__PURE__ */ map(functorEffect);
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
              return modify_2(function(handlers) {
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
                    var childrenIn = map20(slot.pop)(read(childrenInRef))();
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
                    var isDuplicate = map20(function($69) {
                      return isJust(slot.get($69));
                    })(read(childrenOutRef))();
                    when3(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_2(slot.set($$var2))(childrenOutRef)();
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
              var shouldProcessHandlers = map20(isNothing)(read(v.pendingHandlers))();
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
              flip(modify_2)(v.selfRef)(mapDriverState(function(ds$prime) {
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
                  var $52 = maybe(false)($$null)(mmore);
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
              modify_2(function(handlers) {
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
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
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
  var map21 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ function() {
    var $6 = map21(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $15 = map21(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy10 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
      state3 = 2;
      return val;
    };
  };
  var $$void7 = /* @__PURE__ */ $$void(functorEffect);
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap4 = /* @__PURE__ */ unwrap();
  var when4 = /* @__PURE__ */ when(applicativeEffect);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map24 = /* @__PURE__ */ map(functorEffect);
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
          var buildThunk2 = buildThunk(unwrap4)(spec);
          var $lazy_patch = $runtime_lazy10("patch", "Halogen.VDom.Driver", function() {
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
          var $lazy_render = $runtime_lazy10("render", "Halogen.VDom.Driver", function() {
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
          var $lazy_renderComponentSlot = $runtime_lazy10("renderComponentSlot", "Halogen.VDom.Driver", function() {
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
        return bind14(liftEffect6(map24(toDocument)(bindFlipped7(document2)(windowImpl))))(function(document3) {
          return runUI(renderSpec(document3)(element3))(component2)(i2);
        });
      };
    };
  };

  // output/Token/index.js
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
        return "jitoSOL";
      }
      ;
      if (v instanceof FeelsSOL) {
        return "feelsSOL";
      }
      ;
      if (v instanceof Token) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Token (line 53, column 1 - line 56, column 31): " + [v.constructor.name]);
    }
  };
  var registerToken = function(registry) {
    return function(token) {
      return modify_2(function(tokens) {
        return cons(token)(tokens);
      })(registry);
    };
  };
  var getTokenByTicker = function(ticker) {
    return function(tokens) {
      var v = filter(function(t) {
        return t.ticker === ticker;
      })(tokens);
      if (v.length === 1) {
        return new Just(v[0]);
      }
      ;
      return Nothing.value;
    };
  };
  var getTokenFromRegistry = function(registry) {
    return function(ticker) {
      return function __do7() {
        var tokens = read(registry)();
        return getTokenByTicker(ticker)(tokens);
      };
    };
  };
  var getSystemTokens = /* @__PURE__ */ function() {
    return [{
      id: 1,
      ticker: "jitoSOL",
      name: "Jito Staked SOL",
      tokenType: JitoSOL.value,
      totalSupply: 1e6,
      creator: "system",
      createdAt: 0,
      launched: true,
      stakedFeelsSOL: 0
    }, {
      id: 2,
      ticker: "feelsSOL",
      name: "Feels SOL",
      tokenType: FeelsSOL.value,
      totalSupply: 1e6,
      creator: "system",
      createdAt: 0,
      launched: true,
      stakedFeelsSOL: 0
    }];
  }();
  var initTokenRegistry = /* @__PURE__ */ $$new(getSystemTokens);
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
        launched: false,
        totalSupply: 1e6,
        stakedFeelsSOL: 0
      };
    };
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

  // output/LendingRecord/index.js
  var show2 = /* @__PURE__ */ show(showNumber);
  var discard5 = /* @__PURE__ */ discard(discardUnit)(bindEither);
  var when5 = /* @__PURE__ */ when(applicativeEither);
  var eq12 = /* @__PURE__ */ eq(eqTokenType);
  var Infinite = /* @__PURE__ */ function() {
    function Infinite2() {
    }
    ;
    Infinite2.value = new Infinite2();
    return Infinite2;
  }();
  var Days30 = /* @__PURE__ */ function() {
    function Days302() {
    }
    ;
    Days302.value = new Days302();
    return Days302;
  }();
  var Days60 = /* @__PURE__ */ function() {
    function Days602() {
    }
    ;
    Days602.value = new Days602();
    return Days602;
  }();
  var Days90 = /* @__PURE__ */ function() {
    function Days902() {
    }
    ;
    Days902.value = new Days902();
    return Days902;
  }();
  var SwapTerms = /* @__PURE__ */ function() {
    function SwapTerms2() {
    }
    ;
    SwapTerms2.value = new SwapTerms2();
    return SwapTerms2;
  }();
  var StakingTerms = /* @__PURE__ */ function() {
    function StakingTerms2(value0) {
      this.value0 = value0;
    }
    ;
    StakingTerms2.create = function(value0) {
      return new StakingTerms2(value0);
    };
    return StakingTerms2;
  }();
  var LeverageTerms = /* @__PURE__ */ function() {
    function LeverageTerms2(value0) {
      this.value0 = value0;
    }
    ;
    LeverageTerms2.create = function(value0) {
      return new LeverageTerms2(value0);
    };
    return LeverageTerms2;
  }();
  var Available = /* @__PURE__ */ function() {
    function Available2(value0) {
      this.value0 = value0;
    }
    ;
    Available2.create = function(value0) {
      return new Available2(value0);
    };
    return Available2;
  }();
  var Matched = /* @__PURE__ */ function() {
    function Matched2() {
    }
    ;
    Matched2.value = new Matched2();
    return Matched2;
  }();
  var Active = /* @__PURE__ */ function() {
    function Active2() {
    }
    ;
    Active2.value = new Active2();
    return Active2;
  }();
  var Unbonding = /* @__PURE__ */ function() {
    function Unbonding2(value0) {
      this.value0 = value0;
    }
    ;
    Unbonding2.create = function(value0) {
      return new Unbonding2(value0);
    };
    return Unbonding2;
  }();
  var Closed = /* @__PURE__ */ function() {
    function Closed2() {
    }
    ;
    Closed2.value = new Closed2();
    return Closed2;
  }();
  var Lender = /* @__PURE__ */ function() {
    function Lender2() {
    }
    ;
    Lender2.value = new Lender2();
    return Lender2;
  }();
  var Borrower = /* @__PURE__ */ function() {
    function Borrower2() {
    }
    ;
    Borrower2.value = new Borrower2();
    return Borrower2;
  }();
  var showUnbondingPeriod = {
    show: function(v) {
      if (v instanceof Infinite) {
        return "Infinite";
      }
      ;
      if (v instanceof Days30) {
        return "30 days";
      }
      ;
      if (v instanceof Days60) {
        return "60 days";
      }
      ;
      if (v instanceof Days90) {
        return "90 days";
      }
      ;
      throw new Error("Failed pattern match at LendingRecord (line 49, column 1 - line 53, column 26): " + [v.constructor.name]);
    }
  };
  var show1 = /* @__PURE__ */ show(showUnbondingPeriod);
  var showLendingTerms = {
    show: function(v) {
      if (v instanceof SwapTerms) {
        return "Swap";
      }
      ;
      if (v instanceof StakingTerms) {
        return "Staking (" + (show1(v.value0) + ")");
      }
      ;
      if (v instanceof LeverageTerms) {
        return show2(v.value0) + "x Leverage";
      }
      ;
      throw new Error("Failed pattern match at LendingRecord (line 70, column 1 - line 73, column 56): " + [v.constructor.name]);
    }
  };
  var showLendingStatus = {
    show: function(v) {
      if (v instanceof Available) {
        return "Available (" + (show2(v.value0) + ")");
      }
      ;
      if (v instanceof Matched) {
        return "Matched";
      }
      ;
      if (v instanceof Active) {
        return "Active";
      }
      ;
      if (v instanceof Unbonding) {
        return "Unbonding until " + show2(v.value0);
      }
      ;
      if (v instanceof Closed) {
        return "Closed";
      }
      ;
      throw new Error("Failed pattern match at LendingRecord (line 85, column 1 - line 90, column 25): " + [v.constructor.name]);
    }
  };
  var showLendingSide = {
    show: function(v) {
      if (v instanceof Lender) {
        return "Lender";
      }
      ;
      if (v instanceof Borrower) {
        return "Borrower";
      }
      ;
      throw new Error("Failed pattern match at LendingRecord (line 39, column 1 - line 41, column 29): " + [v.constructor.name]);
    }
  };
  var isAvailable = function(record) {
    if (record.status instanceof Available) {
      return record.status.value0 > 0;
    }
    ;
    return false;
  };
  var getAvailableAmount = function(record) {
    if (record.status instanceof Available) {
      return record.status.value0;
    }
    ;
    return 0;
  };
  var eqUnbondingPeriod = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Infinite && y instanceof Infinite) {
          return true;
        }
        ;
        if (x instanceof Days30 && y instanceof Days30) {
          return true;
        }
        ;
        if (x instanceof Days60 && y instanceof Days60) {
          return true;
        }
        ;
        if (x instanceof Days90 && y instanceof Days90) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eqLendingSide = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Lender && y instanceof Lender) {
          return true;
        }
        ;
        if (x instanceof Borrower && y instanceof Borrower) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eq4 = /* @__PURE__ */ eq(eqLendingSide);
  var isLender = function(record) {
    return eq4(record.side)(Lender.value);
  };
  var validateLendingRecord = function(record) {
    return discard5(when5(record.lendAmount <= 0)(new Left("Lend amount must be positive")))(function() {
      return discard5(when5(record.collateralAmount <= 0)(new Left(function() {
        var $73 = isLender(record);
        if ($73) {
          return "Collateral ratio must be positive";
        }
        ;
        return "Collateral amount must be positive";
      }())))(function() {
        return discard5(when5(eq12(record.lendAsset)(record.collateralAsset))(new Left("Lend and collateral assets must be different")))(function() {
          return discard5(function() {
            if (record.terms instanceof LeverageTerms) {
              return when5(record.terms.value0 < 1 || record.terms.value0 > 10)(new Left("Leverage must be between 1x and 10x"));
            }
            ;
            return new Right(unit);
          }())(function() {
            return discard5(function() {
              if (record.side instanceof Lender) {
                if (record.status instanceof Available) {
                  return when5(record.status.value0 > record.lendAmount)(new Left("Available amount cannot exceed total lend amount"));
                }
                ;
                return new Right(unit);
              }
              ;
              if (record.side instanceof Borrower) {
                if (record.status instanceof Available) {
                  return new Left("Borrower records cannot have Available status");
                }
                ;
                return new Right(unit);
              }
              ;
              throw new Error("Failed pattern match at LendingRecord (line 193, column 3 - line 201, column 22): " + [record.side.constructor.name]);
            }())(function() {
              return new Right(unit);
            });
          });
        });
      });
    });
  };
  var createLenderRecord = function(id3) {
    return function(owner) {
      return function(lendAsset) {
        return function(amount) {
          return function(collateralAsset) {
            return function(ratio) {
              return function(terms) {
                return function(timestamp) {
                  return {
                    id: id3,
                    side: Lender.value,
                    owner,
                    lendAsset,
                    lendAmount: amount,
                    collateralAsset,
                    collateralAmount: ratio,
                    terms,
                    status: new Available(amount),
                    createdAt: timestamp,
                    matchedWith: Nothing.value,
                    executedAt: Nothing.value
                  };
                };
              };
            };
          };
        };
      };
    };
  };
  var createBorrowerRecord = function(id3) {
    return function(owner) {
      return function(lendAsset) {
        return function(amount) {
          return function(collateralAsset) {
            return function(collateralAmount) {
              return function(terms) {
                return function(timestamp) {
                  return {
                    id: id3,
                    side: Borrower.value,
                    owner,
                    lendAsset,
                    lendAmount: amount,
                    collateralAsset,
                    collateralAmount,
                    terms,
                    status: Active.value,
                    createdAt: timestamp,
                    matchedWith: Nothing.value,
                    executedAt: Nothing.value
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  // output/LendingBook/index.js
  var initLendingBook = function __do2() {
    var records = $$new([])();
    var nextId = $$new(1)();
    var lastUpdate = $$new(0)();
    return {
      records,
      nextId,
      lastUpdate
    };
  };
  var getUserRecords = function(book) {
    return function(owner) {
      return function __do7() {
        var records = read(book.records)();
        return filter(function(r) {
          return r.owner === owner;
        })(records);
      };
    };
  };
  var getNextId = function(book) {
    return function __do7() {
      var id3 = read(book.nextId)();
      write(id3 + 1 | 0)(book.nextId)();
      return id3;
    };
  };
  var getLenderRecords = function(book) {
    return function __do7() {
      var records = read(book.records)();
      return filter(function(r) {
        return isLender(r) && isAvailable(r);
      })(records);
    };
  };
  var getActiveRecords = function(book) {
    return function __do7() {
      var records = read(book.records)();
      return filter(function(r) {
        if (r.status instanceof Active) {
          return true;
        }
        ;
        if (r.status instanceof Unbonding) {
          return true;
        }
        ;
        if (r.status instanceof Available) {
          return true;
        }
        ;
        return false;
      })(records);
    };
  };
  var addRecord = function(book) {
    return function(record) {
      return modify_2(function(records) {
        return cons(record)(records);
      })(book.records);
    };
  };
  var createLendOffer = function(book) {
    return function(owner) {
      return function(lendAsset) {
        return function(amount) {
          return function(collateralAsset) {
            return function(ratio) {
              return function(terms) {
                return function __do7() {
                  var id3 = getNextId(book)();
                  var timestamp = currentTime();
                  var record = createLenderRecord(id3)(owner)(lendAsset)(amount)(collateralAsset)(ratio)(terms)(timestamp);
                  var v = validateLendingRecord(record);
                  if (v instanceof Left) {
                    return new Left(v.value0);
                  }
                  ;
                  if (v instanceof Right) {
                    addRecord(book)(record)();
                    return new Right(record);
                  }
                  ;
                  throw new Error("Failed pattern match at LendingBook (line 112, column 3 - line 116, column 26): " + [v.constructor.name]);
                };
              };
            };
          };
        };
      };
    };
  };

  // output/NFV/index.js
  var sum2 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var map25 = /* @__PURE__ */ map(functorArray);
  var when6 = /* @__PURE__ */ when(applicativeEffect);
  var getNFVBalance = function(state3) {
    return read(state3.balance);
  };
  var defaultNFVDistribution = {
    floorWeight: 0.6,
    nearSpotWeight: 0.3,
    tailWeight: 0.1,
    floorRange: {
      min: 0.5,
      max: 0.9
    },
    nearSpotRange: {
      min: 0.9,
      max: 1.1
    },
    tailRange: {
      min: 1.1,
      max: 2
    },
    ticksPerRange: 5
  };
  var initNFV = function __do3() {
    var balance = $$new(0)();
    var contributions = $$new([])();
    var now = currentTime();
    var initialMetrics = {
      totalBalance: 0,
      growthRate24h: 0,
      growthRate7d: 0,
      contributionsByType: {
        swap: 0,
        staking: 0,
        leverage: 0,
        gateway: 0
      },
      utilizationRate: 0,
      lastUpdated: now
    };
    var metricsCache = $$new(initialMetrics)();
    var lastUpdate = $$new(now)();
    var distribution = $$new(defaultNFVDistribution)();
    return {
      balance,
      contributions,
      metricsCache,
      lastUpdate,
      distribution
    };
  };
  var calculateBreakdown = function(contributions) {
    var swapTotal = sum2(map25(function(v) {
      return v.amount;
    })(filter(function(c) {
      if (c.source instanceof SwapTerms) {
        return true;
      }
      ;
      return false;
    })(contributions)));
    var stakingTotal = sum2(map25(function(v) {
      return v.amount;
    })(filter(function(c) {
      if (c.source instanceof StakingTerms) {
        return true;
      }
      ;
      return false;
    })(contributions)));
    var leverageTotal = sum2(map25(function(v) {
      return v.amount;
    })(filter(function(c) {
      if (c.source instanceof LeverageTerms) {
        return true;
      }
      ;
      return false;
    })(contributions)));
    return {
      swap: swapTotal,
      staking: stakingTotal,
      leverage: leverageTotal,
      gateway: 0
    };
  };
  var updateMetrics = function(state3) {
    return function __do7() {
      var now = currentTime();
      var balance = read(state3.balance)();
      var contributions = read(state3.contributions)();
      var weekAgo = now - 7 * 24 * 60 * 60 * 1e3;
      var weekContributions = filter(function(c) {
        return c.timestamp > weekAgo;
      })(contributions);
      var growth7d = sum2(map25(function(v) {
        return v.amount;
      })(weekContributions));
      var growthRate7d = function() {
        var $39 = balance > 0;
        if ($39) {
          return growth7d / balance;
        }
        ;
        return 0;
      }();
      var dayAgo = now - 24 * 60 * 60 * 1e3;
      var recentContributions = filter(function(c) {
        return c.timestamp > dayAgo;
      })(contributions);
      var growth24h = sum2(map25(function(v) {
        return v.amount;
      })(recentContributions));
      var growthRate24h = function() {
        var $40 = balance > 0;
        if ($40) {
          return growth24h / balance;
        }
        ;
        return 0;
      }();
      var breakdown = calculateBreakdown(contributions);
      var metrics = {
        totalBalance: balance,
        growthRate24h,
        growthRate7d,
        contributionsByType: breakdown,
        utilizationRate: 0,
        lastUpdated: now
      };
      write(metrics)(state3.metricsCache)();
      return write(now)(state3.lastUpdate)();
    };
  };
  var contributeToNFV = function(state3) {
    return function(lendingTerms) {
      return function(amount) {
        return function(positionId) {
          return function __do7() {
            modify_2(function(v) {
              return v + amount;
            })(state3.balance)();
            var now = currentTime();
            var contribution = {
              amount,
              source: lendingTerms,
              timestamp: now,
              positionId
            };
            var contributions = read(state3.contributions)();
            write(cons(contribution)(contributions))(state3.contributions)();
            var lastUpdate = read(state3.lastUpdate)();
            return when6(now - lastUpdate > 3e5)(updateMetrics(state3))();
          };
        };
      };
    };
  };
  var getNFVMetrics = function(state3) {
    return function __do7() {
      var now = currentTime();
      var lastUpdate = read(state3.lastUpdate)();
      when6(now - lastUpdate > 3e5)(updateMetrics(state3))();
      return read(state3.metricsCache)();
    };
  };

  // output/SyntheticSOL/index.js
  var show3 = /* @__PURE__ */ show(showTokenType);
  var pure9 = /* @__PURE__ */ pure(applicativeEffect);
  var validateCollateral = function(v) {
    if (v instanceof JitoSOL) {
      return new Right(unit);
    }
    ;
    return new Left("Invalid collateral: " + (show3(v) + ". Only JitoSOL accepted for FeelsSOL minting."));
  };
  var initSyntheticSOL = function(oracle) {
    return function __do7() {
      var supply = $$new(0)();
      var backing = $$new(0)();
      var lastUpdate = $$new(0)();
      var cachedPrice = $$new(Nothing.value)();
      return {
        totalFeelsSOLSupply: supply,
        totalJitoSOLBacking: backing,
        priceOracle: oracle,
        lastOracleUpdate: lastUpdate,
        cachedPrice
      };
    };
  };
  var fetchFreshPrice = function(state3) {
    return function(timestamp) {
      return function __do7() {
        var price = state3.priceOracle();
        var oraclePrice = {
          price,
          timestamp,
          confidence: 0.95
        };
        write(timestamp)(state3.lastOracleUpdate)();
        write(new Just(oraclePrice))(state3.cachedPrice)();
        return oraclePrice;
      };
    };
  };
  var getOraclePrice = function(state3) {
    return function __do7() {
      var now = currentTime();
      var lastUpdate = read(state3.lastOracleUpdate)();
      var $21 = now - lastUpdate < 3e4;
      if ($21) {
        var cached = read(state3.cachedPrice)();
        if (cached instanceof Just) {
          return cached.value0;
        }
        ;
        if (cached instanceof Nothing) {
          return fetchFreshPrice(state3)(now)();
        }
        ;
        throw new Error("Failed pattern match at SyntheticSOL (line 96, column 7 - line 98, column 45): " + [cached.constructor.name]);
      }
      ;
      return fetchFreshPrice(state3)(now)();
    };
  };
  var mintFeelsSOL = function(state3) {
    return function(jitoSOLAmount) {
      var $25 = jitoSOLAmount <= 0;
      if ($25) {
        return pure9(new Left("Amount must be positive"));
      }
      ;
      var v = validateCollateral(JitoSOL.value);
      if (v instanceof Left) {
        return pure9(new Left(v.value0));
      }
      ;
      if (v instanceof Right) {
        return function __do7() {
          var oraclePrice = getOraclePrice(state3)();
          var feelsSOLAmount = jitoSOLAmount * oraclePrice.price;
          var currentSupply = read(state3.totalFeelsSOLSupply)();
          var currentBacking = read(state3.totalJitoSOLBacking)();
          write(currentSupply + feelsSOLAmount)(state3.totalFeelsSOLSupply)();
          write(currentBacking + jitoSOLAmount)(state3.totalJitoSOLBacking)();
          var timestamp = currentTime();
          return new Right({
            feelsSOLMinted: feelsSOLAmount,
            jitoSOLLocked: jitoSOLAmount,
            exchangeRate: oraclePrice.price,
            timestamp
          });
        };
      }
      ;
      throw new Error("Failed pattern match at SyntheticSOL (line 133, column 7 - line 156, column 14): " + [v.constructor.name]);
    };
  };
  var burnFeelsSOL = function(state3) {
    return function(feelsSOLAmount) {
      var $29 = feelsSOLAmount <= 0;
      if ($29) {
        return pure9(new Left("Amount must be positive"));
      }
      ;
      return function __do7() {
        var currentSupply = read(state3.totalFeelsSOLSupply)();
        var $30 = feelsSOLAmount > currentSupply;
        if ($30) {
          return new Left("Insufficient FeelsSOL supply to burn");
        }
        ;
        var oraclePrice = getOraclePrice(state3)();
        var jitoSOLToRelease = feelsSOLAmount / oraclePrice.price;
        var currentBacking = read(state3.totalJitoSOLBacking)();
        var $31 = jitoSOLToRelease > currentBacking;
        if ($31) {
          return new Left("Insufficient JitoSOL backing");
        }
        ;
        write(currentSupply - feelsSOLAmount)(state3.totalFeelsSOLSupply)();
        write(currentBacking - jitoSOLToRelease)(state3.totalJitoSOLBacking)();
        var timestamp = currentTime();
        return new Right({
          feelsSOLBurned: feelsSOLAmount,
          jitoSOLReleased: jitoSOLToRelease,
          exchangeRate: oraclePrice.price,
          timestamp
        });
      };
    };
  };

  // output/Gateway/index.js
  var eq13 = /* @__PURE__ */ eq(eqTokenType);
  var map26 = /* @__PURE__ */ map(functorArray);
  var pure10 = /* @__PURE__ */ pure(applicativeEffect);
  var show4 = /* @__PURE__ */ show(showNumber);
  var Enter = /* @__PURE__ */ function() {
    function Enter2() {
    }
    ;
    Enter2.value = new Enter2();
    return Enter2;
  }();
  var Exit = /* @__PURE__ */ function() {
    function Exit2() {
    }
    ;
    Exit2.value = new Exit2();
    return Exit2;
  }();
  var updateBalanceArray = function(balances) {
    return function(owner) {
      return function(token) {
        return function(delta) {
          var v = find2(function(b2) {
            return b2.owner === owner && eq13(b2.token)(token);
          })(balances);
          if (v instanceof Just) {
            return map26(function(b2) {
              var $23 = b2.owner === owner && eq13(b2.token)(token);
              if ($23) {
                return {
                  owner: b2.owner,
                  token: b2.token,
                  amount: b2.amount + delta
                };
              }
              ;
              return b2;
            })(balances);
          }
          ;
          if (v instanceof Nothing) {
            var $25 = delta > 0;
            if ($25) {
              return cons({
                owner,
                token,
                amount: delta
              })(balances);
            }
            ;
            return balances;
          }
          ;
          throw new Error("Failed pattern match at Gateway (line 237, column 3 - line 245, column 20): " + [v.constructor.name]);
        };
      };
    };
  };
  var updateBalance = function(balancesRef) {
    return function(owner) {
      return function(token) {
        return function(delta) {
          return function __do7() {
            var balances = read(balancesRef)();
            var updated = updateBalanceArray(balances)(owner)(token)(delta);
            return write(updated)(balancesRef)();
          };
        };
      };
    };
  };
  var initGateway = function(oracle) {
    return function(entryFee) {
      return function(exitFee) {
        return function(balances) {
          return function(nfv) {
            return function __do7() {
              var syntheticSOLState = initSyntheticSOL(oracle)();
              return {
                syntheticSOL: syntheticSOLState,
                entryFee,
                exitFee,
                userBalances: balances,
                nfvAllocationRate: 0.25,
                nfvState: nfv
              };
            };
          };
        };
      };
    };
  };
  var checkBalance = function(balancesRef) {
    return function(owner) {
      return function(token) {
        return function(requiredAmount) {
          return function __do7() {
            var balances = read(balancesRef)();
            var v = find2(function(b2) {
              return b2.owner === owner && eq13(b2.token)(token);
            })(balances);
            if (v instanceof Just) {
              return v.value0.amount >= requiredAmount;
            }
            ;
            if (v instanceof Nothing) {
              return false;
            }
            ;
            throw new Error("Failed pattern match at Gateway (line 218, column 3 - line 220, column 26): " + [v.constructor.name]);
          };
        };
      };
    };
  };
  var enterSystem = function(state3) {
    return function(user) {
      return function(jitoAmount) {
        var $31 = jitoAmount <= 0;
        if ($31) {
          return pure10(new Left("Amount must be positive"));
        }
        ;
        return function __do7() {
          var hasBalance = checkBalance(state3.userBalances)(user)(JitoSOL.value)(jitoAmount)();
          var $32 = !hasBalance;
          if ($32) {
            return new Left("Insufficient JitoSOL balance. Need " + show4(jitoAmount));
          }
          ;
          var feeAmount = jitoAmount * state3.entryFee;
          var netJitoAmount = jitoAmount - feeAmount;
          var mintResult = mintFeelsSOL(state3.syntheticSOL)(netJitoAmount)();
          if (mintResult instanceof Left) {
            return new Left(mintResult.value0);
          }
          ;
          if (mintResult instanceof Right) {
            var oraclePrice = getOraclePrice(state3.syntheticSOL)();
            var feeInFeelsSOL = feeAmount * oraclePrice.price;
            var nfvContribution = feeInFeelsSOL * state3.nfvAllocationRate;
            updateBalance(state3.userBalances)(user)(JitoSOL.value)(-jitoAmount)();
            updateBalance(state3.userBalances)(user)(FeelsSOL.value)(mintResult.value0.feelsSOLMinted)();
            contributeToNFV(state3.nfvState)(SwapTerms.value)(nfvContribution)(Nothing.value)();
            var timestamp = currentTime();
            return new Right({
              direction: Enter.value,
              inputAmount: {
                tokenType: JitoSOL.value,
                amount: jitoAmount
              },
              outputAmount: {
                tokenType: FeelsSOL.value,
                amount: mintResult.value0.feelsSOLMinted
              },
              exchangeRate: mintResult.value0.exchangeRate,
              fee: feeAmount,
              timestamp
            });
          }
          ;
          throw new Error("Failed pattern match at Gateway (line 112, column 11 - line 136, column 18): " + [mintResult.constructor.name]);
        };
      };
    };
  };
  var exitSystem = function(state3) {
    return function(user) {
      return function(feelsAmount) {
        var $36 = feelsAmount <= 0;
        if ($36) {
          return pure10(new Left("Amount must be positive"));
        }
        ;
        return function __do7() {
          var hasBalance = checkBalance(state3.userBalances)(user)(FeelsSOL.value)(feelsAmount)();
          var $37 = !hasBalance;
          if ($37) {
            return new Left("Insufficient FeelsSOL balance. Need " + show4(feelsAmount));
          }
          ;
          var burnResult = burnFeelsSOL(state3.syntheticSOL)(feelsAmount)();
          if (burnResult instanceof Left) {
            return new Left(burnResult.value0);
          }
          ;
          if (burnResult instanceof Right) {
            var feeAmount = burnResult.value0.jitoSOLReleased * state3.exitFee;
            var jitoSOLAmount = burnResult.value0.jitoSOLReleased - feeAmount;
            var oraclePrice = getOraclePrice(state3.syntheticSOL)();
            var feeInFeelsSOL = feeAmount * oraclePrice.price;
            var nfvContribution = feeInFeelsSOL * state3.nfvAllocationRate;
            updateBalance(state3.userBalances)(user)(FeelsSOL.value)(-feelsAmount)();
            updateBalance(state3.userBalances)(user)(JitoSOL.value)(jitoSOLAmount)();
            contributeToNFV(state3.nfvState)(SwapTerms.value)(nfvContribution)(Nothing.value)();
            var timestamp = currentTime();
            return new Right({
              direction: Exit.value,
              inputAmount: {
                tokenType: FeelsSOL.value,
                amount: feelsAmount
              },
              outputAmount: {
                tokenType: JitoSOL.value,
                amount: jitoSOLAmount
              },
              exchangeRate: burnResult.value0.exchangeRate,
              fee: feeAmount,
              timestamp
            });
          }
          ;
          throw new Error("Failed pattern match at Gateway (line 160, column 11 - line 188, column 18): " + [burnResult.constructor.name]);
        };
      };
    };
  };

  // output/Oracle/index.js
  var initOracle = function(lendingBook) {
    return function(nfvState) {
      return function __do7() {
        var now = currentTime();
        var initialState = {
          snapshots: [],
          lastUpdate: now,
          priceHistory: []
        };
        var state3 = $$new(initialState)();
        return {
          state: state3,
          lendingBook,
          nfvState
        };
      };
    };
  };

  // output/Incentives/index.js
  var defaultDynamicsConfig = {
    baseRates: {
      swap: 0.02,
      staking: 0.04,
      leverage: 0.06
    },
    spreads: {
      minSpread: 1e-3,
      maxSpread: 0.05,
      targetSpread: 0.01
    },
    nfvAllocation: {
      swap: 0.15,
      staking: 0.25,
      leverage: 0.3
    },
    adjustments: {
      volatilityWeight: 0.3,
      utilizationWeight: 0.4,
      healthWeight: 0.2,
      crossRiskFactor: 0.1
    },
    bounds: {
      minLenderRate: 1e-3,
      maxBorrowerRate: 0.5
    }
  };
  var initMarketDynamics = function(oracle) {
    return function(nfvState) {
      return function __do7() {
        var config = $$new(defaultDynamicsConfig)();
        return {
          config,
          oracle,
          nfvState
        };
      };
    };
  };

  // output/Protocol/index.js
  var pure11 = /* @__PURE__ */ pure(applicativeEffect);
  var show5 = /* @__PURE__ */ show(showInt);
  var show12 = /* @__PURE__ */ show(showNumber);
  var eq14 = /* @__PURE__ */ eq(eqTokenType);
  var append12 = /* @__PURE__ */ append(semigroupArray);
  var sum3 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var map27 = /* @__PURE__ */ map(functorArray);
  var nub3 = /* @__PURE__ */ nub(ordString);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
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
  var GatewayEntered = /* @__PURE__ */ function() {
    function GatewayEntered2(value0) {
      this.value0 = value0;
    }
    ;
    GatewayEntered2.create = function(value0) {
      return new GatewayEntered2(value0);
    };
    return GatewayEntered2;
  }();
  var GatewayExited = /* @__PURE__ */ function() {
    function GatewayExited2(value0) {
      this.value0 = value0;
    }
    ;
    GatewayExited2.create = function(value0) {
      return new GatewayExited2(value0);
    };
    return GatewayExited2;
  }();
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
  var ProtocolStatsResult = /* @__PURE__ */ function() {
    function ProtocolStatsResult2(value0) {
      this.value0 = value0;
    }
    ;
    ProtocolStatsResult2.create = function(value0) {
      return new ProtocolStatsResult2(value0);
    };
    return ProtocolStatsResult2;
  }();
  var NFVMetricsResult = /* @__PURE__ */ function() {
    function NFVMetricsResult2(value0) {
      this.value0 = value0;
    }
    ;
    NFVMetricsResult2.create = function(value0) {
      return new NFVMetricsResult2(value0);
    };
    return NFVMetricsResult2;
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
  var GetLenderOffers = /* @__PURE__ */ function() {
    function GetLenderOffers2() {
    }
    ;
    GetLenderOffers2.value = new GetLenderOffers2();
    return GetLenderOffers2;
  }();
  var GetProtocolStats = /* @__PURE__ */ function() {
    function GetProtocolStats2() {
    }
    ;
    GetProtocolStats2.value = new GetProtocolStats2();
    return GetProtocolStats2;
  }();
  var GetNFVMetrics = /* @__PURE__ */ function() {
    function GetNFVMetrics2() {
    }
    ;
    GetNFVMetrics2.value = new GetNFVMetrics2();
    return GetNFVMetrics2;
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
  var InvalidCommand = /* @__PURE__ */ function() {
    function InvalidCommand2(value0) {
      this.value0 = value0;
    }
    ;
    InvalidCommand2.create = function(value0) {
      return new InvalidCommand2(value0);
    };
    return InvalidCommand2;
  }();
  var InsufficientBalance = /* @__PURE__ */ function() {
    function InsufficientBalance2(value0) {
      this.value0 = value0;
    }
    ;
    InsufficientBalance2.create = function(value0) {
      return new InsufficientBalance2(value0);
    };
    return InsufficientBalance2;
  }();
  var TokenNotFound = /* @__PURE__ */ function() {
    function TokenNotFound2(value0) {
      this.value0 = value0;
    }
    ;
    TokenNotFound2.create = function(value0) {
      return new TokenNotFound2(value0);
    };
    return TokenNotFound2;
  }();
  var PositionNotFound = /* @__PURE__ */ function() {
    function PositionNotFound2(value0) {
      this.value0 = value0;
    }
    ;
    PositionNotFound2.create = function(value0) {
      return new PositionNotFound2(value0);
    };
    return PositionNotFound2;
  }();
  var UserNotFound = /* @__PURE__ */ function() {
    function UserNotFound2(value0) {
      this.value0 = value0;
    }
    ;
    UserNotFound2.create = function(value0) {
      return new UserNotFound2(value0);
    };
    return UserNotFound2;
  }();
  var InvalidAmount = /* @__PURE__ */ function() {
    function InvalidAmount2(value0) {
      this.value0 = value0;
    }
    ;
    InvalidAmount2.create = function(value0) {
      return new InvalidAmount2(value0);
    };
    return InvalidAmount2;
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
  var CreateLendingPosition = /* @__PURE__ */ function() {
    function CreateLendingPosition2(value0, value1, value22, value32, value42, value52, value62) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
    }
    ;
    CreateLendingPosition2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return new CreateLendingPosition2(value0, value1, value22, value32, value42, value52, value62);
                };
              };
            };
          };
        };
      };
    };
    return CreateLendingPosition2;
  }();
  var EnterGateway = /* @__PURE__ */ function() {
    function EnterGateway3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    EnterGateway3.create = function(value0) {
      return function(value1) {
        return new EnterGateway3(value0, value1);
      };
    };
    return EnterGateway3;
  }();
  var ExitGateway = /* @__PURE__ */ function() {
    function ExitGateway3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ExitGateway3.create = function(value0) {
      return function(value1) {
        return new ExitGateway3(value0, value1);
      };
    };
    return ExitGateway3;
  }();
  var subscribe3 = function(runtime) {
    return function(callback) {
      return function __do7() {
        var id3 = read(runtime.nextListenerId)();
        write(id3 + 1 | 0)(runtime.nextListenerId)();
        modify_2(function(listeners) {
          return cons({
            id: id3,
            callback
          })(listeners);
        })(runtime.listeners)();
        return id3;
      };
    };
  };
  var showProtocolError = {
    show: function(v) {
      if (v instanceof InvalidCommand) {
        return "Invalid command: " + v.value0;
      }
      ;
      if (v instanceof InsufficientBalance) {
        return "Insufficient balance: " + v.value0;
      }
      ;
      if (v instanceof TokenNotFound) {
        return "Token not found: " + v.value0;
      }
      ;
      if (v instanceof PositionNotFound) {
        return "Position not found: " + show5(v.value0);
      }
      ;
      if (v instanceof UserNotFound) {
        return "User not found: " + v.value0;
      }
      ;
      if (v instanceof InvalidAmount) {
        return "Invalid amount: " + show12(v.value0);
      }
      ;
      if (v instanceof SystemError) {
        return "System error: " + v.value0;
      }
      ;
      throw new Error("Failed pattern match at Protocol (line 129, column 1 - line 136, column 51): " + [v.constructor.name]);
    }
  };
  var processQuery = function(state3) {
    return function(v) {
      if (v instanceof GetUserTokens) {
        return pure11(new Right(new TokenList([])));
      }
      ;
      if (v instanceof GetAllTokens) {
        return function __do7() {
          var allTokens = read(state3.tokenRegistry)();
          return new Right(new TokenList(allTokens));
        };
      }
      ;
      if (v instanceof GetUserPositions) {
        return function __do7() {
          var positions = getUserRecords(state3.lendingBook)(v.value0)();
          return new Right(new PositionList(positions));
        };
      }
      ;
      if (v instanceof GetUserBalance) {
        return function __do7() {
          var balances = read(state3.balances)();
          var balance = function() {
            var v1 = find2(function(b2) {
              return b2.owner === v.value0 && eq14(b2.token)(v.value1);
            })(balances);
            if (v1 instanceof Just) {
              return v1.value0.amount;
            }
            ;
            if (v1 instanceof Nothing) {
              return 0;
            }
            ;
            throw new Error("Failed pattern match at Protocol (line 328, column 19 - line 330, column 25): " + [v1.constructor.name]);
          }();
          return new Right(new Balance(balance));
        };
      }
      ;
      if (v instanceof GetLenderOffers) {
        return function __do7() {
          var offers = getLenderRecords(state3.lendingBook)();
          return new Right(new LenderOfferList(offers));
        };
      }
      ;
      if (v instanceof GetProtocolStats) {
        return function __do7() {
          var activeRecords = getActiveRecords(state3.lendingBook)();
          var lenderRecords = getLenderRecords(state3.lendingBook)();
          var allRecords = append12(activeRecords)(lenderRecords);
          var totalValueLocked = sum3(map27(function(r) {
            return r.lendAmount;
          })(allRecords));
          var uniqueUsers = nub3(map27(function(r) {
            return r.owner;
          })(allRecords));
          var userCount = function() {
            var $66 = length(uniqueUsers) === 0;
            if ($66) {
              return 1;
            }
            ;
            return length(uniqueUsers);
          }();
          var tokenList = read(state3.tokenRegistry)();
          var launchedCount = length(filter(function(t) {
            return t.launched;
          })(tokenList));
          var nfvBalance = getNFVBalance(state3.nfvState)();
          var totalLenderOffers = length(lenderRecords);
          var balances = read(state3.balances)();
          var jitoSOLLocked = sum3(map27(function(b2) {
            var $67 = eq14(b2.token)(JitoSOL.value);
            if ($67) {
              return b2.amount;
            }
            ;
            return 0;
          })(balances));
          var feelsSOLSupply = sum3(map27(function(b2) {
            var $68 = eq14(b2.token)(FeelsSOL.value);
            if ($68) {
              return b2.amount;
            }
            ;
            return 0;
          })(balances));
          return new Right(new ProtocolStatsResult({
            totalValueLocked,
            totalUsers: userCount,
            activePositions: length(activeRecords),
            launchedTokens: launchedCount,
            totalLenderOffers,
            nfvBalance,
            feelsSOLSupply,
            jitoSOLLocked
          }));
        };
      }
      ;
      if (v instanceof GetNFVMetrics) {
        return function __do7() {
          var balance = getNFVBalance(state3.nfvState)();
          var metrics = getNFVMetrics(state3.nfvState)();
          return new Right(new NFVMetricsResult({
            balance,
            growthRate24h: metrics.growthRate24h,
            utilizationRate: metrics.utilizationRate
          }));
        };
      }
      ;
      if (v instanceof GetPositionTargetToken) {
        var targetToken = function() {
          var v1 = find2(function(m) {
            return m.positionId === v.value0;
          })(state3.positionTokenMap);
          if (v1 instanceof Just) {
            return new Just(v1.value0.tokenTicker);
          }
          ;
          if (v1 instanceof Nothing) {
            return Nothing.value;
          }
          ;
          throw new Error("Failed pattern match at Protocol (line 378, column 23 - line 380, column 29): " + [v1.constructor.name]);
        }();
        return pure11(new Right(new TargetTokenInfo(targetToken)));
      }
      ;
      return pure11(new Left(new InvalidCommand("Query not implemented")));
    };
  };
  var processCommand = function(state3) {
    return function(v) {
      if (v instanceof CreateToken) {
        var $73 = v.value1 === "" || v.value2 === "";
        if ($73) {
          return pure11(new Left(new InvalidCommand("Ticker and name cannot be empty")));
        }
        ;
        var tokenParams = {
          ticker: v.value1,
          name: v.value2,
          creator: v.value0
        };
        return function __do7() {
          var newToken = createAndRegisterToken(state3.tokenRegistry)(tokenParams)();
          var timestamp = currentTime();
          var newState = {
            balances: state3.balances,
            currentUser: state3.currentUser,
            gateway: state3.gateway,
            lendingBook: state3.lendingBook,
            marketDynamics: state3.marketDynamics,
            nfvState: state3.nfvState,
            oracle: state3.oracle,
            positionTokenMap: state3.positionTokenMap,
            tokenRegistry: state3.tokenRegistry,
            timestamp
          };
          return new Right(new Tuple(newState, new TokenCreated(newToken)));
        };
      }
      ;
      if (v instanceof CreateLendingPosition) {
        var $77 = v.value2 <= 0;
        if ($77) {
          return pure11(new Left(new InvalidAmount(v.value2)));
        }
        ;
        return function __do7() {
          var id3 = generateRecordId();
          var timestamp = currentTime();
          var record = function() {
            if (v.value5 instanceof StakingTerms) {
              return createLenderRecord(id3)(v.value0)(v.value1)(v.value2)(v.value3)(v.value4 / v.value2)(v.value5)(timestamp);
            }
            ;
            return createBorrowerRecord(id3)(v.value0)(v.value1)(v.value2)(v.value3)(v.value4)(v.value5)(timestamp);
          }();
          var result = createLendOffer(state3.lendingBook)(v.value0)(v.value1)(v.value2)(v.value3)(v.value4)(v.value5)();
          if (result instanceof Left) {
            return new Left(new SystemError(result.value0));
          }
          ;
          if (result instanceof Right) {
            var newMapping = function() {
              if (v.value6 instanceof Just) {
                return cons({
                  positionId: result.value0.id,
                  tokenTicker: v.value6.value0
                })(state3.positionTokenMap);
              }
              ;
              if (v.value6 instanceof Nothing) {
                return state3.positionTokenMap;
              }
              ;
              throw new Error("Failed pattern match at Protocol (line 250, column 30 - line 252, column 52): " + [v.value6.constructor.name]);
            }();
            (function() {
              if (v.value5 instanceof StakingTerms && v.value6 instanceof Just) {
                var maybeToken = getTokenFromRegistry(state3.tokenRegistry)(v.value6.value0)();
                if (maybeToken instanceof Just) {
                  var newStakedAmount = maybeToken.value0.stakedFeelsSOL + v.value2;
                  var shouldLaunch = newStakedAmount >= 100;
                  return unit;
                }
                ;
                if (maybeToken instanceof Nothing) {
                  return unit;
                }
                ;
                throw new Error("Failed pattern match at Protocol (line 259, column 17 - line 266, column 39): " + [maybeToken.constructor.name]);
              }
              ;
              return unit;
            })();
            var newState = {
              balances: state3.balances,
              currentUser: state3.currentUser,
              gateway: state3.gateway,
              lendingBook: state3.lendingBook,
              marketDynamics: state3.marketDynamics,
              nfvState: state3.nfvState,
              oracle: state3.oracle,
              tokenRegistry: state3.tokenRegistry,
              positionTokenMap: newMapping,
              timestamp
            };
            return new Right(new Tuple(newState, new PositionCreated(result.value0)));
          }
          ;
          throw new Error("Failed pattern match at Protocol (line 246, column 9 - line 274, column 74): " + [result.constructor.name]);
        };
      }
      ;
      if (v instanceof EnterGateway) {
        return function __do7() {
          var result = enterSystem(state3.gateway)(v.value0)(v.value1)();
          if (result instanceof Left) {
            return new Left(new SystemError(result.value0));
          }
          ;
          if (result instanceof Right) {
            var timestamp = currentTime();
            var newState = {
              balances: state3.balances,
              currentUser: state3.currentUser,
              gateway: state3.gateway,
              lendingBook: state3.lendingBook,
              marketDynamics: state3.marketDynamics,
              nfvState: state3.nfvState,
              oracle: state3.oracle,
              positionTokenMap: state3.positionTokenMap,
              tokenRegistry: state3.tokenRegistry,
              timestamp
            };
            return new Right(new Tuple(newState, new GatewayEntered({
              user: v.value0,
              feelsSOLMinted: result.value0.outputAmount.amount
            })));
          }
          ;
          throw new Error("Failed pattern match at Protocol (line 278, column 5 - line 284, column 66): " + [result.constructor.name]);
        };
      }
      ;
      if (v instanceof ExitGateway) {
        return function __do7() {
          var result = exitSystem(state3.gateway)(v.value0)(v.value1)();
          if (result instanceof Left) {
            return new Left(new SystemError(result.value0));
          }
          ;
          if (result instanceof Right) {
            var timestamp = currentTime();
            var newState = {
              balances: state3.balances,
              currentUser: state3.currentUser,
              gateway: state3.gateway,
              lendingBook: state3.lendingBook,
              marketDynamics: state3.marketDynamics,
              nfvState: state3.nfvState,
              oracle: state3.oracle,
              positionTokenMap: state3.positionTokenMap,
              tokenRegistry: state3.tokenRegistry,
              timestamp
            };
            return new Right(new Tuple(newState, new GatewayExited({
              user: v.value0,
              jitoSOLReceived: result.value0.outputAmount.amount
            })));
          }
          ;
          throw new Error("Failed pattern match at Protocol (line 288, column 5 - line 294, column 67): " + [result.constructor.name]);
        };
      }
      ;
      return pure11(new Left(new InvalidCommand("Command not implemented")));
    };
  };
  var initProtocol = function __do4() {
    var tokenRegistry = initTokenRegistry();
    var lendingBook = initLendingBook();
    var nfvState = initNFV();
    var oracle = initOracle(lendingBook)(nfvState)();
    var marketDynamics = initMarketDynamics(oracle)(nfvState)();
    var balances = $$new([{
      owner: "main-user",
      token: JitoSOL.value,
      amount: 5e3
    }, {
      owner: "main-user",
      token: FeelsSOL.value,
      amount: 5e3
    }])();
    var priceOracle = pure11(1.05);
    var gateway = initGateway(priceOracle)(1e-3)(2e-3)(balances)(nfvState)();
    var timestamp = currentTime();
    var initialState = {
      tokenRegistry,
      lendingBook,
      gateway,
      nfvState,
      oracle,
      marketDynamics,
      balances,
      positionTokenMap: [],
      currentUser: "user1",
      timestamp
    };
    var stateRef = $$new(initialState)();
    var listenersRef = $$new([])();
    var nextListenerIdRef = $$new(0)();
    return {
      state: stateRef,
      listeners: listenersRef,
      nextListenerId: nextListenerIdRef
    };
  };
  var executeQuery = function(runtime) {
    return function(query2) {
      return function __do7() {
        var state3 = read(runtime.state)();
        return processQuery(state3)(query2)();
      };
    };
  };
  var executeCommand = function(runtime) {
    return function(cmd) {
      return function __do7() {
        var state3 = read(runtime.state)();
        var result = processCommand(state3)(cmd)();
        if (result instanceof Right) {
          write(result.value0.value0)(runtime.state)();
          var listeners = read(runtime.listeners)();
          traverse2(function(l) {
            return l.callback(result.value0.value0);
          })(listeners)();
          return new Right(result.value0.value1);
        }
        ;
        if (result instanceof Left) {
          return new Left(result.value0);
        }
        ;
        throw new Error("Failed pattern match at Protocol (line 195, column 3 - line 205, column 32): " + [result.constructor.name]);
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

  // output/Utils/index.js
  var show6 = /* @__PURE__ */ show(showNumber);
  var show13 = /* @__PURE__ */ show(showInt);
  var formatPercentage = function(n) {
    return show6(toNumber2(floor3(n * 100))) + "%";
  };
  var formatAmount = function(amount) {
    var rounded = toNumber2(floor3(amount * 100)) / 100;
    var $14 = amount === toNumber2(floor3(amount));
    if ($14) {
      return show13(floor3(amount));
    }
    ;
    return show6(rounded);
  };

  // output/Simulation/index.js
  var map28 = /* @__PURE__ */ map(functorArray);
  var show7 = /* @__PURE__ */ show(showTokenType);
  var show14 = /* @__PURE__ */ show(showLendingTerms);
  var show22 = /* @__PURE__ */ show(showInt);
  var pure13 = /* @__PURE__ */ pure(applicativeEffect);
  var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumCodePoint);
  var max7 = /* @__PURE__ */ max(ordInt);
  var sequence2 = /* @__PURE__ */ sequence(traversableArray)(applicativeEffect);
  var traverse3 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
  var show32 = /* @__PURE__ */ show(showNumber);
  var show42 = /* @__PURE__ */ show(showLendingStatus);
  var foldl3 = /* @__PURE__ */ foldl(foldableArray);
  var append13 = /* @__PURE__ */ append(semigroupArray);
  var sum4 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
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
  var CreateLendOffer = /* @__PURE__ */ function() {
    function CreateLendOffer2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    CreateLendOffer2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new CreateLendOffer2(value0, value1, value22, value32, value42, value52);
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
  var ClosePosition = /* @__PURE__ */ function() {
    function ClosePosition2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ClosePosition2.create = function(value0) {
      return function(value1) {
        return new ClosePosition2(value0, value1);
      };
    };
    return ClosePosition2;
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
  var updateAccountBalances = function(accounts) {
    return function(userId) {
      return function(amount) {
        return function(asset) {
          return map28(function(acc) {
            var $113 = acc.id === userId;
            if ($113) {
              if (asset instanceof JitoSOL) {
                return {
                  id: acc.id,
                  feelsSOLBalance: acc.feelsSOLBalance,
                  activePositions: acc.activePositions,
                  netPnL: acc.netPnL,
                  profile: acc.profile,
                  jitoSOLBalance: acc.jitoSOLBalance + amount,
                  totalDeposited: acc.totalDeposited + function() {
                    var $115 = amount > 0;
                    if ($115) {
                      return amount;
                    }
                    ;
                    return 0;
                  }(),
                  totalWithdrawn: acc.totalWithdrawn + function() {
                    var $116 = amount < 0;
                    if ($116) {
                      return -amount;
                    }
                    ;
                    return 0;
                  }()
                };
              }
              ;
              if (asset instanceof FeelsSOL) {
                return {
                  id: acc.id,
                  jitoSOLBalance: acc.jitoSOLBalance,
                  activePositions: acc.activePositions,
                  netPnL: acc.netPnL,
                  profile: acc.profile,
                  feelsSOLBalance: acc.feelsSOLBalance + amount,
                  totalDeposited: acc.totalDeposited + function() {
                    var $117 = amount > 0;
                    if ($117) {
                      return amount;
                    }
                    ;
                    return 0;
                  }(),
                  totalWithdrawn: acc.totalWithdrawn + function() {
                    var $118 = amount < 0;
                    if ($118) {
                      return -amount;
                    }
                    ;
                    return 0;
                  }()
                };
              }
              ;
              return acc;
            }
            ;
            return acc;
          })(accounts);
        };
      };
    };
  };
  var showTradingAction = {
    show: function(v) {
      if (v instanceof EnterProtocol) {
        return v.value0 + (" enters " + (formatAmount(v.value1) + (" " + show7(v.value2))));
      }
      ;
      if (v instanceof ExitProtocol) {
        return v.value0 + (" exits " + (formatAmount(v.value1) + (" " + show7(v.value2))));
      }
      ;
      if (v instanceof CreateToken2) {
        return v.value0 + (" creates token " + (v.value1 + (" (" + (v.value2 + ")"))));
      }
      ;
      if (v instanceof CreateLendOffer) {
        return v.value0 + (" offers " + (formatAmount(v.value2) + (" " + (show7(v.value1) + (" @ " + (formatAmount(v.value4) + (" " + (show7(v.value3) + (" (" + (show14(v.value5) + ")"))))))))));
      }
      ;
      if (v instanceof TakeLoan) {
        return v.value0 + (" borrows " + (formatAmount(v.value2) + (" " + (show7(v.value1) + (" with " + (formatAmount(v.value4) + (" " + (show7(v.value3) + (" (" + (show14(v.value5) + ")"))))))))));
      }
      ;
      if (v instanceof ClosePosition) {
        return v.value0 + (" closes position #" + show22(v.value1));
      }
      ;
      if (v instanceof WaitBlocks) {
        return "Wait " + (show22(v.value0) + " blocks");
      }
      ;
      throw new Error("Failed pattern match at Simulation (line 105, column 1 - line 112, column 65): " + [v.constructor.name]);
    }
  };
  var show52 = /* @__PURE__ */ show(showTradingAction);
  var markAccountActive = function(accounts) {
    return function(userId) {
      return function(amount) {
        return map28(function(acc) {
          var $146 = acc.id === userId;
          if ($146) {
            return {
              id: acc.id,
              activePositions: acc.activePositions,
              feelsSOLBalance: acc.feelsSOLBalance,
              jitoSOLBalance: acc.jitoSOLBalance,
              profile: acc.profile,
              totalWithdrawn: acc.totalWithdrawn,
              totalDeposited: acc.totalDeposited + amount,
              netPnL: acc.netPnL + amount * 0.01
            };
          }
          ;
          return acc;
        })(accounts);
      };
    };
  };
  var generateTokenCreationAction = function(config) {
    return function(state3) {
      return function __do7() {
        var accountIndex = randomInt(0)(length(state3.accounts) - 1 | 0)();
        var v = head(drop(accountIndex)(state3.accounts));
        if (v instanceof Just) {
          var rand1 = randomInt(65)(90)();
          var rand2 = randomInt(65)(90)();
          var rand3 = randomInt(65)(90)();
          var c3 = function() {
            var v1 = toEnum2(rand3);
            if (v1 instanceof Just) {
              return singleton4(v1.value0);
            }
            ;
            if (v1 instanceof Nothing) {
              return "A";
            }
            ;
            throw new Error("Failed pattern match at Simulation (line 324, column 16 - line 326, column 32): " + [v1.constructor.name]);
          }();
          var c2 = function() {
            var v1 = toEnum2(rand2);
            if (v1 instanceof Just) {
              return singleton4(v1.value0);
            }
            ;
            if (v1 instanceof Nothing) {
              return "A";
            }
            ;
            throw new Error("Failed pattern match at Simulation (line 321, column 16 - line 323, column 32): " + [v1.constructor.name]);
          }();
          var c1 = function() {
            var v1 = toEnum2(rand1);
            if (v1 instanceof Just) {
              return singleton4(v1.value0);
            }
            ;
            if (v1 instanceof Nothing) {
              return "A";
            }
            ;
            throw new Error("Failed pattern match at Simulation (line 318, column 16 - line 320, column 32): " + [v1.constructor.name]);
          }();
          var ticker = c1 + (c2 + c3);
          var name15 = ticker + " Token";
          return new CreateToken2(v.value0.id, ticker, name15);
        }
        ;
        if (v instanceof Nothing) {
          return new WaitBlocks(1);
        }
        ;
        throw new Error("Failed pattern match at Simulation (line 312, column 3 - line 330, column 35): " + [v.constructor.name]);
      };
    };
  };
  var generateMarketScenario = function(config) {
    return function(currentBlock) {
      return function __do7() {
        var baseVolatility = random();
        var volatility2 = config.priceVolatility * baseVolatility;
        if (config.scenario instanceof BullMarket) {
          var trend = random();
          return 1e-3 + trend * 5e-3;
        }
        ;
        if (config.scenario instanceof BearMarket) {
          var trend = random();
          return -6e-3 + trend * 5e-3;
        }
        ;
        if (config.scenario instanceof SidewaysMarket) {
          var noise = random();
          return (noise - 0.5) * 2e-3;
        }
        ;
        if (config.scenario instanceof VolatileMarket) {
          var swing = random();
          var direction = random();
          var magnitude = 0.01 + swing * 0.04;
          var $156 = direction > 0.5;
          if ($156) {
            return magnitude;
          }
          ;
          return -magnitude;
        }
        ;
        if (config.scenario instanceof CrashScenario) {
          var $157 = currentBlock < 10;
          if ($157) {
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
        throw new Error("Failed pattern match at Simulation (line 249, column 3 - line 280, column 54): " + [config.scenario.constructor.name]);
      };
    };
  };
  var generateLendingTerms = function(config) {
    return function __do7() {
      var termType = random();
      var $158 = termType < config.stakingPreference;
      if ($158) {
        var periodRand = random();
        var period = function() {
          var $159 = periodRand < 0.5;
          if ($159) {
            return Days30.value;
          }
          ;
          var $160 = periodRand < 0.8;
          if ($160) {
            return Days60.value;
          }
          ;
          return Days90.value;
        }();
        return new StakingTerms(period);
      }
      ;
      var $161 = termType < config.stakingPreference + config.leveragePreference;
      if ($161) {
        var leverageRand = random();
        var leverage = 1.5 + leverageRand * 8.5;
        return new LeverageTerms(leverage);
      }
      ;
      return SwapTerms.value;
    };
  };
  var generateLendOfferAction = function(config) {
    return function(account) {
      var $162 = account.feelsSOLBalance <= 0;
      if ($162) {
        return pure13(new WaitBlocks(1));
      }
      ;
      return function __do7() {
        var portion = random();
        var amount = account.feelsSOLBalance * (0.2 + portion * 0.6);
        var ratioRand = random();
        var collateralRatio = function() {
          if (account.profile instanceof Conservative) {
            return 1.2 + ratioRand * 0.3;
          }
          ;
          if (account.profile instanceof Moderate) {
            return 1.1 + ratioRand * 0.4;
          }
          ;
          if (account.profile instanceof Aggressive) {
            return 1.05 + ratioRand * 0.25;
          }
          ;
          if (account.profile instanceof Whale) {
            return 1.15 + ratioRand * 0.35;
          }
          ;
          if (account.profile instanceof Arbitrageur) {
            return 1.08 + ratioRand * 0.17;
          }
          ;
          if (account.profile instanceof Retail) {
            return 1.25 + ratioRand * 0.5;
          }
          ;
          throw new Error("Failed pattern match at Simulation (line 417, column 29 - line 423, column 45): " + [account.profile.constructor.name]);
        }();
        var terms = generateLendingTerms(config)();
        return new CreateLendOffer(account.id, FeelsSOL.value, amount, JitoSOL.value, collateralRatio, terms);
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
        return generateLendOfferAction(config)(v.value0);
      }
      ;
      if (v instanceof Nothing) {
        return function __do7() {
          var accountIndex = randomInt(0)(length(state3.accounts) - 1 | 0)();
          var v1 = head(drop(accountIndex)(state3.accounts));
          if (v1 instanceof Just) {
            var $167 = v1.value0.jitoSOLBalance > 0;
            if ($167) {
              return new EnterProtocol(v1.value0.id, v1.value0.jitoSOLBalance * 0.4, JitoSOL.value);
            }
            ;
            return new WaitBlocks(1);
          }
          ;
          if (v1 instanceof Nothing) {
            return new WaitBlocks(1);
          }
          ;
          throw new Error("Failed pattern match at Simulation (line 343, column 7 - line 348, column 39): " + [v1.constructor.name]);
        };
      }
      ;
      throw new Error("Failed pattern match at Simulation (line 338, column 3 - line 348, column 39): " + [v.constructor.name]);
    };
  };
  var generateExitAction = function(config) {
    return function(account) {
      var $169 = account.feelsSOLBalance <= 0;
      if ($169) {
        return pure13(new WaitBlocks(1));
      }
      ;
      return function __do7() {
        var portion = random();
        var amount = account.feelsSOLBalance * (0.1 + portion * 0.5);
        return new ExitProtocol(account.id, amount, FeelsSOL.value);
      };
    };
  };
  var generateEntryAction = function(config) {
    return function(account) {
      return function __do7() {
        var portion = random();
        var amount = function() {
          if (account.profile instanceof Conservative) {
            return account.jitoSOLBalance * (0.1 + portion * 0.2);
          }
          ;
          if (account.profile instanceof Moderate) {
            return account.jitoSOLBalance * (0.2 + portion * 0.3);
          }
          ;
          if (account.profile instanceof Aggressive) {
            return account.jitoSOLBalance * (0.4 + portion * 0.5);
          }
          ;
          if (account.profile instanceof Whale) {
            return account.jitoSOLBalance * (0.05 + portion * 0.15);
          }
          ;
          if (account.profile instanceof Arbitrageur) {
            return account.jitoSOLBalance * (0.3 + portion * 0.4);
          }
          ;
          if (account.profile instanceof Retail) {
            return account.jitoSOLBalance * (0.5 + portion * 0.4);
          }
          ;
          throw new Error("Failed pattern match at Simulation (line 384, column 16 - line 390, column 65): " + [account.profile.constructor.name]);
        }();
        return new EnterProtocol(account.id, amount, JitoSOL.value);
      };
    };
  };
  var generateCloseAction = function(config) {
    return function(account) {
      var v = head(account.activePositions);
      if (v instanceof Just) {
        return pure13(new ClosePosition(account.id, v.value0));
      }
      ;
      if (v instanceof Nothing) {
        return pure13(new WaitBlocks(1));
      }
      ;
      throw new Error("Failed pattern match at Simulation (line 450, column 3 - line 452, column 35): " + [v.constructor.name]);
    };
  };
  var generateBorrowAction = function(config) {
    return function(account) {
      var $173 = account.jitoSOLBalance <= 0;
      if ($173) {
        return pure13(new WaitBlocks(1));
      }
      ;
      return function __do7() {
        var portion = random();
        var maxBorrowAmount = account.jitoSOLBalance / 1.5;
        var amount = maxBorrowAmount * (0.1 + portion * 0.4);
        var collateralAmount = amount * 1.5;
        var terms = generateLendingTerms(config)();
        return new TakeLoan(account.id, FeelsSOL.value, amount, JitoSOL.value, collateralAmount, terms);
      };
    };
  };
  var generateRandomAction = function(config) {
    return function(state3) {
      return function __do7() {
        var accountIndex = randomInt(0)(length(state3.accounts) - 1 | 0)();
        var account = function() {
          var v = head(drop(accountIndex)(state3.accounts));
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
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
          throw new Error("Failed pattern match at Simulation (line 355, column 17 - line 358, column 97): " + [v.constructor.name]);
        }();
        var $176 = account.feelsSOLBalance <= 0 && account.jitoSOLBalance > 0;
        if ($176) {
          return generateEntryAction(config)(account)();
        }
        ;
        var $177 = length(account.activePositions) > 5;
        if ($177) {
          return generateCloseAction(config)(account)();
        }
        ;
        var actionRoll = random();
        var $178 = actionRoll < 0.15;
        if ($178) {
          return generateEntryAction(config)(account)();
        }
        ;
        var $179 = actionRoll < 0.25;
        if ($179) {
          return generateExitAction(config)(account)();
        }
        ;
        var $180 = actionRoll < 0.5;
        if ($180) {
          return generateLendOfferAction(config)(account)();
        }
        ;
        var $181 = actionRoll < 0.75;
        if ($181) {
          return generateBorrowAction(config)(account)();
        }
        ;
        var $182 = actionRoll < 0.9;
        if ($182) {
          return generateCloseAction(config)(account)();
        }
        ;
        return new WaitBlocks(1);
      };
    };
  };
  var generateTradingSequence = function(config) {
    return function(state3) {
      return function __do7() {
        var baseActions = random();
        var baseNumActions = floor3(config.actionFrequency * (0.5 + baseActions));
        var numActions = max7(3)(baseNumActions);
        var actions = sequence2(map28(function(i2) {
          var $183 = i2 === 1;
          if ($183) {
            return generateTokenCreationAction(config)(state3);
          }
          ;
          var $184 = i2 === 2;
          if ($184) {
            return generatePositionCreationAction(config)(state3);
          }
          ;
          return generateRandomAction(config)(state3);
        })(range2(1)(numActions)))();
        return actions;
      };
    };
  };
  var generateAccounts = function(config) {
    var generateAccount = function(profiles) {
      return function(userId) {
        return function __do7() {
          var profileIndex = randomInt(0)(length(profiles) - 1 | 0)();
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
            throw new Error("Failed pattern match at Simulation (line 215, column 21 - line 217, column 32): " + [v.constructor.name]);
          }();
          var baseBalance = random();
          var multiplier = function() {
            if (profile instanceof Whale) {
              return 1e4 + baseBalance * 9e4;
            }
            ;
            if (profile instanceof Aggressive) {
              return 1e3 + baseBalance * 9e3;
            }
            ;
            if (profile instanceof Conservative) {
              return 100 + baseBalance * 900;
            }
            ;
            if (profile instanceof Arbitrageur) {
              return 5e3 + baseBalance * 15e3;
            }
            ;
            if (profile instanceof Moderate) {
              return 500 + baseBalance * 2e3;
            }
            ;
            if (profile instanceof Retail) {
              return 50 + baseBalance * 200;
            }
            ;
            throw new Error("Failed pattern match at Simulation (line 221, column 24 - line 227, column 49): " + [profile.constructor.name]);
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
      var accountIds = sequence2(map28(function(i2) {
        return pure13("user" + show22(i2));
      })(range2(1)(config.numAccounts)))();
      return traverse3(generateAccount(config.accountProfiles))(accountIds)();
    };
  };
  var initSimulationWithLendingBook = function(config) {
    return function(existingLendingBook) {
      return function __do7() {
        var accounts = generateAccounts(config)();
        var nfv = initNFV();
        var priceOracle = pure13(config.initialJitoSOLPrice);
        var syntheticSOL = initSyntheticSOL(priceOracle)();
        var userBalances = $$new([])();
        var gateway = {
          syntheticSOL,
          entryFee: 1e-3,
          exitFee: 2e-3,
          nfvAllocationRate: 0.1,
          userBalances,
          nfvState: nfv
        };
        return {
          accounts,
          lendingBook: existingLendingBook,
          gateway,
          currentBlock: 0,
          currentPrice: config.initialJitoSOLPrice,
          priceHistory: [config.initialJitoSOLPrice],
          actionHistory: [],
          nextPositionId: 1
        };
      };
    };
  };
  var executeAction = function(stateEffect) {
    return function(action2) {
      return function __do7() {
        var state3 = stateEffect();
        if (action2 instanceof EnterProtocol) {
          log("Executing: " + show52(action2))();
          var updatedAccounts1 = updateAccountBalances(state3.accounts)(action2.value0)(-action2.value1)(JitoSOL.value);
          var updatedAccounts2 = updateAccountBalances(updatedAccounts1)(action2.value0)(action2.value1)(FeelsSOL.value);
          var feeAmount = action2.value1 * 1e-3;
          contributeToNFV(state3.gateway.nfvState)(SwapTerms.value)(feeAmount)(Nothing.value)();
          return {
            actionHistory: state3.actionHistory,
            currentBlock: state3.currentBlock,
            currentPrice: state3.currentPrice,
            gateway: state3.gateway,
            lendingBook: state3.lendingBook,
            nextPositionId: state3.nextPositionId,
            priceHistory: state3.priceHistory,
            accounts: updatedAccounts2
          };
        }
        ;
        if (action2 instanceof ExitProtocol) {
          log("Executing: " + show52(action2))();
          var updatedAccounts1 = updateAccountBalances(state3.accounts)(action2.value0)(-action2.value1)(FeelsSOL.value);
          var updatedAccounts2 = updateAccountBalances(updatedAccounts1)(action2.value0)(action2.value1)(JitoSOL.value);
          return {
            actionHistory: state3.actionHistory,
            currentBlock: state3.currentBlock,
            currentPrice: state3.currentPrice,
            gateway: state3.gateway,
            lendingBook: state3.lendingBook,
            nextPositionId: state3.nextPositionId,
            priceHistory: state3.priceHistory,
            accounts: updatedAccounts2
          };
        }
        ;
        if (action2 instanceof CreateToken2) {
          log("Executing: " + show52(action2))();
          return state3;
        }
        ;
        if (action2 instanceof CreateLendOffer) {
          log("Executing: " + show52(action2))();
          var result = createLendOffer(state3.lendingBook)(action2.value0)(action2.value1)(action2.value2)(action2.value3)(action2.value4)(action2.value5)();
          if (result instanceof Left) {
            log("Failed to create lending offer: " + result.value0)();
            return state3;
          }
          ;
          if (result instanceof Right) {
            log("Created lending offer with ID: " + (show22(result.value0.id) + (" - " + (show32(result.value0.lendAmount) + (" " + (show7(result.value0.lendAsset) + (" (status: " + (show42(result.value0.status) + ")"))))))))();
            var feeAmount = action2.value2 * 5e-4;
            contributeToNFV(state3.gateway.nfvState)(action2.value5)(feeAmount)(Nothing.value)();
            return state3;
          }
          ;
          throw new Error("Failed pattern match at Simulation (line 562, column 7 - line 575, column 21): " + [result.constructor.name]);
        }
        ;
        if (action2 instanceof TakeLoan) {
          log("Executing: " + show52(action2))();
          var feeAmount = action2.value2 * 2e-3;
          contributeToNFV(state3.gateway.nfvState)(action2.value5)(feeAmount)(Nothing.value)();
          var updatedAccounts = markAccountActive(state3.accounts)(action2.value0)(action2.value2);
          return {
            actionHistory: state3.actionHistory,
            currentBlock: state3.currentBlock,
            currentPrice: state3.currentPrice,
            gateway: state3.gateway,
            lendingBook: state3.lendingBook,
            nextPositionId: state3.nextPositionId,
            priceHistory: state3.priceHistory,
            accounts: updatedAccounts
          };
        }
        ;
        if (action2 instanceof ClosePosition) {
          log("Executing: " + show52(action2))();
          return state3;
        }
        ;
        if (action2 instanceof WaitBlocks) {
          return state3;
        }
        ;
        throw new Error("Failed pattern match at Simulation (line 531, column 3 - line 591, column 17): " + [action2.constructor.name]);
      };
    };
  };
  var executeSimulationBlock = function(config) {
    return function(state3) {
      return function(blockNum) {
        return function __do7() {
          var priceChange = generateMarketScenario(config)(blockNum)();
          var newPrice = state3.currentPrice * (1 + priceChange);
          var actions = generateTradingSequence(config)(state3)();
          var newState = foldl3(executeAction)(pure13(state3))(actions)();
          return {
            accounts: newState.accounts,
            gateway: newState.gateway,
            lendingBook: newState.lendingBook,
            nextPositionId: newState.nextPositionId,
            currentBlock: blockNum,
            currentPrice: newPrice,
            priceHistory: cons(newPrice)(state3.priceHistory),
            actionHistory: append13(state3.actionHistory)(actions)
          };
        };
      };
    };
  };
  var executeSimulation = function(config) {
    return function(initialState) {
      var executeBlock = function(stateEffect) {
        return function(blockNum) {
          return function __do7() {
            var state3 = stateEffect();
            return executeSimulationBlock(config)(state3)(blockNum)();
          };
        };
      };
      return foldl3(executeBlock)(pure13(initialState))(range2(1)(config.simulationBlocks));
    };
  };
  var calculateResults = function(config) {
    return function(finalState) {
      var priceChange = (finalState.currentPrice - config.initialJitoSOLPrice) / config.initialJitoSOLPrice;
      var avgPrice = sum4(finalState.priceHistory) / toNumber2(length(finalState.priceHistory));
      var variance = sum4(map28(function(p2) {
        return (p2 - avgPrice) * (p2 - avgPrice);
      })(finalState.priceHistory)) / toNumber2(length(finalState.priceHistory));
      var volatility2 = sqrt(variance) / avgPrice;
      return pure13({
        totalVolume: 0,
        totalFees: 0,
        activePositions: 0,
        totalUsers: length(finalState.accounts),
        priceChange,
        volatility: volatility2,
        protocolTVL: 0,
        averageUtilization: 0,
        scenarioSuccess: true
      });
    };
  };

  // output/UI/index.js
  var map29 = /* @__PURE__ */ map(functorArray);
  var type_19 = /* @__PURE__ */ type_18(isPropInputType);
  var show8 = /* @__PURE__ */ show(showInt);
  var value13 = /* @__PURE__ */ value12(isPropString);
  var show15 = /* @__PURE__ */ show(showUnbondingPeriod);
  var show23 = /* @__PURE__ */ show(showLendingSide);
  var show33 = /* @__PURE__ */ show(showLendingStatus);
  var show43 = /* @__PURE__ */ show(showTokenType);
  var append14 = /* @__PURE__ */ append(semigroupArray);
  var show53 = /* @__PURE__ */ show(showNumber);
  var discard6 = /* @__PURE__ */ discard(discardUnit);
  var show62 = /* @__PURE__ */ show(showProtocolError);
  var pure14 = /* @__PURE__ */ pure(applicativeHalogenM);
  var bind15 = /* @__PURE__ */ bind(bindHalogenM);
  var discard23 = /* @__PURE__ */ discard6(bindHalogenM);
  var modify_3 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var get2 = /* @__PURE__ */ get(monadStateHalogenM);
  var traverse_7 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray);
  var $$void8 = /* @__PURE__ */ $$void(functorEffect);
  var eq22 = /* @__PURE__ */ eq(eqUnbondingPeriod);
  var traverse4 = /* @__PURE__ */ traverse(traversableArray)(applicativeHalogenM);
  var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
  var void1 = /* @__PURE__ */ $$void(functorHalogenM);
  var pure15 = /* @__PURE__ */ pure(applicativeEffect);
  var bind22 = /* @__PURE__ */ bind(bindAff);
  var discard32 = /* @__PURE__ */ discard6(bindAff);
  var liftEffect7 = /* @__PURE__ */ liftEffect(monadEffectAff);
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
  var SelectAsset = /* @__PURE__ */ function() {
    function SelectAsset2(value0) {
      this.value0 = value0;
    }
    ;
    SelectAsset2.create = function(value0) {
      return new SelectAsset2(value0);
    };
    return SelectAsset2;
  }();
  var SelectCollateralAsset = /* @__PURE__ */ function() {
    function SelectCollateralAsset2(value0) {
      this.value0 = value0;
    }
    ;
    SelectCollateralAsset2.create = function(value0) {
      return new SelectCollateralAsset2(value0);
    };
    return SelectCollateralAsset2;
  }();
  var SetUnbondingPeriod = /* @__PURE__ */ function() {
    function SetUnbondingPeriod2(value0) {
      this.value0 = value0;
    }
    ;
    SetUnbondingPeriod2.create = function(value0) {
      return new SetUnbondingPeriod2(value0);
    };
    return SetUnbondingPeriod2;
  }();
  var SetLeverage = /* @__PURE__ */ function() {
    function SetLeverage2(value0) {
      this.value0 = value0;
    }
    ;
    SetLeverage2.create = function(value0) {
      return new SetLeverage2(value0);
    };
    return SetLeverage2;
  }();
  var CreatePosition = /* @__PURE__ */ function() {
    function CreatePosition2() {
    }
    ;
    CreatePosition2.value = new CreatePosition2();
    return CreatePosition2;
  }();
  var CreateTokenUI = /* @__PURE__ */ function() {
    function CreateTokenUI2() {
    }
    ;
    CreateTokenUI2.value = new CreateTokenUI2();
    return CreateTokenUI2;
  }();
  var ToggleGateway = /* @__PURE__ */ function() {
    function ToggleGateway2() {
    }
    ;
    ToggleGateway2.value = new ToggleGateway2();
    return ToggleGateway2;
  }();
  var UpdateJitoSOLAmount = /* @__PURE__ */ function() {
    function UpdateJitoSOLAmount2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateJitoSOLAmount2.create = function(value0) {
      return new UpdateJitoSOLAmount2(value0);
    };
    return UpdateJitoSOLAmount2;
  }();
  var UpdateFeelsSOLAmount = /* @__PURE__ */ function() {
    function UpdateFeelsSOLAmount2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateFeelsSOLAmount2.create = function(value0) {
      return new UpdateFeelsSOLAmount2(value0);
    };
    return UpdateFeelsSOLAmount2;
  }();
  var EnterGateway2 = /* @__PURE__ */ function() {
    function EnterGateway3() {
    }
    ;
    EnterGateway3.value = new EnterGateway3();
    return EnterGateway3;
  }();
  var ExitGateway2 = /* @__PURE__ */ function() {
    function ExitGateway3() {
    }
    ;
    ExitGateway3.value = new ExitGateway3();
    return ExitGateway3;
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
  var renderWalletPanel = function(state3) {
    var renderBalance = function(label5) {
      return function(amount) {
        return div3([class_("list-item__content")])([div3([class_("label")])([text5(label5)]), div3([class_("value")])([text5(formatAmount(amount))])]);
      };
    };
    return div3([class_("panel")])([h2_([text5("Your Wallet")]), div3([class_("form-fields")])([renderBalance("JitoSOL")(state3.jitoSOLBalance), renderBalance("FeelsSOL")(state3.feelsSOLBalance)])]);
  };
  var renderUserTokensPanel = function(tokens) {
    var renderToken = function(token) {
      return div3([class_("list-item")])([div3([class_("list-item__header")])([strong_([text5(token.ticker)]), text5(" " + token.name)]), div3([class_("token-status")])([function() {
        if (token.launched) {
          return span3([class_("status-launched")])([text5("Launched")]);
        }
        ;
        return span3([class_("status-pending")])([text5(formatAmount(token.stakedFeelsSOL) + "/100 FeelsSOL")]);
      }()])]);
    };
    return div3([class_("panel")])([h2_([text5("Your Created Tokens")]), function() {
      var $111 = length(tokens) === 0;
      if ($111) {
        return p_([text5("No tokens created yet")]);
      }
      ;
      return div3([class_("token-list")])(map29(renderToken)(tokens));
    }()]);
  };
  var renderTokenCreatorPanel = function(v) {
    return div3([class_("panel")])([h2_([text5("Create Feels Token")]), div3([class_("form-group")])([input([type_19(InputText.value), id2("token-ticker"), placeholder3("Token Ticker (e.g., ALPHA)"), class_("form__input")]), input([type_19(InputText.value), id2("token-name"), placeholder3("Token Name"), class_("form__input")]), button([onClick(function(v1) {
      return CreateTokenUI.value;
    }), class_("btn btn--primary")])([text5("Create Token")])]), p([class_("info-text")])([text5("Tokens launch when 100 FeelsSOL is staked")])]);
  };
  var renderSystemPanel = function(state3) {
    var renderMetric = function(label5) {
      return function(value1) {
        return div3([class_("list-item__content")])([div3([class_("label")])([text5(label5)]), div3([class_("value")])([text5(value1)])]);
      };
    };
    return div3([class_("panel")])([h2_([text5("System Metrics")]), function() {
      if (state3.protocolStats instanceof Nothing) {
        return div_([text5("Loading metrics...")]);
      }
      ;
      if (state3.protocolStats instanceof Just) {
        return div3([class_("form-fields")])([renderMetric("Total Value Locked")(formatAmount(state3.protocolStats.value0.totalValueLocked)), renderMetric("FeelsSOL Supply")(formatAmount(state3.protocolStats.value0.feelsSOLSupply)), renderMetric("JitoSOL Locked")(formatAmount(state3.protocolStats.value0.jitoSOLLocked)), renderMetric("NFV Balance")(formatAmount(state3.protocolStats.value0.nfvBalance)), renderMetric("Active Users")(show8(state3.protocolStats.value0.totalUsers)), renderMetric("Active Positions")(show8(state3.protocolStats.value0.activePositions)), renderMetric("Lender Offers")(show8(state3.protocolStats.value0.totalLenderOffers)), renderMetric("Launched Tokens")(show8(state3.protocolStats.value0.launchedTokens))]);
      }
      ;
      throw new Error("Failed pattern match at UI (line 241, column 7 - line 253, column 12): " + [state3.protocolStats.constructor.name]);
    }()]);
  };
  var renderSimulationPanel = function(state3) {
    var renderResult = function(label5) {
      return function(value1) {
        return div3([class_("list-item__content")])([div3([class_("label")])([text5(label5)]), div3([class_("value")])([text5(value1)])]);
      };
    };
    var renderSimulationResults = function(results) {
      return div3([class_("simulation-results")])([h3_([text5("Simulation Results")]), div3([class_("results-grid")])([renderResult("Protocol TVL")(formatAmount(results.protocolTVL)), renderResult("Total Volume")(formatAmount(results.totalVolume)), renderResult("Price Change")(formatPercentage(results.priceChange)), renderResult("Volatility")(formatPercentage(results.volatility)), renderResult("Active Users")(show8(results.totalUsers)), renderResult("Total Fees")(formatAmount(results.totalFees))])]);
    };
    var parseScenario = function(v) {
      if (v === "bear") {
        return BearMarket.value;
      }
      ;
      if (v === "volatile") {
        return VolatileMarket.value;
      }
      ;
      if (v === "stable") {
        return SidewaysMarket.value;
      }
      ;
      return BullMarket.value;
    };
    return div3([class_("panel")])([h2_([text5("Market Simulation")]), div3([class_("position-form")])([div3([class_("form-group")])([label_([text5("Market Scenario:")]), select3([onValueChange(function(v) {
      return new UpdateSimulationConfig(function(v1) {
        return {
          numAccounts: v1.numAccounts,
          simulationBlocks: v1.simulationBlocks,
          initialJitoSOLPrice: v1.initialJitoSOLPrice,
          priceVolatility: v1.priceVolatility,
          accountProfiles: v1.accountProfiles,
          actionFrequency: v1.actionFrequency,
          leveragePreference: v1.leveragePreference,
          stakingPreference: v1.stakingPreference,
          scenario: parseScenario(v)
        };
      });
    }), class_("form__select")])([option([value13("bull")])([text5("Bull Market")]), option([value13("bear")])([text5("Bear Market")]), option([value13("volatile")])([text5("High Volatility")]), option([value13("stable")])([text5("Stable")])])]), div3([class_("form-group")])([label_([text5("User Count:")]), input([type_19(InputNumber.value), value13(show8(state3.simulationConfig.numAccounts)), onValueChange(function(v) {
      return new UpdateSimulationConfig(function(v1) {
        return {
          scenario: v1.scenario,
          simulationBlocks: v1.simulationBlocks,
          initialJitoSOLPrice: v1.initialJitoSOLPrice,
          priceVolatility: v1.priceVolatility,
          accountProfiles: v1.accountProfiles,
          actionFrequency: v1.actionFrequency,
          leveragePreference: v1.leveragePreference,
          stakingPreference: v1.stakingPreference,
          numAccounts: fromMaybe(10)(fromString2(v))
        };
      });
    }), min5(1), max6(100), class_("form__input")])]), div3([class_("form-group")])([label_([text5("Blocks to Simulate:")]), input([type_19(InputNumber.value), value13(show8(state3.simulationConfig.simulationBlocks)), onValueChange(function(v) {
      return new UpdateSimulationConfig(function(v1) {
        return {
          scenario: v1.scenario,
          numAccounts: v1.numAccounts,
          initialJitoSOLPrice: v1.initialJitoSOLPrice,
          priceVolatility: v1.priceVolatility,
          accountProfiles: v1.accountProfiles,
          actionFrequency: v1.actionFrequency,
          leveragePreference: v1.leveragePreference,
          stakingPreference: v1.stakingPreference,
          simulationBlocks: fromMaybe(100)(fromString2(v))
        };
      });
    }), min5(10), max6(1e3), class_("form__input")])])]), button([onClick(function(v) {
      return RunSimulation.value;
    }), class_("btn btn--primary btn--large"), disabled10(state3.simulationRunning)])([text5(function() {
      if (state3.simulationRunning) {
        return "Running...";
      }
      ;
      return "Run Simulation";
    }())]), function() {
      if (state3.simulationResults instanceof Nothing) {
        return text5("");
      }
      ;
      if (state3.simulationResults instanceof Just) {
        return renderSimulationResults(state3.simulationResults.value0);
      }
      ;
      throw new Error("Failed pattern match at UI (line 701, column 7 - line 703, column 56): " + [state3.simulationResults.constructor.name]);
    }()]);
  };
  var renderPositionsPanel = function(positions) {
    var statusClass = function(status) {
      return "status-active";
    };
    var formatTerms = function(v) {
      if (v instanceof SwapTerms) {
        return "Swap";
      }
      ;
      if (v instanceof StakingTerms) {
        return "Staking (" + (show15(v.value0) + ")");
      }
      ;
      if (v instanceof LeverageTerms) {
        return "Leverage (" + (formatAmount(v.value0) + "x)");
      }
      ;
      throw new Error("Failed pattern match at UI (line 606, column 19 - line 609, column 68): " + [v.constructor.name]);
    };
    var renderPosition = function(pos) {
      return div3([class_("list-item")])([div3([class_("list-item__header")])([text5("Position #" + (show8(pos.id) + (" (" + (show23(pos.side) + ")")))), span3([class_("status " + statusClass(pos.status))])([text5(show33(pos.status))])]), div3([class_("list-item__content")])([div_([text5(formatAmount(pos.lendAmount) + (" " + show43(pos.lendAsset)))]), div_([text5("Collateral: " + (formatAmount(pos.collateralAmount) + (" " + show43(pos.collateralAsset))))]), div_([text5("Terms: " + formatTerms(pos.terms))])])]);
    };
    return div3([class_("panel")])([h2_([text5("Your Created Positions")]), function() {
      var $121 = length(positions) === 0;
      if ($121) {
        return p_([text5("No active positions")]);
      }
      ;
      return div3([class_("position-list")])(map29(renderPosition)(positions));
    }()]);
  };
  var renderLoanBookPanel = function(offers) {
    var renderOfferStatus = function(offer) {
      var available = getAvailableAmount(offer);
      var percentFilled = function() {
        var $122 = offer.lendAmount > 0;
        if ($122) {
          return (offer.lendAmount - available) / offer.lendAmount * 100;
        }
        ;
        return 0;
      }();
      var $123 = available <= 0;
      if ($123) {
        return span3([class_("status-filled")])([text5("Filled")]);
      }
      ;
      var $124 = percentFilled > 0;
      if ($124) {
        return span3([class_("status-partial")])([text5(show8(floor3(percentFilled)) + "% Filled")]);
      }
      ;
      return span3([class_("status-available")])([text5("Available")]);
    };
    var formatTerms = function(v) {
      if (v instanceof SwapTerms) {
        return "Swap";
      }
      ;
      if (v instanceof StakingTerms) {
        return "Staking (" + (show15(v.value0) + ")");
      }
      ;
      if (v instanceof LeverageTerms) {
        return "Leverage (" + (formatAmount(v.value0) + "x)");
      }
      ;
      throw new Error("Failed pattern match at UI (line 532, column 19 - line 535, column 68): " + [v.constructor.name]);
    };
    var formatRate = function(v) {
      if (v instanceof SwapTerms) {
        return "Market";
      }
      ;
      if (v instanceof StakingTerms) {
        return "5% APY";
      }
      ;
      if (v instanceof LeverageTerms) {
        return "Variable";
      }
      ;
      throw new Error("Failed pattern match at UI (line 537, column 18 - line 540, column 36): " + [v.constructor.name]);
    };
    var formatCollateral = function(offer) {
      return show43(offer.collateralAsset) + (" " + function() {
        if (offer.side instanceof Lender) {
          return formatAmount(offer.collateralAmount * 100) + "%";
        }
        ;
        return formatAmount(offer.collateralAmount);
      }());
    };
    var formatAge = function(createdAt) {
      return "New";
    };
    var formatAddress = function(addr) {
      var $132 = length3(addr) > 12;
      if ($132) {
        return take2(6)(addr) + ("..." + drop3(length3(addr) - 4 | 0)(addr));
      }
      ;
      return addr;
    };
    var renderOffer = function(offer) {
      return div3([class_("offer__item")])([div3([class_("offer__id")])([text5("#" + show8(offer.id))]), div3([class_("offer__owner")])([text5(formatAddress(offer.owner))]), div3([class_("offer__asset")])([text5(show43(offer.lendAsset))]), div3([class_("offer__amount")])([text5(formatAmount(getAvailableAmount(offer)) + (" / " + formatAmount(offer.lendAmount)))]), div3([class_("offer__collateral")])([text5(formatCollateral(offer))]), div3([class_("offer__terms")])([text5(formatTerms(offer.terms))]), div3([class_("offer__status")])([renderOfferStatus(offer)])]);
    };
    return div3([class_("panel")])([h2_([text5("Available Offers")]), function() {
      var $133 = length(offers) === 0;
      if ($133) {
        return p_([text5("No lending offers available")]);
      }
      ;
      return div3([class_("offer-list")])([div3([class_("offer__header")])([div_([text5("ID")]), div_([text5("Lender")]), div_([text5("Asset")]), div_([text5("Amount (Avail/Total)")]), div_([text5("Collateral Required")]), div_([text5("Terms")]), div_([text5("Status")])]), div_(map29(renderOffer)(offers))]);
    }()]);
  };
  var renderGatewayPanel = function(state3) {
    return div3([class_("panel")])([h2_([text5("Gateway")]), div3([class_("form-group")])([label_([text5("JitoSOL Amount (for entering):")]), input([type_19(InputNumber.value), value13(formatAmount(state3.jitoSOLAmount)), onValueChange(function(v) {
      return new UpdateJitoSOLAmount(fromMaybe(0)(fromString(v)));
    }), class_("form__input")])]), div3([class_("form-group")])([label_([text5("FeelsSOL Amount (for exiting):")]), input([type_19(InputNumber.value), value13(formatAmount(state3.feelsSOLAmount)), onValueChange(function(v) {
      return new UpdateFeelsSOLAmount(fromMaybe(0)(fromString(v)));
    }), class_("form__input")])]), div3([class_("gateway-actions")])([button([onClick(function(v) {
      return EnterGateway2.value;
    }), class_("btn btn--primary")])([text5("JitoSOL \u2192"), br_, text5("FeelsSOL")]), button([onClick(function(v) {
      return ExitGateway2.value;
    }), class_("btn btn--primary")])([text5("FeelsSOL \u2192"), br_, text5("JitoSOL")])])]);
  };
  var parseTokenType = function(v) {
    if (v === "JitoSOL") {
      return JitoSOL.value;
    }
    ;
    if (v === "FeelsSOL") {
      return FeelsSOL.value;
    }
    ;
    return new Token(v);
  };
  var renderCreatePositionPanel = function(state3) {
    var renderLeverageOptions = function(v) {
      return div3([class_("form-group")])([label_([text5("Leverage:")]), input([type_19(InputNumber.value), value13(formatAmount(state3.leverage)), onValueChange(function(v1) {
        return new SetLeverage(fromMaybe(2)(fromString(v1)));
      }), min5(1), max6(10), attr2("step")("0.5"), class_("form__input")])]);
    };
    var renderLendOptions = function(state$prime) {
      return div_([div3([class_("form-group")])([label_([text5("Unbonding Period:")]), select3([onValueChange(function(v) {
        return new SetUnbondingPeriod(function() {
          if (v === "infinite") {
            return Infinite.value;
          }
          ;
          if (v === "30") {
            return Days30.value;
          }
          ;
          if (v === "60") {
            return Days60.value;
          }
          ;
          return Days90.value;
        }());
      }), class_("form__select")])([option([value13("infinite"), selected2(true)])([text5("Infinite (Swap)")]), option([value13("30")])([text5("30 Days")]), option([value13("60")])([text5("60 Days")]), option([value13("90")])([text5("90 Days")])])])]);
    };
    return div3([class_("panel")])([h2_([text5("Create Position")]), div3([class_("position-form")])([div3([class_("form-group")])([label_([text5("Amount:")]), input([type_19(InputNumber.value), value13(formatAmount(state3.inputAmount)), onValueChange(function(v) {
      return new UpdateInputAmount(fromMaybe(0)(fromString(v)));
    }), class_("form__input")])]), div3([class_("form-group")])([label_([text5("Input Asset:")]), select3([onValueChange(function(v) {
      return new SelectCollateralAsset(parseTokenType(v));
    }), class_("form__select")])(append14([option([value13("JitoSOL")])([text5("JitoSOL")]), option([value13("FeelsSOL")])([text5("FeelsSOL")])])(map29(function(token) {
      return option([value13(token.ticker)])([text5(token.ticker)]);
    })(state3.userTokens)))]), div3([class_("form-group")])([label_([text5("Output Asset:")]), select3([onValueChange(function(v) {
      return new SelectAsset(parseTokenType(v));
    }), class_("form__select")])(append14([option([value13("FeelsSOL")])([text5("FeelsSOL")]), option([value13("JitoSOL")])([text5("JitoSOL")])])(map29(function(token) {
      return option([value13(token.ticker)])([text5(token.ticker)]);
    })(state3.userTokens)))]), renderLendOptions(state3), renderLeverageOptions(state3), button([onClick(function(v) {
      return CreatePosition.value;
    }), class_("btn btn--primary btn--large")])([text5("Create Position")])])]);
  };
  var formatChartPoint = function(point) {
    return '{"timestamp":' + (show53(point.timestamp) + (',"price":' + (show53(point.price) + (',"nfvValue":' + (show53(point.nfvValue) + (',"tokens":[' + (joinWith(",")(map29(function(t) {
      return '{"ticker":"' + (t.ticker + ('","price":' + (show53(t.price) + (',"nfvFloor":' + (show53(t.nfvFloor) + "}")))));
    })(point.tokens)) + "]}")))))));
  };
  var handleAction = function(dictMonadAff) {
    var liftEffect12 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    var processTokenCreation = function(protocol2) {
      return function(action2) {
        if (action2 instanceof CreateToken2) {
          return liftEffect12(function __do7() {
            log("Creating token: " + (action2.value1 + (" for user: " + action2.value0)))();
            var result = executeCommand(protocol2)(new CreateToken(action2.value0, action2.value1, action2.value2))();
            if (result instanceof Right) {
              return log("Successfully created token: " + action2.value1)();
            }
            ;
            if (result instanceof Left) {
              return log("Failed to create token: " + show62(result.value0))();
            }
            ;
            throw new Error("Failed pattern match at UI (line 1062, column 9 - line 1064, column 67): " + [result.constructor.name]);
          });
        }
        ;
        return pure14(unit);
      };
    };
    var isCreateTokenAction = function(action2) {
      if (action2 instanceof CreateToken2) {
        return true;
      }
      ;
      return false;
    };
    return function(v) {
      if (v instanceof Initialize2) {
        return bind15(liftEffect12(initProtocol))(function(protocol2) {
          return bind15(liftEffect12(subscribe3(protocol2)(function(v1) {
            return log("Protocol state changed, refreshing UI data");
          })))(function() {
            return discard23(modify_3(function(v1) {
              var $148 = {};
              for (var $149 in v1) {
                if ({}.hasOwnProperty.call(v1, $149)) {
                  $148[$149] = v1[$149];
                }
                ;
              }
              ;
              $148.protocol = new Just(protocol2);
              $148.loading = false;
              return $148;
            }))(function() {
              return handleAction(dictMonadAff)(RefreshData.value);
            });
          });
        });
      }
      ;
      if (v instanceof RefreshData) {
        return bind15(get2)(function(state3) {
          if (state3.protocol instanceof Nothing) {
            return pure14(unit);
          }
          ;
          if (state3.protocol instanceof Just) {
            return bind15(liftEffect12(executeQuery(state3.protocol.value0)(new GetUserPositions(state3.currentUser))))(function(posResult) {
              return discard23(function() {
                if (posResult instanceof Right && posResult.value0 instanceof PositionList) {
                  return modify_3(function(v1) {
                    var $153 = {};
                    for (var $154 in v1) {
                      if ({}.hasOwnProperty.call(v1, $154)) {
                        $153[$154] = v1[$154];
                      }
                      ;
                    }
                    ;
                    $153.userPositions = posResult.value0.value0;
                    return $153;
                  });
                }
                ;
                return pure14(unit);
              }())(function() {
                return bind15(liftEffect12(executeQuery(state3.protocol.value0)(GetLenderOffers.value)))(function(offersResult) {
                  return discard23(function() {
                    if (offersResult instanceof Right && offersResult.value0 instanceof LenderOfferList) {
                      return discard23(liftEffect12(log("RefreshData: Found " + (show8(length(offersResult.value0.value0)) + " lender offers"))))(function() {
                        return discard23(liftEffect12(traverse_7(function(offer) {
                          return log("  Offer #" + (show8(offer.id) + (": " + (show53(offer.lendAmount) + (" " + (show43(offer.lendAsset) + (" (status: " + (show33(offer.status) + ")"))))))));
                        })(offersResult.value0.value0)))(function() {
                          return modify_3(function(v1) {
                            var $159 = {};
                            for (var $160 in v1) {
                              if ({}.hasOwnProperty.call(v1, $160)) {
                                $159[$160] = v1[$160];
                              }
                              ;
                            }
                            ;
                            $159.lenderOffers = offersResult.value0.value0;
                            return $159;
                          });
                        });
                      });
                    }
                    ;
                    if (offersResult instanceof Left) {
                      return liftEffect12(log("RefreshData: Failed to get lender offers: " + show62(offersResult.value0)));
                    }
                    ;
                    return pure14(unit);
                  }())(function() {
                    return bind15(liftEffect12(executeQuery(state3.protocol.value0)(GetProtocolStats.value)))(function(statsResult) {
                      return discard23(function() {
                        if (statsResult instanceof Right && statsResult.value0 instanceof ProtocolStatsResult) {
                          return discard23(liftEffect12(log("RefreshData: Updated protocol stats - TVL: " + (show53(statsResult.value0.value0.totalValueLocked) + (", Users: " + show8(statsResult.value0.value0.totalUsers))))))(function() {
                            return modify_3(function(v1) {
                              var $166 = {};
                              for (var $167 in v1) {
                                if ({}.hasOwnProperty.call(v1, $167)) {
                                  $166[$167] = v1[$167];
                                }
                                ;
                              }
                              ;
                              $166.protocolStats = new Just(statsResult.value0.value0);
                              return $166;
                            });
                          });
                        }
                        ;
                        if (statsResult instanceof Left) {
                          return liftEffect12(log("RefreshData: Failed to get protocol stats: " + show62(statsResult.value0)));
                        }
                        ;
                        return pure14(unit);
                      }())(function() {
                        return bind15(liftEffect12(executeQuery(state3.protocol.value0)(new GetUserBalance(state3.currentUser, JitoSOL.value))))(function(jitoBalanceResult) {
                          return discard23(function() {
                            if (jitoBalanceResult instanceof Right && jitoBalanceResult.value0 instanceof Balance) {
                              return modify_3(function(v1) {
                                var $173 = {};
                                for (var $174 in v1) {
                                  if ({}.hasOwnProperty.call(v1, $174)) {
                                    $173[$174] = v1[$174];
                                  }
                                  ;
                                }
                                ;
                                $173.jitoSOLBalance = jitoBalanceResult.value0.value0;
                                return $173;
                              });
                            }
                            ;
                            return pure14(unit);
                          }())(function() {
                            return bind15(liftEffect12(executeQuery(state3.protocol.value0)(new GetUserBalance(state3.currentUser, FeelsSOL.value))))(function(feelsBalanceResult) {
                              return discard23(function() {
                                if (feelsBalanceResult instanceof Right && feelsBalanceResult.value0 instanceof Balance) {
                                  return modify_3(function(v1) {
                                    var $179 = {};
                                    for (var $180 in v1) {
                                      if ({}.hasOwnProperty.call(v1, $180)) {
                                        $179[$180] = v1[$180];
                                      }
                                      ;
                                    }
                                    ;
                                    $179.feelsSOLBalance = feelsBalanceResult.value0.value0;
                                    return $179;
                                  });
                                }
                                ;
                                return pure14(unit);
                              }())(function() {
                                return modify_3(function(v1) {
                                  var $184 = {};
                                  for (var $185 in v1) {
                                    if ({}.hasOwnProperty.call(v1, $185)) {
                                      $184[$185] = v1[$185];
                                    }
                                    ;
                                  }
                                  ;
                                  $184.error = Nothing.value;
                                  return $184;
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
          throw new Error("Failed pattern match at UI (line 756, column 5 - line 806, column 40): " + [state3.protocol.constructor.name]);
        });
      }
      ;
      if (v instanceof RenderChart) {
        return discard23(liftEffect12(log("Rendering price chart...")))(function() {
          return bind15(get2)(function(currentState) {
            return discard23(liftEffect12(log("Price history length: " + show8(length(currentState.priceHistory)))))(function() {
              return discard23(liftEffect12(log("Chart data JSON: " + ("[" + (joinWith(",")(map29(formatChartPoint)(currentState.priceHistory)) + "]")))))(function() {
                return discard23(function() {
                  var v1 = head(currentState.priceHistory);
                  if (v1 instanceof Just) {
                    return liftEffect12(log("First data point has " + (show8(length(v1.value0.tokens)) + " tokens")));
                  }
                  ;
                  if (v1 instanceof Nothing) {
                    return liftEffect12(log("No price history data"));
                  }
                  ;
                  throw new Error("Failed pattern match at UI (line 814, column 5 - line 816, column 60): " + [v1.constructor.name]);
                }())(function() {
                  return liftEffect12($$void8(setTimeout2(checkAndInitializeChart)(250)));
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof UpdateInputAmount) {
        return modify_3(function(v1) {
          var $190 = {};
          for (var $191 in v1) {
            if ({}.hasOwnProperty.call(v1, $191)) {
              $190[$191] = v1[$191];
            }
            ;
          }
          ;
          $190.inputAmount = v.value0;
          return $190;
        });
      }
      ;
      if (v instanceof SelectAsset) {
        return modify_3(function(v1) {
          var $194 = {};
          for (var $195 in v1) {
            if ({}.hasOwnProperty.call(v1, $195)) {
              $194[$195] = v1[$195];
            }
            ;
          }
          ;
          $194.selectedAsset = v.value0;
          return $194;
        });
      }
      ;
      if (v instanceof SelectCollateralAsset) {
        return modify_3(function(v1) {
          var $198 = {};
          for (var $199 in v1) {
            if ({}.hasOwnProperty.call(v1, $199)) {
              $198[$199] = v1[$199];
            }
            ;
          }
          ;
          $198.collateralAsset = v.value0;
          return $198;
        });
      }
      ;
      if (v instanceof SetUnbondingPeriod) {
        return modify_3(function(v1) {
          var $202 = {};
          for (var $203 in v1) {
            if ({}.hasOwnProperty.call(v1, $203)) {
              $202[$203] = v1[$203];
            }
            ;
          }
          ;
          $202.unbondingPeriod = v.value0;
          return $202;
        });
      }
      ;
      if (v instanceof SetLeverage) {
        return modify_3(function(v1) {
          var $206 = {};
          for (var $207 in v1) {
            if ({}.hasOwnProperty.call(v1, $207)) {
              $206[$207] = v1[$207];
            }
            ;
          }
          ;
          $206.leverage = v.value0;
          return $206;
        });
      }
      ;
      if (v instanceof CreateTokenUI) {
        return bind15(get2)(function(state3) {
          if (state3.protocol instanceof Nothing) {
            return pure14(unit);
          }
          ;
          if (state3.protocol instanceof Just) {
            return bind15(liftEffect12(getElementById("token-ticker")))(function(tickerElem) {
              return bind15(liftEffect12(getElementById("token-name")))(function(nameElem) {
                var v1 = toMaybe(nameElem);
                var v2 = toMaybe(tickerElem);
                if (v2 instanceof Just && v1 instanceof Just) {
                  return bind15(liftEffect12(getValue(v2.value0)))(function(ticker) {
                    return bind15(liftEffect12(getValue(v1.value0)))(function(name15) {
                      var $213 = trim(ticker) === "" || trim(name15) === "";
                      if ($213) {
                        return modify_3(function(v3) {
                          var $214 = {};
                          for (var $215 in v3) {
                            if ({}.hasOwnProperty.call(v3, $215)) {
                              $214[$215] = v3[$215];
                            }
                            ;
                          }
                          ;
                          $214.error = new Just("Ticker and name are required");
                          return $214;
                        });
                      }
                      ;
                      return bind15(liftEffect12(executeCommand(state3.protocol.value0)(new CreateToken(state3.currentUser, ticker, name15))))(function(result) {
                        if (result instanceof Right && result.value0 instanceof TokenCreated) {
                          return discard23(liftEffect12(log("Token created: " + result.value0.value0.ticker)))(function() {
                            return discard23(modify_3(function(s) {
                              var $218 = {};
                              for (var $219 in s) {
                                if ({}.hasOwnProperty.call(s, $219)) {
                                  $218[$219] = s[$219];
                                }
                                ;
                              }
                              ;
                              $218.userTokens = cons(result.value0.value0)(s.userTokens);
                              return $218;
                            }))(function() {
                              return handleAction(dictMonadAff)(RefreshData.value);
                            });
                          });
                        }
                        ;
                        if (result instanceof Right) {
                          return modify_3(function(v3) {
                            var $223 = {};
                            for (var $224 in v3) {
                              if ({}.hasOwnProperty.call(v3, $224)) {
                                $223[$224] = v3[$224];
                              }
                              ;
                            }
                            ;
                            $223.error = new Just("Unexpected result from token creation");
                            return $223;
                          });
                        }
                        ;
                        if (result instanceof Left) {
                          return modify_3(function(v3) {
                            var $227 = {};
                            for (var $228 in v3) {
                              if ({}.hasOwnProperty.call(v3, $228)) {
                                $227[$228] = v3[$228];
                              }
                              ;
                            }
                            ;
                            $227.error = new Just(show62(result.value0));
                            return $227;
                          });
                        }
                        ;
                        throw new Error("Failed pattern match at UI (line 856, column 17 - line 866, column 60): " + [result.constructor.name]);
                      });
                    });
                  });
                }
                ;
                return modify_3(function(v3) {
                  var $233 = {};
                  for (var $234 in v3) {
                    if ({}.hasOwnProperty.call(v3, $234)) {
                      $233[$234] = v3[$234];
                    }
                    ;
                  }
                  ;
                  $233.error = new Just("Could not find form elements");
                  return $233;
                });
              });
            });
          }
          ;
          throw new Error("Failed pattern match at UI (line 837, column 5 - line 868, column 72): " + [state3.protocol.constructor.name]);
        });
      }
      ;
      if (v instanceof CreatePosition) {
        return bind15(get2)(function(state3) {
          if (state3.protocol instanceof Nothing) {
            return pure14(unit);
          }
          ;
          if (state3.protocol instanceof Just) {
            var terms = function() {
              var $238 = state3.leverage > 1;
              if ($238) {
                return new LeverageTerms(state3.leverage);
              }
              ;
              var $239 = eq22(state3.unbondingPeriod)(Infinite.value);
              if ($239) {
                return SwapTerms.value;
              }
              ;
              return new StakingTerms(state3.unbondingPeriod);
            }();
            var collateralAmount = state3.inputAmount * 1.5;
            return bind15(liftEffect12(executeCommand(state3.protocol.value0)(new CreateLendingPosition(state3.currentUser, state3.selectedAsset, state3.inputAmount, state3.collateralAsset, collateralAmount, terms, Nothing.value))))(function(result) {
              if (result instanceof Right && result.value0 instanceof PositionCreated) {
                return discard23(liftEffect12(log("Position created: #" + show8(result.value0.value0.id))))(function() {
                  return handleAction(dictMonadAff)(RefreshData.value);
                });
              }
              ;
              if (result instanceof Right) {
                return modify_3(function(v1) {
                  var $243 = {};
                  for (var $244 in v1) {
                    if ({}.hasOwnProperty.call(v1, $244)) {
                      $243[$244] = v1[$244];
                    }
                    ;
                  }
                  ;
                  $243.error = new Just("Unexpected result from position creation");
                  return $243;
                });
              }
              ;
              if (result instanceof Left) {
                return modify_3(function(v1) {
                  var $247 = {};
                  for (var $248 in v1) {
                    if ({}.hasOwnProperty.call(v1, $248)) {
                      $247[$248] = v1[$248];
                    }
                    ;
                  }
                  ;
                  $247.error = new Just(show62(result.value0));
                  return $247;
                });
              }
              ;
              throw new Error("Failed pattern match at UI (line 896, column 9 - line 904, column 52): " + [result.constructor.name]);
            });
          }
          ;
          throw new Error("Failed pattern match at UI (line 872, column 5 - line 904, column 52): " + [state3.protocol.constructor.name]);
        });
      }
      ;
      if (v instanceof ToggleGateway) {
        return modify_3(function(s) {
          var $252 = {};
          for (var $253 in s) {
            if ({}.hasOwnProperty.call(s, $253)) {
              $252[$253] = s[$253];
            }
            ;
          }
          ;
          $252.showGateway = !s.showGateway;
          return $252;
        });
      }
      ;
      if (v instanceof UpdateJitoSOLAmount) {
        return modify_3(function(v1) {
          var $255 = {};
          for (var $256 in v1) {
            if ({}.hasOwnProperty.call(v1, $256)) {
              $255[$256] = v1[$256];
            }
            ;
          }
          ;
          $255.jitoSOLAmount = v.value0;
          return $255;
        });
      }
      ;
      if (v instanceof UpdateFeelsSOLAmount) {
        return modify_3(function(v1) {
          var $259 = {};
          for (var $260 in v1) {
            if ({}.hasOwnProperty.call(v1, $260)) {
              $259[$260] = v1[$260];
            }
            ;
          }
          ;
          $259.feelsSOLAmount = v.value0;
          return $259;
        });
      }
      ;
      if (v instanceof EnterGateway2) {
        return bind15(get2)(function(state3) {
          if (state3.protocol instanceof Nothing) {
            return pure14(unit);
          }
          ;
          if (state3.protocol instanceof Just) {
            return bind15(liftEffect12(executeCommand(state3.protocol.value0)(new EnterGateway(state3.currentUser, state3.jitoSOLAmount))))(function(result) {
              if (result instanceof Right && result.value0 instanceof GatewayEntered) {
                return discard23(liftEffect12(log("Entered gateway: " + (show53(result.value0.value0.feelsSOLMinted) + " FeelsSOL minted"))))(function() {
                  return handleAction(dictMonadAff)(RefreshData.value);
                });
              }
              ;
              if (result instanceof Right) {
                return modify_3(function(v1) {
                  var $267 = {};
                  for (var $268 in v1) {
                    if ({}.hasOwnProperty.call(v1, $268)) {
                      $267[$268] = v1[$268];
                    }
                    ;
                  }
                  ;
                  $267.error = new Just("Unexpected result from gateway entry");
                  return $267;
                });
              }
              ;
              if (result instanceof Left) {
                return modify_3(function(v1) {
                  var $271 = {};
                  for (var $272 in v1) {
                    if ({}.hasOwnProperty.call(v1, $272)) {
                      $271[$272] = v1[$272];
                    }
                    ;
                  }
                  ;
                  $271.error = new Just(show62(result.value0));
                  return $271;
                });
              }
              ;
              throw new Error("Failed pattern match at UI (line 923, column 9 - line 930, column 52): " + [result.constructor.name]);
            });
          }
          ;
          throw new Error("Failed pattern match at UI (line 917, column 5 - line 930, column 52): " + [state3.protocol.constructor.name]);
        });
      }
      ;
      if (v instanceof ExitGateway2) {
        return bind15(get2)(function(state3) {
          if (state3.protocol instanceof Nothing) {
            return pure14(unit);
          }
          ;
          if (state3.protocol instanceof Just) {
            return bind15(liftEffect12(executeCommand(state3.protocol.value0)(new ExitGateway(state3.currentUser, state3.feelsSOLAmount))))(function(result) {
              if (result instanceof Right && result.value0 instanceof GatewayExited) {
                return discard23(liftEffect12(log("Exited gateway: " + (show53(result.value0.value0.jitoSOLReceived) + " JitoSOL received"))))(function() {
                  return handleAction(dictMonadAff)(RefreshData.value);
                });
              }
              ;
              if (result instanceof Right) {
                return modify_3(function(v1) {
                  var $280 = {};
                  for (var $281 in v1) {
                    if ({}.hasOwnProperty.call(v1, $281)) {
                      $280[$281] = v1[$281];
                    }
                    ;
                  }
                  ;
                  $280.error = new Just("Unexpected result from gateway exit");
                  return $280;
                });
              }
              ;
              if (result instanceof Left) {
                return modify_3(function(v1) {
                  var $284 = {};
                  for (var $285 in v1) {
                    if ({}.hasOwnProperty.call(v1, $285)) {
                      $284[$285] = v1[$285];
                    }
                    ;
                  }
                  ;
                  $284.error = new Just(show62(result.value0));
                  return $284;
                });
              }
              ;
              throw new Error("Failed pattern match at UI (line 941, column 9 - line 948, column 52): " + [result.constructor.name]);
            });
          }
          ;
          throw new Error("Failed pattern match at UI (line 934, column 5 - line 948, column 52): " + [state3.protocol.constructor.name]);
        });
      }
      ;
      if (v instanceof UpdateSimulationConfig) {
        return modify_3(function(s) {
          var $289 = {};
          for (var $290 in s) {
            if ({}.hasOwnProperty.call(s, $290)) {
              $289[$290] = s[$290];
            }
            ;
          }
          ;
          $289.simulationConfig = v.value0(s.simulationConfig);
          return $289;
        });
      }
      ;
      if (v instanceof RunSimulation) {
        return discard23(liftEffect12(log("Starting simulation...")))(function() {
          return discard23(modify_3(function(v1) {
            var $293 = {};
            for (var $294 in v1) {
              if ({}.hasOwnProperty.call(v1, $294)) {
                $293[$294] = v1[$294];
              }
              ;
            }
            ;
            $293.simulationRunning = true;
            $293.error = Nothing.value;
            return $293;
          }))(function() {
            return bind15(get2)(function(state3) {
              if (state3.protocol instanceof Nothing) {
                return modify_3(function(v1) {
                  var $297 = {};
                  for (var $298 in v1) {
                    if ({}.hasOwnProperty.call(v1, $298)) {
                      $297[$298] = v1[$298];
                    }
                    ;
                  }
                  ;
                  $297.error = new Just("Protocol not initialized");
                  return $297;
                });
              }
              ;
              if (state3.protocol instanceof Just) {
                return bind15(liftEffect12(read(state3.protocol.value0.state)))(function(protocolState) {
                  return bind15(liftEffect12(initSimulationWithLendingBook(state3.simulationConfig)(protocolState.lendingBook)))(function(simState) {
                    return bind15(liftEffect12(executeSimulation(state3.simulationConfig)(simState)))(function(finalState) {
                      return bind15(liftEffect12(calculateResults(state3.simulationConfig)(finalState)))(function(results) {
                        return discard23(liftEffect12(log("Simulation completed with " + (show8(results.totalUsers) + " users"))))(function() {
                          var tokenCreationActions = filter(isCreateTokenAction)(finalState.actionHistory);
                          return discard23(liftEffect12(log("Processing " + (show8(length(tokenCreationActions)) + " token creation actions"))))(function() {
                            return bind15(traverse4(processTokenCreation(state3.protocol.value0))(tokenCreationActions))(function() {
                              return bind15(liftEffect12(executeQuery(state3.protocol.value0)(GetAllTokens.value)))(function(allTokensResult) {
                                var allTokens = function() {
                                  if (allTokensResult instanceof Right && allTokensResult.value0 instanceof TokenList) {
                                    return allTokensResult.value0.value0;
                                  }
                                  ;
                                  return [];
                                }();
                                return discard23(liftEffect12(log("Including " + (show8(length(allTokens)) + " tokens in price history"))))(function() {
                                  var reversedHistory = reverse(finalState.priceHistory);
                                  var convertedPriceHistory = function() {
                                    var $303 = length(reversedHistory) > 0;
                                    if ($303) {
                                      return map29(function(v1) {
                                        var tokenPrices = map29(function(token) {
                                          var launchBonus = function() {
                                            if (token.launched) {
                                              return 0.3;
                                            }
                                            ;
                                            return 0;
                                          }();
                                          var charSum = length3(token.ticker) * 100 | 0;
                                          var growthRate = function() {
                                            var v2 = mod3(charSum)(5);
                                            if (v2 === 0) {
                                              return 0.015;
                                            }
                                            ;
                                            if (v2 === 1) {
                                              return -8e-3;
                                            }
                                            ;
                                            if (v2 === 2) {
                                              return 0.025;
                                            }
                                            ;
                                            if (v2 === 3) {
                                              return 2e-3;
                                            }
                                            ;
                                            return 0.012;
                                          }();
                                          var volatility2 = function() {
                                            var v2 = mod3(charSum)(3);
                                            if (v2 === 0) {
                                              return 0.05;
                                            }
                                            ;
                                            if (v2 === 1) {
                                              return 0.15;
                                            }
                                            ;
                                            return 0.25;
                                          }();
                                          var noise = volatility2 * (toNumber2(mod3(v1.value1 + charSum | 0)(11)) - 5) / 10;
                                          var trendPrice = 1 * (1 + growthRate * toNumber2(v1.value1) + noise);
                                          return {
                                            ticker: token.ticker,
                                            price: trendPrice + launchBonus,
                                            nfvFloor: trendPrice * 0.85
                                          };
                                        })(allTokens);
                                        var baseTimestamp = toNumber2(v1.value1) * 6e4;
                                        return {
                                          timestamp: baseTimestamp,
                                          price: v1.value0,
                                          nfvValue: v1.value0 * 0.98,
                                          tokens: tokenPrices
                                        };
                                      })(zip(reversedHistory)(range2(0)(length(reversedHistory) - 1 | 0)));
                                    }
                                    ;
                                    return [];
                                  }();
                                  return discard23(liftEffect12(log("Simulation used shared lending book - protocol state automatically updated")))(function() {
                                    return discard23(handleAction(dictMonadAff)(RefreshData.value))(function() {
                                      return discard23(modify_3(function(v1) {
                                        var $310 = {};
                                        for (var $311 in v1) {
                                          if ({}.hasOwnProperty.call(v1, $311)) {
                                            $310[$311] = v1[$311];
                                          }
                                          ;
                                        }
                                        ;
                                        $310.simulationRunning = false;
                                        $310.simulationResults = new Just(results);
                                        $310.priceHistory = convertedPriceHistory;
                                        return $310;
                                      }))(function() {
                                        return discard23(liftEffect12(log("State updated, scheduling chart render...")))(function() {
                                          return void1(fork(discard23(liftEffect12($$void8(setTimeout2(pure15(unit))(200))))(function() {
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
              }
              ;
              throw new Error("Failed pattern match at UI (line 958, column 5 - line 1052, column 35): " + [state3.protocol.constructor.name]);
            });
          });
        });
      }
      ;
      throw new Error("Failed pattern match at UI (line 739, column 16 - line 1052, column 35): " + [v.constructor.name]);
    };
  };
  var renderPriceChartPanel = function(state3) {
    return div3([class_("panel")])([h2_([text5("Price History")]), function() {
      var v = length(state3.priceHistory);
      if (v === 0) {
        return div3([class_("no-price-data")])([text5("No price data available - run simulation to generate history")]);
      }
      ;
      return div_([div3([class_("chart-container")])([element2("canvas")([id2("price-chart"), attr2("width")("600"), attr2("height")("300")])([])]), div3([class_("chart-data"), style("display: none;")])([text5("[" + (joinWith(",")(map29(formatChartPoint)(state3.priceHistory)) + "]"))])]);
    }()]);
  };
  var render = function(state3) {
    return div3([class_("app")])([function() {
      if (state3.loading) {
        return div3([class_("loading")])([text5("Initializing protocol...")]);
      }
      ;
      return div3([class_("main-layout two-column")])([div3([class_("left-column")])([renderWalletPanel(state3), renderGatewayPanel(state3), renderTokenCreatorPanel(state3), renderUserTokensPanel(state3.userTokens), renderCreatePositionPanel(state3), renderPositionsPanel(state3.userPositions), renderSystemPanel(state3), renderSimulationPanel(state3)]), div3([class_("right-column")])([renderPriceChartPanel(state3), renderLoanBookPanel(state3.lenderOffers)])]);
    }()]);
  };
  var defaultSimulationConfig = /* @__PURE__ */ function() {
    return {
      scenario: BullMarket.value,
      numAccounts: 10,
      simulationBlocks: 100,
      initialJitoSOLPrice: 1.05,
      priceVolatility: 0.02,
      accountProfiles: [Whale.value, Aggressive.value, Conservative.value],
      actionFrequency: 2,
      leveragePreference: 0.3,
      stakingPreference: 0.5
    };
  }();
  var initialUIState = /* @__PURE__ */ function() {
    return {
      protocol: Nothing.value,
      currentUser: "main-user",
      inputAmount: 100,
      selectedAsset: FeelsSOL.value,
      collateralAsset: JitoSOL.value,
      unbondingPeriod: Infinite.value,
      leverage: 2,
      showGateway: true,
      jitoSOLAmount: 100,
      feelsSOLAmount: 100,
      jitoSOLBalance: 5e3,
      feelsSOLBalance: 5e3,
      simulationConfig: defaultSimulationConfig,
      simulationResults: Nothing.value,
      simulationRunning: false,
      priceHistory: [],
      userTokens: [],
      userPositions: [],
      lenderOffers: [],
      protocolStats: Nothing.value,
      loading: true,
      error: Nothing.value
    };
  }();
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
  var renderUI = function __do5() {
    log("Starting Feels Protocol UI...")();
    return runHalogenAff(bind22(selectElement("#app"))(function(appElement) {
      if (appElement instanceof Nothing) {
        return discard32(liftEffect7(log("Could not find #app element")))(function() {
          return bind22(awaitBody)(runUI2(component1)(unit));
        });
      }
      ;
      if (appElement instanceof Just) {
        return discard32(liftEffect7(log("Mounting UI component")))(function() {
          return runUI2(component1)(unit)(appElement.value0);
        });
      }
      ;
      throw new Error("Failed pattern match at UI (line 1077, column 5 - line 1083, column 37): " + [appElement.constructor.name]);
    }))();
  };

  // output/Main/index.js
  var main2 = function __do6() {
    log("Starting Feels Protocol - Everything is Lending")();
    return onDOMReady(function __do7() {
      log("DOM ready, initializing Halogen application")();
      renderUI();
      return log("Halogen application initialized successfully")();
    })();
  };

  // <stdin>
  main2();
})();
