function shouldRaise(f) {
  try {
    f();
  } catch (e) {
    console.log("OK");
    return;
  }
  throw new Error("should have raised");
}
function shouldNotRaise(f) {
  try {
    f();
    console.log("OK");
    return;
  } catch (e) {
    throw new Error("should not have raised");
  }
}

// a0, a1 and b are three separate instances.
var a0 = require("./a.js")(global);
var a1 = require("./a.js")(global);
var b = require("./b.js")(global);
shouldNotRaise(() => a0.tryWith(a0.raise));
shouldRaise(() => a1.tryWith(a0.raise));
shouldRaise(() => b.tryWith(a0.raise));

shouldRaise(() => a0.tryWith(a1.raise));
shouldNotRaise(() => a1.tryWith(a1.raise));
shouldRaise(() => b.tryWith(a1.raise));

shouldRaise(() => a0.tryWith(b.raise));
shouldRaise(() => a1.tryWith(b.raise));
shouldNotRaise(() => b.tryWith(b.raise));

// a2 is different from a0,a1 and b
var a2 = require("./a-iife.js");
shouldNotRaise(() => a2.tryWith(a2.raise));
shouldRaise(() => a0.tryWith(a2.raise));
shouldRaise(() => a2.tryWith(a0.raise));
shouldRaise(() => a1.tryWith(a2.raise));
shouldRaise(() => a2.tryWith(a1.raise));
shouldRaise(() => b.tryWith(a2.raise));
shouldRaise(() => a2.tryWith(b.raise));

// a3 is the same as a2
var a3 = require("./a-iife.js");
shouldNotRaise(() => a2.tryWith(a3.raise));
shouldNotRaise(() => a3.tryWith(a2.raise));
shouldRaise(() => a0.tryWith(a3.raise));
shouldRaise(() => a3.tryWith(a0.raise));
shouldRaise(() => a1.tryWith(a3.raise));
shouldRaise(() => a3.tryWith(a1.raise));
shouldRaise(() => b.tryWith(a3.raise));
shouldRaise(() => a3.tryWith(b.raise));
