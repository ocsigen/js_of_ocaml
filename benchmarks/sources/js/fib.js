function fib(n) {
  return (n < 2)?1:(fib(n-1) + fib(n-2));
}

var n = 40;

if (fib(n) !== 165580141) throw "error!";
