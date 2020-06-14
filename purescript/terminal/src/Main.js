exports.consoleClear
  = console.clear;

exports.onKeypressImpl = function (input, output, terminal, f) {
  const readline = require("readline");
  const rl = readline.createInterface({
    input: input,
    output: output,
    prompt: "",
    terminal: terminal
  });
  return function() {
    process.stdin.on('keypress', (c, k) => {
      f(k)()
    });
  }
}



