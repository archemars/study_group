const readline=require("readline");
readline.emitKeypressEvents(process.stdin);
const rl=readline.createInterface({
    input:process.stdin,
    output:process.stdout,
    prompt:"",
    terminal:true
});

process.stdin.on('keypress', (c, k) => {
  console.log(c, k)
});
