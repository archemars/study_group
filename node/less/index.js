// var LineStream = require('./line_stream');
// 
// var fileName = 'hoge.txt'
// var fileReaderStream = require('fs').createReadStream(fileName, {bufferSize: 10});
// fileReaderStream.setEncoding('utf8');
// 
// var lineStream = new LineStream()
// var index = 0;
// lineStream.on('data', function(data) {
//   index++;
//   console.log(index + ': ' + data);
// });
// 
// fileReaderStream.pipe(lineStream);
// lineStream.resume();
// 


// const Tail = require('nodejs-tail');
// 
// const filename = 'hoge.txt';
// const tail = new Tail(filename);
//  
// tail.on('line', (line) => {
//   process.stdout.write(line);
// })
//  
// tail.on('close', () => {
//   console.log('watching stopped');
// })
//  
// tail.watch();
//  
// setTimeout(() => {
//   tail.close();
// }, 30000);
// 

// const readline = require('readline');
// const debounce = require('debounce');

// 
// var fileName = 'hoge.txt'
// var fileReaderStream = require('fs').createReadStream(fileName, {bufferSize: 10});
// fileReaderStream.setEncoding('utf8');
// const rl = readline.createInterface({
//   input: fileReaderStream,
//   output: process.stdout
// });
// 
// rl.on('line', (input) => {
//   console.log(`Received: ${input}`);
// });

// const values = ['hoge', 'lorem ipsum', 'dolor sit amet'];
// // const rl = readline.createInterface(process.stdin);
// const rl = readline.createInterface({
//   input: process.stdin,
//   output: process.stdout
// });
// const showResults = debounce(() => {
//   console.log(
//     '\n',
//     values.filter((val) => val.startsWith(rl.line)).join(' ')
//   );
// }, 300);
// process.stdin.on('keypress', (c, k) => {
//   showResults();
// });
// 
// readline.cursorTo(process.stdout, 100, 210, () => {
//   console.log('AAAAAAAAAAAAAAAA')
// })



// const fs = require('fs');
// const readline = require('readline');
// 
// const rl = readline.createInterface({
//     input: fs.createReadStream('hoge.txt'),
//     output: process.stdout,
//     terminal: false
// });
// 
// rl.on('line', (line) => {
//     console.log(line);
// });

// const lineReader = require('line-reader');
// 
// lineReader.eachLine('hoge.txt', (line, last) => {
//     console.log(line);
//       if(line.includes('NEW')) {
//         // stop reading and close the file
//         return false;
//     }
// 
// });

// const readline = require('linebyline');
// 
// // read all lines
// rl = readline('hoge.txt');
// 
// // listen for `line` event
// rl.on('line', (line, lineCount, byteCount) => {
//     console.log(line);
// }).on('error', (err) => {
//     console.error(err);
// });


const editor = require('tiny-cli-editor')
 
editor('Hello this is dog.')
.on('data', (text) => {
    // do something with the text
})
.on('abort', (text) => {
    // do something with the text
})
.on('submit', (text) => {
    // do something with the text
})

