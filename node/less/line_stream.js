"use strict";

module.exports = LineStream;

var stream = require('stream');
var util = require('util');

function LineStream() {
    this.writable = true;
    this.readable = true;
    this.ended = false;
    this.paused = true;
    this.encoding = 'utf8';
    this.buf = '';
};

util.inherits(LineStream, stream.Stream);

/* readable stream function */
LineStream.prototype.setEncoding = function (encoding) {
  this.encoding = encoding;
};

LineStream.prototype.pause = function () {
  this.paused = true;
};

LineStream.prototype.resume = function () {
  this.paused = false;
  /* TODO: 'drain' */
};

/* writable stream function */
LineStream.prototype.write = function (data) {
  if (this.ended || this.paused) {
    return false;
  }
  if (Buffer.isBuffer(data)) {
    this.buf += data.toString(this.encoding); 
  } else {
    this.buf += data;
  }
  this.searchLine();
  return true;
};

LineStream.prototype.searchLine = function () {
  while(true) {
    var pos = 0;
    var index = this.buf.indexOf('\n');
    var line;
    if (index > -1) {
      line = this.buf.substring(pos, index);
      this.emit('data', line);
      this.buf = this.buf.substring(index + 1);
      continue;
    }
    break;
  }
};

LineStream.prototype.pipe = function (dest) {
  this.on('data', function(data) {
    dest.write(data);
  });
  return dest;
};

LineStream.prototype.end = function () {
  if (this.buf) {
    this.emit('data', this.buf);
  }
  this.ended = true;
  this.destroy();
};

LineStream.prototype.destroy = function () {
  this.ended = true;
  this.readable = false;
  this.writable = false;
  this.emit('end');
  this.emit('close');
};

