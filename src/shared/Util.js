'use strict';

exports.stringifyPretty = indent => obj => JSON.stringify(obj, null, indent);

// generate string id length 32
exports.genIdImpl = () => {
  const p1 = Math.random().toString(36).substring(2, 10);
  const p2 = Math.random().toString(36).substring(2, 10);
  const p3 = Math.random().toString(36).substring(2, 10);
  const p4 = Math.random().toString(36).substring(2, 10);
  // console.log((p1 + p2 + p3 + p4).length);
  return p1 + p2 + p3 + p4;
}
