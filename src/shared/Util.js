'use strict';

exports.stringifyPretty = indent => obj => JSON.stringify(obj, null, indent);
