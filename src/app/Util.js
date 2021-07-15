'use strict';

const jwt_decode = require('jwt-decode').default;

exports.jwtDecode = (token) => jwt_decode(token);
exports.jwtDecodeHeader = (token) => jwt_decode(token, { header: true });
