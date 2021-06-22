'use strict';

const dotenv = require('dotenv');

exports.dotenvConfig = () => dotenv.config();

exports.getNodeEnvImpl = () => process.env.NODE_ENV;
exports.getUsersImpl = () => process.env.USERS;

const tokenSecret = require('crypto').randomBytes(32).toString('hex');
const jwt = require('jsonwebtoken');

exports.tokenSecret = tokenSecret;
exports.jwtSign = payload => key => options => jwt.sign(payload, key, options);
