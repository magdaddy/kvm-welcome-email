'use strict';

const dotenv = require('dotenv');

exports.dotenvConfig = () => dotenv.config();

exports.getNodeEnvImpl = () => process.env.NODE_ENV;
