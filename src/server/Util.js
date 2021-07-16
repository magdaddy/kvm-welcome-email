'use strict';

// env

const dotenv = require('dotenv');

exports.dotenvConfig = () => dotenv.config();

exports.getNodeEnvImpl = () => process.env.NODE_ENV;
exports.getUsersImpl = () => process.env.USERS;


// jwt

const tokenSecret = require('crypto').randomBytes(32).toString('hex');
const jwt = require('jsonwebtoken');

exports.tokenSecret = tokenSecret;
exports.jwtSign = payload => key => options => jwt.sign(payload, key, options);
exports.jwtVerifyImpl = token => () => jwt.verify(token, tokenSecret);


// turf

const turf = require('@turf/turf');
const fs = require('fs');
let dach = fs.readFileSync('grenzen_dach.geojson');

dach = JSON.parse(dach);
turf.toWgs84(dach, {mutate: true});

const grenzenAt = dach.features[0];
const grenzenDe = dach.features[1];
const grenzenCh = dach.features[2];

const toTurfPoint = ({ lat, lng }) => turf.point([lng, lat]);

exports.isInAt = p => turf.booleanPointInPolygon(toTurfPoint(p), grenzenAt);
exports.isInDe = p => turf.booleanPointInPolygon(toTurfPoint(p), grenzenDe);
exports.isInCh = p => turf.booleanPointInPolygon(toTurfPoint(p), grenzenCh);
