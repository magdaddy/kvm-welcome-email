'use strict';

const winston = require('winston');

const logger = winston.createLogger({
  level: 'verbose',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  // defaultMeta: { service: 'user-service' },
  transports: [
    new winston.transports.File({ filename: 'logfile.log', level: 'info' }),
    // new winston.transports.File({ filename: 'combined.log' }),
  ],
});

//
// If we're not in production then log to the `console` with the format:
// `${info.level}: ${info.message} JSON.stringify({ ...rest }) `
//
if (process.env.NODE_ENV !== 'production') {
  logger.add(new winston.transports.Console({
    format: winston.format.combine(
      winston.format.colorize(),
      winston.format.simple()
    )
  }));
}

//using the logger and its configured transports, to save the logs created by Morgan
const morgan = require('morgan');

const myStream = {
  write: (text) => {
      logger.http(text)
  }
}

exports.morgan = morgan('dev', { stream: myStream });

exports.log = info => () => logger.log(info);
