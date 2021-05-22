const path = require('path');

module.exports = [
  {
    entry: './src/import.js',
    output: {
      filename: 'import.js',
      path: path.resolve(__dirname, 'public'),
    },
    mode: 'production',
  },
  {
    entry: './pure.js',
    output: {
      filename: 'main.js',
      path: path.resolve(__dirname, 'public'),
    },
    mode: 'production',
  },
];