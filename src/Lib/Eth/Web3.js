var Eth = require('ethjs');


exports.bytesFromASCII = Eth.fromAscii;


exports.buildWeb3 = function (config) {
  return new Eth(new Eth.HttpProvider(config));
};


exports._getInjectedWeb3 = function() {
  if (typeof web3 !== 'undefined') {
    return new Eth(web3.currentProvider);
  } else {
    return undefined;
  }
};
