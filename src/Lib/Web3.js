var Web3 = require('web3');


exports.buildWeb3 = function (config) {
  return new Web3(new Web3.providers.HttpProvider(config));
};


exports._getInjectedWeb3 = function() {
  if (typeof web3 !== 'undefined') {
    return web3;
  } else {
    return undefined;
  }
};
