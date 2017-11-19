var R = require('ramda');
var buildContract = require('truffle-contract');


function requireABI (name) {
  return require('signhash-contracts/build/contracts/' + name + '.json');
}


var abis = R.map(requireABI, {
  SignHash: 'SignHash',
});


function fixTruffleAPI(contract) {
  // This rebinds `.call` of all contract methods to default
  // `Function.prototype.call` implementation. Some environments /
  // packages don't except .call to be rebound to a custom behavior
  // and fail unexpectedly when working with `truffle-contract`
  // (eg. testcafe and its hammerhead).
  for (var i = 0; i < contract.abi.length; i++) {
    var item = contract.abi[i];
    if (item.type == "function") {
      contract[item.name].call = Function.prototype.call;
    }
  }
  return contract;
}


var getContract = R.curry(function (abi, web3) {
  var contract = buildContract(abi);
  contract.setProvider(web3.currentProvider);
  return contract;
});


exports._getDeployed = function (contract) {
  return function () {
    return contract.deployed().then(fixTruffleAPI);
  };
};


exports.loadSignerContract = getContract(abis.SignHash);

exports._sign = R.curry(function(contract, checksum, address) {
  return function () {
    return contract.sign(checksum, { from: address });
  };
});
