exports.requireContractData = function(name) {
  return require('signhash-contracts/build/contracts/' + name + '.json');
};
