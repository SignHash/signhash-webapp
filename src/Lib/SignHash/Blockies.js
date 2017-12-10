var blockies = require('../../../vendor/blockies.js');


exports._buildAddressBlockie = function(spec, address) {
  return blockies.create({
    seed: address.toLowerCase(),
    size: spec.size,
    scale: spec.scale
  }).toDataURL();
};
