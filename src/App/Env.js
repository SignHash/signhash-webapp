exports.env = {
  rpcUrl: process.env.RPC_URL || 'http://localhost:8545',
  publicPath:
    process.env.NODE_ENV === 'production'
    ? '/dist/' : 'http://localhost:8080/dist/'
};
