// Benchmark.Suite.prototype.run([options={}])
exports.runSuite = function(suite) {
  return function(){
    suite.run();
  };
};
