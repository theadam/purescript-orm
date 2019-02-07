function sortNumber(a, b) {
  return a - b;
}

exports.arrayifyResult = function(f) {
  return f.map(function(row) {
    const keys = Object.keys(row)
      .slice()
      .map(Number)
      .sort(sortNumber);
    return keys.map(function(key) {
      return row[key];
    });
  });
};
