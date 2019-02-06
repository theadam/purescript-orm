exports.arrayifyResult = function(f) {
  return f.map(function(row) {
    const keys = Object.keys(row)
      .slice()
      .sort();
    return keys.map(function(key) {
      return row[key];
    });
  });
};
