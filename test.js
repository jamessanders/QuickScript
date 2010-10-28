var fs   = require("fs");
var p    = require("path");

function print (str) {
  process.stdout.write(str + "\n", 'ascii')
  next()
}

function is_dir(path) {
  ex <- p.exists(path)
  if (ex) {
    (err, stat) <- fs.stat(path);  
    next(stat.nlink != 1)
  } else {
    next(false);
  }
}

function readDir (path) {
  x <- print (path)
  function aux (a, b) {
    (err, d) <- readDir(b);
    if (err) next (a.concat(b));
    else next(a.concat(d));  
  }
  isdir <- is_dir(path);
  if (isdir) {
    (err, dir) <- fs.readdir(path);
    all <- foldAsync(aux, [path], map( \(d) return p.join(path,d), dir));
    next(err, all);
  } else {
    next("Not a directory",[])
  }
};

var c = 2
(err, accum) <- readDir(process.argv[c]);

