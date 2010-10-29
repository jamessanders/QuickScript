var fs   = require("fs");
var p    = require("path");

function wrap (v) {
  next(v);
}

function print (str) {
  if (process.stdout.write(str + "\n", 'ascii')) {
    next();
  } else {
    %process.nextTick();
    next();
  }
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
  %print (path)
  function aux (a, b) {
    (err, d) <- readDir(p.join(path,b));
    if (err) next (a.concat(b));
    else next(a.concat(d));  
  }
  isdir <- is_dir(path);
  if (isdir) {
    (err, dir) <- fs.readdir(path);
    all <- foldAsync(aux, [path], dir);
    next(err, all);
  } else {
    next("Not a directory",[])
  }
};

c <- wrap(2)
if (process.argv[c]) {
  (err, accum) <- readDir(process.argv[c]);
  %print("Finished");
} else {
  %print ("You must provide a path");
}

err <- process.on('uncaughtException')
console.log(err)
process.exit()
