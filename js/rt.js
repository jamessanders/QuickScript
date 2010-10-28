function __makeIterators (env) {

  ////////////////////////////////////////////////////////////////////////
  // Iterators 
  ////////////////////////////////////////////////////////////////////////

  function StopIteration () { return true };
  env.StopIteration = StopIteration;

  function ListIterator (ls) {
    var i = 0;
    this.next = function () {
      if (i < ls.length)
        return ls[i++];
      else 
        return StopIteration;
    };
  };

  function ZipIterator (a, b) {
    this.next = function () {
      var x = a.next();
      var y = b.next();
      if (x != StopIteration && y != StopIteration) 
        return [x, y];
      else
        return StopIteration;
    };
  };

  function CycleIterator (ls) {
    var i = 0;
    var rec = [];
    this.next = function () {
      var x = ls.next();
      if (x == StopIteration) {
        this.next = function () {
          if (i >= rec.length) {
            i = 0;
          }
          return rec[i++];
        };
        return this.next();
      } else {
        rec.push(x);
        return x;
      }
    };
  };
    
  function ObjectIterator (obj) {
    var ls = [];
    for (k in obj) {
      if (obj.hasOwnProperty(k)){
        ls.push([k, obj[k]]);
      }
    }
    var it = new ListIterator(ls);
    this.next = function () { return it.next() };
  };

  function EmptyIterator () {
    this.next = function () { return StopIteration };
  };

  function RangeIterator (a,b) {
    this.next = function () { 
      if (b && a >= b) return StopIteration ;
      else return a++;
    };
  };

  var Iterator = function (obj) { 
    if (obj != undefined && obj.hasOwnProperty("next")) {
      return obj
    } else if (obj instanceof Array) {
      return (new ListIterator(obj));
    } else if (obj instanceof Object) {
      return (new ObjectIterator(obj));
    } else {
      return (new EmptyIterator());
    }
  };
  env.Iterator = Iterator;

  function Enumerator (iterator, accum) {
    this.stop = false;
    this.accum = accum;
    this.end = function (ret) {
      this.stop = true;
      if (ret != undefined) {
        this.accum = ret;
      }
    };

    this.run = function (fn) {
      var n = iterator.next();
      while (n != StopIteration && !this.stop) {
        var ret = (fn.call(this, this.accum, n));
        if (!this.stop) {
          this.accum = ret;
        }
        n = iterator.next();
      }
      return this.accum;
    };
  }

  function AsyncEnumerator (iterator, accum, callback) {
    this.stop = false;
    this.accum = accum;
    this.end = function (ret) {
      this.stop = true;
      if (ret != undefined) {
        this.accum = ret;
      }
      if (callback) callback(this.accum);
    };

    this.run = function (fn) {
      var self = this;
      var n = iterator.next();
      if (n != StopIteration && !this.stop) {
        fn.call(this, this.accum, n, function (ret) {
          if (!self.stop) {
            self.accum = ret;
            self.run(fn);
          } else {
            if (callback) callback(self.accum);
          }
        });
      } else {
        if (callback) callback(this.accum);
      }
    }
  }
  env.AsyncEnumerator = AsyncEnumerator;

  var each = function each (obj, fn) {
    var iterator = Iterator(obj);
    return (new Enumerator(iterator, undefined).run(function(a,b) { return fn.call(this, b) }));
  };
  env.each = each;

  var map = function map (fn, obj) {
    var en = new Enumerator(Iterator(obj), []);
    return en.run(function(a,b) {
      return a.concat([fn.call(this,b)]);
    });
  };
  env.map  = map;

  var fold = function fold (fn, def, obj) {
    return (new Enumerator(Iterator(obj), def)).run(fn);
  };
  env.fold = fold;

  function foldAsync (fn, start, iterator, next) {
    return (new AsyncEnumerator(Iterator(iterator), start, next)).run(fn);
  }
  env.foldAsync = foldAsync;

  function mapAsync (fn, iterator, next) {
    var ae = new AsyncEnumerator (Iterator(iterator), [], next);
    return ae.run(function (a, b, next) {
      fn.call(this, b, function (x) {
        return next (a.concat(x));
      });
    });
  };
  env.mapAsync = mapAsync;

  function eachAsync (iterator, fn, next) {
    var ae = new AsyncEnumerator (Iterator(iterator), [], next);
    return ae.run(function (a, b, next) {
      fn.call(this, b, function (x) {
        return next (x);
      });
    }); 
  };
  env.eachAsync = eachAsync;

  function filter (fn, iterator) {
    fold(iterator, [], function (a, b) {
      if (fn(a)) return a.concat(b);
      else return a
    });
  };
  env.filter = filter;

  // function takeWhile (fn, iterator) {
  //   fold(iterator, 
  //   };

  var zip = function zip (a, b) { 
    return (new ZipIterator(Iterator(a),Iterator(b))); 
  };
  env.zip = zip;

  var cycle = function cycle(ls) {
    return (new CycleIterator(Iterator(ls)));
  };
  env.cycle = cycle;

  var range = function range (start, end) {
    return (new RangeIterator(start, end));
  };
  env.range = range;
    
  var toList = function (obj) { 
    var iterator = Iterator(obj);
    return map(function(b){ return b; }, iterator); 
  };
  env.toList = toList;
}

function globalLoadIterators () {
  try {
    __makeIterators(window);
  } catch (err) {
    __makeIterators(global);
  }
};

if (typeof(exports) !== "undefined") {
  exports.globalLoadIterators = globalLoadIterators;
  __makeIterators (exports);
} else {
  globalLoadIterators();
}
