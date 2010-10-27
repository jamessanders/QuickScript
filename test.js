testOne   <- require("test1")
testTwo   <- require("test2")
testThree <- require("test3")

function getOne () {
  next (1);
}
function getTwo () {
  next (2);
}
function getThree () {
  one <- getOne();
  two <- getTwo();
  added <- add (one, two);
  next(added);
}
function add(a, b) {
  next(a+b);
}
function rand (max) {
  next(Math.floor(Math.random()*max));
}

r = 0;
while (r < 201) {
  three <- getThree();
  r <- rand(200);
  console.log(r);
}
