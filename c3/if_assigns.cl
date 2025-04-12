class Main inherits IO {
  main() : Object {
    let
    a : Int
    in {
    a <- 1;
    if (~(a <- a) + (a <- a)) < (a <- a)
    then
      out_int(1)
    else
      out_int(0)
    fi;}
  };
};