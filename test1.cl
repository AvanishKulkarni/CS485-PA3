class Main inherits IO {
    main() : Object {
      out_int(2+let x : Int, y : Int in {x <- in_int(); x*x;})
    };
  };