class Main inherits IO {
    main () : Object {
             let x : Int in
      let e : Int in
      let f : Int in
      let g : Int in{
                    x <- f;
                    e <- 2- x <- 5 + x;
                    x <- 5;
                    f <- e * e;
                    g <- f / (x+1);
                    out_int(g);
                }
    };
};