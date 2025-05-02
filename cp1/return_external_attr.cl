class External {
  get() : Int {{
    while false loop 1 pool;
    10;
  }};
};

class Main inherits IO {
  main(): Object {{
    let x: External <- new External in 
    out_int(x.get());
  }};
};

