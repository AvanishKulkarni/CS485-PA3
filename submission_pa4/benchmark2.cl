class Main inherits IO {
  sum(a: Int, b: Int): Int {
    a + b
  };

  main() : Object {
    let
    a : Int, b : Int, c : Int,
    d : Int, e : Int, f : Int,
    g : Int, h : Int, i : Int,
    j : Int, k : Int, l : Int,
    m : Int, n : Int
    in {
      a <- ~48; b <- ~84; c <- 42;
      d <- 18; e <- ~84; f <- ~58;
      g <- 5; h <- 56; i <- 39;
      j <- ~2; k <- 75; l <- ~32;
      m <- 83; n <- ~99;
      out_int(sum(a,b) * 1); -- identities
      out_int(sum(c,d) + 0); 
      out_int(sum(e,f) / 1); 
      out_int(sum(g,h) - 0); 
      out_int(sum(i,j) * 1); 
      out_int(sum(k,l) * 0); -- mult by 0
      out_int(sum(m,n) * 0); 
      out_string("\n");
    }
  };
};
