class Main inherits IO {
  main() : Object {{
      let x : Int in
      let y : Int in
      let z : Int in
      let a : Int in
      let b : Int in
      let c : Int in
      let d : Int in
      let e : Int in
      let f : Int in
      let g : Int in
      let h : Int in
      let i : Int in
      let j : Int in
      let k : Int in
      let l : Int in
      let m : Int in
      let n : Int in
      let o : Int in
      let p : Int in
      let q : Int in
      let r : Int in
      let s : Int in
      let t : Int in
      let u : Int in
      let v : Int in
      let w : Int in
      let zzz : Int in
      let final : Int in
        -- Deeply nested, recursive-style madness, no actual recursion though
        let no_binds : Int in
          {
            x <- in_int();  --10
            y <- x + 1; -- 11
            z <- y * 3; -- 33
            a <- z - x; -- 23
            b <- (a * a) + x; --539
            c <- b / 2; -- 269
            d <- ~(c - 100); -- 169
            e <- if x < 10 then d + 50 else d - 50 fi; -- -219
            f <- e * e; --47961
            g <- f / (x + 1); -- 4360
            (* out_int(g); *)
            h <- if g = 0 then 1 else g fi;
            i <- h + x * y - z / 2;
            j <- if not (i <= 0) then i else ~i fi;
            k <- j * j * j;
            l <- k - (j + i);
            m <- if x < 0 then ~x else x fi;
            n <- (m + l) * (k - l + 1);
            o <- if (n < 0) then ~n else n fi;
            p <- o / (x + 1);
            q <- p * x - y + 3;
            r <- q / (if x = 1 then 1 else x fi);
            s <- r + h - k;
            t <- if (s <= 0) then ~s else s fi;
            u <- t * t * t;
            v <- u + r - q;
            w <- v - z + y;
            zzz <- w * 2 + 1;
            final <- if zzz < 0 then ~zzz else zzz fi;
            out_int(final);
            };
    }
  };
};
