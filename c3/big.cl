class Main inherits IO {
    main(): Object {
       {
          let x: Int, y: Int, z: Int, cond1: Bool, cond2: Bool in {
             x <- 10;  -- input 1
             y <- 2;  -- input 2
             z <- x * y + (x - y) / y; -- avoid div by zero 14
             cond1 <- if x < y then
                         if not (z = 0) then true else false fi
                      else false
                      fi; --false
 
             cond2 <- if z < x+y then
                         true
                      else 
                        if z < x - y then
                            true
                        else false
                        fi
                      fi; -- false
 
             if cond1 then {
                let a: Int, b: Int, flag: Bool in {
                   a <- x + z * 2;
                   b <- y * y - x;
                   flag <- if b < a then
                              if not (a = b) then true else false fi
                           else false fi;
                   if flag then {
                      (* out_int(a + b); *) -- output 1
                      2;
                   } else {
                      (* out_int(a - b); *) -- output 2
                      1;
                   } fi;
                };
             } else {
                let m: Int, n: Int, check: Bool in {
                   m <- z * z + x; -- 14 * 14 + 10 = 206
                   n <- (y + 2) * (x - 3); -- 4 * 7 = 21
                   out_int(n);
                   check <- if m = n then
                               true
                            else 
                                if n < m then
                                true -- true
                                else
                                false
                                fi
                            fi;
                   if check then {
                      out_int(m); -- output 3
                   } else {
                      (* out_int(n); *)0; -- output 4
                   }fi;
                };
             }fi;
          };
          0;
       }
    };
 };