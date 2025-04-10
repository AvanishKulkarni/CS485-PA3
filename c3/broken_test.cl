class Main inherits IO {
    main () : Object {
        (* out_int(1+2) *)
        (* {let x : Int, y :Int in {
            x <- 2;
            out_int(x);
        };

        let x : Int in {
            x <- 5;
            out_int(x);
            let x : Int in {
                x <- 10;
                out_int(10);
            };
        };} *)
        (* if 1 < 5 then
            out_int(143)
        else
            out_int(234234)
        fi *)
        (* out_int(~55+~(10/5*~2)) *)
        (* test(1+2) *)
        {
            let x : Int, y : Int, z : Int in {
                x<-y;
                x<- 1+x;
                let x : Int in {
                out_int(x);
                let y : Int in
                    out_int(y);
                };
                out_int(x);
            };
            let x : Int in out_int(x);
        }
    };
(*     test(x: Int) : Int {
        x + 1
    }; *)
};