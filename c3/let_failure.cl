class Main inherits IO {
    main() : Object {
        let x : Int in {
            x <- 1;
            let x : Int in {
                x<- 2;
                let x : Int in {
                    out_int(x <- 3);
                };
                out_int(x);
            };
            out_int(x);
        }
    };
};