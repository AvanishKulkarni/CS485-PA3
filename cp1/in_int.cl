class Main inherits IO {
    main() : Object {
        let i : Int <- 0 in {
            while i < 15 loop
            let x: Int <- in_int() in
            {
                out_int(x);
                out_string("\n");
                i <- i+1;
            }
            pool;
        }
    };
};