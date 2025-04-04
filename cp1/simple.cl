class Main inherits IO {
    main(): Object {
        let b : Int in {
            let a : Int in {
                b <- 2;
                out_int(a + 5);
                b+a;
            };
        }
    };
};