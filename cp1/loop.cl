class Main inherits IO {
    main() : Object {
        let x : Int <- 2 in {
            while 0 < x loop {
                out_string("hello\n");
                x <- x - 1;
            } pool;
        }
    };
};
