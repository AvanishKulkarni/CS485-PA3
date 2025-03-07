class Main inherits IO {
    main() : Object {
        let x : Int <- in_int() in {
            out_int(x * 2);
            out_string("\n");
        }
    };
};