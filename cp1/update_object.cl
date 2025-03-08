class Main inherits IO {
    a : String <- "Hello";
    main(): Object {
        {
            out_string(a);
            out_string("\n");
            a <- update(a, " World!");
            out_string(a);
            out_string("\n");
        }
    };
    update(s : String, s2: String) : String {
        s.concat(s2)
    };
};