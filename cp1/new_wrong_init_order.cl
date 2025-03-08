class A inherits IO {
    x : Int <- z + 5;
    y : Int <- x + 1;
    z : Int <- y;

    print() : Object {
        {
            out_int(x);
            out_string(" ");
            out_int(y);
            out_string(" ");
            out_int(z);
            out_string("\n");
        }
    };
};

class Main {
    main() : Object {
        (new A).print()
    };
};