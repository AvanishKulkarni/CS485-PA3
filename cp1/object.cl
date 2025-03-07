class Main inherits IO {
    x : A <- new A;
    b: B <- new B;
    main () : Object {
        {
            x.set_string("This is x");
            x.set_c(b, 85);
            x.print();
            let y : A <- x.copy() in
            {
                out_string("y is of type ");
                out_string(y.type_name());
                out_string("\n");
                y.print();
            };
            abort();
            out_string("Uh oh, failed to abort\n");
        }
    };
};

class A inherits IO{
    a : Int <- 52;
    b : String;
    c: B;
    set_string(s : String) : String {
        b <- s
    };
    set_c(b : B, i : Int) : Object {
        {
            c <- b;
            c.set_z(i);
        }
    };
    print() : Object {
        {
            out_string("a=");
            out_int(a);
            out_string(", b=");
            out_string(b);
            out_string(", c.z = ");
            out_int(c.get_z());
            out_string("\n");
        }
    };
};

class B {
    z: Int <- 0;
    set_z(a: Int) : Int {
        z <- a
    };
    get_z() : Int {
        z
    };
};
