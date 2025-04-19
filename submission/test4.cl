class Main inherits IO {
    a: A <- new A;
    b: A <- new A;
    main() : Object {
        {
            a.set(45);
            b.set(75);
            update(a, b);
            update(a, a);
            update(b, b);
            let x : A <- new A in
            let y : A <- new A in
            {
                x.set(1);
                y.set(30);
                update(x, y);
                update(x, x);
            };
        }
    };
    update(x: A, y: A) : Object {
        {
            out_int(x.get());
            out_string(" ");
            out_int(y.get());
            out_string(" ");
            x.set(x.get()+2);
            out_int(x.get());
            out_string(" ");
            out_int(y.get());
            out_string("\n");
        }
    };
};

class A {
    x: Int;
    set(y: Int) : Int {
        x <- y
    };
    get() : Int {
        x
    };
};