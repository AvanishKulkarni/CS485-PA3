class A {
    a : Int <- 5;
    update(a : Int) : Int {
        a <- 2
    };
    get() : Int {
        a
    };
};

class Main inherits IO{
    main() : Object {
        let x : A <- new A in
        {
            x.update(3);
            out_int(x.get());
        }
    };
};