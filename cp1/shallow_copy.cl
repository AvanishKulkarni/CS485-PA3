Class IntWrapper {
    a : Int;

    set(x : Int) : IntWrapper {{ a <- x; self; }};
    get() : Int { a };
};

Class Copier {
    a : IntWrapper <- (new IntWrapper);

    set(x : Int) : IntWrapper { a <- a.set(x) };
    get() : Int { a.get() };
};

class Main inherits IO {

    a : Copier <- (new Copier);
    b : Copier;

    main() : Object {
        {
            a.set(5); 
            b <- a.copy();
            out_int(a.get());
            out_int(b.get());
        }
    };
};