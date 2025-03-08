class Main inherits IO {
    a: B;
    main() : Object {
        a@A.print()
    };
};

class A inherits IO{
    x : Int <- 5;
    print(): Object {
        out_int(x)
    };
};

class B inherits A {
    print() : Object {
        out_int(x+2)
    };
};