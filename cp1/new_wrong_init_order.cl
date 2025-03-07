class A inherits IO {
    z : Int <- y;
    y : Int <- x + 1;
    x : Int <- 1;
};

class Main {
    main() : Object {
        (new A)
    };
};