class A inherits IO {
    print() : Object { out_string(self.type_name().concat("\n")) };
};

class Main inherits A {

    a : A <- self;

    print() : Object { out_string(self.type_name().concat("\n")) };

    main() : Object {{
        a <- (new A);
        a.print();
    }};
};