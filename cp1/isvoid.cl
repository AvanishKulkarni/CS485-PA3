class Main inherits IO {
    a: A;
    b: A <- (new A);
    main() : Object{
        {if isvoid(a) then
            out_string("a is void\n")
        else
            out_string("a is not void\n")
        fi;
        if isvoid(b) then
            out_string("b is void\n")
        else
            out_string("b is not void\n")
        fi;
        }
    };
};

class A {
    x: Int;
    y: Int;
};