class Main inherits IO {
    a: A;
    b: A <- (new A);
    c: B <- (new B);
    d: B;
    e: A <- b;
    f: A <- (new A);
    main() : Object{
        {if a=a then
            out_string("a=a\n")
        else
            out_string("a!=a\n")
        fi;
        if a=b then
            out_string("a=b\n")
        else
            out_string("a!=b\n")
        fi;
        if a=c then
            out_string("a=c\n")
        else
            out_string("a!=c\n")
        fi;
        if a=d then
            out_string("a=d\n")
        else
            out_string("a!=d\n")
        fi;
        if b=e then
            out_string("b=e\n")
        else
            out_string("b!=e\n")
        fi;
        if b=f then
            out_string("b=f\n")
        else
            out_string("b!=f\n")
        fi;
        }
    };
};

class A {
    x: Int;
    y: Int;
};
class B {
    a: String;
    b: Int;
};