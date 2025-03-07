class Main inherits IO {
    a: Int;
    b: String;
    c: Bool;
    d: A;
    main() : Object {
        {
            if a = 0 then 
                out_string("Default a = 0\n") 
            else 
                out_string("Default a != 0") 
            fi;
            if b = "" then
                out_string("Default b = empty string\n")
            else
                out_string("Default b != empty string\n")
            fi;
            if c = false then
                out_string("Default c = false\n")
            else
                out_string("Default c != false\n")
            fi;
            if isvoid(d) then
                out_string("Default d is void\n")
            else
                out_string("Default d is not void\n")
            fi;
        }
    };
};
class A {
    x: Int;
};