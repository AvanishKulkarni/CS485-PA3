class Wrapper {
    n : String;

    concat(s: String) : String {
        n <- n.concat(s)
    };
};

class Main inherits IO {

    print_strings(a: String, b: String, c: String) : Object {{
        out_string(a);
        out_string(b);
        out_string(c);
    }};

    main() : Object {
        {
            let x : Wrapper <- (new Wrapper) in {
                print_strings(x.concat("hello "), x.concat("world "), x.concat("\n"));
            };
        }   
    };
};