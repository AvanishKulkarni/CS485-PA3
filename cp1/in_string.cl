class Main inherits IO {
    main() : Object {
        let x : String <- in_string(), y : String <- in_string(), z : String <- in_string() in {
            out_string(x.concat(y).concat(z).concat("\n"));
        }
    };
};