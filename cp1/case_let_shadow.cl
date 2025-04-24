class Main inherits IO {
    main () : Object {
        case "bad" of
            a : String =>
                let a : String <- "good" in
                out_string(a);
        esac
    };
};