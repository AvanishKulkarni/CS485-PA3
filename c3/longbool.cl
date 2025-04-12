class Main inherits IO {
    main() : Object {
        let x : Bool in {
            x <- (5 + 8012 * 5125) < (2123 / 24);
            x <- x = (521 < 0);
            if x then
                out_int(1)
            else
                out_int(0)
            fi;
        }
    };
};