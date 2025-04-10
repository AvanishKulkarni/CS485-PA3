class Main inherits IO {
    main() : Object {
        let x : Int in {
            x <- in_int();
            if x < 0 then {
                out_int(~x);
                (* out_string(" neg\n"); *)

            } else {
                out_int(x);
                (* out_string(" pos\n"); *)
            } fi;

        }
    };
};