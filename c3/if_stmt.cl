class Main inherits IO {
    main() : Object {{
        let x : Bool in {
        if x <- true then {
            out_int(1);
        } else {
            out_int(2);
        } fi;
        };
        -- let y : Bool <- true in 
        -- if y then {
        --     1;
        -- } else {
        --     2;
        -- } fi;
    }};
};
