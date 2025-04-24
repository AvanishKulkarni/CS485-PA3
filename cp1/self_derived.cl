class Base {
    identify() : String {
        "Base"
    };
    duplicate(): SELF_TYPE {
        new SELF_TYPE
    };
};

class Derived inherits Base {
    identify() : String {
        "Derived"
    };
};

class Main inherits IO {
    main () : Object {
        out_string((new Derived).duplicate().identify())
    };
};