class Main inherits IO {
  myself: SELF_TYPE <- self;

  main(): Object {
    (new Derivative).identify()
  };
};

class Derivative inherits Main {
  identify(): Object {
    out_string(myself.type_name())
  };
};
