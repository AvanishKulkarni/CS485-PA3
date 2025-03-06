class A {};

class B {};

class Main {
    main() : Object {
        let number : Int <- 1 in {
            case number of 
                x: A => 1;
                y: B => 2;
            esac;
        }
    };
};