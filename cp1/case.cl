class Main inherits IO {
    main() : Object {
        let a : A, z : Int in
        {
            a <- new D;
            z <-
            case a of
                x: B => 1;
                y: C => 2;
                z: E => 3;
                b: F => 4;
                c: G => 5;
            esac;
            out_int(z);
        }
    };
};

class A {

};

class B inherits A {};
class C inherits A {};
class D inherits B {};
class E inherits C {};
class F inherits A {};
class G inherits C {};
class Z {};