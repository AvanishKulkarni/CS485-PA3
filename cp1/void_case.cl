class Main {
    main() : Object {
        let nothing : Object <- 
            while false loop {
                1;
            } pool 
            in 
        {
            case nothing of 
                x: Int => 1;
                y: Object => 2;
            esac;
        }
    };
};