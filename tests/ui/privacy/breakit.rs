//@ build-fail
//@ aux-crate: breakit=breakit.rs

fn main() {
    // If associated types were properly normalized for type privacy checking,
    // then this would be an error "type `Priv` is private".
    type Priv = <u8 as breakit::PubTr>::Assoc;
    type Unreachable = <Priv as breakit::GetUnreachable>::Assoc;
    Unreachable::generic::<i32>();
}

//~? ERROR missing optimized MIR
