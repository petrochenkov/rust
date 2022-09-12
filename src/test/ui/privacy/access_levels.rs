#![feature(rustc_attrs)]

#[rustc_access_level]
mod outer { //~ ERROR Public: None, Exported: None, Reachable: None, ReachableFromImplTrait: None
    #[rustc_access_level]
    pub mod inner1 { //~ ERROR Public: None, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub

        #[rustc_access_level]
        extern "C" {} //~ ERROR Public: None, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub

        #[rustc_access_level]
        pub trait PubTrait { //~ ERROR Public: None, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub
            #[rustc_access_level]
            const A: i32; //~ ERROR Public: None, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub
            #[rustc_access_level]
            type B; //~ ERROR Public: None, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub
        }

        #[rustc_access_level]
        struct PrivStruct; //~ ERROR Public: None, Exported: None, Reachable: None, ReachableFromImplTrait: None

        #[rustc_access_level]
        pub union PubUnion { //~ ERROR Public: None, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub
            #[rustc_access_level]
            a: u8, //~ ERROR Public: None, Exported: None, Reachable: None, ReachableFromImplTrait: None
            #[rustc_access_level]
            pub b: u8, //~ ERROR Public: None, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub
        }

        #[rustc_access_level]
        pub enum Enum { //~ ERROR Public: None, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub
            #[rustc_access_level]
            A( //~ ERROR Public: None, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub
                #[rustc_access_level]
                PubUnion,  //~ ERROR Public: None, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub
            ),
        }
    }

    #[rustc_access_level]
    macro_rules! none_macro { //~ Public: None, Exported: None, Reachable: None, ReachableFromImplTrait: None
        () => {};
    }

    #[macro_export]
    #[rustc_access_level]
    macro_rules! public_macro { //~ Public: pub, Exported: pub, Reachable: pub, ReachableFromImplTrait: pub
        () => {};
    }

    #[rustc_access_level]
    pub struct ReachableStruct { //~ ERROR Public: None, Exported: None, Reachable: pub, ReachableFromImplTrait: pub
        #[rustc_access_level]
        pub a: u8, //~ ERROR Public: None, Exported: None, Reachable: pub, ReachableFromImplTrait: pub
    }
}

pub use outer::inner1;

pub fn foo() -> outer::ReachableStruct { outer::ReachableStruct {a: 0} }

fn main() {}
