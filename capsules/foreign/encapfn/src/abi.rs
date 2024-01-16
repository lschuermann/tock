pub trait EncapfnABI {
    type ParametersContainer;
    type ReturnValueContainer;
}

pub trait Encapfn4WRegABI: EncapfnABI {
    // TODO: do we have a "dontcare" integer value here? Maybe we can
    // avoid explicitly loading these values into registers then.
    #[inline(always)]
    fn encode_1w(a: usize) -> Self::ParametersContainer {
        Self::encode_4w(a, 0, 0, 0)
    }

    #[inline(always)]
    fn encode_2w(a: usize, b: usize) -> Self::ParametersContainer {
        Self::encode_4w(a, b, 0, 0)
    }

    #[inline(always)]
    fn encode_3w(a: usize, b: usize, c: usize) -> Self::ParametersContainer {
        Self::encode_4w(a, b, c, 0)
    }

    fn encode_4w(a: usize, b: usize, c: usize, d: usize) -> Self::ParametersContainer;
}

pub trait Encapfn8WRegABI: Encapfn4WRegABI {
    #[inline(always)]
    fn encode_5w(a: usize, b: usize, c: usize, d: usize, e: usize) -> Self::ParametersContainer {
        Self::encode_8w(a, b, c, d, e, 0, 0, 0)
    }

    #[inline(always)]
    fn encode_6w(
        a: usize,
        b: usize,
        c: usize,
        d: usize,
        e: usize,
        f: usize,
    ) -> Self::ParametersContainer {
        Self::encode_8w(a, b, c, d, e, f, 0, 0)
    }

    #[inline(always)]
    fn encode_7w(
        a: usize,
        b: usize,
        c: usize,
        d: usize,
        e: usize,
        f: usize,
        g: usize,
    ) -> Self::ParametersContainer {
        Self::encode_8w(a, b, c, d, e, f, g, 0)
    }

    fn encode_8w(
        a: usize,
        b: usize,
        c: usize,
        d: usize,
        e: usize,
        f: usize,
        g: usize,
        h: usize,
    ) -> Self::ParametersContainer;
}

impl<ABI: Encapfn8WRegABI> Encapfn4WRegABI for ABI {
    #[inline(always)]
    fn encode_4w(a: usize, b: usize, c: usize, d: usize) -> Self::ParametersContainer {
        Self::encode_8w(a, b, c, d, 0, 0, 0, 0)
    }
}

pub mod rv32i {
    use super::{Encapfn8WRegABI, EncapfnABI};

    pub enum Riscv32iCABI {}

    impl EncapfnABI for Riscv32iCABI {
        type ParametersContainer = [usize; 8];
        type ReturnValueContainer = [usize; 2];
    }

    impl Encapfn8WRegABI for Riscv32iCABI {
        fn encode_8w(
            a: usize,
            b: usize,
            c: usize,
            d: usize,
            e: usize,
            f: usize,
            g: usize,
            h: usize,
        ) -> Self::ParametersContainer {
            [a, b, c, d, e, f, g, h]
        }
    }
}

pub mod armv7m {
    use super::{Encapfn4WRegABI, EncapfnABI};

    pub enum ArmV7MCABI {}

    impl EncapfnABI for ArmV7MCABI {
        type ParametersContainer = [usize; 4];
        type ReturnValueContainer = [usize; 2];
    }

    impl Encapfn4WRegABI for ArmV7MCABI {
        fn encode_4w(a: usize, b: usize, c: usize, d: usize) -> Self::ParametersContainer {
            [a, b, c, d]
        }
    }
}

pub mod mock {
    use super::{Encapfn4WRegABI, EncapfnABI};

    pub enum MockABI {}

    impl EncapfnABI for MockABI {
        type ParametersContainer = [usize; 4];
        type ReturnValueContainer = [usize; 2];
    }

    impl Encapfn4WRegABI for MockABI {
        fn encode_4w(a: usize, b: usize, c: usize, d: usize) -> Self::ParametersContainer {
            [a, b, c, d]
        }
    }
}
