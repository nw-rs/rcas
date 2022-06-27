use core::ops::{Add, Mul};

use libm::powf;

#[derive(Debug, Clone, Copy)]
pub enum Number {
    Float(f32),
    Int(i32),
    Frac(i16, i16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NumberType {
    Float,
    Integer,
    Fraction,
}

impl Number {
    fn get_type(&self) -> NumberType {
        match self {
            Number::Float(_) => NumberType::Float,
            Number::Int(_) => NumberType::Integer,
            Number::Frac(_, _) => NumberType::Fraction,
        }
    }

    fn same_type(&self, other: &Self) -> bool {
        self.get_type() == other.get_type()
    }

    fn as_int(self) -> Option<i32> {
        if let Self::Int(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn as_float(self) -> Option<f32> {
        if let Self::Float(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn as_frac_components(self) -> Option<(i16, i16)> {
        if let Self::Frac(nom, den) = self {
            Some((nom, den))
        } else {
            None
        }
    }

    fn simplify(self) -> Self {
        if let Self::Frac(nom, den) = self {
            if nom % den == 0 {
                Self::Int((nom / den).into())
            } else {
                self
            }
            // if result.is_negative() {
            //     result
            // } else {
            //     let Self::Frac(mut nom, mut den) = result;
            //     if nom < 0 {
            //         nom *= -1;
            //     }
            //     if den < 0 {
            //         den *= -1;
            //     }
            //     Self::Frac(nom, den)
            // }
        } else {
            self
        }
    }

    fn is_negative(&self) -> bool {
        match self {
            Number::Float(f) => f < &0.0,
            Number::Int(i) => i < &0,
            Number::Frac(n, d) => (n < &0) ^ (d < &0),
        }
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.same_type(&rhs) {
            match self {
                Self::Frac(lhn, lhd) => {
                    let (rhn, rhd) = rhs.as_frac_components().unwrap();
                    Self::Frac(lhn * rhn, lhd * rhd)
                }
                Self::Float(lhn) => Self::Float(lhn * rhs.as_float().unwrap()),
                Self::Int(lhn) => Self::Int(lhn * rhs.as_int().unwrap()),
            }
        } else {
            todo!()
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.same_type(&rhs) {
            match self {
                Self::Frac(lhn, lhd) => {
                    let (rhn, rhd) = rhs.as_frac_components().unwrap();
                    Self::Frac(lhn * rhd + rhn * lhd, lhd * rhd).simplify()
                }
                Self::Float(lhn) => Self::Float(lhn + rhs.as_float().unwrap()),
                Self::Int(lhn) => Self::Int(lhn + rhs.as_int().unwrap()),
            }
        } else {
            if let Self::Float(lhn) = self {
                if let Self::Int(rhn) = rhs {
                    Self::Float(rhn as f32 + lhn)
                } else {
                    let (rhn, rhd) = rhs.as_frac_components().unwrap();
                    Self::Float(f32::from(rhn) / f32::from(rhd) + lhn)
                }
            } else if let Self::Int(lhn) = self {
                if let Self::Float(rhn) = rhs {
                    Self::Float(lhn as f32 + rhn)
                } else {
                    let (rhn, rhd) = rhs.as_frac_components().unwrap();
                    Self::Float(f32::from(rhn) / f32::from(rhd) + lhn as f32)
                }
            } else {
                let (lhn, lhd) = self.as_frac_components().unwrap();
                if let Self::Int(rhn) = rhs {
                    Self::Float(f32::from(lhn) / f32::from(lhd) + rhn as f32)
                } else {
                    let rhn = rhs.as_float().unwrap();
                    Self::Float(f32::from(lhn) / f32::from(lhd) + rhn)
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Expr<'a> {
    Const(Number),
    Var(char),
    Expr(&'a Expr<'a>),
    Pow(&'a Expr<'a>, &'a Expr<'a>),
    Mult(&'a Expr<'a>, &'a Expr<'a>),
    Add(&'a Expr<'a>, &'a Expr<'a>),
}

impl<'a> Expr<'a> {
    fn get_number(&self) -> Option<Number> {
        if let Expr::Const(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    fn is_number(&self) -> bool {
        self.get_number().is_some()
    }

    pub fn evaluate(self) -> Self {
        match self {
            Expr::Expr(e) => e.evaluate(),
            Expr::Pow(b, p) => {
                let base = b.evaluate();
                let power = p.evaluate();
                let bn = base.get_number();
                let pn = power.get_number();
                if bn.is_some() && pn.is_some() {
                    let bn = bn.unwrap();
                    let pn = pn.unwrap();
                    Expr::Const(if let Number::Int(base) = bn {
                        if let Number::Int(power) = pn {
                            if base == 2 {
                                Number::Int(2 << power)
                            } else {
                                Number::Float(powf(base as f32, power as f32))
                            }
                        } else if let Number::Float(power) = pn {
                            Number::Float(powf(base as f32, power))
                        } else {
                            let (pnom, pden) = pn.as_frac_components().unwrap();
                            Number::Float(powf(base as f32, f32::from(pnom) / f32::from(pden)))
                        }
                    } else if let Number::Float(base) = bn {
                        if let Number::Float(power) = pn {
                            Number::Float(powf(base, power))
                        } else if let Number::Int(power) = pn {
                            Number::Float(powf(base, power as f32))
                        } else {
                            let (pnom, pden) = pn.as_frac_components().unwrap();
                            Number::Float(powf(base, f32::from(pnom) / f32::from(pden)))
                        }
                    } else {
                        let (bnom, bden) = bn.as_frac_components().unwrap();
                        if let Number::Frac(pnom, pden) = pn {
                            Number::Float(powf(
                                f32::from(bnom) / f32::from(bden),
                                f32::from(pnom) / f32::from(pden),
                            ))
                        } else if let Number::Float(power) = pn {
                            Number::Float(powf(f32::from(bnom) / f32::from(bden), power))
                        } else {
                            let power = pn.as_int().unwrap();
                            Number::Float(powf(f32::from(bnom) / f32::from(bden), power as f32))
                        }
                    })
                } else {
                    Expr::Pow(&base, &power)
                }
            }
            Expr::Mult(lhs, rhs) => {
                let lhs = lhs.evaluate();
                let rhs = rhs.evaluate();
                let lhn = lhs.get_number();
                let rhn = rhs.get_number();
                if lhn.is_some() && rhn.is_some() {
                    Expr::Const(lhn.unwrap() * rhn.unwrap())
                } else {
                    Expr::Mult(&lhs, &rhs)
                }
            }
            Expr::Add(lhs, rhs) => {
                let lhs = lhs.evaluate();
                let rhs = rhs.evaluate();
                let lhn = lhs.get_number();
                let rhn = rhs.get_number();
                if lhn.is_some() && rhn.is_some() {
                    Expr::Const(lhn.unwrap() + rhn.unwrap())
                } else {
                    Expr::Add(&lhs, &rhs)
                }
            }
            Expr::Const(num) => Expr::Const(num.simplify()),
            _ => self,
        }
    }
}

fn test<'a>() -> Expr<'a> {
    let thing = Expr::Mult(
        &Expr::Pow(&Expr::Var('x'), &Expr::Const(Number::Int(2))),
        &Expr::Add(&Expr::Const(Number::Frac(1, 10)), &Expr::Var('x')),
    );
    return thing;
}

fn other() {
    let test = test();
}
