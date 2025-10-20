pub fn gcd(a: u32, b: u32) -> u32 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

pub fn lcm(a: u32, b: u32) -> u32 {
    let ab_gcd = gcd(a, b);
    a / ab_gcd * b
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_gcd() {
        let a = 30;
        let b = 20;
        assert_eq!(gcd(a, b), 10);
    }

    #[test]
    fn test_lcm() {
        let a = 30;
        let b = 20;
        assert_eq!(lcm(a, b), 60);
    }
}
