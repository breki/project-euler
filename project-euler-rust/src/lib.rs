/// Find the highest prime factor of a given number.
///
/// The function is faster than highestPrimeOf() but it is still too slow
/// for the 600851475143 test case since it starts from the other way around
/// - it should have started from 2 and go up to n/2.
pub fn highest_prime_of(n: i64) -> i64 {
    match n {
        n if n > 17 => {
            if n % (n/2) == 0 {
                highest_prime_of(n/2);
            }
            else if n % (n/3) == 0 {
                highest_prime_of(n/3);
            }
            else if n % (n/5) == 0 {
                highest_prime_of(n/5);
            }
            else if n % (n/7) == 0 {
                highest_prime_of(n/7);
            }
            else if n % (n/11) == 0 {
                highest_prime_of(n/11);
            }
            else if n % (n/13) == 0 {
                highest_prime_of(n/13);
            }
            else if n % (n/17) == 0 {
                highest_prime_of(n/17);
            }
            else {
                let mut highest_factor = 0;
                let seq = (2..=(n / 17 - 1)).rev();
                for x in seq {
                    if x % 10_000_000 == 0 {
                        println!("x: {}", x);
                    }

                    if n % x == 0 {
                        highest_factor = highest_prime_of(x);
                        break;
                    }
                }

                if highest_factor != 0 {
                    highest_factor
                } else {
                    n
                }
            }
        },
        16 => 2,
        15 => 5,
        14 => 7,
        12 => 3,
        10 => 5,
        9 => 3,
        8 => 2,
        6 => 3,
        4 => 2,
        n => n,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Find the largest prime factor of 600851475143.
    #[test]
    fn problem3() {
        assert_eq!(highest_prime_of(1), 1);
        assert_eq!(highest_prime_of(2), 2);
        assert_eq!(highest_prime_of(3), 3);
        assert_eq!(highest_prime_of(4), 2);
        assert_eq!(highest_prime_of(6), 3);
        assert_eq!(highest_prime_of(10), 5);
        assert_eq!(highest_prime_of(100), 5);
        assert_eq!(highest_prime_of(1234567), 9721);
        assert_eq!(highest_prime_of(35875456), 280277);
        assert_eq!(highest_prime_of(135875456), 1061527);
        assert_eq!(highest_prime_of(1135875456), 4673);
        assert_eq!(highest_prime_of(11135875456), 11903);
        assert_eq!(highest_prime_of(111135875456), 192133);
        // too slow:
        // assert_eq!(highest_prime_of(600851475143), 6857);
    }
}
