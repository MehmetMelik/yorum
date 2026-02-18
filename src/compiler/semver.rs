use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
}

impl Version {
    pub fn parse(s: &str) -> Result<Version, String> {
        let s = s.strip_prefix('v').unwrap_or(s);
        let parts: Vec<&str> = s.split('.').collect();
        if parts.len() != 3 {
            return Err(format!(
                "invalid version '{}': expected MAJOR.MINOR.PATCH",
                s
            ));
        }
        let major = parts[0]
            .parse::<u64>()
            .map_err(|_| format!("invalid version '{}': non-numeric major", s))?;
        let minor = parts[1]
            .parse::<u64>()
            .map_err(|_| format!("invalid version '{}': non-numeric minor", s))?;
        let patch = parts[2]
            .parse::<u64>()
            .map_err(|_| format!("invalid version '{}': non-numeric patch", s))?;
        Ok(Version {
            major,
            minor,
            patch,
        })
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.major
            .cmp(&other.major)
            .then(self.minor.cmp(&other.minor))
            .then(self.patch.cmp(&other.patch))
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_valid() {
        assert_eq!(
            Version::parse("1.0.0").unwrap(),
            Version {
                major: 1,
                minor: 0,
                patch: 0
            }
        );
        assert_eq!(
            Version::parse("v1.2.3").unwrap(),
            Version {
                major: 1,
                minor: 2,
                patch: 3
            }
        );
        assert_eq!(
            Version::parse("0.0.1").unwrap(),
            Version {
                major: 0,
                minor: 0,
                patch: 1
            }
        );
    }

    #[test]
    fn test_parse_invalid() {
        assert!(Version::parse("1.0").is_err());
        assert!(Version::parse("abc").is_err());
        assert!(Version::parse("1.0.0.0").is_err());
    }

    #[test]
    fn test_ordering() {
        let v100 = Version::parse("1.0.0").unwrap();
        let v101 = Version::parse("1.0.1").unwrap();
        let v110 = Version::parse("1.1.0").unwrap();
        let v200 = Version::parse("2.0.0").unwrap();
        assert!(v100 < v101);
        assert!(v101 < v110);
        assert!(v110 < v200);
    }

    #[test]
    fn test_display() {
        let v = Version::parse("v1.2.3").unwrap();
        assert_eq!(v.to_string(), "1.2.3");
    }
}
