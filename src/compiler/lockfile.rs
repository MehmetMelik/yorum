use serde::{Deserialize, Serialize};
use std::path::Path;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LockFile {
    #[serde(rename = "package", default)]
    pub packages: Vec<LockedPackage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedPackage {
    pub name: String,
    pub version: String,
    pub source: String,
}

impl LockedPackage {
    pub fn git_source(url: &str, sha: &str) -> String {
        format!("git+{}#{}", url, sha)
    }

    pub fn path_source(path: &str) -> String {
        format!("path+{}", path)
    }

    pub fn parse_git_sha(&self) -> Option<&str> {
        self.source
            .strip_prefix("git+")
            .and_then(|rest| rest.split('#').nth(1))
    }
}

impl LockFile {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn find_package(&self, name: &str) -> Option<&LockedPackage> {
        self.packages.iter().find(|p| p.name == name)
    }

    pub fn read(path: &Path) -> Result<LockFile, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| format!("cannot read '{}': {}", path.display(), e))?;
        toml::from_str(&content).map_err(|e| format!("failed to parse yorum.lock: {}", e))
    }

    pub fn write(&self, path: &Path) -> Result<(), String> {
        let content = toml::to_string_pretty(self)
            .map_err(|e| format!("failed to serialize yorum.lock: {}", e))?;
        std::fs::write(path, content)
            .map_err(|e| format!("cannot write '{}': {}", path.display(), e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_round_trip() {
        let lock = LockFile {
            packages: vec![
                LockedPackage {
                    name: "math_utils".to_string(),
                    version: "1.0.0".to_string(),
                    source: LockedPackage::git_source(
                        "https://github.com/user/math_utils.git",
                        "a1b2c3d",
                    ),
                },
                LockedPackage {
                    name: "local_lib".to_string(),
                    version: "0.2.0".to_string(),
                    source: LockedPackage::path_source("../my_lib"),
                },
            ],
        };

        let dir = std::env::temp_dir().join("yorum_lockfile_test");
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("yorum.lock");

        lock.write(&path).unwrap();
        let loaded = LockFile::read(&path).unwrap();

        assert_eq!(loaded.packages.len(), 2);
        assert_eq!(loaded.packages[0].name, "math_utils");
        assert_eq!(loaded.packages[1].name, "local_lib");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_parse_git_source() {
        let pkg = LockedPackage {
            name: "foo".to_string(),
            version: "1.0.0".to_string(),
            source: "git+https://example.com/foo.git#abc123def".to_string(),
        };
        assert_eq!(pkg.parse_git_sha(), Some("abc123def"));
    }

    #[test]
    fn test_find_package() {
        let lock = LockFile {
            packages: vec![
                LockedPackage {
                    name: "alpha".to_string(),
                    version: "1.0.0".to_string(),
                    source: "path+../alpha".to_string(),
                },
                LockedPackage {
                    name: "beta".to_string(),
                    version: "2.0.0".to_string(),
                    source: "path+../beta".to_string(),
                },
            ],
        };
        assert!(lock.find_package("alpha").is_some());
        assert!(lock.find_package("beta").is_some());
        assert!(lock.find_package("gamma").is_none());
    }
}
