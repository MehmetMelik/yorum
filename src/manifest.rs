use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct YorumManifest {
    pub package: PackageInfo,
    #[serde(default)]
    pub dependencies: HashMap<String, DependencySpec>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PackageInfo {
    pub name: String,
    pub version: String,
    #[serde(default = "default_src_dir")]
    pub src_dir: String,
}

fn default_src_dir() -> String {
    "src".to_string()
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct DependencySpec {
    pub git: Option<String>,
    pub tag: Option<String>,
    pub branch: Option<String>,
    pub rev: Option<String>,
    pub path: Option<String>,
}

impl DependencySpec {
    pub fn validate(&self, name: &str) -> Result<(), String> {
        let has_git = self.git.is_some();
        let has_path = self.path.is_some();

        if has_git && has_path {
            return Err(format!(
                "dependency '{}': cannot specify both 'git' and 'path'",
                name
            ));
        }
        if !has_git && !has_path {
            return Err(format!(
                "dependency '{}': must specify either 'git' or 'path'",
                name
            ));
        }

        if has_path && (self.tag.is_some() || self.branch.is_some() || self.rev.is_some()) {
            return Err(format!(
                "dependency '{}': 'tag', 'branch', and 'rev' are not allowed for path dependencies",
                name
            ));
        }

        if has_git {
            let ref_count = [
                self.tag.is_some(),
                self.branch.is_some(),
                self.rev.is_some(),
            ]
            .iter()
            .filter(|&&x| x)
            .count();
            if ref_count > 1 {
                return Err(format!(
                    "dependency '{}': specify at most one of 'tag', 'branch', or 'rev'",
                    name
                ));
            }
        }

        Ok(())
    }
}

impl std::str::FromStr for YorumManifest {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        toml::from_str(s).map_err(|e| format!("failed to parse yorum.toml: {}", e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deserialize_git_dep() {
        let toml_str = r#"
[package]
name = "test"
version = "0.1.0"

[dependencies]
math_utils = { git = "https://github.com/user/math_utils.git", tag = "v1.0.0" }
"#;
        let manifest: YorumManifest = toml_str.parse().unwrap();
        let dep = &manifest.dependencies["math_utils"];
        assert_eq!(
            dep.git.as_deref(),
            Some("https://github.com/user/math_utils.git")
        );
        assert_eq!(dep.tag.as_deref(), Some("v1.0.0"));
        assert!(dep.path.is_none());
        dep.validate("math_utils").unwrap();
    }

    #[test]
    fn test_deserialize_path_dep() {
        let toml_str = r#"
[package]
name = "test"
version = "0.1.0"

[dependencies]
local_lib = { path = "../my_lib" }
"#;
        let manifest: YorumManifest = toml_str.parse().unwrap();
        let dep = &manifest.dependencies["local_lib"];
        assert_eq!(dep.path.as_deref(), Some("../my_lib"));
        assert!(dep.git.is_none());
        dep.validate("local_lib").unwrap();
    }

    #[test]
    fn test_deserialize_no_deps() {
        let toml_str = r#"
[package]
name = "test"
version = "0.1.0"
"#;
        let manifest: YorumManifest = toml_str.parse().unwrap();
        assert!(manifest.dependencies.is_empty());
    }

    #[test]
    fn test_deserialize_mixed_deps() {
        let toml_str = r#"
[package]
name = "test"
version = "0.1.0"

[dependencies]
remote = { git = "https://example.com/remote.git", branch = "develop" }
local = { path = "./libs/local" }
"#;
        let manifest: YorumManifest = toml_str.parse().unwrap();
        assert_eq!(manifest.dependencies.len(), 2);
        manifest.dependencies["remote"].validate("remote").unwrap();
        manifest.dependencies["local"].validate("local").unwrap();
    }

    #[test]
    fn test_validate_rejects_both_git_and_path() {
        let spec = DependencySpec {
            git: Some("https://example.com/repo.git".to_string()),
            path: Some("../lib".to_string()),
            tag: None,
            branch: None,
            rev: None,
        };
        assert!(spec.validate("bad").is_err());
        assert!(spec
            .validate("bad")
            .unwrap_err()
            .contains("cannot specify both"));
    }

    #[test]
    fn test_validate_rejects_neither_git_nor_path() {
        let spec = DependencySpec {
            git: None,
            path: None,
            tag: None,
            branch: None,
            rev: None,
        };
        assert!(spec.validate("bad").is_err());
        assert!(spec
            .validate("bad")
            .unwrap_err()
            .contains("must specify either"));
    }

    #[test]
    fn test_validate_rejects_multiple_refs() {
        let spec = DependencySpec {
            git: Some("https://example.com/repo.git".to_string()),
            path: None,
            tag: Some("v1.0".to_string()),
            branch: Some("main".to_string()),
            rev: None,
        };
        assert!(spec.validate("bad").is_err());
        assert!(spec.validate("bad").unwrap_err().contains("at most one"));
    }

    #[test]
    fn test_validate_rejects_ref_on_path_dep() {
        let spec = DependencySpec {
            git: None,
            path: Some("../lib".to_string()),
            tag: Some("v1.0".to_string()),
            branch: None,
            rev: None,
        };
        assert!(spec.validate("bad").is_err());
        assert!(spec
            .validate("bad")
            .unwrap_err()
            .contains("not allowed for path"));
    }
}
