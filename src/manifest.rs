use serde::Deserialize;
use std::collections::HashMap;

#[derive(Debug, Deserialize)]
pub struct YorumManifest {
    pub package: PackageInfo,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
}

#[derive(Debug, Deserialize)]
pub struct PackageInfo {
    pub name: String,
    pub version: String,
    #[serde(default = "default_src_dir")]
    pub src_dir: String,
}

fn default_src_dir() -> String {
    "src".to_string()
}

impl std::str::FromStr for YorumManifest {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        toml::from_str(s).map_err(|e| format!("failed to parse yorum.toml: {}", e))
    }
}
