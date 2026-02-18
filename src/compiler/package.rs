use crate::manifest::{DependencySpec, YorumManifest};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Clone)]
pub struct ResolvedDep {
    pub name: String,
    pub version: String,
    pub src_dir: PathBuf,
    pub git_commit: Option<String>,
}

pub struct PackageCache {
    cache_dir: PathBuf,
}

impl PackageCache {
    pub fn new() -> Result<Self, String> {
        let home = std::env::var("HOME")
            .or_else(|_| std::env::var("USERPROFILE"))
            .map_err(|_| "cannot determine home directory".to_string())?;
        let cache_dir = PathBuf::from(home).join(".yorum").join("cache");
        std::fs::create_dir_all(&cache_dir)
            .map_err(|e| format!("cannot create cache directory: {}", e))?;
        Ok(PackageCache { cache_dir })
    }

    #[cfg(test)]
    pub fn with_dir(cache_dir: PathBuf) -> Result<Self, String> {
        std::fs::create_dir_all(&cache_dir)
            .map_err(|e| format!("cannot create cache directory: {}", e))?;
        Ok(PackageCache { cache_dir })
    }

    pub fn cache_dir_for(&self, name: &str, url: &str) -> PathBuf {
        let mut hasher = DefaultHasher::new();
        url.hash(&mut hasher);
        let hex = format!("{:016x}", hasher.finish());
        self.cache_dir.join(format!("{}-{}", name, &hex[..8]))
    }

    /// Check out a specific commit in a cached repo without fetching from the remote.
    /// Returns Ok if the cache exists and the SHA matches; Err otherwise (caller should
    /// fall back to a full resolve).
    pub fn checkout_locked(
        &self,
        name: &str,
        url: &str,
        locked_sha: &str,
    ) -> Result<PathBuf, String> {
        let repo_dir = self.cache_dir_for(name, url);
        if !repo_dir.exists() {
            return Err(format!(
                "dependency '{}': cache miss for locked SHA {}",
                name, locked_sha
            ));
        }

        // Check if HEAD already matches the locked SHA (no checkout needed)
        let current_sha = PackageCache::get_git_sha(&repo_dir)?;
        if current_sha == locked_sha {
            return Ok(repo_dir);
        }

        // Try to checkout the locked SHA without fetching
        let output = Command::new("git")
            .args(["checkout", locked_sha])
            .current_dir(&repo_dir)
            .output()
            .map_err(|e| format!("failed to run git: {}", e))?;
        if !output.status.success() {
            return Err(format!(
                "dependency '{}': locked SHA {} not available in cache",
                name, locked_sha
            ));
        }

        Ok(repo_dir)
    }

    pub fn fetch_git(
        &self,
        name: &str,
        url: &str,
        tag: Option<&str>,
        branch: Option<&str>,
        rev: Option<&str>,
    ) -> Result<PathBuf, String> {
        check_git_installed()?;

        let repo_dir = self.cache_dir_for(name, url);

        if repo_dir.exists() {
            // Cache hit — update to desired ref
            self.update_git(&repo_dir, name, tag, branch, rev)?;
        } else if let Some(rev) = rev {
            // Full clone needed for specific revision
            let output = Command::new("git")
                .args(["clone", url, repo_dir.to_str().unwrap()])
                .output()
                .map_err(|e| format!("failed to run git: {}", e))?;
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                return Err(format!(
                    "failed to fetch dependency '{}': git clone failed:\n  {}",
                    name,
                    stderr.trim()
                ));
            }
            let output = Command::new("git")
                .args(["checkout", rev])
                .current_dir(&repo_dir)
                .output()
                .map_err(|e| format!("failed to run git: {}", e))?;
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                return Err(format!(
                    "failed to fetch dependency '{}': git checkout {} failed:\n  {}",
                    name,
                    rev,
                    stderr.trim()
                ));
            }
        } else {
            // Shallow clone with branch/tag
            let ref_arg = tag.or(branch).unwrap_or("main");
            let output = Command::new("git")
                .args([
                    "clone",
                    "--depth",
                    "1",
                    "--branch",
                    ref_arg,
                    url,
                    repo_dir.to_str().unwrap(),
                ])
                .output()
                .map_err(|e| format!("failed to run git: {}", e))?;
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                return Err(format!(
                    "failed to fetch dependency '{}': git clone failed:\n  {}",
                    name,
                    stderr.trim()
                ));
            }
        }

        Ok(repo_dir)
    }

    fn update_git(
        &self,
        repo_dir: &Path,
        name: &str,
        tag: Option<&str>,
        branch: Option<&str>,
        rev: Option<&str>,
    ) -> Result<(), String> {
        // Fetch latest
        let output = Command::new("git")
            .args(["fetch", "origin"])
            .current_dir(repo_dir)
            .output()
            .map_err(|e| format!("failed to run git: {}", e))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!(
                "failed to update dependency '{}': git fetch failed:\n  {}",
                name,
                stderr.trim()
            ));
        }

        // Checkout desired ref
        let checkout_ref = if let Some(rev) = rev {
            rev.to_string()
        } else if let Some(tag) = tag {
            format!("tags/{}", tag)
        } else if let Some(branch) = branch {
            format!("origin/{}", branch)
        } else {
            "origin/main".to_string()
        };

        let output = Command::new("git")
            .args(["checkout", &checkout_ref])
            .current_dir(repo_dir)
            .output()
            .map_err(|e| format!("failed to run git: {}", e))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!(
                "failed to update dependency '{}': git checkout failed:\n  {}",
                name,
                stderr.trim()
            ));
        }

        Ok(())
    }

    pub fn get_git_sha(repo_dir: &Path) -> Result<String, String> {
        let output = Command::new("git")
            .args(["rev-parse", "HEAD"])
            .current_dir(repo_dir)
            .output()
            .map_err(|e| format!("failed to run git: {}", e))?;
        if !output.status.success() {
            return Err("git rev-parse HEAD failed".to_string());
        }
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    }
}

fn check_git_installed() -> Result<(), String> {
    Command::new("git")
        .arg("--version")
        .output()
        .map_err(|_| "git is not installed. Install git to use package dependencies.".to_string())
        .and_then(|o| {
            if o.status.success() {
                Ok(())
            } else {
                Err("git is not installed. Install git to use package dependencies.".to_string())
            }
        })
}

pub fn resolve_path_dep(
    name: &str,
    dep_path: &str,
    project_root: &Path,
) -> Result<PathBuf, String> {
    let resolved = if Path::new(dep_path).is_absolute() {
        PathBuf::from(dep_path)
    } else {
        project_root.join(dep_path)
    };

    let canonical = resolved.canonicalize().map_err(|e| {
        format!(
            "dependency '{}': path '{}' does not exist: {}",
            name, dep_path, e
        )
    })?;

    // Verify it's a valid Yorum package
    if !canonical.join("yorum.toml").exists() {
        return Err(format!(
            "dependency '{}' is not a valid Yorum package (no yorum.toml found at '{}')",
            name,
            canonical.display()
        ));
    }

    Ok(canonical)
}

pub fn read_dep_manifest(dep_dir: &Path) -> Result<YorumManifest, String> {
    let manifest_path = dep_dir.join("yorum.toml");
    let content = std::fs::read_to_string(&manifest_path)
        .map_err(|e| format!("cannot read '{}': {}", manifest_path.display(), e))?;
    content.parse()
}

/// Resolve a dependency using a locked SHA (for reproducible builds).
/// For git deps, checks out the exact locked SHA without fetching.
/// For path deps, behaves identically to `resolve_dep`.
pub fn resolve_dep_locked(
    name: &str,
    spec: &DependencySpec,
    project_root: &Path,
    cache: &PackageCache,
    locked_sha: Option<&str>,
) -> Result<ResolvedDep, String> {
    spec.validate(name)?;

    let (dep_dir, git_commit) = if let Some(ref path) = spec.path {
        let dir = resolve_path_dep(name, path, project_root)?;
        (dir, None)
    } else if let Some(ref url) = spec.git {
        if let Some(sha) = locked_sha {
            match cache.checkout_locked(name, url, sha) {
                Ok(dir) => (dir, Some(sha.to_string())),
                Err(_) => {
                    // Cache miss or SHA not available — fall back to full fetch,
                    // then verify we got the expected SHA
                    let dir = cache.fetch_git(
                        name,
                        url,
                        spec.tag.as_deref(),
                        spec.branch.as_deref(),
                        Some(sha),
                    )?;
                    (dir, Some(sha.to_string()))
                }
            }
        } else {
            return resolve_dep(name, spec, project_root, cache);
        }
    } else {
        return Err(format!(
            "dependency '{}': must specify either 'git' or 'path'",
            name
        ));
    };

    let dep_manifest = read_dep_manifest(&dep_dir)?;

    if dep_manifest.package.name != name {
        return Err(format!(
            "dependency '{}': package name in yorum.toml is '{}', expected '{}'",
            name, dep_manifest.package.name, name
        ));
    }

    let src_dir = dep_dir.join(&dep_manifest.package.src_dir);
    if !src_dir.exists() {
        return Err(format!(
            "dependency '{}': source directory '{}' does not exist",
            name,
            src_dir.display()
        ));
    }

    Ok(ResolvedDep {
        name: name.to_string(),
        version: dep_manifest.package.version,
        src_dir,
        git_commit,
    })
}

pub fn resolve_dep(
    name: &str,
    spec: &DependencySpec,
    project_root: &Path,
    cache: &PackageCache,
) -> Result<ResolvedDep, String> {
    spec.validate(name)?;

    let (dep_dir, git_commit) = if let Some(ref path) = spec.path {
        let dir = resolve_path_dep(name, path, project_root)?;
        (dir, None)
    } else if let Some(ref url) = spec.git {
        let dir = cache.fetch_git(
            name,
            url,
            spec.tag.as_deref(),
            spec.branch.as_deref(),
            spec.rev.as_deref(),
        )?;
        let sha = PackageCache::get_git_sha(&dir)?;
        (dir, Some(sha))
    } else {
        return Err(format!(
            "dependency '{}': must specify either 'git' or 'path'",
            name
        ));
    };

    let dep_manifest = read_dep_manifest(&dep_dir)?;

    // Validate that the package name in yorum.toml matches the dependency key
    if dep_manifest.package.name != name {
        return Err(format!(
            "dependency '{}': package name in yorum.toml is '{}', expected '{}'",
            name, dep_manifest.package.name, name
        ));
    }

    let src_dir = dep_dir.join(&dep_manifest.package.src_dir);
    if !src_dir.exists() {
        return Err(format!(
            "dependency '{}': source directory '{}' does not exist",
            name,
            src_dir.display()
        ));
    }

    Ok(ResolvedDep {
        name: name.to_string(),
        version: dep_manifest.package.version,
        src_dir,
        git_commit,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_dir_computation() {
        let cache = PackageCache::with_dir(std::env::temp_dir().join("yorum_test_cache")).unwrap();
        let dir1 = cache.cache_dir_for("math", "https://example.com/math.git");
        let dir2 = cache.cache_dir_for("math", "https://example.com/math.git");
        let dir3 = cache.cache_dir_for("math", "https://other.com/math.git");

        // Same name+url → same dir
        assert_eq!(dir1, dir2);
        // Different url → different dir
        assert_ne!(dir1, dir3);
        // Contains the name
        assert!(dir1
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .starts_with("math-"));
    }

    #[test]
    fn test_path_resolution() {
        let dir = std::env::temp_dir().join("yorum_test_path_dep");
        let lib_dir = dir.join("my_lib");
        let _ = std::fs::create_dir_all(&lib_dir);
        std::fs::write(
            lib_dir.join("yorum.toml"),
            "[package]\nname = \"my_lib\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let result = resolve_path_dep("my_lib", "my_lib", &dir);
        assert!(result.is_ok());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_dep_missing_manifest() {
        let dir = std::env::temp_dir().join("yorum_test_no_manifest");
        let lib_dir = dir.join("no_manifest_lib");
        let _ = std::fs::create_dir_all(&lib_dir);

        let result = resolve_path_dep("no_manifest_lib", "no_manifest_lib", &dir);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not a valid Yorum package"));

        let _ = std::fs::remove_dir_all(&dir);
    }
}
