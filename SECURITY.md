# Security Policy

## Reporting a Vulnerability

If you discover a security vulnerability in Yorum, please report it responsibly.

**Do not open a public issue.** Instead, reach out via X/Twitter at **[@mehmetmelik](https://x.com/mehmetmelik)** with:

- A description of the vulnerability
- Steps to reproduce the issue
- The potential impact
- Any suggested fixes (optional)

You should receive a response within 7 days. The issue will be assessed and a fix will be prioritized based on severity.

## Scope

Security concerns for Yorum include but are not limited to:

- **Compiler bugs** that produce unsafe LLVM IR (e.g., missing bounds checks, buffer overflows in generated code)
- **Code injection** via malicious input to the compiler or LSP server
- **Denial of service** through crafted source files that cause the compiler to hang or consume excessive resources

## Supported Versions

Security fixes are applied to the latest release on the `main` branch. Older versions are not actively maintained.

## Disclosure

Once a fix is released, the vulnerability will be disclosed publicly with credit to the reporter (unless anonymity is requested).
