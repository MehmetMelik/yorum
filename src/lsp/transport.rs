use std::io::{self, BufRead, Write};

/// Read a single JSON-RPC message from an LSP-style stream.
///
/// The protocol uses HTTP-like headers terminated by `\r\n\r\n`,
/// with `Content-Length` specifying the body size in bytes.
pub fn read_message(reader: &mut impl BufRead) -> io::Result<Option<String>> {
    let mut content_length: Option<usize> = None;

    // Read headers until empty line
    loop {
        let mut header = String::new();
        let bytes_read = reader.read_line(&mut header)?;
        if bytes_read == 0 {
            // EOF
            return Ok(None);
        }

        let header = header.trim();
        if header.is_empty() {
            break;
        }

        if let Some(value) = header.strip_prefix("Content-Length:") {
            content_length = value.trim().parse().ok();
        }
        // Ignore other headers (e.g. Content-Type)
    }

    let length = content_length.ok_or_else(|| {
        io::Error::new(io::ErrorKind::InvalidData, "missing Content-Length header")
    })?;

    let mut body = vec![0u8; length];
    reader.read_exact(&mut body)?;

    String::from_utf8(body)
        .map(Some)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

/// Write a JSON-RPC message with the proper Content-Length header.
pub fn write_message(writer: &mut impl Write, body: &str) -> io::Result<()> {
    write!(writer, "Content-Length: {}\r\n\r\n{}", body.len(), body)?;
    writer.flush()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_read_message() {
        let input = "Content-Length: 13\r\n\r\n{\"key\":\"val\"}";
        let mut reader = Cursor::new(input.as_bytes());
        let msg = read_message(&mut reader).unwrap().unwrap();
        assert_eq!(msg, "{\"key\":\"val\"}");
    }

    #[test]
    fn test_read_message_eof() {
        let mut reader = Cursor::new(b"" as &[u8]);
        let msg = read_message(&mut reader).unwrap();
        assert!(msg.is_none());
    }

    #[test]
    fn test_read_message_extra_headers() {
        let input = "Content-Length: 2\r\nContent-Type: application/json\r\n\r\n{}";
        let mut reader = Cursor::new(input.as_bytes());
        let msg = read_message(&mut reader).unwrap().unwrap();
        assert_eq!(msg, "{}");
    }

    #[test]
    fn test_write_message() {
        let mut buf = Vec::new();
        write_message(&mut buf, "{\"id\":1}").unwrap();
        let output = String::from_utf8(buf).unwrap();
        assert_eq!(output, "Content-Length: 8\r\n\r\n{\"id\":1}");
    }

    #[test]
    fn test_roundtrip() {
        let original = r#"{"jsonrpc":"2.0","id":1,"method":"initialize"}"#;
        let mut buf = Vec::new();
        write_message(&mut buf, original).unwrap();

        let mut reader = Cursor::new(buf);
        let msg = read_message(&mut reader).unwrap().unwrap();
        assert_eq!(msg, original);
    }
}
