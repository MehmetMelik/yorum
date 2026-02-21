use super::*;

impl Codegen {
    pub(crate) fn emit_builtin_decls(&mut self) {
        self.globals.push_str("; ── External declarations ──\n");
        self.globals.push_str("declare i32 @printf(ptr, ...)\n");
        self.globals.push_str("declare i32 @puts(ptr)\n");
        self.globals.push_str("declare ptr @malloc(i64)\n");
        self.globals.push_str("declare void @free(ptr)\n");
        self.globals.push_str("declare void @abort()\n");
        self.globals.push_str("declare i64 @strlen(ptr)\n");
        self.globals.push_str("declare ptr @strcpy(ptr, ptr)\n");
        self.globals.push_str("declare ptr @strcat(ptr, ptr)\n");
        self.globals.push_str("declare i32 @strcmp(ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_create(ptr, ptr, ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_join(i64, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_mutex_init(ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_mutex_lock(ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_mutex_unlock(ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_mutex_destroy(ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_cond_init(ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_cond_wait(ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_cond_signal(ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_cond_destroy(ptr)\n");
        self.globals
            .push_str("declare ptr @memcpy(ptr, ptr, i64)\n");
        self.globals
            .push_str("declare i32 @snprintf(ptr, i64, ptr, ...)\n");
        self.globals.push_str("declare i64 @atol(ptr)\n");
        self.globals.push_str("declare i32 @putchar(i32)\n");
        self.globals.push_str("declare ptr @realloc(ptr, i64)\n");
        self.globals.push_str("declare ptr @fopen(ptr, ptr)\n");
        self.globals.push_str("declare i32 @fclose(ptr)\n");
        self.globals.push_str("declare i32 @fseek(ptr, i64, i32)\n");
        self.globals.push_str("declare i64 @ftell(ptr)\n");
        self.globals
            .push_str("declare i64 @fread(ptr, i64, i64, ptr)\n");
        self.globals
            .push_str("declare i64 @fwrite(ptr, i64, i64, ptr)\n");
        self.globals.push_str("declare i64 @write(i32, ptr, i64)\n");
        self.globals.push_str("declare void @exit(i32)\n");
        self.globals
            .push_str("declare ptr @memset(ptr, i32, i64)\n");
        // Math intrinsics/libm
        self.globals
            .push_str("declare double @llvm.fabs.f64(double)\n");
        self.globals
            .push_str("declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64)\n");
        self.globals
            .push_str("declare double @llvm.sqrt.f64(double)\n");
        self.globals
            .push_str("declare double @llvm.pow.f64(double, double)\n");
        self.globals
            .push_str("declare double @llvm.floor.f64(double)\n");
        self.globals
            .push_str("declare double @llvm.ceil.f64(double)\n");
        self.globals
            .push_str("declare double @llvm.round.f64(double)\n");
        self.globals
            .push_str("declare double @llvm.minnum.f64(double, double)\n");
        self.globals
            .push_str("declare double @llvm.maxnum.f64(double, double)\n");
        self.globals.push_str("declare double @sin(double)\n");
        self.globals.push_str("declare double @cos(double)\n");
        self.globals.push_str("declare double @tan(double)\n");
        self.globals.push_str("declare double @log(double)\n");
        self.globals.push_str("declare double @log10(double)\n");
        self.globals.push_str("declare double @exp(double)\n");
        // String utility externals
        self.globals.push_str("declare ptr @strstr(ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @strncmp(ptr, ptr, i64)\n");
        // Enhanced I/O externals
        self.globals.push_str("declare i32 @access(ptr, i32)\n");
        self.globals.push_str("declare ptr @getenv(ptr)\n");
        self.globals.push_str("declare i32 @fflush(ptr)\n");
        self.globals.push_str("declare ptr @fgets(ptr, i32, ptr)\n");
        self.globals
            .push_str("declare i32 @gettimeofday(ptr, ptr)\n");
        if cfg!(target_os = "macos") {
            self.globals.push_str("@__stdinp = external global ptr\n");
        } else {
            self.globals.push_str("@stdin = external global ptr\n");
        }
        // Networking externals
        self.globals
            .push_str("declare i32 @socket(i32, i32, i32)\n");
        self.globals
            .push_str("declare i32 @connect(i32, ptr, i32)\n");
        self.globals.push_str("declare i32 @bind(i32, ptr, i32)\n");
        self.globals.push_str("declare i32 @listen(i32, i32)\n");
        self.globals
            .push_str("declare i32 @accept(i32, ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @setsockopt(i32, i32, i32, ptr, i32)\n");
        self.globals.push_str("declare i32 @close(i32)\n");
        self.globals.push_str("declare i64 @read(i32, ptr, i64)\n");
        self.globals
            .push_str("declare i32 @getaddrinfo(ptr, ptr, ptr, ptr)\n");
        self.globals.push_str("declare void @freeaddrinfo(ptr)\n");
        self.globals
            .push_str("declare ptr @inet_ntop(i32, ptr, ptr, i32)\n");
        self.globals
            .push_str("declare i64 @sendto(i32, ptr, i64, i32, ptr, i32)\n");
        self.globals
            .push_str("declare i64 @recvfrom(i32, ptr, i64, i32, ptr, ptr)\n\n");
        // Globals for command-line arguments (stored by main)
        self.globals
            .push_str("@__yorum_argc = internal global i32 0\n");
        self.globals
            .push_str("@__yorum_argv = internal global ptr null\n\n");

        // Format strings (%lld\n\0 = 6 bytes, %f\n\0 = 4 bytes)
        self.globals
            .push_str("@.fmt.int = private unnamed_addr constant [6 x i8] c\"%lld\\0A\\00\"\n");
        self.globals
            .push_str("@.fmt.float = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\"\n");
        self.globals
            .push_str("@.fmt.true = private unnamed_addr constant [6 x i8] c\"true\\0A\\00\"\n");
        self.globals
            .push_str("@.fmt.false = private unnamed_addr constant [7 x i8] c\"false\\0A\\00\"\n");
        self.globals.push_str(
            "@.fmt.bounds = private unnamed_addr constant [40 x i8] c\"array index out of bounds: %lld >= %lld\\00\"\n",
        );
        self.globals.push_str(
            "@.fmt.contract = private unnamed_addr constant [24 x i8] c\"contract violation: %s\\0A\\00\"\n",
        );
        self.globals
            .push_str("@.fmt.lld = private unnamed_addr constant [5 x i8] c\"%lld\\00\"\n");
        self.globals.push_str(
            "@.fmt.pop_empty = private unnamed_addr constant [22 x i8] c\"pop from empty array\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.fmt.map_key = private unnamed_addr constant [25 x i8] c\"map key not found: '%s'\\0A\\00\"\n",
        );
        self.globals
            .push_str("@.str.r = private unnamed_addr constant [2 x i8] c\"r\\00\"\n");
        self.globals
            .push_str("@.str.w = private unnamed_addr constant [2 x i8] c\"w\\00\"\n");
        self.globals
            .push_str("@.str.newline = private unnamed_addr constant [1 x i8] c\"\\0A\"\n");
        self.globals
            .push_str("@.str.a = private unnamed_addr constant [2 x i8] c\"a\\00\"\n");
        self.globals
            .push_str("@.fmt.str = private unnamed_addr constant [3 x i8] c\"%s\\00\"\n");
        // Networking constants
        self.globals
            .push_str("@.str.empty = private unnamed_addr constant [1 x i8] c\"\\00\"\n");
        // String conversion format strings
        self.globals
            .push_str("@.fmt.g = private unnamed_addr constant [3 x i8] c\"%g\\00\"\n");
        self.globals
            .push_str("@.str.true = private unnamed_addr constant [5 x i8] c\"true\\00\"\n");
        self.globals
            .push_str("@.str.false = private unnamed_addr constant [6 x i8] c\"false\\00\"\n");
        self.globals.push_str(
            "@.str.http_req_line = private unnamed_addr constant [15 x i8] c\"%s %s HTTP/1.0\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_host_hdr = private unnamed_addr constant [30 x i8] c\"\\0D\\0AHost: %s\\0D\\0AConnection: close\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_cl_hdr = private unnamed_addr constant [23 x i8] c\"\\0D\\0AContent-Length: %lld\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_crlf2 = private unnamed_addr constant [5 x i8] c\"\\0D\\0A\\0D\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_crlf = private unnamed_addr constant [3 x i8] c\"\\0D\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_sep = private unnamed_addr constant [5 x i8] c\"\\0D\\0A\\0D\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_get_method = private unnamed_addr constant [4 x i8] c\"GET\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_post_method = private unnamed_addr constant [5 x i8] c\"POST\\00\"\n",
        );
        self.globals.push('\n');
    }

    pub(crate) fn emit_builtin_helpers(&mut self) {
        self.emit_builtin_io_helpers();
        self.emit_builtin_concurrency_helpers();
        self.emit_builtin_conversion_helpers();
        self.emit_builtin_map_core_helpers();
        self.emit_builtin_math_helpers();
        self.emit_builtin_string_helpers();
        self.emit_builtin_collection_helpers();
    }

    /// Print, bounds check, string basics (len/concat/eq), contract fail.
    pub(crate) fn emit_builtin_io_helpers(&mut self) {
        // print_int
        self.body.push_str(
            "define void @print_int(i64 %x) {\n\
             entry:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.int, i64 %x)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // print_float
        self.body.push_str(
            "define void @print_float(double %x) {\n\
             entry:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.float, double %x)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // print_bool
        self.body.push_str(
            "define void @print_bool(i1 %x) {\n\
             entry:\n\
             \x20 br i1 %x, label %is_true, label %is_false\n\
             is_true:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.true)\n\
             \x20 ret void\n\
             is_false:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.false)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // print_str — prints a global string pointer
        self.body.push_str(
            "define void @print_str(ptr %s) {\n\
             entry:\n\
             \x20 call i32 @puts(ptr %s)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // __yorum_bounds_check — aborts on out-of-bounds access
        self.body.push_str(
            "define void @__yorum_bounds_check(i64 %idx, i64 %len) {\n\
             entry:\n\
             \x20 %neg = icmp slt i64 %idx, 0\n\
             \x20 br i1 %neg, label %fail, label %check_upper\n\
             check_upper:\n\
             \x20 %oob = icmp sge i64 %idx, %len\n\
             \x20 br i1 %oob, label %fail, label %ok\n\
             fail:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.bounds, i64 %idx, i64 %len)\n\
             \x20 call void @abort()\n\
             \x20 unreachable\n\
             ok:\n\
             \x20 ret void\n\
             }\n\n",
        );
        // str_len — returns length of a string
        self.body.push_str(
            "define i64 @str_len(ptr %s) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 ret i64 %len\n\
             }\n\n",
        );
        // str_concat — concatenates two strings into a new heap-allocated string
        // Uses memcpy instead of strcpy/strcat to avoid redundant strlen scans
        self.body.push_str(
            "define ptr @str_concat(ptr %a, ptr %b) {\n\
             entry:\n\
             \x20 %la = call i64 @strlen(ptr %a)\n\
             \x20 %lb = call i64 @strlen(ptr %b)\n\
             \x20 %sum = add i64 %la, %lb\n\
             \x20 %total = add i64 %sum, 1\n\
             \x20 %buf = call ptr @malloc(i64 %total)\n\
             \x20 call ptr @memcpy(ptr %buf, ptr %a, i64 %la)\n\
             \x20 %dest = getelementptr i8, ptr %buf, i64 %la\n\
             \x20 call ptr @memcpy(ptr %dest, ptr %b, i64 %lb)\n\
             \x20 %end = getelementptr i8, ptr %buf, i64 %sum\n\
             \x20 store i8 0, ptr %end\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_eq — compares two strings for equality
        self.body.push_str(
            "define i1 @str_eq(ptr %a, ptr %b) {\n\
             entry:\n\
             \x20 %cmp = call i32 @strcmp(ptr %a, ptr %b)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n\
             \x20 ret i1 %eq\n\
             }\n\n",
        );
        // __yorum_contract_fail — prints error message and aborts
        self.body.push_str(
            "define void @__yorum_contract_fail(ptr %msg) {\n\
             entry:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.contract, ptr %msg)\n\
             \x20 call void @abort()\n\
             \x20 unreachable\n\
             }\n\n",
        );
    }

    /// Channel creation, send, and receive.
    pub(crate) fn emit_builtin_concurrency_helpers(&mut self) {
        // __yorum_chan_create — creates a channel (mutex + condvar + value slot + ready flag)
        // Channel layout: { mutex (64 bytes), condvar (64 bytes), value (8 bytes), ready (i32) }
        // Total: 144 bytes (padded)
        self.body.push_str(
            "define ptr @__yorum_chan_create() {\n\
             entry:\n\
             \x20 %ch = call ptr @malloc(i64 144)\n\
             \x20 call i32 @pthread_mutex_init(ptr %ch, ptr null)\n\
             \x20 %cond = getelementptr i8, ptr %ch, i64 64\n\
             \x20 call i32 @pthread_cond_init(ptr %cond, ptr null)\n\
             \x20 %ready = getelementptr i8, ptr %ch, i64 136\n\
             \x20 store i32 0, ptr %ready\n\
             \x20 ret ptr %ch\n\
             }\n\n",
        );
        // __yorum_chan_send — waits until slot is empty, stores value, and signals
        self.body.push_str(
            "define void @__yorum_chan_send(ptr %ch, i64 %val) {\n\
             entry:\n\
             \x20 call i32 @pthread_mutex_lock(ptr %ch)\n\
             \x20 br label %send_wait\n\
             send_wait:\n\
             \x20 %ready = getelementptr i8, ptr %ch, i64 136\n\
             \x20 %r = load i32, ptr %ready\n\
             \x20 %is_full = icmp eq i32 %r, 1\n\
             \x20 br i1 %is_full, label %send_do_wait, label %send_ready\n\
             send_do_wait:\n\
             \x20 %cond0 = getelementptr i8, ptr %ch, i64 64\n\
             \x20 call i32 @pthread_cond_wait(ptr %cond0, ptr %ch)\n\
             \x20 br label %send_wait\n\
             send_ready:\n\
             \x20 %slot = getelementptr i8, ptr %ch, i64 128\n\
             \x20 store i64 %val, ptr %slot\n\
             \x20 %ready2 = getelementptr i8, ptr %ch, i64 136\n\
             \x20 store i32 1, ptr %ready2\n\
             \x20 %cond = getelementptr i8, ptr %ch, i64 64\n\
             \x20 call i32 @pthread_cond_signal(ptr %cond)\n\
             \x20 call i32 @pthread_mutex_unlock(ptr %ch)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // __yorum_chan_recv — waits for value and returns it
        self.body.push_str(
            "define i64 @__yorum_chan_recv(ptr %ch) {\n\
             entry:\n\
             \x20 call i32 @pthread_mutex_lock(ptr %ch)\n\
             \x20 br label %wait_loop\n\
             wait_loop:\n\
             \x20 %ready = getelementptr i8, ptr %ch, i64 136\n\
             \x20 %r = load i32, ptr %ready\n\
             \x20 %is_ready = icmp eq i32 %r, 1\n\
             \x20 br i1 %is_ready, label %done, label %do_wait\n\
             do_wait:\n\
             \x20 %cond = getelementptr i8, ptr %ch, i64 64\n\
             \x20 call i32 @pthread_cond_wait(ptr %cond, ptr %ch)\n\
             \x20 br label %wait_loop\n\
             done:\n\
             \x20 %slot = getelementptr i8, ptr %ch, i64 128\n\
             \x20 %val = load i64, ptr %slot\n\
             \x20 store i32 0, ptr %ready\n\
             \x20 %cond2 = getelementptr i8, ptr %ch, i64 64\n\
             \x20 call i32 @pthread_cond_signal(ptr %cond2)\n\
             \x20 call i32 @pthread_mutex_unlock(ptr %ch)\n\
             \x20 ret i64 %val\n\
             }\n\n",
        );
    }

    /// Type conversions (char/int/float/str), string indexing, char classification, file I/O.
    pub(crate) fn emit_builtin_conversion_helpers(&mut self) {
        // print_char — prints a single character
        self.body.push_str(
            "define void @print_char(i8 %c) {\n\
             entry:\n\
             \x20 %ext = zext i8 %c to i32\n\
             \x20 call i32 @putchar(i32 %ext)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // char_to_int — zero-extends i8 to i64
        self.body.push_str(
            "define i64 @char_to_int(i8 %c) {\n\
             entry:\n\
             \x20 %ext = zext i8 %c to i64\n\
             \x20 ret i64 %ext\n\
             }\n\n",
        );
        // int_to_char — truncates i64 to i8
        self.body.push_str(
            "define i8 @int_to_char(i64 %n) {\n\
             entry:\n\
             \x20 %trunc = trunc i64 %n to i8\n\
             \x20 ret i8 %trunc\n\
             }\n\n",
        );
        // int_to_float — converts i64 to double
        self.body.push_str(
            "define double @int_to_float(i64 %n) {\n\
             entry:\n\
             \x20 %f = sitofp i64 %n to double\n\
             \x20 ret double %f\n\
             }\n\n",
        );
        // float_to_int — converts double to i64
        self.body.push_str(
            "define i64 @float_to_int(double %f) {\n\
             entry:\n\
             \x20 %n = fptosi double %f to i64\n\
             \x20 ret i64 %n\n\
             }\n\n",
        );
        // int_to_str — converts i64 to heap-allocated decimal string
        self.body.push_str(
            "define ptr @int_to_str(i64 %n) {\n\
             entry:\n\
             \x20 %buf = call ptr @malloc(i64 24)\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %buf, i64 24, ptr @.fmt.lld, i64 %n)\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // float_to_str — converts double to heap-allocated string using %g
        self.body.push_str(
            "define ptr @float_to_str(double %n) {\n\
             entry:\n\
             \x20 %buf = call ptr @malloc(i64 32)\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %buf, i64 32, ptr @.fmt.g, double %n)\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // bool_to_str — converts i1 to "true" or "false"
        self.body.push_str(
            "define ptr @bool_to_str(i1 %b) {\n\
             entry:\n\
             \x20 br i1 %b, label %is.true, label %is.false\n\
             is.true:\n\
             \x20 ret ptr @.str.true\n\
             is.false:\n\
             \x20 ret ptr @.str.false\n\
             }\n\n",
        );
        // str_to_int — parses string to i64
        self.body.push_str(
            "define i64 @str_to_int(ptr %s) {\n\
             entry:\n\
             \x20 %n = call i64 @atol(ptr %s)\n\
             \x20 ret i64 %n\n\
             }\n\n",
        );
        // str_charAt — index into string with bounds check, returns i8
        self.body.push_str(
            "define i8 @str_charAt(ptr %s, i64 %i) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 call void @__yorum_bounds_check(i64 %i, i64 %len)\n\
             \x20 %ptr = getelementptr i8, ptr %s, i64 %i\n\
             \x20 %c = load i8, ptr %ptr\n\
             \x20 ret i8 %c\n\
             }\n\n",
        );
        // str_sub — extract substring: str_sub(s, start, len) -> string
        self.body.push_str(
            "define ptr @str_sub(ptr %s, i64 %start, i64 %len) {\n\
             entry:\n\
             \x20 %total = add i64 %len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %total)\n\
             \x20 %src = getelementptr i8, ptr %s, i64 %start\n\
             \x20 call ptr @memcpy(ptr %buf, ptr %src, i64 %len)\n\
             \x20 %end = getelementptr i8, ptr %buf, i64 %len\n\
             \x20 store i8 0, ptr %end\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_from_char — convert a char to a single-character string
        self.body.push_str(
            "define ptr @str_from_char(i8 %c) {\n\
             entry:\n\
             \x20 %buf = call ptr @malloc(i64 2)\n\
             \x20 store i8 %c, ptr %buf\n\
             \x20 %end = getelementptr i8, ptr %buf, i64 1\n\
             \x20 store i8 0, ptr %end\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // char_is_alpha — returns true if c is a-z or A-Z
        self.body.push_str(
            "define i1 @char_is_alpha(i8 %c) {\n\
             entry:\n\
             \x20 %ge_a = icmp sge i8 %c, 97\n\
             \x20 %le_z = icmp sle i8 %c, 122\n\
             \x20 %lower = and i1 %ge_a, %le_z\n\
             \x20 %ge_A = icmp sge i8 %c, 65\n\
             \x20 %le_Z = icmp sle i8 %c, 90\n\
             \x20 %upper = and i1 %ge_A, %le_Z\n\
             \x20 %result = or i1 %lower, %upper\n\
             \x20 ret i1 %result\n\
             }\n\n",
        );
        // char_is_digit — returns true if c is 0-9
        self.body.push_str(
            "define i1 @char_is_digit(i8 %c) {\n\
             entry:\n\
             \x20 %ge_0 = icmp sge i8 %c, 48\n\
             \x20 %le_9 = icmp sle i8 %c, 57\n\
             \x20 %result = and i1 %ge_0, %le_9\n\
             \x20 ret i1 %result\n\
             }\n\n",
        );
        // char_is_whitespace — returns true if c is space, tab, newline, or carriage return
        self.body.push_str(
            "define i1 @char_is_whitespace(i8 %c) {\n\
             entry:\n\
             \x20 %is_space = icmp eq i8 %c, 32\n\
             \x20 %is_tab = icmp eq i8 %c, 9\n\
             \x20 %is_nl = icmp eq i8 %c, 10\n\
             \x20 %is_cr = icmp eq i8 %c, 13\n\
             \x20 %or1 = or i1 %is_space, %is_tab\n\
             \x20 %or2 = or i1 %or1, %is_nl\n\
             \x20 %result = or i1 %or2, %is_cr\n\
             \x20 ret i1 %result\n\
             }\n\n",
        );
        // file_read — reads entire file into a heap-allocated string
        self.body.push_str(
            "define ptr @file_read(ptr %path) {\n\
             entry:\n\
             \x20 %f = call ptr @fopen(ptr %path, ptr @.str.r)\n\
             \x20 %is_null = icmp eq ptr %f, null\n\
             \x20 br i1 %is_null, label %fail, label %opened\n\
             fail:\n\
             \x20 %empty = call ptr @malloc(i64 1)\n\
             \x20 store i8 0, ptr %empty\n\
             \x20 ret ptr %empty\n\
             opened:\n\
             \x20 call i32 @fseek(ptr %f, i64 0, i32 2)\n\
             \x20 %size = call i64 @ftell(ptr %f)\n\
             \x20 call i32 @fseek(ptr %f, i64 0, i32 0)\n\
             \x20 %buf_size = add i64 %size, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_size)\n\
             \x20 call i64 @fread(ptr %buf, i64 1, i64 %size, ptr %f)\n\
             \x20 %end = getelementptr i8, ptr %buf, i64 %size\n\
             \x20 store i8 0, ptr %end\n\
             \x20 call i32 @fclose(ptr %f)\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // file_write — writes string to file, returns true on success
        self.body.push_str(
            "define i1 @file_write(ptr %path, ptr %content) {\n\
             entry:\n\
             \x20 %f = call ptr @fopen(ptr %path, ptr @.str.w)\n\
             \x20 %is_null = icmp eq ptr %f, null\n\
             \x20 br i1 %is_null, label %fail, label %opened\n\
             fail:\n\
             \x20 ret i1 0\n\
             opened:\n\
             \x20 %len = call i64 @strlen(ptr %content)\n\
             \x20 %written = call i64 @fwrite(ptr %content, i64 1, i64 %len, ptr %f)\n\
             \x20 call i32 @fclose(ptr %f)\n\
             \x20 %ok = icmp eq i64 %written, %len\n\
             \x20 ret i1 %ok\n\
             }\n\n",
        );
        // print_err — writes string to stderr (fd 2) with newline
        self.body.push_str(
            "define void @print_err(ptr %s) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 call i64 @write(i32 2, ptr %s, i64 %len)\n\
             \x20 call i64 @write(i32 2, ptr @.str.newline, i64 1)\n\
             \x20 ret void\n\
             }\n\n",
        );
    }

    /// HashMap core: hash, find_slot, grow, new, set, get, has.
    pub(crate) fn emit_builtin_map_core_helpers(&mut self) {
        // ── HashMap helpers ──
        // Map struct layout (40 bytes):
        //   offset 0:  ptr keys     (array of ptr to C strings)
        //   offset 8:  ptr values   (array of i64)
        //   offset 16: ptr flags    (array of i8: 0=empty, 1=occupied)
        //   offset 24: i64 capacity
        //   offset 32: i64 size

        // __yorum_hash_string — FNV-1a hash
        self.body.push_str(
            "define i64 @__yorum_hash_string(ptr %s) {\n\
             entry:\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cont ]\n\
             \x20 %h = phi i64 [ -3750763034362895579, %entry ], [ %h3, %cont ]\n\
             \x20 %cp = getelementptr i8, ptr %s, i64 %i\n\
             \x20 %c = load i8, ptr %cp\n\
             \x20 %done = icmp eq i8 %c, 0\n\
             \x20 br i1 %done, label %end, label %cont\n\
             cont:\n\
             \x20 %cv = zext i8 %c to i64\n\
             \x20 %h2 = xor i64 %h, %cv\n\
             \x20 %h3 = mul i64 %h2, 1099511628211\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             end:\n\
             \x20 ret i64 %h\n\
             }\n\n",
        );

        // __yorum_map_find_slot — find slot for key (used by set/get/has)
        // Returns index of matching slot or first empty slot.
        // Continues probing past tombstones (flag==2) to maintain chain integrity.
        // %map = ptr to map struct, %key = ptr to C string
        // Also takes %cap = capacity for convenience.
        self.body.push_str(
            "define i64 @__yorum_map_find_slot(ptr %map, ptr %key, i64 %cap) {\n\
             entry:\n\
             \x20 %hash = call i64 @__yorum_hash_string(ptr %key)\n\
             \x20 %mask = sub i64 %cap, 1\n\
             \x20 %start = and i64 %hash, %mask\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %probe\n\
             probe:\n\
             \x20 %idx = phi i64 [ %start, %entry ], [ %next, %advance ]\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %idx\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %is_empty = icmp eq i8 %flag, 0\n\
             \x20 br i1 %is_empty, label %done, label %check_tombstone\n\
             check_tombstone:\n\
             \x20 %is_tomb = icmp eq i8 %flag, 2\n\
             \x20 br i1 %is_tomb, label %advance, label %check_key\n\
             check_key:\n\
             \x20 %kp = getelementptr ptr, ptr %keys_p, i64 %idx\n\
             \x20 %k = load ptr, ptr %kp\n\
             \x20 %cmp = call i32 @strcmp(ptr %k, ptr %key)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n\
             \x20 br i1 %eq, label %done, label %advance\n\
             advance:\n\
             \x20 %next_raw = add i64 %idx, 1\n\
             \x20 %next = and i64 %next_raw, %mask\n\
             \x20 br label %probe\n\
             done:\n\
             \x20 ret i64 %idx\n\
             }\n\n",
        );

        // __yorum_map_grow — double capacity and rehash
        self.body.push_str(
            "define void @__yorum_map_grow(ptr %map) {\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %old_cap = load i64, ptr %cap_p\n\
             \x20 %new_cap = mul i64 %old_cap, 2\n\
             \x20 ; allocate new arrays\n\
             \x20 %kb = mul i64 %new_cap, 8\n\
             \x20 %vb = mul i64 %new_cap, 8\n\
             \x20 %new_keys = call ptr @malloc(i64 %kb)\n\
             \x20 %new_vals = call ptr @malloc(i64 %vb)\n\
             \x20 %new_flags = call ptr @malloc(i64 %new_cap)\n\
             \x20 call ptr @memset(ptr %new_flags, i32 0, i64 %new_cap)\n\
             \x20 ; load old arrays\n\
             \x20 %old_keys = load ptr, ptr %map\n\
             \x20 %vals_p = getelementptr i8, ptr %map, i64 8\n\
             \x20 %old_vals = load ptr, ptr %vals_p\n\
             \x20 %flags_p = getelementptr i8, ptr %map, i64 16\n\
             \x20 %old_flags = load ptr, ptr %flags_p\n\
             \x20 ; store new arrays and capacity\n\
             \x20 store ptr %new_keys, ptr %map\n\
             \x20 store ptr %new_vals, ptr %vals_p\n\
             \x20 store ptr %new_flags, ptr %flags_p\n\
             \x20 store i64 %new_cap, ptr %cap_p\n\
             \x20 %tomb_p = getelementptr i8, ptr %map, i64 40\n\
             \x20 store i64 0, ptr %tomb_p\n\
             \x20 br label %rehash_loop\n\
             rehash_loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %rehash_cont ]\n\
             \x20 %cmp = icmp slt i64 %i, %old_cap\n\
             \x20 br i1 %cmp, label %rehash_body, label %rehash_done\n\
             rehash_body:\n\
             \x20 %ofp = getelementptr i8, ptr %old_flags, i64 %i\n\
             \x20 %of = load i8, ptr %ofp\n\
             \x20 %occ = icmp eq i8 %of, 1\n\
             \x20 br i1 %occ, label %rehash_insert, label %rehash_cont\n\
             rehash_insert:\n\
             \x20 %okp = getelementptr ptr, ptr %old_keys, i64 %i\n\
             \x20 %ok = load ptr, ptr %okp\n\
             \x20 %ovp = getelementptr i64, ptr %old_vals, i64 %i\n\
             \x20 %ov = load i64, ptr %ovp\n\
             \x20 %slot = call i64 @__yorum_map_find_slot(ptr %map, ptr %ok, i64 %new_cap)\n\
             \x20 %nkp = getelementptr ptr, ptr %new_keys, i64 %slot\n\
             \x20 store ptr %ok, ptr %nkp\n\
             \x20 %nvp = getelementptr i64, ptr %new_vals, i64 %slot\n\
             \x20 store i64 %ov, ptr %nvp\n\
             \x20 %nfp = getelementptr i8, ptr %new_flags, i64 %slot\n\
             \x20 store i8 1, ptr %nfp\n\
             \x20 br label %rehash_cont\n\
             rehash_cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %rehash_loop\n\
             rehash_done:\n\
             \x20 call void @free(ptr %old_keys)\n\
             \x20 call void @free(ptr %old_vals)\n\
             \x20 call void @free(ptr %old_flags)\n\
             \x20 ret void\n\
             }\n\n",
        );

        // map_new — allocate and initialize a new hash map (capacity 16)
        // Map struct layout (48 bytes):
        //   offset 0:  ptr   keys
        //   offset 8:  ptr   vals
        //   offset 16: ptr   flags
        //   offset 24: i64   capacity
        //   offset 32: i64   size (occupied count)
        //   offset 40: i64   tombstones
        self.body.push_str(
            "define ptr @map_new() {\n\
             entry:\n\
             \x20 %map = call ptr @malloc(i64 48)\n\
             \x20 %keys = call ptr @malloc(i64 128)\n\
             \x20 %vals = call ptr @malloc(i64 128)\n\
             \x20 %flags = call ptr @malloc(i64 16)\n\
             \x20 call ptr @memset(ptr %flags, i32 0, i64 16)\n\
             \x20 store ptr %keys, ptr %map\n\
             \x20 %vp = getelementptr i8, ptr %map, i64 8\n\
             \x20 store ptr %vals, ptr %vp\n\
             \x20 %fp = getelementptr i8, ptr %map, i64 16\n\
             \x20 store ptr %flags, ptr %fp\n\
             \x20 %cp = getelementptr i8, ptr %map, i64 24\n\
             \x20 store i64 16, ptr %cp\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 store i64 0, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %map, i64 40\n\
             \x20 store i64 0, ptr %tp\n\
             \x20 ret ptr %map\n\
             }\n\n",
        );

        // map_set — insert or update key-value pair
        // Accepts empty (flag==0) or tombstone (flag==2) slots for insertion.
        // Load factor check includes tombstones to prevent infinite probe loops.
        self.body.push_str(
            "define void @map_set(ptr %map, ptr %key, i64 %val) {\n\
             entry:\n\
             \x20 ; check load factor: (size + tombstones)*4 >= cap*3 → grow\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %map, i64 40\n\
             \x20 %tombs = load i64, ptr %tp\n\
             \x20 %used = add i64 %size, %tombs\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %s4 = mul i64 %used, 4\n\
             \x20 %c3 = mul i64 %cap, 3\n\
             \x20 %need_grow = icmp sge i64 %s4, %c3\n\
             \x20 br i1 %need_grow, label %grow, label %find\n\
             grow:\n\
             \x20 call void @__yorum_map_grow(ptr %map)\n\
             \x20 br label %find\n\
             find:\n\
             \x20 %cap2 = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot(ptr %map, ptr %key, i64 %cap2)\n\
             \x20 ; check if slot is occupied (flag==1 → update) or empty/tombstone (insert)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fslot = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fslot\n\
             \x20 %is_occupied = icmp eq i8 %flag, 1\n\
             \x20 br i1 %is_occupied, label %update, label %insert\n\
             insert:\n\
             \x20 ; copy key string\n\
             \x20 %klen = call i64 @strlen(ptr %key)\n\
             \x20 %kbuf_sz = add i64 %klen, 1\n\
             \x20 %kbuf = call ptr @malloc(i64 %kbuf_sz)\n\
             \x20 call ptr @strcpy(ptr %kbuf, ptr %key)\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %kslot = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 store ptr %kbuf, ptr %kslot\n\
             \x20 store i8 1, ptr %fslot\n\
             \x20 %new_size = add i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 br label %store_val\n\
             update:\n\
             \x20 br label %store_val\n\
             store_val:\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %vslot = getelementptr i64, ptr %vals_p, i64 %slot\n\
             \x20 store i64 %val, ptr %vslot\n\
             \x20 ret void\n\
             }\n\n",
        );

        // map_get — look up value by key (aborts if not found)
        self.body.push_str(
            "define i64 @map_get(ptr %map, ptr %key) {\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot(ptr %map, ptr %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 br i1 %found, label %ok, label %fail\n\
             fail:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.map_key, ptr %key)\n\
             \x20 call void @abort()\n\
             \x20 unreachable\n\
             ok:\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %vp = getelementptr i64, ptr %vals_p, i64 %slot\n\
             \x20 %v = load i64, ptr %vp\n\
             \x20 ret i64 %v\n\
             }\n\n",
        );

        // map_has — check if key exists
        self.body.push_str(
            "define i1 @map_has(ptr %map, ptr %key) {\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot(ptr %map, ptr %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 ret i1 %found\n\
             }\n\n",
        );
    }

    /// abs, min, max for int and float.
    pub(crate) fn emit_builtin_math_helpers(&mut self) {
        // ── Math builtins ──

        // abs_int — absolute value of int
        self.body.push_str(
            "define i64 @abs_int(i64 %x) {\n\
             entry:\n\
             \x20 %neg = icmp slt i64 %x, 0\n\
             \x20 %pos = sub i64 0, %x\n\
             \x20 %result = select i1 %neg, i64 %pos, i64 %x\n\
             \x20 ret i64 %result\n\
             }\n\n",
        );
        // abs_float — absolute value of float
        self.body.push_str(
            "define double @abs_float(double %x) {\n\
             entry:\n\
             \x20 %result = call double @llvm.fabs.f64(double %x)\n\
             \x20 ret double %result\n\
             }\n\n",
        );
        // min_int
        self.body.push_str(
            "define i64 @min_int(i64 %a, i64 %b) {\n\
             entry:\n\
             \x20 %cmp = icmp slt i64 %a, %b\n\
             \x20 %result = select i1 %cmp, i64 %a, i64 %b\n\
             \x20 ret i64 %result\n\
             }\n\n",
        );
        // max_int
        self.body.push_str(
            "define i64 @max_int(i64 %a, i64 %b) {\n\
             entry:\n\
             \x20 %cmp = icmp sgt i64 %a, %b\n\
             \x20 %result = select i1 %cmp, i64 %a, i64 %b\n\
             \x20 ret i64 %result\n\
             }\n\n",
        );
        // min_float
        self.body.push_str(
            "define double @min_float(double %a, double %b) {\n\
             entry:\n\
             \x20 %result = call double @llvm.minnum.f64(double %a, double %b)\n\
             \x20 ret double %result\n\
             }\n\n",
        );
        // max_float
        self.body.push_str(
            "define double @max_float(double %a, double %b) {\n\
             entry:\n\
             \x20 %result = call double @llvm.maxnum.f64(double %a, double %b)\n\
             \x20 ret double %result\n\
             }\n\n",
        );
        // sqrt, pow, floor, ceil, round — these use LLVM intrinsics and are
        // inlined at call sites (see emit_expr Call handler) to avoid defining
        // wrapper functions whose names clash with C library symbols (e.g.,
        // @pow wrapping @llvm.pow.f64 which LLVM lowers to C's pow → infinite
        // recursion). No wrapper functions emitted here.

        // sin, cos, tan, log, log10, exp — these are just external libm
        // declarations (already in emit_builtin_decls), called directly by the
        // standard call dispatch path. No wrapper functions needed.
    }

    /// String search (contains, index_of, starts/ends_with, trim) and
    /// transformation (replace, split, upper/lower, repeat).
    pub(crate) fn emit_builtin_string_helpers(&mut self) {
        // ── String utility builtins ──

        // str_contains — check if substring exists
        self.body.push_str(
            "define i1 @str_contains(ptr %s, ptr %sub) {\n\
             entry:\n\
             \x20 %p = call ptr @strstr(ptr %s, ptr %sub)\n\
             \x20 %found = icmp ne ptr %p, null\n\
             \x20 ret i1 %found\n\
             }\n\n",
        );
        // str_index_of — find index of substring, returns -1 if not found
        self.body.push_str(
            "define i64 @str_index_of(ptr %s, ptr %sub) {\n\
             entry:\n\
             \x20 %p = call ptr @strstr(ptr %s, ptr %sub)\n\
             \x20 %is_null = icmp eq ptr %p, null\n\
             \x20 br i1 %is_null, label %not_found, label %found\n\
             not_found:\n\
             \x20 ret i64 -1\n\
             found:\n\
             \x20 %si = ptrtoint ptr %s to i64\n\
             \x20 %pi = ptrtoint ptr %p to i64\n\
             \x20 %idx = sub i64 %pi, %si\n\
             \x20 ret i64 %idx\n\
             }\n\n",
        );
        // str_starts_with — check if string starts with prefix
        self.body.push_str(
            "define i1 @str_starts_with(ptr %s, ptr %prefix) {\n\
             entry:\n\
             \x20 %plen = call i64 @strlen(ptr %prefix)\n\
             \x20 %cmp = call i32 @strncmp(ptr %s, ptr %prefix, i64 %plen)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n\
             \x20 ret i1 %eq\n\
             }\n\n",
        );
        // str_ends_with — check if string ends with suffix
        self.body.push_str(
            "define i1 @str_ends_with(ptr %s, ptr %suffix) {\n\
             entry:\n\
             \x20 %slen = call i64 @strlen(ptr %s)\n\
             \x20 %suflen = call i64 @strlen(ptr %suffix)\n\
             \x20 %too_short = icmp slt i64 %slen, %suflen\n\
             \x20 br i1 %too_short, label %no, label %check\n\
             no:\n\
             \x20 ret i1 0\n\
             check:\n\
             \x20 %offset = sub i64 %slen, %suflen\n\
             \x20 %tail = getelementptr i8, ptr %s, i64 %offset\n\
             \x20 %cmp = call i32 @strcmp(ptr %tail, ptr %suffix)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n\
             \x20 ret i1 %eq\n\
             }\n\n",
        );
        // str_trim — trim leading and trailing whitespace
        self.body.push_str(
            "define ptr @str_trim(ptr %s) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 br label %skip_leading\n\
             skip_leading:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %leading_cont ]\n\
             \x20 %done_l = icmp sge i64 %i, %len\n\
             \x20 br i1 %done_l, label %empty, label %check_leading\n\
             check_leading:\n\
             \x20 %cp = getelementptr i8, ptr %s, i64 %i\n\
             \x20 %c = load i8, ptr %cp\n\
             \x20 %is_sp = icmp eq i8 %c, 32\n\
             \x20 %is_tab = icmp eq i8 %c, 9\n\
             \x20 %is_nl = icmp eq i8 %c, 10\n\
             \x20 %is_cr = icmp eq i8 %c, 13\n\
             \x20 %w1 = or i1 %is_sp, %is_tab\n\
             \x20 %w2 = or i1 %w1, %is_nl\n\
             \x20 %is_ws = or i1 %w2, %is_cr\n\
             \x20 br i1 %is_ws, label %leading_cont, label %find_end\n\
             leading_cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %skip_leading\n\
             empty:\n\
             \x20 %e = call ptr @malloc(i64 1)\n\
             \x20 store i8 0, ptr %e\n\
             \x20 ret ptr %e\n\
             find_end:\n\
             \x20 %start = phi i64 [ %i, %check_leading ]\n\
             \x20 %last_init = sub i64 %len, 1\n\
             \x20 br label %skip_trailing\n\
             skip_trailing:\n\
             \x20 %j = phi i64 [ %last_init, %find_end ], [ %j_prev, %trailing_cont ]\n\
             \x20 %done_t = icmp slt i64 %j, %start\n\
             \x20 br i1 %done_t, label %empty, label %check_trailing\n\
             check_trailing:\n\
             \x20 %cp2 = getelementptr i8, ptr %s, i64 %j\n\
             \x20 %c2 = load i8, ptr %cp2\n\
             \x20 %is_sp2 = icmp eq i8 %c2, 32\n\
             \x20 %is_tab2 = icmp eq i8 %c2, 9\n\
             \x20 %is_nl2 = icmp eq i8 %c2, 10\n\
             \x20 %is_cr2 = icmp eq i8 %c2, 13\n\
             \x20 %w3 = or i1 %is_sp2, %is_tab2\n\
             \x20 %w4 = or i1 %w3, %is_nl2\n\
             \x20 %is_ws2 = or i1 %w4, %is_cr2\n\
             \x20 br i1 %is_ws2, label %trailing_cont, label %copy\n\
             trailing_cont:\n\
             \x20 %j_prev = sub i64 %j, 1\n\
             \x20 br label %skip_trailing\n\
             copy:\n\
             \x20 %end = phi i64 [ %j, %check_trailing ]\n\
             \x20 %new_len = sub i64 %end, %start\n\
             \x20 %new_len1 = add i64 %new_len, 1\n\
             \x20 %buf_sz = add i64 %new_len1, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 %src = getelementptr i8, ptr %s, i64 %start\n\
             \x20 call ptr @memcpy(ptr %buf, ptr %src, i64 %new_len1)\n\
             \x20 %term = getelementptr i8, ptr %buf, i64 %new_len1\n\
             \x20 store i8 0, ptr %term\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_replace — replace all occurrences of 'from' with 'to'
        self.body.push_str(
            "define ptr @str_replace(ptr %s, ptr %from, ptr %to) {\n\
             entry:\n\
             \x20 %slen = call i64 @strlen(ptr %s)\n\
             \x20 %flen = call i64 @strlen(ptr %from)\n\
             \x20 %tlen = call i64 @strlen(ptr %to)\n\
             \x20 ; allocate generous buffer: slen * (tlen+1) + 1\n\
             \x20 %max1 = add i64 %tlen, 1\n\
             \x20 %max2 = add i64 %slen, 1\n\
             \x20 %max3 = mul i64 %max2, %max1\n\
             \x20 %buf_sz = add i64 %max3, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 store i8 0, ptr %buf\n\
             \x20 %is_empty_from = icmp eq i64 %flen, 0\n\
             \x20 br i1 %is_empty_from, label %just_copy, label %loop\n\
             just_copy:\n\
             \x20 call ptr @strcpy(ptr %buf, ptr %s)\n\
             \x20 ret ptr %buf\n\
             loop:\n\
             \x20 %cur = phi ptr [ %s, %entry ], [ %after, %replace ]\n\
             \x20 %found = call ptr @strstr(ptr %cur, ptr %from)\n\
             \x20 %is_null = icmp eq ptr %found, null\n\
             \x20 br i1 %is_null, label %done, label %replace\n\
             replace:\n\
             \x20 ; append chars before match\n\
             \x20 %ci = ptrtoint ptr %cur to i64\n\
             \x20 %fi = ptrtoint ptr %found to i64\n\
             \x20 %prefix_len = sub i64 %fi, %ci\n\
             \x20 %blen = call i64 @strlen(ptr %buf)\n\
             \x20 %dst = getelementptr i8, ptr %buf, i64 %blen\n\
             \x20 call ptr @memcpy(ptr %dst, ptr %cur, i64 %prefix_len)\n\
             \x20 %dst_end = getelementptr i8, ptr %dst, i64 %prefix_len\n\
             \x20 store i8 0, ptr %dst_end\n\
             \x20 ; append replacement\n\
             \x20 call ptr @strcat(ptr %buf, ptr %to)\n\
             \x20 %after = getelementptr i8, ptr %found, i64 %flen\n\
             \x20 br label %loop\n\
             done:\n\
             \x20 ; append remaining string\n\
             \x20 call ptr @strcat(ptr %buf, ptr %cur)\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_split — split string by delimiter, returns array of strings
        self.body.push_str(
            "define ptr @str_split(ptr %s, ptr %delim) {\n\
             entry:\n\
             \x20 %dlen = call i64 @strlen(ptr %delim)\n\
             \x20 ; allocate fat pointer { ptr, i64, i64 }\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 ; initial capacity 8\n\
             \x20 %data = call ptr @malloc(i64 64)\n\
             \x20 %d_gep = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %d_gep\n\
             \x20 %l_gep = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %l_gep\n\
             \x20 %c_gep = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 8, ptr %c_gep\n\
             \x20 %is_empty_delim = icmp eq i64 %dlen, 0\n\
             \x20 br i1 %is_empty_delim, label %whole, label %split_loop\n\
             whole:\n\
             \x20 ; empty delimiter: return array with original string\n\
             \x20 %sdup = call i64 @strlen(ptr %s)\n\
             \x20 %sdup_sz = add i64 %sdup, 1\n\
             \x20 %sdup_buf = call ptr @malloc(i64 %sdup_sz)\n\
             \x20 call ptr @strcpy(ptr %sdup_buf, ptr %s)\n\
             \x20 store ptr %sdup_buf, ptr %data\n\
             \x20 store i64 1, ptr %l_gep\n\
             \x20 ret ptr %fat\n\
             split_loop:\n\
             \x20 %cur = phi ptr [ %s, %entry ], [ %next, %store1 ]\n\
             \x20 %found = call ptr @strstr(ptr %cur, ptr %delim)\n\
             \x20 %is_null = icmp eq ptr %found, null\n\
             \x20 br i1 %is_null, label %last_part, label %add_part\n\
             add_part:\n\
             \x20 %ci = ptrtoint ptr %cur to i64\n\
             \x20 %fi = ptrtoint ptr %found to i64\n\
             \x20 %plen = sub i64 %fi, %ci\n\
             \x20 %pbuf_sz = add i64 %plen, 1\n\
             \x20 %pbuf = call ptr @malloc(i64 %pbuf_sz)\n\
             \x20 call ptr @memcpy(ptr %pbuf, ptr %cur, i64 %plen)\n\
             \x20 %pterm = getelementptr i8, ptr %pbuf, i64 %plen\n\
             \x20 store i8 0, ptr %pterm\n\
             \x20 ; push to array (grow if needed)\n\
             \x20 %len1 = load i64, ptr %l_gep\n\
             \x20 %cap1 = load i64, ptr %c_gep\n\
             \x20 %need1 = icmp eq i64 %len1, %cap1\n\
             \x20 br i1 %need1, label %grow1, label %store1\n\
             grow1:\n\
             \x20 %nc1 = shl i64 %cap1, 1\n\
             \x20 %nb1 = mul i64 %nc1, 8\n\
             \x20 %d1 = load ptr, ptr %d_gep\n\
             \x20 %nd1 = call ptr @realloc(ptr %d1, i64 %nb1)\n\
             \x20 store ptr %nd1, ptr %d_gep\n\
             \x20 store i64 %nc1, ptr %c_gep\n\
             \x20 br label %store1\n\
             store1:\n\
             \x20 %d2 = load ptr, ptr %d_gep\n\
             \x20 %slot1 = getelementptr ptr, ptr %d2, i64 %len1\n\
             \x20 store ptr %pbuf, ptr %slot1\n\
             \x20 %nl1 = add i64 %len1, 1\n\
             \x20 store i64 %nl1, ptr %l_gep\n\
             \x20 %next = getelementptr i8, ptr %found, i64 %dlen\n\
             \x20 br label %split_loop\n\
             last_part:\n\
             \x20 %last_cur = phi ptr [ %cur, %split_loop ]\n\
             \x20 %llen = call i64 @strlen(ptr %last_cur)\n\
             \x20 %lbuf_sz = add i64 %llen, 1\n\
             \x20 %lbuf = call ptr @malloc(i64 %lbuf_sz)\n\
             \x20 call ptr @strcpy(ptr %lbuf, ptr %last_cur)\n\
             \x20 ; push last part\n\
             \x20 %len2 = load i64, ptr %l_gep\n\
             \x20 %cap2 = load i64, ptr %c_gep\n\
             \x20 %need2 = icmp eq i64 %len2, %cap2\n\
             \x20 br i1 %need2, label %grow2, label %store2\n\
             grow2:\n\
             \x20 %nc2 = shl i64 %cap2, 1\n\
             \x20 %nb2 = mul i64 %nc2, 8\n\
             \x20 %d3 = load ptr, ptr %d_gep\n\
             \x20 %nd2 = call ptr @realloc(ptr %d3, i64 %nb2)\n\
             \x20 store ptr %nd2, ptr %d_gep\n\
             \x20 store i64 %nc2, ptr %c_gep\n\
             \x20 br label %store2\n\
             store2:\n\
             \x20 %d4 = load ptr, ptr %d_gep\n\
             \x20 %slot2 = getelementptr ptr, ptr %d4, i64 %len2\n\
             \x20 store ptr %lbuf, ptr %slot2\n\
             \x20 %nl2 = add i64 %len2, 1\n\
             \x20 store i64 %nl2, ptr %l_gep\n\
             \x20 ret ptr %fat\n\
             }\n\n",
        );
        // str_to_upper — convert string to uppercase
        self.body.push_str(
            "define ptr @str_to_upper(ptr %s) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 %buf_sz = add i64 %len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cont ]\n\
             \x20 %done = icmp sge i64 %i, %buf_sz\n\
             \x20 br i1 %done, label %end, label %body\n\
             body:\n\
             \x20 %sp = getelementptr i8, ptr %s, i64 %i\n\
             \x20 %c = load i8, ptr %sp\n\
             \x20 %ge_a = icmp sge i8 %c, 97\n\
             \x20 %le_z = icmp sle i8 %c, 122\n\
             \x20 %is_lower = and i1 %ge_a, %le_z\n\
             \x20 %upper = sub i8 %c, 32\n\
             \x20 %out = select i1 %is_lower, i8 %upper, i8 %c\n\
             \x20 %dp = getelementptr i8, ptr %buf, i64 %i\n\
             \x20 store i8 %out, ptr %dp\n\
             \x20 br label %cont\n\
             cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             end:\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_to_lower — convert string to lowercase
        self.body.push_str(
            "define ptr @str_to_lower(ptr %s) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 %buf_sz = add i64 %len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cont ]\n\
             \x20 %done = icmp sge i64 %i, %buf_sz\n\
             \x20 br i1 %done, label %end, label %body\n\
             body:\n\
             \x20 %sp = getelementptr i8, ptr %s, i64 %i\n\
             \x20 %c = load i8, ptr %sp\n\
             \x20 %ge_A = icmp sge i8 %c, 65\n\
             \x20 %le_Z = icmp sle i8 %c, 90\n\
             \x20 %is_upper = and i1 %ge_A, %le_Z\n\
             \x20 %lower = add i8 %c, 32\n\
             \x20 %out = select i1 %is_upper, i8 %lower, i8 %c\n\
             \x20 %dp = getelementptr i8, ptr %buf, i64 %i\n\
             \x20 store i8 %out, ptr %dp\n\
             \x20 br label %cont\n\
             cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             end:\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_repeat — repeat string n times
        self.body.push_str(
            "define ptr @str_repeat(ptr %s, i64 %n) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 %total = mul i64 %len, %n\n\
             \x20 %buf_sz = add i64 %total, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 store i8 0, ptr %buf\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cat ]\n\
             \x20 %done = icmp sge i64 %i, %n\n\
             \x20 br i1 %done, label %end, label %cat\n\
             cat:\n\
             \x20 call ptr @strcat(ptr %buf, ptr %s)\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             end:\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
    }

    /// Collection utilities (contains, sort), map utilities (size, remove, keys, values),
    /// map aliases, enhanced I/O (file_exists, file_append, read_line, etc.), and networking.
    pub(crate) fn emit_builtin_collection_helpers(&mut self) {
        // ── Collection utility builtins ──

        // contains_int — linear scan of [int] array for a value
        self.body.push_str(
            "define i1 @contains_int(ptr %arr, i64 %val) {\n\
             entry:\n\
             \x20 %l_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 1\n\
             \x20 %len = load i64, ptr %l_gep\n\
             \x20 %d_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 0\n\
             \x20 %data = load ptr, ptr %d_gep\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cont ]\n\
             \x20 %done = icmp sge i64 %i, %len\n\
             \x20 br i1 %done, label %not_found, label %check\n\
             check:\n\
             \x20 %ep = getelementptr i64, ptr %data, i64 %i\n\
             \x20 %e = load i64, ptr %ep\n\
             \x20 %eq = icmp eq i64 %e, %val\n\
             \x20 br i1 %eq, label %found, label %cont\n\
             cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             found:\n\
             \x20 ret i1 1\n\
             not_found:\n\
             \x20 ret i1 0\n\
             }\n\n",
        );
        // contains_str — linear scan of [string] array using strcmp
        self.body.push_str(
            "define i1 @contains_str(ptr %arr, ptr %val) {\n\
             entry:\n\
             \x20 %l_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 1\n\
             \x20 %len = load i64, ptr %l_gep\n\
             \x20 %d_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 0\n\
             \x20 %data = load ptr, ptr %d_gep\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cont ]\n\
             \x20 %done = icmp sge i64 %i, %len\n\
             \x20 br i1 %done, label %not_found, label %check\n\
             check:\n\
             \x20 %ep = getelementptr ptr, ptr %data, i64 %i\n\
             \x20 %e = load ptr, ptr %ep\n\
             \x20 %cmp = call i32 @strcmp(ptr %e, ptr %val)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n\
             \x20 br i1 %eq, label %found, label %cont\n\
             cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             found:\n\
             \x20 ret i1 1\n\
             not_found:\n\
             \x20 ret i1 0\n\
             }\n\n",
        );
        // sort_int — copy array and in-place heap sort, O(n log n)
        // Uses sift-down heapify with alloca-based loop variables
        self.body.push_str(
            "define ptr @sort_int(ptr %arr) {\n\
             entry:\n\
             \x20 %l_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 1\n\
             \x20 %len = load i64, ptr %l_gep\n\
             \x20 %d_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 0\n\
             \x20 %data = load ptr, ptr %d_gep\n\
             \x20 ; copy data\n\
             \x20 %bytes = mul i64 %len, 8\n\
             \x20 %new_data = call ptr @malloc(i64 %bytes)\n\
             \x20 call ptr @memcpy(ptr %new_data, ptr %data, i64 %bytes)\n\
             \x20 ; heap sort phase 1: build max-heap\n\
             \x20 %bi_addr = alloca i64\n\
             \x20 %half = sdiv i64 %len, 2\n\
             \x20 %start = sub i64 %half, 1\n\
             \x20 store i64 %start, ptr %bi_addr\n\
             \x20 br label %build_loop\n\
             build_loop:\n\
             \x20 %bi = load i64, ptr %bi_addr\n\
             \x20 %b_done = icmp slt i64 %bi, 0\n\
             \x20 br i1 %b_done, label %extract_init, label %build_sift\n\
             build_sift:\n\
             \x20 %root_addr = alloca i64\n\
             \x20 store i64 %bi, ptr %root_addr\n\
             \x20 br label %sift_b\n\
             sift_b:\n\
             \x20 %sb_r = load i64, ptr %root_addr\n\
             \x20 %sb_left1 = add i64 %sb_r, %sb_r\n\
             \x20 %sb_left = add i64 %sb_left1, 1\n\
             \x20 %sb_right = add i64 %sb_left, 1\n\
             \x20 ; start with largest = root\n\
             \x20 %lg_addr = alloca i64\n\
             \x20 store i64 %sb_r, ptr %lg_addr\n\
             \x20 ; check left child\n\
             \x20 %sb_lvalid = icmp slt i64 %sb_left, %len\n\
             \x20 br i1 %sb_lvalid, label %sift_b_lcmp, label %sift_b_rchk\n\
             sift_b_lcmp:\n\
             \x20 %sb_lg1 = load i64, ptr %lg_addr\n\
             \x20 %sb_lp = getelementptr i64, ptr %new_data, i64 %sb_left\n\
             \x20 %sb_lv = load i64, ptr %sb_lp\n\
             \x20 %sb_bp = getelementptr i64, ptr %new_data, i64 %sb_lg1\n\
             \x20 %sb_bv = load i64, ptr %sb_bp\n\
             \x20 %sb_lgt = icmp sgt i64 %sb_lv, %sb_bv\n\
             \x20 br i1 %sb_lgt, label %sift_b_lset, label %sift_b_rchk\n\
             sift_b_lset:\n\
             \x20 store i64 %sb_left, ptr %lg_addr\n\
             \x20 br label %sift_b_rchk\n\
             sift_b_rchk:\n\
             \x20 %sb_rvalid = icmp slt i64 %sb_right, %len\n\
             \x20 br i1 %sb_rvalid, label %sift_b_rcmp, label %sift_b_chk\n\
             sift_b_rcmp:\n\
             \x20 %sb_lg2 = load i64, ptr %lg_addr\n\
             \x20 %sb_rp = getelementptr i64, ptr %new_data, i64 %sb_right\n\
             \x20 %sb_rv = load i64, ptr %sb_rp\n\
             \x20 %sb_b3p = getelementptr i64, ptr %new_data, i64 %sb_lg2\n\
             \x20 %sb_b3v = load i64, ptr %sb_b3p\n\
             \x20 %sb_rgt = icmp sgt i64 %sb_rv, %sb_b3v\n\
             \x20 br i1 %sb_rgt, label %sift_b_rset, label %sift_b_chk\n\
             sift_b_rset:\n\
             \x20 store i64 %sb_right, ptr %lg_addr\n\
             \x20 br label %sift_b_chk\n\
             sift_b_chk:\n\
             \x20 %sb_largest = load i64, ptr %lg_addr\n\
             \x20 %sb_r2 = load i64, ptr %root_addr\n\
             \x20 %sb_changed = icmp ne i64 %sb_largest, %sb_r2\n\
             \x20 br i1 %sb_changed, label %sift_b_swap, label %build_cont\n\
             sift_b_swap:\n\
             \x20 %sb_r3 = load i64, ptr %root_addr\n\
             \x20 %sb_rp2 = getelementptr i64, ptr %new_data, i64 %sb_r3\n\
             \x20 %sb_rv2 = load i64, ptr %sb_rp2\n\
             \x20 %sb_lg3 = load i64, ptr %lg_addr\n\
             \x20 %sb_lp2 = getelementptr i64, ptr %new_data, i64 %sb_lg3\n\
             \x20 %sb_lv2 = load i64, ptr %sb_lp2\n\
             \x20 store i64 %sb_lv2, ptr %sb_rp2\n\
             \x20 store i64 %sb_rv2, ptr %sb_lp2\n\
             \x20 store i64 %sb_lg3, ptr %root_addr\n\
             \x20 br label %sift_b\n\
             build_cont:\n\
             \x20 %bi2 = load i64, ptr %bi_addr\n\
             \x20 %bi_next = sub i64 %bi2, 1\n\
             \x20 store i64 %bi_next, ptr %bi_addr\n\
             \x20 br label %build_loop\n\
             extract_init:\n\
             \x20 %end_addr = alloca i64\n\
             \x20 %len_m1 = sub i64 %len, 1\n\
             \x20 store i64 %len_m1, ptr %end_addr\n\
             \x20 br label %extract_loop\n\
             extract_loop:\n\
             \x20 %end = load i64, ptr %end_addr\n\
             \x20 %e_done = icmp sle i64 %end, 0\n\
             \x20 br i1 %e_done, label %build_fat, label %extract_swap\n\
             extract_swap:\n\
             \x20 %e0p = getelementptr i64, ptr %new_data, i64 0\n\
             \x20 %e0v = load i64, ptr %e0p\n\
             \x20 %end2 = load i64, ptr %end_addr\n\
             \x20 %eep = getelementptr i64, ptr %new_data, i64 %end2\n\
             \x20 %eev = load i64, ptr %eep\n\
             \x20 store i64 %eev, ptr %e0p\n\
             \x20 store i64 %e0v, ptr %eep\n\
             \x20 ; sift down [0, end)\n\
             \x20 %se_root = alloca i64\n\
             \x20 store i64 0, ptr %se_root\n\
             \x20 br label %sift_e\n\
             sift_e:\n\
             \x20 %se_r = load i64, ptr %se_root\n\
             \x20 %se_left1 = add i64 %se_r, %se_r\n\
             \x20 %se_left = add i64 %se_left1, 1\n\
             \x20 %se_right = add i64 %se_left, 1\n\
             \x20 %se_lg_addr = alloca i64\n\
             \x20 store i64 %se_r, ptr %se_lg_addr\n\
             \x20 %se_end = load i64, ptr %end_addr\n\
             \x20 %se_lvalid = icmp slt i64 %se_left, %se_end\n\
             \x20 br i1 %se_lvalid, label %sift_e_lcmp, label %sift_e_rchk\n\
             sift_e_lcmp:\n\
             \x20 %se_lg1 = load i64, ptr %se_lg_addr\n\
             \x20 %se_lp = getelementptr i64, ptr %new_data, i64 %se_left\n\
             \x20 %se_lv = load i64, ptr %se_lp\n\
             \x20 %se_bp = getelementptr i64, ptr %new_data, i64 %se_lg1\n\
             \x20 %se_bv = load i64, ptr %se_bp\n\
             \x20 %se_lgt = icmp sgt i64 %se_lv, %se_bv\n\
             \x20 br i1 %se_lgt, label %sift_e_lset, label %sift_e_rchk\n\
             sift_e_lset:\n\
             \x20 store i64 %se_left, ptr %se_lg_addr\n\
             \x20 br label %sift_e_rchk\n\
             sift_e_rchk:\n\
             \x20 %se_end2 = load i64, ptr %end_addr\n\
             \x20 %se_rvalid = icmp slt i64 %se_right, %se_end2\n\
             \x20 br i1 %se_rvalid, label %sift_e_rcmp, label %sift_e_chk\n\
             sift_e_rcmp:\n\
             \x20 %se_lg2 = load i64, ptr %se_lg_addr\n\
             \x20 %se_rp = getelementptr i64, ptr %new_data, i64 %se_right\n\
             \x20 %se_rv = load i64, ptr %se_rp\n\
             \x20 %se_b3p = getelementptr i64, ptr %new_data, i64 %se_lg2\n\
             \x20 %se_b3v = load i64, ptr %se_b3p\n\
             \x20 %se_rgt = icmp sgt i64 %se_rv, %se_b3v\n\
             \x20 br i1 %se_rgt, label %sift_e_rset, label %sift_e_chk\n\
             sift_e_rset:\n\
             \x20 store i64 %se_right, ptr %se_lg_addr\n\
             \x20 br label %sift_e_chk\n\
             sift_e_chk:\n\
             \x20 %se_largest = load i64, ptr %se_lg_addr\n\
             \x20 %se_r2 = load i64, ptr %se_root\n\
             \x20 %se_changed = icmp ne i64 %se_largest, %se_r2\n\
             \x20 br i1 %se_changed, label %sift_e_swap, label %sift_e_done\n\
             sift_e_swap:\n\
             \x20 %se_r3 = load i64, ptr %se_root\n\
             \x20 %se_rp2 = getelementptr i64, ptr %new_data, i64 %se_r3\n\
             \x20 %se_rv2 = load i64, ptr %se_rp2\n\
             \x20 %se_lg3 = load i64, ptr %se_lg_addr\n\
             \x20 %se_lp2 = getelementptr i64, ptr %new_data, i64 %se_lg3\n\
             \x20 %se_lv2 = load i64, ptr %se_lp2\n\
             \x20 store i64 %se_lv2, ptr %se_rp2\n\
             \x20 store i64 %se_rv2, ptr %se_lp2\n\
             \x20 store i64 %se_lg3, ptr %se_root\n\
             \x20 br label %sift_e\n\
             sift_e_done:\n\
             \x20 %end3 = load i64, ptr %end_addr\n\
             \x20 %end_next = sub i64 %end3, 1\n\
             \x20 store i64 %end_next, ptr %end_addr\n\
             \x20 br label %extract_loop\n\
             build_fat:\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %fd = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %new_data, ptr %fd\n\
             \x20 %fl = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 %len, ptr %fl\n\
             \x20 %fc = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %len, ptr %fc\n\
             \x20 ret ptr %fat\n\
             }\n\n",
        );
        // sort_str — copy array and in-place heap sort using strcmp, O(n log n)
        self.body.push_str(
            "define ptr @sort_str(ptr %arr) {\n\
             entry:\n\
             \x20 %l_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 1\n\
             \x20 %len = load i64, ptr %l_gep\n\
             \x20 %d_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 0\n\
             \x20 %data = load ptr, ptr %d_gep\n\
             \x20 ; copy data\n\
             \x20 %bytes = mul i64 %len, 8\n\
             \x20 %new_data = call ptr @malloc(i64 %bytes)\n\
             \x20 call ptr @memcpy(ptr %new_data, ptr %data, i64 %bytes)\n\
             \x20 ; heap sort phase 1: build max-heap\n\
             \x20 %bi_addr = alloca i64\n\
             \x20 %half = sdiv i64 %len, 2\n\
             \x20 %start = sub i64 %half, 1\n\
             \x20 store i64 %start, ptr %bi_addr\n\
             \x20 br label %build_loop\n\
             build_loop:\n\
             \x20 %bi = load i64, ptr %bi_addr\n\
             \x20 %b_done = icmp slt i64 %bi, 0\n\
             \x20 br i1 %b_done, label %extract_init, label %build_sift\n\
             build_sift:\n\
             \x20 %root_addr = alloca i64\n\
             \x20 store i64 %bi, ptr %root_addr\n\
             \x20 br label %sift_b\n\
             sift_b:\n\
             \x20 %sb_r = load i64, ptr %root_addr\n\
             \x20 %sb_left1 = add i64 %sb_r, %sb_r\n\
             \x20 %sb_left = add i64 %sb_left1, 1\n\
             \x20 %sb_right = add i64 %sb_left, 1\n\
             \x20 %lg_addr = alloca i64\n\
             \x20 store i64 %sb_r, ptr %lg_addr\n\
             \x20 %sb_lvalid = icmp slt i64 %sb_left, %len\n\
             \x20 br i1 %sb_lvalid, label %sift_b_lcmp, label %sift_b_rchk\n\
             sift_b_lcmp:\n\
             \x20 %sb_lg1 = load i64, ptr %lg_addr\n\
             \x20 %sb_lp = getelementptr ptr, ptr %new_data, i64 %sb_left\n\
             \x20 %sb_lv = load ptr, ptr %sb_lp\n\
             \x20 %sb_bp = getelementptr ptr, ptr %new_data, i64 %sb_lg1\n\
             \x20 %sb_bv = load ptr, ptr %sb_bp\n\
             \x20 %sb_cmp = call i32 @strcmp(ptr %sb_lv, ptr %sb_bv)\n\
             \x20 %sb_lgt = icmp sgt i32 %sb_cmp, 0\n\
             \x20 br i1 %sb_lgt, label %sift_b_lset, label %sift_b_rchk\n\
             sift_b_lset:\n\
             \x20 store i64 %sb_left, ptr %lg_addr\n\
             \x20 br label %sift_b_rchk\n\
             sift_b_rchk:\n\
             \x20 %sb_rvalid = icmp slt i64 %sb_right, %len\n\
             \x20 br i1 %sb_rvalid, label %sift_b_rcmp, label %sift_b_chk\n\
             sift_b_rcmp:\n\
             \x20 %sb_lg2 = load i64, ptr %lg_addr\n\
             \x20 %sb_rp = getelementptr ptr, ptr %new_data, i64 %sb_right\n\
             \x20 %sb_rv = load ptr, ptr %sb_rp\n\
             \x20 %sb_b3p = getelementptr ptr, ptr %new_data, i64 %sb_lg2\n\
             \x20 %sb_b3v = load ptr, ptr %sb_b3p\n\
             \x20 %sb_cmp2 = call i32 @strcmp(ptr %sb_rv, ptr %sb_b3v)\n\
             \x20 %sb_rgt = icmp sgt i32 %sb_cmp2, 0\n\
             \x20 br i1 %sb_rgt, label %sift_b_rset, label %sift_b_chk\n\
             sift_b_rset:\n\
             \x20 store i64 %sb_right, ptr %lg_addr\n\
             \x20 br label %sift_b_chk\n\
             sift_b_chk:\n\
             \x20 %sb_largest = load i64, ptr %lg_addr\n\
             \x20 %sb_r2 = load i64, ptr %root_addr\n\
             \x20 %sb_changed = icmp ne i64 %sb_largest, %sb_r2\n\
             \x20 br i1 %sb_changed, label %sift_b_swap, label %build_cont\n\
             sift_b_swap:\n\
             \x20 %sb_r3 = load i64, ptr %root_addr\n\
             \x20 %sb_rp2 = getelementptr ptr, ptr %new_data, i64 %sb_r3\n\
             \x20 %sb_rv2 = load ptr, ptr %sb_rp2\n\
             \x20 %sb_lg3 = load i64, ptr %lg_addr\n\
             \x20 %sb_lp2 = getelementptr ptr, ptr %new_data, i64 %sb_lg3\n\
             \x20 %sb_lv2 = load ptr, ptr %sb_lp2\n\
             \x20 store ptr %sb_lv2, ptr %sb_rp2\n\
             \x20 store ptr %sb_rv2, ptr %sb_lp2\n\
             \x20 store i64 %sb_lg3, ptr %root_addr\n\
             \x20 br label %sift_b\n\
             build_cont:\n\
             \x20 %bi2 = load i64, ptr %bi_addr\n\
             \x20 %bi_next = sub i64 %bi2, 1\n\
             \x20 store i64 %bi_next, ptr %bi_addr\n\
             \x20 br label %build_loop\n\
             extract_init:\n\
             \x20 %end_addr = alloca i64\n\
             \x20 %len_m1 = sub i64 %len, 1\n\
             \x20 store i64 %len_m1, ptr %end_addr\n\
             \x20 br label %extract_loop\n\
             extract_loop:\n\
             \x20 %end = load i64, ptr %end_addr\n\
             \x20 %e_done = icmp sle i64 %end, 0\n\
             \x20 br i1 %e_done, label %build_fat, label %extract_swap\n\
             extract_swap:\n\
             \x20 %e0p = getelementptr ptr, ptr %new_data, i64 0\n\
             \x20 %e0v = load ptr, ptr %e0p\n\
             \x20 %end2 = load i64, ptr %end_addr\n\
             \x20 %eep = getelementptr ptr, ptr %new_data, i64 %end2\n\
             \x20 %eev = load ptr, ptr %eep\n\
             \x20 store ptr %eev, ptr %e0p\n\
             \x20 store ptr %e0v, ptr %eep\n\
             \x20 %se_root = alloca i64\n\
             \x20 store i64 0, ptr %se_root\n\
             \x20 br label %sift_e\n\
             sift_e:\n\
             \x20 %se_r = load i64, ptr %se_root\n\
             \x20 %se_left1 = add i64 %se_r, %se_r\n\
             \x20 %se_left = add i64 %se_left1, 1\n\
             \x20 %se_right = add i64 %se_left, 1\n\
             \x20 %se_lg_addr = alloca i64\n\
             \x20 store i64 %se_r, ptr %se_lg_addr\n\
             \x20 %se_end = load i64, ptr %end_addr\n\
             \x20 %se_lvalid = icmp slt i64 %se_left, %se_end\n\
             \x20 br i1 %se_lvalid, label %sift_e_lcmp, label %sift_e_rchk\n\
             sift_e_lcmp:\n\
             \x20 %se_lg1 = load i64, ptr %se_lg_addr\n\
             \x20 %se_lp = getelementptr ptr, ptr %new_data, i64 %se_left\n\
             \x20 %se_lv = load ptr, ptr %se_lp\n\
             \x20 %se_bp = getelementptr ptr, ptr %new_data, i64 %se_lg1\n\
             \x20 %se_bv = load ptr, ptr %se_bp\n\
             \x20 %se_cmp = call i32 @strcmp(ptr %se_lv, ptr %se_bv)\n\
             \x20 %se_lgt = icmp sgt i32 %se_cmp, 0\n\
             \x20 br i1 %se_lgt, label %sift_e_lset, label %sift_e_rchk\n\
             sift_e_lset:\n\
             \x20 store i64 %se_left, ptr %se_lg_addr\n\
             \x20 br label %sift_e_rchk\n\
             sift_e_rchk:\n\
             \x20 %se_end2 = load i64, ptr %end_addr\n\
             \x20 %se_rvalid = icmp slt i64 %se_right, %se_end2\n\
             \x20 br i1 %se_rvalid, label %sift_e_rcmp, label %sift_e_chk\n\
             sift_e_rcmp:\n\
             \x20 %se_lg2 = load i64, ptr %se_lg_addr\n\
             \x20 %se_rp = getelementptr ptr, ptr %new_data, i64 %se_right\n\
             \x20 %se_rv = load ptr, ptr %se_rp\n\
             \x20 %se_b3p = getelementptr ptr, ptr %new_data, i64 %se_lg2\n\
             \x20 %se_b3v = load ptr, ptr %se_b3p\n\
             \x20 %se_cmp2 = call i32 @strcmp(ptr %se_rv, ptr %se_b3v)\n\
             \x20 %se_rgt = icmp sgt i32 %se_cmp2, 0\n\
             \x20 br i1 %se_rgt, label %sift_e_rset, label %sift_e_chk\n\
             sift_e_rset:\n\
             \x20 store i64 %se_right, ptr %se_lg_addr\n\
             \x20 br label %sift_e_chk\n\
             sift_e_chk:\n\
             \x20 %se_largest = load i64, ptr %se_lg_addr\n\
             \x20 %se_r2 = load i64, ptr %se_root\n\
             \x20 %se_changed = icmp ne i64 %se_largest, %se_r2\n\
             \x20 br i1 %se_changed, label %sift_e_swap, label %sift_e_done\n\
             sift_e_swap:\n\
             \x20 %se_r3 = load i64, ptr %se_root\n\
             \x20 %se_rp2 = getelementptr ptr, ptr %new_data, i64 %se_r3\n\
             \x20 %se_rv2 = load ptr, ptr %se_rp2\n\
             \x20 %se_lg3 = load i64, ptr %se_lg_addr\n\
             \x20 %se_lp2 = getelementptr ptr, ptr %new_data, i64 %se_lg3\n\
             \x20 %se_lv2 = load ptr, ptr %se_lp2\n\
             \x20 store ptr %se_lv2, ptr %se_rp2\n\
             \x20 store ptr %se_rv2, ptr %se_lp2\n\
             \x20 store i64 %se_lg3, ptr %se_root\n\
             \x20 br label %sift_e\n\
             sift_e_done:\n\
             \x20 %end3 = load i64, ptr %end_addr\n\
             \x20 %end_next = sub i64 %end3, 1\n\
             \x20 store i64 %end_next, ptr %end_addr\n\
             \x20 br label %extract_loop\n\
             build_fat:\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %fd = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %new_data, ptr %fd\n\
             \x20 %fl = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 %len, ptr %fl\n\
             \x20 %fc = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %len, ptr %fc\n\
             \x20 ret ptr %fat\n\
             }\n\n",
        );

        // ── Map utility builtins ──

        // map_size — load size field from map struct
        self.body.push_str(
            "define i64 @map_size(ptr %map) {\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 ret i64 %size\n\
             }\n\n",
        );
        // map_remove — find and mark slot as tombstone (flag=2), decrement size
        // Uses tombstone to preserve linear probing chains.
        self.body.push_str(
            "define i1 @map_remove(ptr %map, ptr %key) {\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot(ptr %map, ptr %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 br i1 %found, label %remove, label %done\n\
             remove:\n\
             \x20 store i8 2, ptr %fp\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %new_size = sub i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 %tomb_p = getelementptr i8, ptr %map, i64 40\n\
             \x20 %tombs = load i64, ptr %tomb_p\n\
             \x20 %new_tombs = add i64 %tombs, 1\n\
             \x20 store i64 %new_tombs, ptr %tomb_p\n\
             \x20 ; free the key string\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %kp = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 %k = load ptr, ptr %kp\n\
             \x20 call void @free(ptr %k)\n\
             \x20 ret i1 1\n\
             done:\n\
             \x20 ret i1 0\n\
             }\n\n",
        );
        // map_keys — collect all keys into a [string] array
        self.body.push_str(
            "define ptr @map_keys(ptr %map) {\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 ; allocate result array\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %dbytes = mul i64 %size, 8\n\
             \x20 %data = call ptr @malloc(i64 %dbytes)\n\
             \x20 %fd = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %fd\n\
             \x20 %fl = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %fl\n\
             \x20 %fc = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %size, ptr %fc\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %loop_cont ]\n\
             \x20 %out_idx = phi i64 [ 0, %entry ], [ %out_next, %loop_cont ]\n\
             \x20 %done = icmp sge i64 %i, %cap\n\
             \x20 br i1 %done, label %finish, label %check\n\
             check:\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %i\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %occ = icmp eq i8 %flag, 1\n\
             \x20 br i1 %occ, label %copy_key, label %skip\n\
             copy_key:\n\
             \x20 %kp = getelementptr ptr, ptr %keys_p, i64 %i\n\
             \x20 %k = load ptr, ptr %kp\n\
             \x20 %dp = getelementptr ptr, ptr %data, i64 %out_idx\n\
             \x20 store ptr %k, ptr %dp\n\
             \x20 %out_inc = add i64 %out_idx, 1\n\
             \x20 br label %loop_cont\n\
             skip:\n\
             \x20 br label %loop_cont\n\
             loop_cont:\n\
             \x20 %out_next = phi i64 [ %out_inc, %copy_key ], [ %out_idx, %skip ]\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             finish:\n\
             \x20 store i64 %out_idx, ptr %fl\n\
             \x20 ret ptr %fat\n\
             }\n\n",
        );
        // map_values — collect all values into an [int] array
        self.body.push_str(
            "define ptr @map_values(ptr %map) {\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 ; allocate result array\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %dbytes = mul i64 %size, 8\n\
             \x20 %data = call ptr @malloc(i64 %dbytes)\n\
             \x20 %fd = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %fd\n\
             \x20 %fl = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %fl\n\
             \x20 %fc = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %size, ptr %fc\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %loop_cont ]\n\
             \x20 %out_idx = phi i64 [ 0, %entry ], [ %out_next, %loop_cont ]\n\
             \x20 %done = icmp sge i64 %i, %cap\n\
             \x20 br i1 %done, label %finish, label %check\n\
             check:\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %i\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %occ = icmp eq i8 %flag, 1\n\
             \x20 br i1 %occ, label %copy_val, label %skip\n\
             copy_val:\n\
             \x20 %vp = getelementptr i64, ptr %vals_p, i64 %i\n\
             \x20 %v = load i64, ptr %vp\n\
             \x20 %dp = getelementptr i64, ptr %data, i64 %out_idx\n\
             \x20 store i64 %v, ptr %dp\n\
             \x20 %out_inc = add i64 %out_idx, 1\n\
             \x20 br label %loop_cont\n\
             skip:\n\
             \x20 br label %loop_cont\n\
             loop_cont:\n\
             \x20 %out_next = phi i64 [ %out_inc, %copy_val ], [ %out_idx, %skip ]\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             finish:\n\
             \x20 store i64 %out_idx, ptr %fl\n\
             \x20 ret ptr %fat\n\
             }\n\n",
        );

        // ── Map<string, int> aliases for monomorphized names ──
        // The hardcoded helpers above use unmangled names (map_new, map_set, etc.)
        // After monomorphization, calls use mangled names (map_new__string__int, etc.)
        // Emit LLVM aliases so the mangled names resolve to the existing definitions.
        let map_fns = [
            ("map_new", "map_new__string__int", "ptr ()"),
            ("map_set", "map_set__string__int", "void (ptr, ptr, i64)"),
            ("map_get", "map_get__string__int", "i64 (ptr, ptr)"),
            ("map_has", "map_has__string__int", "i1 (ptr, ptr)"),
            ("map_size", "map_size__string__int", "i64 (ptr)"),
            ("map_remove", "map_remove__string__int", "i1 (ptr, ptr)"),
            ("map_keys", "map_keys__string__int", "ptr (ptr)"),
            ("map_values", "map_values__string__int", "ptr (ptr)"),
        ];
        for (orig, mangled, _ty) in &map_fns {
            self.body
                .push_str(&format!("@{} = alias ptr, ptr @{}\n", mangled, orig));
        }
        self.body.push('\n');

        // ── Enhanced I/O builtins ──

        // file_exists — check if file exists using access()
        self.body.push_str(
            "define i1 @file_exists(ptr %path) {\n\
             entry:\n\
             \x20 %rc = call i32 @access(ptr %path, i32 0)\n\
             \x20 %ok = icmp eq i32 %rc, 0\n\
             \x20 ret i1 %ok\n\
             }\n\n",
        );
        // file_append — append content to file
        self.body.push_str(
            "define i1 @file_append(ptr %path, ptr %content) {\n\
             entry:\n\
             \x20 %f = call ptr @fopen(ptr %path, ptr @.str.a)\n\
             \x20 %is_null = icmp eq ptr %f, null\n\
             \x20 br i1 %is_null, label %fail, label %opened\n\
             fail:\n\
             \x20 ret i1 0\n\
             opened:\n\
             \x20 %len = call i64 @strlen(ptr %content)\n\
             \x20 %written = call i64 @fwrite(ptr %content, i64 1, i64 %len, ptr %f)\n\
             \x20 call i32 @fclose(ptr %f)\n\
             \x20 %ok = icmp eq i64 %written, %len\n\
             \x20 ret i1 %ok\n\
             }\n\n",
        );
        // read_line — read a line from stdin (up to 4096 chars)
        let stdin_sym = if cfg!(target_os = "macos") {
            "@__stdinp"
        } else {
            "@stdin"
        };
        self.body.push_str(&format!(
            "define ptr @read_line() {{\n\
             entry:\n\
             \x20 %buf = call ptr @malloc(i64 4096)\n\
             \x20 %sin = load ptr, ptr {stdin_sym}\n\
             \x20 %result = call ptr @fgets(ptr %buf, i32 4096, ptr %sin)\n\
             \x20 %is_null = icmp eq ptr %result, null\n\
             \x20 br i1 %is_null, label %eof, label %got_line\n\
             eof:\n\
             \x20 store i8 0, ptr %buf\n\
             \x20 ret ptr %buf\n\
             got_line:\n\
             \x20 ; strip trailing newline if present\n\
             \x20 %len = call i64 @strlen(ptr %buf)\n\
             \x20 %has_len = icmp sgt i64 %len, 0\n\
             \x20 br i1 %has_len, label %check_nl, label %done\n\
             check_nl:\n\
             \x20 %last_idx = sub i64 %len, 1\n\
             \x20 %last_p = getelementptr i8, ptr %buf, i64 %last_idx\n\
             \x20 %last_c = load i8, ptr %last_p\n\
             \x20 %is_nl = icmp eq i8 %last_c, 10\n\
             \x20 br i1 %is_nl, label %strip_nl, label %done\n\
             strip_nl:\n\
             \x20 store i8 0, ptr %last_p\n\
             \x20 br label %done\n\
             done:\n\
             \x20 ret ptr %buf\n\
             }}\n\n",
            stdin_sym = stdin_sym,
        ));
        // print_flush — print string without newline and flush
        self.body.push_str(
            "define void @print_flush(ptr %s) {\n\
             entry:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.str, ptr %s)\n\
             \x20 call i32 @fflush(ptr null)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // env_get — get environment variable, returns "" if not set
        self.body.push_str(
            "define ptr @env_get(ptr %name) {\n\
             entry:\n\
             \x20 %val = call ptr @getenv(ptr %name)\n\
             \x20 %is_null = icmp eq ptr %val, null\n\
             \x20 br i1 %is_null, label %not_set, label %found\n\
             not_set:\n\
             \x20 %empty = call ptr @malloc(i64 1)\n\
             \x20 store i8 0, ptr %empty\n\
             \x20 ret ptr %empty\n\
             found:\n\
             \x20 ; copy the value to a heap buffer (getenv result is not ours to keep)\n\
             \x20 %len = call i64 @strlen(ptr %val)\n\
             \x20 %buf_sz = add i64 %len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 call ptr @strcpy(ptr %buf, ptr %val)\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // time_ms — get current time in milliseconds using gettimeofday
        // gettimeofday fills a { i64 sec, i64 usec } struct (on 64-bit)
        self.body.push_str(
            "define i64 @time_ms() {\n\
             entry:\n\
             \x20 %tv = alloca { i64, i64 }\n\
             \x20 call i32 @gettimeofday(ptr %tv, ptr null)\n\
             \x20 %sec_p = getelementptr { i64, i64 }, ptr %tv, i32 0, i32 0\n\
             \x20 %sec = load i64, ptr %sec_p\n\
             \x20 %usec_p = getelementptr { i64, i64 }, ptr %tv, i32 0, i32 1\n\
             \x20 %usec = load i64, ptr %usec_p\n\
             \x20 %ms_sec = mul i64 %sec, 1000\n\
             \x20 %ms_usec = sdiv i64 %usec, 1000\n\
             \x20 %ms = add i64 %ms_sec, %ms_usec\n\
             \x20 ret i64 %ms\n\
             }\n\n",
        );

        self.emit_networking_helpers();
    }

    pub(crate) fn emit_networking_helpers(&mut self) {
        // Platform-specific constants
        let sol_socket = if cfg!(target_os = "macos") {
            "65535"
        } else {
            "1"
        };
        let so_reuseaddr = if cfg!(target_os = "macos") { "4" } else { "2" };

        // ── sockaddr_in filling ──
        // macOS: { i8 len=16, i8 family=2, i16 port, i32 addr, [8 x i8] zero } = 16 bytes
        // Linux: { i16 family=2, i16 port, i32 addr, [8 x i8] zero } = 16 bytes
        //
        // We use a flat [16 x i8] buffer and store bytes directly for portability.

        // tcp_connect(host: string, port: int) -> int
        // Uses getaddrinfo for host resolution, then socket + connect
        self.body.push_str(
            "define i64 @tcp_connect(ptr %host, i64 %port) {\n\
             entry:\n\
             \x20 %port_buf = alloca [16 x i8]\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %port_buf, i64 16, ptr @.fmt.lld, i64 %port)\n\
             \x20 %hints = alloca [48 x i8]\n\
             \x20 call ptr @memset(ptr %hints, i32 0, i64 48)\n\
             \x20 %ai_family_p = getelementptr i8, ptr %hints, i64 4\n\
             \x20 store i32 0, ptr %ai_family_p\n\
             \x20 %ai_socktype_p = getelementptr i8, ptr %hints, i64 8\n\
             \x20 store i32 1, ptr %ai_socktype_p\n\
             \x20 %result_p = alloca ptr\n\
             \x20 %gai = call i32 @getaddrinfo(ptr %host, ptr %port_buf, ptr %hints, ptr %result_p)\n\
             \x20 %gai_fail = icmp ne i32 %gai, 0\n\
             \x20 br i1 %gai_fail, label %fail, label %resolved\n\
             resolved:\n\
             \x20 %res = load ptr, ptr %result_p\n\
             \x20 %ai_family_res_p = getelementptr i8, ptr %res, i64 4\n\
             \x20 %ai_family_val = load i32, ptr %ai_family_res_p\n\
             \x20 %ai_socktype_res_p = getelementptr i8, ptr %res, i64 8\n\
             \x20 %ai_socktype_val = load i32, ptr %ai_socktype_res_p\n\
             \x20 %ai_protocol_p = getelementptr i8, ptr %res, i64 12\n\
             \x20 %ai_protocol_val = load i32, ptr %ai_protocol_p\n\
             \x20 %fd = call i32 @socket(i32 %ai_family_val, i32 %ai_socktype_val, i32 %ai_protocol_val)\n\
             \x20 %fd64 = sext i32 %fd to i64\n\
             \x20 %fd_fail = icmp slt i32 %fd, 0\n\
             \x20 br i1 %fd_fail, label %free_fail, label %do_connect\n\
             do_connect:\n\
             \x20 %ai_addrlen_p = getelementptr i8, ptr %res, i64 16\n\
             \x20 %ai_addrlen = load i32, ptr %ai_addrlen_p\n\
             \x20 %ai_addr_p = getelementptr i8, ptr %res, i64 32\n\
             \x20 %ai_addr = load ptr, ptr %ai_addr_p\n\
             \x20 %conn = call i32 @connect(i32 %fd, ptr %ai_addr, i32 %ai_addrlen)\n\
             \x20 call void @freeaddrinfo(ptr %res)\n\
             \x20 %conn_fail = icmp ne i32 %conn, 0\n\
             \x20 br i1 %conn_fail, label %close_fail, label %success\n\
             success:\n\
             \x20 ret i64 %fd64\n\
             close_fail:\n\
             \x20 call i32 @close(i32 %fd)\n\
             \x20 ret i64 -1\n\
             free_fail:\n\
             \x20 call void @freeaddrinfo(ptr %res)\n\
             \x20 ret i64 -1\n\
             fail:\n\
             \x20 ret i64 -1\n\
             }\n\n",
        );

        // tcp_listen(host: string, port: int) -> int
        self.body.push_str(&format!(
            "define i64 @tcp_listen(ptr %host, i64 %port) {{\n\
             entry:\n\
             \x20 %fd = call i32 @socket(i32 2, i32 1, i32 0)\n\
             \x20 %fd64 = sext i32 %fd to i64\n\
             \x20 %fd_fail = icmp slt i32 %fd, 0\n\
             \x20 br i1 %fd_fail, label %fail, label %set_reuse\n\
             set_reuse:\n\
             \x20 %one = alloca i32\n\
             \x20 store i32 1, ptr %one\n\
             \x20 call i32 @setsockopt(i32 %fd, i32 {sol_socket}, i32 {so_reuseaddr}, ptr %one, i32 4)\n\
             \x20 %addr = alloca [16 x i8]\n\
             \x20 call ptr @memset(ptr %addr, i32 0, i64 16)\n\
             {sockaddr_fill}\
             \x20 %port32 = trunc i64 %port to i32\n\
             \x20 %port_n = call i32 @__yorum_htons(i32 %port32)\n\
             \x20 %port16 = trunc i32 %port_n to i16\n\
             \x20 %port_p = getelementptr i8, ptr %addr, i64 {port_offset}\n\
             \x20 store i16 %port16, ptr %port_p\n\
             \x20 %bind_r = call i32 @bind(i32 %fd, ptr %addr, i32 16)\n\
             \x20 %bind_fail = icmp ne i32 %bind_r, 0\n\
             \x20 br i1 %bind_fail, label %close_fail, label %do_listen\n\
             do_listen:\n\
             \x20 %listen_r = call i32 @listen(i32 %fd, i32 128)\n\
             \x20 %listen_fail = icmp ne i32 %listen_r, 0\n\
             \x20 br i1 %listen_fail, label %close_fail, label %success\n\
             success:\n\
             \x20 ret i64 %fd64\n\
             close_fail:\n\
             \x20 call i32 @close(i32 %fd)\n\
             \x20 ret i64 -1\n\
             fail:\n\
             \x20 ret i64 -1\n\
             }}\n\n",
            sol_socket = sol_socket,
            so_reuseaddr = so_reuseaddr,
            sockaddr_fill = if cfg!(target_os = "macos") {
                "\x20 %len_p = getelementptr i8, ptr %addr, i64 0\n\
                 \x20 store i8 16, ptr %len_p\n\
                 \x20 %fam_p = getelementptr i8, ptr %addr, i64 1\n\
                 \x20 store i8 2, ptr %fam_p\n"
            } else {
                "\x20 %fam_p = getelementptr i8, ptr %addr, i64 0\n\
                 \x20 store i16 2, ptr %fam_p\n"
            },
            port_offset = 2,
        ));

        // tcp_accept(fd: int) -> int
        self.body.push_str(
            "define i64 @tcp_accept(i64 %fd) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %client = call i32 @accept(i32 %fd32, ptr null, ptr null)\n\
             \x20 %client64 = sext i32 %client to i64\n\
             \x20 ret i64 %client64\n\
             }\n\n",
        );

        // tcp_send(fd: int, data: string) -> int
        self.body.push_str(
            "define i64 @tcp_send(i64 %fd, ptr %data) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %len = call i64 @strlen(ptr %data)\n\
             \x20 %sent = call i64 @write(i32 %fd32, ptr %data, i64 %len)\n\
             \x20 ret i64 %sent\n\
             }\n\n",
        );

        // tcp_recv(fd: int, max_len: int) -> string
        self.body.push_str(
            "define ptr @tcp_recv(i64 %fd, i64 %max_len) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %buf_sz = add i64 %max_len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 %n = call i64 @read(i32 %fd32, ptr %buf, i64 %max_len)\n\
             \x20 %fail = icmp sle i64 %n, 0\n\
             \x20 br i1 %fail, label %empty, label %ok\n\
             ok:\n\
             \x20 %end_p = getelementptr i8, ptr %buf, i64 %n\n\
             \x20 store i8 0, ptr %end_p\n\
             \x20 ret ptr %buf\n\
             empty:\n\
             \x20 store i8 0, ptr %buf\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );

        // tcp_close(fd: int) -> unit
        self.body.push_str(
            "define void @tcp_close(i64 %fd) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 call i32 @close(i32 %fd32)\n\
             \x20 ret void\n\
             }\n\n",
        );

        // udp_socket() -> int
        self.body.push_str(
            "define i64 @udp_socket() {\n\
             entry:\n\
             \x20 %fd = call i32 @socket(i32 2, i32 2, i32 0)\n\
             \x20 %fd64 = sext i32 %fd to i64\n\
             \x20 ret i64 %fd64\n\
             }\n\n",
        );

        // udp_bind(fd: int, host: string, port: int) -> int
        self.body.push_str(&format!(
            "define i64 @udp_bind(i64 %fd, ptr %host, i64 %port) {{\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %addr = alloca [16 x i8]\n\
             \x20 call ptr @memset(ptr %addr, i32 0, i64 16)\n\
             {sockaddr_fill}\
             \x20 %port32 = trunc i64 %port to i32\n\
             \x20 %port_n = call i32 @__yorum_htons(i32 %port32)\n\
             \x20 %port16 = trunc i32 %port_n to i16\n\
             \x20 %port_p = getelementptr i8, ptr %addr, i64 2\n\
             \x20 store i16 %port16, ptr %port_p\n\
             \x20 %r = call i32 @bind(i32 %fd32, ptr %addr, i32 16)\n\
             \x20 %r64 = sext i32 %r to i64\n\
             \x20 ret i64 %r64\n\
             }}\n\n",
            sockaddr_fill = if cfg!(target_os = "macos") {
                "\x20 %len_p = getelementptr i8, ptr %addr, i64 0\n\
                 \x20 store i8 16, ptr %len_p\n\
                 \x20 %fam_p = getelementptr i8, ptr %addr, i64 1\n\
                 \x20 store i8 2, ptr %fam_p\n"
            } else {
                "\x20 %fam_p = getelementptr i8, ptr %addr, i64 0\n\
                 \x20 store i16 2, ptr %fam_p\n"
            },
        ));

        // udp_send_to(fd: int, data: string, host: string, port: int) -> int
        // Uses getaddrinfo for host resolution
        self.body.push_str(
            "define i64 @udp_send_to(i64 %fd, ptr %data, ptr %host, i64 %port) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %port_buf = alloca [16 x i8]\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %port_buf, i64 16, ptr @.fmt.lld, i64 %port)\n\
             \x20 %hints = alloca [48 x i8]\n\
             \x20 call ptr @memset(ptr %hints, i32 0, i64 48)\n\
             \x20 %ai_family_p = getelementptr i8, ptr %hints, i64 4\n\
             \x20 store i32 0, ptr %ai_family_p\n\
             \x20 %ai_socktype_p = getelementptr i8, ptr %hints, i64 8\n\
             \x20 store i32 2, ptr %ai_socktype_p\n\
             \x20 %result_p = alloca ptr\n\
             \x20 %gai = call i32 @getaddrinfo(ptr %host, ptr %port_buf, ptr %hints, ptr %result_p)\n\
             \x20 %gai_fail = icmp ne i32 %gai, 0\n\
             \x20 br i1 %gai_fail, label %fail, label %resolved\n\
             resolved:\n\
             \x20 %res = load ptr, ptr %result_p\n\
             \x20 %ai_addrlen_p = getelementptr i8, ptr %res, i64 16\n\
             \x20 %ai_addrlen = load i32, ptr %ai_addrlen_p\n\
             \x20 %ai_addr_p = getelementptr i8, ptr %res, i64 32\n\
             \x20 %ai_addr = load ptr, ptr %ai_addr_p\n\
             \x20 %len = call i64 @strlen(ptr %data)\n\
             \x20 %sent = call i64 @sendto(i32 %fd32, ptr %data, i64 %len, i32 0, ptr %ai_addr, i32 %ai_addrlen)\n\
             \x20 call void @freeaddrinfo(ptr %res)\n\
             \x20 ret i64 %sent\n\
             fail:\n\
             \x20 ret i64 -1\n\
             }\n\n",
        );

        // udp_recv_from(fd: int, max_len: int) -> string
        self.body.push_str(
            "define ptr @udp_recv_from(i64 %fd, i64 %max_len) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %buf_sz = add i64 %max_len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 %n = call i64 @recvfrom(i32 %fd32, ptr %buf, i64 %max_len, i32 0, ptr null, ptr null)\n\
             \x20 %fail = icmp sle i64 %n, 0\n\
             \x20 br i1 %fail, label %empty, label %ok\n\
             ok:\n\
             \x20 %end_p = getelementptr i8, ptr %buf, i64 %n\n\
             \x20 store i8 0, ptr %end_p\n\
             \x20 ret ptr %buf\n\
             empty:\n\
             \x20 store i8 0, ptr %buf\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );

        // dns_resolve(hostname: string) -> string
        self.body.push_str(
            "define ptr @dns_resolve(ptr %hostname) {\n\
             entry:\n\
             \x20 %hints = alloca [48 x i8]\n\
             \x20 call ptr @memset(ptr %hints, i32 0, i64 48)\n\
             \x20 %ai_family_p = getelementptr i8, ptr %hints, i64 4\n\
             \x20 store i32 2, ptr %ai_family_p\n\
             \x20 %ai_socktype_p = getelementptr i8, ptr %hints, i64 8\n\
             \x20 store i32 1, ptr %ai_socktype_p\n\
             \x20 %result_p = alloca ptr\n\
             \x20 %gai = call i32 @getaddrinfo(ptr %hostname, ptr null, ptr %hints, ptr %result_p)\n\
             \x20 %gai_fail = icmp ne i32 %gai, 0\n\
             \x20 br i1 %gai_fail, label %fail, label %resolved\n\
             resolved:\n\
             \x20 %res = load ptr, ptr %result_p\n\
             \x20 %ai_addr_p = getelementptr i8, ptr %res, i64 32\n\
             \x20 %ai_addr = load ptr, ptr %ai_addr_p\n\
             \x20 %sin_addr_p = getelementptr i8, ptr %ai_addr, i64 4\n\
             \x20 %buf = call ptr @malloc(i64 46)\n\
             \x20 %r = call ptr @inet_ntop(i32 2, ptr %sin_addr_p, ptr %buf, i32 46)\n\
             \x20 call void @freeaddrinfo(ptr %res)\n\
             \x20 %ntop_fail = icmp eq ptr %r, null\n\
             \x20 br i1 %ntop_fail, label %fail_free, label %success\n\
             success:\n\
             \x20 ret ptr %buf\n\
             fail_free:\n\
             \x20 call void @free(ptr %buf)\n\
             \x20 br label %fail\n\
             fail:\n\
             \x20 %empty = call ptr @malloc(i64 1)\n\
             \x20 store i8 0, ptr %empty\n\
             \x20 ret ptr %empty\n\
             }\n\n",
        );

        // __yorum_parse_url — internal helper, not user-facing
        // Parses "http://host:port/path" or "http://host/path"
        // Stores host, port, path via pointer params
        // Returns 0 on success, -1 on failure
        self.body.push_str(
            "define i32 @__yorum_parse_url(ptr %url, ptr %host_out, ptr %port_out, ptr %path_out) {\n\
             entry:\n\
             \x20 %url_len = call i64 @strlen(ptr %url)\n\
             \x20 ; Skip past \"http://\" (7 chars) or \"https://\" (8 chars)\n\
             \x20 %c0 = getelementptr i8, ptr %url, i64 4\n\
             \x20 %c0v = load i8, ptr %c0\n\
             \x20 %is_s = icmp eq i8 %c0v, 115\n\
             \x20 %skip = select i1 %is_s, i64 8, i64 7\n\
             \x20 %start = getelementptr i8, ptr %url, i64 %skip\n\
             \x20 %rest_len = sub i64 %url_len, %skip\n\
             \x20 ; Find '/' in rest to separate host:port from path\n\
             \x20 %slash_idx = call i64 @__yorum_find_char(ptr %start, i64 %rest_len, i8 47)\n\
             \x20 %no_slash = icmp slt i64 %slash_idx, 0\n\
             \x20 %host_part_len = select i1 %no_slash, i64 %rest_len, i64 %slash_idx\n\
             \x20 ; Extract path\n\
             \x20 br i1 %no_slash, label %default_path, label %has_path\n\
             has_path:\n\
             \x20 %path_start = getelementptr i8, ptr %start, i64 %slash_idx\n\
             \x20 %path_len = sub i64 %rest_len, %slash_idx\n\
             \x20 %path_buf = call ptr @malloc(i64 %path_len)\n\
             \x20 %path_len_plus1 = add i64 %path_len, 1\n\
             \x20 call ptr @memcpy(ptr %path_buf, ptr %path_start, i64 %path_len_plus1)\n\
             \x20 store ptr %path_buf, ptr %path_out\n\
             \x20 br label %parse_host\n\
             default_path:\n\
             \x20 %def_path = call ptr @malloc(i64 2)\n\
             \x20 store i8 47, ptr %def_path\n\
             \x20 %dp1 = getelementptr i8, ptr %def_path, i64 1\n\
             \x20 store i8 0, ptr %dp1\n\
             \x20 store ptr %def_path, ptr %path_out\n\
             \x20 br label %parse_host\n\
             parse_host:\n\
             \x20 ; Find ':' in host part to separate host from port\n\
             \x20 %colon_idx = call i64 @__yorum_find_char(ptr %start, i64 %host_part_len, i8 58)\n\
             \x20 %no_colon = icmp slt i64 %colon_idx, 0\n\
             \x20 br i1 %no_colon, label %default_port, label %has_port\n\
             has_port:\n\
             \x20 ; Host is start[0..colon_idx]\n\
             \x20 %h_len = add i64 %colon_idx, 1\n\
             \x20 %h_buf = call ptr @malloc(i64 %h_len)\n\
             \x20 call ptr @memcpy(ptr %h_buf, ptr %start, i64 %colon_idx)\n\
             \x20 %h_end = getelementptr i8, ptr %h_buf, i64 %colon_idx\n\
             \x20 store i8 0, ptr %h_end\n\
             \x20 store ptr %h_buf, ptr %host_out\n\
             \x20 ; Port is start[colon_idx+1..host_part_len]\n\
             \x20 %port_off = add i64 %colon_idx, 1\n\
             \x20 %port_start = getelementptr i8, ptr %start, i64 %port_off\n\
             \x20 %port_val = call i64 @atol(ptr %port_start)\n\
             \x20 store i64 %port_val, ptr %port_out\n\
             \x20 ret i32 0\n\
             default_port:\n\
             \x20 ; Host is start[0..host_part_len]\n\
             \x20 %h2_len = add i64 %host_part_len, 1\n\
             \x20 %h2_buf = call ptr @malloc(i64 %h2_len)\n\
             \x20 call ptr @memcpy(ptr %h2_buf, ptr %start, i64 %host_part_len)\n\
             \x20 %h2_end = getelementptr i8, ptr %h2_buf, i64 %host_part_len\n\
             \x20 store i8 0, ptr %h2_end\n\
             \x20 store ptr %h2_buf, ptr %host_out\n\
             \x20 store i64 80, ptr %port_out\n\
             \x20 ret i32 0\n\
             }\n\n",
        );

        // __yorum_find_char — internal helper: find first occurrence of char in buffer
        // Returns index or -1
        self.body.push_str(
            "define i64 @__yorum_find_char(ptr %buf, i64 %len, i8 %ch) {\n\
             entry:\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [0, %entry], [%i_next, %cont]\n\
             \x20 %done = icmp sge i64 %i, %len\n\
             \x20 br i1 %done, label %not_found, label %check\n\
             check:\n\
             \x20 %p = getelementptr i8, ptr %buf, i64 %i\n\
             \x20 %v = load i8, ptr %p\n\
             \x20 %eq = icmp eq i8 %v, %ch\n\
             \x20 br i1 %eq, label %found, label %cont\n\
             cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             found:\n\
             \x20 ret i64 %i\n\
             not_found:\n\
             \x20 ret i64 -1\n\
             }\n\n",
        );

        // http_request(method: string, url: string, headers: string, body: string) -> string
        self.body.push_str(
            "define ptr @http_request(ptr %method, ptr %url, ptr %headers, ptr %body) {\n\
             entry:\n\
             \x20 %host_p = alloca ptr\n\
             \x20 %port_p = alloca i64\n\
             \x20 %path_p = alloca ptr\n\
             \x20 %parse_r = call i32 @__yorum_parse_url(ptr %url, ptr %host_p, ptr %port_p, ptr %path_p)\n\
             \x20 %host = load ptr, ptr %host_p\n\
             \x20 %port = load i64, ptr %port_p\n\
             \x20 %path = load ptr, ptr %path_p\n\
             \x20 ; Connect\n\
             \x20 %fd = call i64 @tcp_connect(ptr %host, i64 %port)\n\
             \x20 %fd_fail = icmp slt i64 %fd, 0\n\
             \x20 br i1 %fd_fail, label %fail, label %connected\n\
             connected:\n\
             \x20 ; Build request into a 4096-byte buffer\n\
             \x20 %req = call ptr @malloc(i64 8192)\n\
             \x20 ; \"METHOD /path HTTP/1.0\"\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %req, i64 8192, ptr @.str.http_req_line, ptr %method, ptr %path)\n\
             \x20 ; \"\\r\\nHost: host\\r\\nConnection: close\"\n\
             \x20 %hdr_buf = alloca [512 x i8]\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %hdr_buf, i64 512, ptr @.str.http_host_hdr, ptr %host)\n\
             \x20 call ptr @strcat(ptr %req, ptr %hdr_buf)\n\
             \x20 ; Append user headers if non-empty\n\
             \x20 %hdr_len = call i64 @strlen(ptr %headers)\n\
             \x20 %has_hdrs = icmp sgt i64 %hdr_len, 0\n\
             \x20 br i1 %has_hdrs, label %add_hdrs, label %check_body\n\
             add_hdrs:\n\
             \x20 call ptr @strcat(ptr %req, ptr @.str.http_crlf)\n\
             \x20 call ptr @strcat(ptr %req, ptr %headers)\n\
             \x20 br label %check_body\n\
             check_body:\n\
             \x20 %body_len = call i64 @strlen(ptr %body)\n\
             \x20 %has_body = icmp sgt i64 %body_len, 0\n\
             \x20 br i1 %has_body, label %add_cl, label %finish_req\n\
             add_cl:\n\
             \x20 ; Content-Length header\n\
             \x20 %cl_buf = alloca [64 x i8]\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %cl_buf, i64 64, ptr @.str.http_cl_hdr, i64 %body_len)\n\
             \x20 call ptr @strcat(ptr %req, ptr %cl_buf)\n\
             \x20 br label %finish_req\n\
             finish_req:\n\
             \x20 ; \\r\\n\\r\\n\n\
             \x20 call ptr @strcat(ptr %req, ptr @.str.http_crlf2)\n\
             \x20 ; Append body if present\n\
             \x20 %has_body2 = icmp sgt i64 %body_len, 0\n\
             \x20 br i1 %has_body2, label %add_body, label %send_req\n\
             add_body:\n\
             \x20 call ptr @strcat(ptr %req, ptr %body)\n\
             \x20 br label %send_req\n\
             send_req:\n\
             \x20 %req_len = call i64 @strlen(ptr %req)\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 call i64 @write(i32 %fd32, ptr %req, i64 %req_len)\n\
             \x20 call void @free(ptr %req)\n\
             \x20 ; Read response in a loop\n\
             \x20 %resp_cap_init = add i64 0, 4096\n\
             \x20 %resp_buf = call ptr @malloc(i64 %resp_cap_init)\n\
             \x20 br label %read_loop\n\
             read_loop:\n\
             \x20 %resp_off = phi i64 [0, %send_req], [%new_off, %grow_done]\n\
             \x20 %resp_ptr = phi ptr [%resp_buf, %send_req], [%new_ptr, %grow_done]\n\
             \x20 %resp_cap = phi i64 [4096, %send_req], [%new_cap, %grow_done]\n\
             \x20 %space = sub i64 %resp_cap, %resp_off\n\
             \x20 %read_dst = getelementptr i8, ptr %resp_ptr, i64 %resp_off\n\
             \x20 %n = call i64 @read(i32 %fd32, ptr %read_dst, i64 %space)\n\
             \x20 %read_done = icmp sle i64 %n, 0\n\
             \x20 br i1 %read_done, label %read_end, label %got_data\n\
             got_data:\n\
             \x20 %new_off = add i64 %resp_off, %n\n\
             \x20 %need_grow = icmp sge i64 %new_off, %resp_cap\n\
             \x20 br i1 %need_grow, label %grow, label %grow_done\n\
             grow:\n\
             \x20 %doubled = mul i64 %resp_cap, 2\n\
             \x20 %grown = call ptr @realloc(ptr %resp_ptr, i64 %doubled)\n\
             \x20 br label %grow_done\n\
             grow_done:\n\
             \x20 %new_ptr = phi ptr [%resp_ptr, %got_data], [%grown, %grow]\n\
             \x20 %new_cap = phi i64 [%resp_cap, %got_data], [%doubled, %grow]\n\
             \x20 br label %read_loop\n\
             read_end:\n\
             \x20 call i32 @close(i32 %fd32)\n\
             \x20 ; Null-terminate\n\
             \x20 %term_p = getelementptr i8, ptr %resp_ptr, i64 %resp_off\n\
             \x20 store i8 0, ptr %term_p\n\
             \x20 ; Find \\r\\n\\r\\n to strip headers\n\
             \x20 %sep = call ptr @strstr(ptr %resp_ptr, ptr @.str.http_sep)\n\
             \x20 %no_sep = icmp eq ptr %sep, null\n\
             \x20 br i1 %no_sep, label %return_all, label %strip_headers\n\
             strip_headers:\n\
             \x20 %body_start = getelementptr i8, ptr %sep, i64 4\n\
             \x20 %body_sz = call i64 @strlen(ptr %body_start)\n\
             \x20 %body_buf_sz = add i64 %body_sz, 1\n\
             \x20 %body_buf = call ptr @malloc(i64 %body_buf_sz)\n\
             \x20 call ptr @memcpy(ptr %body_buf, ptr %body_start, i64 %body_buf_sz)\n\
             \x20 call void @free(ptr %resp_ptr)\n\
             \x20 ret ptr %body_buf\n\
             return_all:\n\
             \x20 ret ptr %resp_ptr\n\
             fail:\n\
             \x20 %empty = call ptr @malloc(i64 1)\n\
             \x20 store i8 0, ptr %empty\n\
             \x20 ret ptr %empty\n\
             }\n\n",
        );

        // http_get(url: string) -> string
        self.body.push_str(
            "define ptr @http_get(ptr %url) {\n\
             entry:\n\
             \x20 %r = call ptr @http_request(ptr @.str.http_get_method, ptr %url, ptr @.str.empty, ptr @.str.empty)\n\
             \x20 ret ptr %r\n\
             }\n\n",
        );

        // http_post(url: string, body: string) -> string
        self.body.push_str(
            "define ptr @http_post(ptr %url, ptr %body) {\n\
             entry:\n\
             \x20 %r = call ptr @http_request(ptr @.str.http_post_method, ptr %url, ptr @.str.empty, ptr %body)\n\
             \x20 ret ptr %r\n\
             }\n\n",
        );

        // htons helper — byte-swap for network byte order
        self.body.push_str(
            "define i32 @__yorum_htons(i32 %x) {\n\
             entry:\n\
             \x20 %lo = and i32 %x, 255\n\
             \x20 %hi = lshr i32 %x, 8\n\
             \x20 %hi_masked = and i32 %hi, 255\n\
             \x20 %lo_shift = shl i32 %lo, 8\n\
             \x20 %result = or i32 %lo_shift, %hi_masked\n\
             \x20 ret i32 %result\n\
             }\n\n",
        );
    }

    // ── Generic Map/Set helper emission ──────────────────────

    /// Scan the program for Map/Set instantiations and emit helpers for each.
    pub(crate) fn emit_generic_map_set_helpers(&mut self, program: &Program) {
        let mut map_suffixes: Vec<(String, String, String)> = Vec::new(); // (suffix, key_llvm, val_llvm)
        let mut set_suffixes: Vec<(String, String)> = Vec::new(); // (suffix, elem_llvm)
                                                                  // Scan all let bindings for Map__* and Set__* types
        for decl in &program.declarations {
            let fns = match decl {
                Declaration::Function(f) => vec![f],
                Declaration::Impl(i) => i.methods.iter().collect(),
                _ => continue,
            };
            for f in fns {
                Self::collect_map_set_suffixes_from_block(
                    &f.body,
                    &mut map_suffixes,
                    &mut set_suffixes,
                );
            }
        }
        // Emit map helpers (deduplicated)
        let mut seen: HashSet<String> = HashSet::new();
        for (suffix, key_llvm, val_llvm) in &map_suffixes {
            if suffix == "string__int" {
                continue; // Already have hardcoded helpers + aliases
            }
            if seen.contains(suffix) {
                continue;
            }
            seen.insert(suffix.clone());
            self.emit_map_helpers_for_suffix(suffix, key_llvm, val_llvm);
        }
        // Emit set helpers (deduplicated)
        let mut seen_set: HashSet<String> = HashSet::new();
        for (suffix, elem_llvm) in &set_suffixes {
            if seen_set.contains(suffix) {
                continue;
            }
            seen_set.insert(suffix.clone());
            self.emit_set_helpers_for_suffix(suffix, elem_llvm);
        }
    }

    pub(crate) fn collect_map_set_suffixes_from_block(
        block: &Block,
        map_out: &mut Vec<(String, String, String)>,
        set_out: &mut Vec<(String, String)>,
    ) {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Let(s) => {
                    if let Type::Named(ref name) = s.ty {
                        if let Some(suffix) = name.strip_prefix("Map__") {
                            let parts: Vec<&str> = suffix.splitn(2, "__").collect();
                            if parts.len() == 2 {
                                let key_llvm = Self::yorum_name_to_llvm(parts[0]);
                                let val_llvm = Self::yorum_name_to_llvm(parts[1]);
                                map_out.push((suffix.to_string(), key_llvm, val_llvm));
                            }
                        } else if let Some(suffix) = name.strip_prefix("Set__") {
                            let elem_llvm = Self::yorum_name_to_llvm(suffix);
                            set_out.push((suffix.to_string(), elem_llvm));
                        }
                    }
                }
                Stmt::If(s) => {
                    Self::collect_map_set_suffixes_from_block(&s.then_block, map_out, set_out);
                    if let Some(ref else_branch) = s.else_branch {
                        match else_branch.as_ref() {
                            ElseBranch::ElseIf(elif) => {
                                let if_stmt = Stmt::If(elif.clone());
                                Self::collect_map_set_suffixes_from_block(
                                    &Block {
                                        stmts: vec![if_stmt],
                                        span: elif.span,
                                    },
                                    map_out,
                                    set_out,
                                );
                            }
                            ElseBranch::Else(b) => {
                                Self::collect_map_set_suffixes_from_block(b, map_out, set_out);
                            }
                        }
                    }
                }
                Stmt::While(s) => {
                    Self::collect_map_set_suffixes_from_block(&s.body, map_out, set_out);
                }
                Stmt::For(s) => {
                    Self::collect_map_set_suffixes_from_block(&s.body, map_out, set_out);
                }
                Stmt::Match(s) => {
                    for arm in &s.arms {
                        Self::collect_map_set_suffixes_from_block(&arm.body, map_out, set_out);
                    }
                }
                _ => {}
            }
        }
    }

    pub(crate) fn yorum_name_to_llvm(name: &str) -> String {
        match name {
            "int" => "i64".to_string(),
            "float" => "double".to_string(),
            "bool" => "i1".to_string(),
            "char" => "i8".to_string(),
            "string" => "ptr".to_string(),
            _ => "ptr".to_string(), // Named types are ptr
        }
    }

    pub(crate) fn key_elem_size(key_llvm: &str) -> usize {
        match key_llvm {
            "i64" | "ptr" | "double" => 8,
            "i8" => 1,
            "i1" => 1,
            _ => 8,
        }
    }

    pub(crate) fn val_elem_size(val_llvm: &str) -> usize {
        match val_llvm {
            "i64" | "ptr" | "double" => 8,
            "i8" => 1,
            "i1" => 1,
            _ => 8,
        }
    }

    /// Emit all map helper functions for a specific (K, V) type pair.
    pub(crate) fn emit_map_helpers_for_suffix(
        &mut self,
        suffix: &str,
        key_llvm: &str,
        val_llvm: &str,
    ) {
        if self.emitted_map_helpers.contains(suffix) {
            return;
        }
        self.emitted_map_helpers.insert(suffix.to_string());

        let key_size = Self::key_elem_size(key_llvm);
        let val_size = Self::val_elem_size(val_llvm);
        let key_is_string = key_llvm == "ptr";
        let parts: Vec<&str> = suffix.splitn(2, "__").collect();
        let key_name = parts[0];

        // Emit hash function for this key type (if not already a string hash)
        let hash_fn = if key_is_string {
            "@__yorum_hash_string".to_string()
        } else {
            let hash_name = format!("__yorum_hash_{}", key_name);
            self.emit_hash_function(&hash_name, key_llvm);
            format!("@{}", hash_name)
        };

        // Emit find_slot function
        self.emit_find_slot(suffix, key_llvm, &hash_fn, key_is_string, key_size);

        // Emit grow function
        self.emit_map_grow(suffix, key_llvm, val_llvm, key_size, val_size);

        // Emit map_new
        self.body.push_str(&format!(
            "define ptr @map_new__{}() {{\n\
             entry:\n\
             \x20 %map = call ptr @malloc(i64 48)\n\
             \x20 %kb = mul i64 16, {}\n\
             \x20 %keys = call ptr @malloc(i64 %kb)\n\
             \x20 %vb = mul i64 16, {}\n\
             \x20 %vals = call ptr @malloc(i64 %vb)\n\
             \x20 %flags = call ptr @malloc(i64 16)\n\
             \x20 call ptr @memset(ptr %flags, i32 0, i64 16)\n\
             \x20 store ptr %keys, ptr %map\n\
             \x20 %vp = getelementptr i8, ptr %map, i64 8\n\
             \x20 store ptr %vals, ptr %vp\n\
             \x20 %fp = getelementptr i8, ptr %map, i64 16\n\
             \x20 store ptr %flags, ptr %fp\n\
             \x20 %cp = getelementptr i8, ptr %map, i64 24\n\
             \x20 store i64 16, ptr %cp\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 store i64 0, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %map, i64 40\n\
             \x20 store i64 0, ptr %tp\n\
             \x20 ret ptr %map\n\
             }}\n\n",
            suffix, key_size, val_size
        ));

        // Emit map_set
        let key_copy = if key_is_string {
            "\x20 ; copy key string\n\
             \x20 %klen = call i64 @strlen(ptr %key)\n\
             \x20 %kbuf_sz = add i64 %klen, 1\n\
             \x20 %kbuf = call ptr @malloc(i64 %kbuf_sz)\n\
             \x20 call ptr @strcpy(ptr %kbuf, ptr %key)\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %kslot = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 store ptr %kbuf, ptr %kslot\n"
                .to_string()
        } else {
            format!(
                "\x20 %keys_p = load ptr, ptr %map\n\
                 \x20 %kslot = getelementptr {}, ptr %keys_p, i64 %slot\n\
                 \x20 store {} %key, ptr %kslot\n",
                key_llvm, key_llvm
            )
        };

        self.body.push_str(&format!(
            "define void @map_set__{suffix}(ptr %map, {key_llvm} %key, {val_llvm} %val) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %map, i64 40\n\
             \x20 %tombs = load i64, ptr %tp\n\
             \x20 %used = add i64 %size, %tombs\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %s4 = mul i64 %used, 4\n\
             \x20 %c3 = mul i64 %cap, 3\n\
             \x20 %need_grow = icmp sge i64 %s4, %c3\n\
             \x20 br i1 %need_grow, label %grow, label %find\n\
             grow:\n\
             \x20 call void @__yorum_map_grow__{suffix}(ptr %map)\n\
             \x20 br label %find\n\
             find:\n\
             \x20 %cap2 = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %key, i64 %cap2)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fslot = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fslot\n\
             \x20 %is_occupied = icmp eq i8 %flag, 1\n\
             \x20 br i1 %is_occupied, label %update, label %insert\n\
             insert:\n\
             {key_copy}\
             \x20 store i8 1, ptr %fslot\n\
             \x20 %new_size = add i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 br label %store_val\n\
             update:\n\
             \x20 br label %store_val\n\
             store_val:\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %vslot = getelementptr {val_llvm}, ptr %vals_p, i64 %slot\n\
             \x20 store {val_llvm} %val, ptr %vslot\n\
             \x20 ret void\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            val_llvm = val_llvm,
            key_copy = key_copy,
        ));

        // Emit map_get
        self.body.push_str(&format!(
            "define {val_llvm} @map_get__{suffix}(ptr %map, {key_llvm} %key) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 br i1 %found, label %ok, label %fail\n\
             fail:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.map_key_generic)\n\
             \x20 call void @abort()\n\
             \x20 unreachable\n\
             ok:\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %vp = getelementptr {val_llvm}, ptr %vals_p, i64 %slot\n\
             \x20 %v = load {val_llvm}, ptr %vp\n\
             \x20 ret {val_llvm} %v\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            val_llvm = val_llvm,
        ));

        // Emit map_has
        self.body.push_str(&format!(
            "define i1 @map_has__{suffix}(ptr %map, {key_llvm} %key) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 ret i1 %found\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
        ));

        // Emit map_size (same for all types)
        self.body.push_str(&format!(
            "define i64 @map_size__{suffix}(ptr %map) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 ret i64 %size\n\
             }}\n\n",
            suffix = suffix,
        ));

        // Emit map_remove
        let key_free = if key_is_string {
            "\x20 %keys_p = load ptr, ptr %map\n\
             \x20 %kp = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 %k = load ptr, ptr %kp\n\
             \x20 call void @free(ptr %k)\n"
                .to_string()
        } else {
            String::new()
        };

        self.body.push_str(&format!(
            "define i1 @map_remove__{suffix}(ptr %map, {key_llvm} %key) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 br i1 %found, label %remove, label %done\n\
             remove:\n\
             \x20 store i8 2, ptr %fp\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %new_size = sub i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 %tomb_p = getelementptr i8, ptr %map, i64 40\n\
             \x20 %tombs = load i64, ptr %tomb_p\n\
             \x20 %new_tombs = add i64 %tombs, 1\n\
             \x20 store i64 %new_tombs, ptr %tomb_p\n\
             {key_free}\
             \x20 ret i1 1\n\
             done:\n\
             \x20 ret i1 0\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            key_free = key_free,
        ));

        // Emit map_keys — returns [K] array
        self.body.push_str(&format!(
            "define ptr @map_keys__{suffix}(ptr %map) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %dbytes = mul i64 %size, {key_size}\n\
             \x20 %data = call ptr @malloc(i64 %dbytes)\n\
             \x20 %fd = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %fd\n\
             \x20 %fl = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %fl\n\
             \x20 %fc = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %size, ptr %fc\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %loop_cont ]\n\
             \x20 %out_idx = phi i64 [ 0, %entry ], [ %out_next, %loop_cont ]\n\
             \x20 %done = icmp sge i64 %i, %cap\n\
             \x20 br i1 %done, label %finish, label %check\n\
             check:\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %i\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %occ = icmp eq i8 %flag, 1\n\
             \x20 br i1 %occ, label %copy_key, label %skip\n\
             copy_key:\n\
             \x20 %kp = getelementptr {key_llvm}, ptr %keys_p, i64 %i\n\
             \x20 %k = load {key_llvm}, ptr %kp\n\
             \x20 %dp = getelementptr {key_llvm}, ptr %data, i64 %out_idx\n\
             \x20 store {key_llvm} %k, ptr %dp\n\
             \x20 %out_inc = add i64 %out_idx, 1\n\
             \x20 br label %loop_cont\n\
             skip:\n\
             \x20 br label %loop_cont\n\
             loop_cont:\n\
             \x20 %out_next = phi i64 [ %out_inc, %copy_key ], [ %out_idx, %skip ]\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             finish:\n\
             \x20 store i64 %out_idx, ptr %fl\n\
             \x20 ret ptr %fat\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            key_size = key_size,
        ));

        // Emit map_values — returns [V] array
        self.body.push_str(&format!(
            "define ptr @map_values__{suffix}(ptr %map) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %dbytes = mul i64 %size, {val_size}\n\
             \x20 %data = call ptr @malloc(i64 %dbytes)\n\
             \x20 %fd = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %fd\n\
             \x20 %fl = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %fl\n\
             \x20 %fc = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %size, ptr %fc\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %loop_cont ]\n\
             \x20 %out_idx = phi i64 [ 0, %entry ], [ %out_next, %loop_cont ]\n\
             \x20 %done = icmp sge i64 %i, %cap\n\
             \x20 br i1 %done, label %finish, label %check\n\
             check:\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %i\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %occ = icmp eq i8 %flag, 1\n\
             \x20 br i1 %occ, label %copy_val, label %skip\n\
             copy_val:\n\
             \x20 %vp = getelementptr {val_llvm}, ptr %vals_p, i64 %i\n\
             \x20 %v = load {val_llvm}, ptr %vp\n\
             \x20 %dp = getelementptr {val_llvm}, ptr %data, i64 %out_idx\n\
             \x20 store {val_llvm} %v, ptr %dp\n\
             \x20 %out_inc = add i64 %out_idx, 1\n\
             \x20 br label %loop_cont\n\
             skip:\n\
             \x20 br label %loop_cont\n\
             loop_cont:\n\
             \x20 %out_next = phi i64 [ %out_inc, %copy_val ], [ %out_idx, %skip ]\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             finish:\n\
             \x20 store i64 %out_idx, ptr %fl\n\
             \x20 ret ptr %fat\n\
             }}\n\n",
            suffix = suffix,
            val_llvm = val_llvm,
            val_size = val_size,
        ));

        // Register fn_ret_types for all mangled names
        let key_name_str = parts[0];
        let val_name_str = parts[1];
        let key_type = Self::yorum_name_to_ast_type(key_name_str);
        let val_type = Self::yorum_name_to_ast_type(val_name_str);

        self.fn_ret_types.insert(
            format!("map_new__{}", suffix),
            Type::Generic("Map".to_string(), vec![key_type.clone(), val_type.clone()]),
        );
        self.fn_ret_types
            .insert(format!("map_set__{}", suffix), Type::Unit);
        self.fn_ret_types
            .insert(format!("map_get__{}", suffix), val_type.clone());
        self.fn_ret_types
            .insert(format!("map_has__{}", suffix), Type::Bool);
        self.fn_ret_types
            .insert(format!("map_size__{}", suffix), Type::Int);
        self.fn_ret_types
            .insert(format!("map_remove__{}", suffix), Type::Bool);
        self.fn_ret_types.insert(
            format!("map_keys__{}", suffix),
            Type::Array(Box::new(key_type)),
        );
        self.fn_ret_types.insert(
            format!("map_values__{}", suffix),
            Type::Array(Box::new(val_type)),
        );

        // Add format string for generic map key error
        if !self.globals.contains("@.fmt.map_key_generic") {
            self.globals.push_str(
                "@.fmt.map_key_generic = private constant [23 x i8] c\"map_get: key not found\\00\"\n",
            );
        }
    }

    pub(crate) fn yorum_name_to_ast_type(name: &str) -> Type {
        match name {
            "int" => Type::Int,
            "float" => Type::Float,
            "bool" => Type::Bool,
            "char" => Type::Char,
            "string" => Type::Str,
            _ => Type::Named(name.to_string()),
        }
    }

    pub(crate) fn emit_hash_function(&mut self, name: &str, key_llvm: &str) {
        // For non-string keys: bit-mix hash
        match key_llvm {
            "i64" => {
                self.body.push_str(&format!(
                    "define i64 @{}(i64 %x) {{\n\
                     entry:\n\
                     \x20 %x1 = xor i64 %x, -4658895280553007687\n\
                     \x20 %x2 = mul i64 %x1, -7723592293110705685\n\
                     \x20 %x3 = lshr i64 %x2, 33\n\
                     \x20 %x4 = xor i64 %x2, %x3\n\
                     \x20 %x5 = mul i64 %x4, -4658895280553007687\n\
                     \x20 ret i64 %x5\n\
                     }}\n\n",
                    name
                ));
            }
            "i8" => {
                self.body.push_str(&format!(
                    "define i64 @{}(i8 %x) {{\n\
                     entry:\n\
                     \x20 %x64 = zext i8 %x to i64\n\
                     \x20 %x1 = xor i64 %x64, -4658895280553007687\n\
                     \x20 %x2 = mul i64 %x1, -7723592293110705685\n\
                     \x20 ret i64 %x2\n\
                     }}\n\n",
                    name
                ));
            }
            "i1" => {
                self.body.push_str(&format!(
                    "define i64 @{}(i1 %x) {{\n\
                     entry:\n\
                     \x20 %x64 = zext i1 %x to i64\n\
                     \x20 %x1 = xor i64 %x64, -4658895280553007687\n\
                     \x20 %x2 = mul i64 %x1, -7723592293110705685\n\
                     \x20 ret i64 %x2\n\
                     }}\n\n",
                    name
                ));
            }
            _ => {
                // Fallback: treat as i64 (shouldn't happen for hashable types)
                self.body.push_str(&format!(
                    "define i64 @{}(i64 %x) {{\n\
                     entry:\n\
                     \x20 ret i64 %x\n\
                     }}\n\n",
                    name
                ));
            }
        }
    }

    pub(crate) fn emit_find_slot(
        &mut self,
        suffix: &str,
        key_llvm: &str,
        hash_fn: &str,
        key_is_string: bool,
        key_size: usize,
    ) {
        let _ = key_size;
        let key_compare = if key_is_string {
            "\x20 %k = load ptr, ptr %kp\n\
             \x20 %cmp = call i32 @strcmp(ptr %k, ptr %key)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n"
                .to_string()
        } else {
            format!(
                "\x20 %k = load {}, ptr %kp\n\
                 \x20 %eq = icmp eq {} %k, %key\n",
                key_llvm, key_llvm
            )
        };

        self.body.push_str(&format!(
            "define i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %key, i64 %cap) {{\n\
             entry:\n\
             \x20 %hash = call i64 {hash_fn}({key_llvm} %key)\n\
             \x20 %mask = sub i64 %cap, 1\n\
             \x20 %start = and i64 %hash, %mask\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %probe\n\
             probe:\n\
             \x20 %idx = phi i64 [ %start, %entry ], [ %next, %advance ]\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %idx\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %is_empty = icmp eq i8 %flag, 0\n\
             \x20 br i1 %is_empty, label %done, label %check_tombstone\n\
             check_tombstone:\n\
             \x20 %is_tomb = icmp eq i8 %flag, 2\n\
             \x20 br i1 %is_tomb, label %advance, label %check_key\n\
             check_key:\n\
             \x20 %kp = getelementptr {key_llvm}, ptr %keys_p, i64 %idx\n\
             {key_compare}\
             \x20 br i1 %eq, label %done, label %advance\n\
             advance:\n\
             \x20 %next_raw = add i64 %idx, 1\n\
             \x20 %next = and i64 %next_raw, %mask\n\
             \x20 br label %probe\n\
             done:\n\
             \x20 ret i64 %idx\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            hash_fn = hash_fn,
            key_compare = key_compare,
        ));
    }

    pub(crate) fn emit_map_grow(
        &mut self,
        suffix: &str,
        key_llvm: &str,
        val_llvm: &str,
        key_size: usize,
        val_size: usize,
    ) {
        self.body.push_str(&format!(
            "define void @__yorum_map_grow__{suffix}(ptr %map) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %old_cap = load i64, ptr %cap_p\n\
             \x20 %new_cap = mul i64 %old_cap, 2\n\
             \x20 %kb = mul i64 %new_cap, {key_size}\n\
             \x20 %vb = mul i64 %new_cap, {val_size}\n\
             \x20 %new_keys = call ptr @malloc(i64 %kb)\n\
             \x20 %new_vals = call ptr @malloc(i64 %vb)\n\
             \x20 %new_flags = call ptr @malloc(i64 %new_cap)\n\
             \x20 call ptr @memset(ptr %new_flags, i32 0, i64 %new_cap)\n\
             \x20 %old_keys = load ptr, ptr %map\n\
             \x20 %vals_p = getelementptr i8, ptr %map, i64 8\n\
             \x20 %old_vals = load ptr, ptr %vals_p\n\
             \x20 %flags_p = getelementptr i8, ptr %map, i64 16\n\
             \x20 %old_flags = load ptr, ptr %flags_p\n\
             \x20 store ptr %new_keys, ptr %map\n\
             \x20 store ptr %new_vals, ptr %vals_p\n\
             \x20 store ptr %new_flags, ptr %flags_p\n\
             \x20 store i64 %new_cap, ptr %cap_p\n\
             \x20 %tomb_p = getelementptr i8, ptr %map, i64 40\n\
             \x20 store i64 0, ptr %tomb_p\n\
             \x20 br label %rehash_loop\n\
             rehash_loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %rehash_cont ]\n\
             \x20 %cmp = icmp slt i64 %i, %old_cap\n\
             \x20 br i1 %cmp, label %rehash_body, label %rehash_done\n\
             rehash_body:\n\
             \x20 %ofp = getelementptr i8, ptr %old_flags, i64 %i\n\
             \x20 %of = load i8, ptr %ofp\n\
             \x20 %occ = icmp eq i8 %of, 1\n\
             \x20 br i1 %occ, label %rehash_insert, label %rehash_cont\n\
             rehash_insert:\n\
             \x20 %okp = getelementptr {key_llvm}, ptr %old_keys, i64 %i\n\
             \x20 %ok = load {key_llvm}, ptr %okp\n\
             \x20 %ovp = getelementptr {val_llvm}, ptr %old_vals, i64 %i\n\
             \x20 %ov = load {val_llvm}, ptr %ovp\n\
             \x20 %slot = call i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %ok, i64 %new_cap)\n\
             \x20 %nkp = getelementptr {key_llvm}, ptr %new_keys, i64 %slot\n\
             \x20 store {key_llvm} %ok, ptr %nkp\n\
             \x20 %nvp = getelementptr {val_llvm}, ptr %new_vals, i64 %slot\n\
             \x20 store {val_llvm} %ov, ptr %nvp\n\
             \x20 %nfp = getelementptr i8, ptr %new_flags, i64 %slot\n\
             \x20 store i8 1, ptr %nfp\n\
             \x20 br label %rehash_cont\n\
             rehash_cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %rehash_loop\n\
             rehash_done:\n\
             \x20 call void @free(ptr %old_keys)\n\
             \x20 call void @free(ptr %old_vals)\n\
             \x20 call void @free(ptr %old_flags)\n\
             \x20 ret void\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            val_llvm = val_llvm,
            key_size = key_size,
            val_size = val_size,
        ));
    }

    /// Emit all set helper functions for a specific T type.
    /// Set struct layout (40 bytes):
    ///   offset 0:  ptr   keys
    ///   offset 8:  ptr   flags
    ///   offset 16: i64   capacity
    ///   offset 24: i64   size
    ///   offset 32: i64   tombstones
    pub(crate) fn emit_set_helpers_for_suffix(&mut self, suffix: &str, elem_llvm: &str) {
        let elem_size = Self::key_elem_size(elem_llvm);
        let elem_is_string = elem_llvm == "ptr";

        // Reuse hash function from map if already emitted, or emit new one
        let hash_fn = if elem_is_string {
            "@__yorum_hash_string".to_string()
        } else {
            let hash_name = format!("__yorum_hash_{}", suffix);
            // Check if hash already emitted (might share with Map)
            if !self.body.contains(&format!("@{}", hash_name)) {
                self.emit_hash_function(&hash_name, elem_llvm);
            }
            format!("@{}", hash_name)
        };

        // Emit find_slot for set
        let key_compare = if elem_is_string {
            "\x20 %k = load ptr, ptr %kp\n\
             \x20 %cmp = call i32 @strcmp(ptr %k, ptr %key)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n"
                .to_string()
        } else {
            format!(
                "\x20 %k = load {}, ptr %kp\n\
                 \x20 %eq = icmp eq {} %k, %key\n",
                elem_llvm, elem_llvm
            )
        };

        self.body.push_str(&format!(
            "define i64 @__yorum_set_find_slot__{suffix}(ptr %set, {elem_llvm} %key, i64 %cap) {{\n\
             entry:\n\
             \x20 %hash = call i64 {hash_fn}({elem_llvm} %key)\n\
             \x20 %mask = sub i64 %cap, 1\n\
             \x20 %start = and i64 %hash, %mask\n\
             \x20 %keys_p = load ptr, ptr %set\n\
             \x20 %flags_pp = getelementptr i8, ptr %set, i64 8\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %probe\n\
             probe:\n\
             \x20 %idx = phi i64 [ %start, %entry ], [ %next, %advance ]\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %idx\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %is_empty = icmp eq i8 %flag, 0\n\
             \x20 br i1 %is_empty, label %done, label %check_tombstone\n\
             check_tombstone:\n\
             \x20 %is_tomb = icmp eq i8 %flag, 2\n\
             \x20 br i1 %is_tomb, label %advance, label %check_key\n\
             check_key:\n\
             \x20 %kp = getelementptr {elem_llvm}, ptr %keys_p, i64 %idx\n\
             {key_compare}\
             \x20 br i1 %eq, label %done, label %advance\n\
             advance:\n\
             \x20 %next_raw = add i64 %idx, 1\n\
             \x20 %next = and i64 %next_raw, %mask\n\
             \x20 br label %probe\n\
             done:\n\
             \x20 ret i64 %idx\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
            hash_fn = hash_fn,
            key_compare = key_compare,
        ));

        // Emit set grow
        self.body.push_str(&format!(
            "define void @__yorum_set_grow__{suffix}(ptr %set) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %set, i64 16\n\
             \x20 %old_cap = load i64, ptr %cap_p\n\
             \x20 %new_cap = mul i64 %old_cap, 2\n\
             \x20 %kb = mul i64 %new_cap, {elem_size}\n\
             \x20 %new_keys = call ptr @malloc(i64 %kb)\n\
             \x20 %new_flags = call ptr @malloc(i64 %new_cap)\n\
             \x20 call ptr @memset(ptr %new_flags, i32 0, i64 %new_cap)\n\
             \x20 %old_keys = load ptr, ptr %set\n\
             \x20 %flags_p = getelementptr i8, ptr %set, i64 8\n\
             \x20 %old_flags = load ptr, ptr %flags_p\n\
             \x20 store ptr %new_keys, ptr %set\n\
             \x20 store ptr %new_flags, ptr %flags_p\n\
             \x20 store i64 %new_cap, ptr %cap_p\n\
             \x20 %tomb_p = getelementptr i8, ptr %set, i64 32\n\
             \x20 store i64 0, ptr %tomb_p\n\
             \x20 br label %rehash_loop\n\
             rehash_loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %rehash_cont ]\n\
             \x20 %cmp = icmp slt i64 %i, %old_cap\n\
             \x20 br i1 %cmp, label %rehash_body, label %rehash_done\n\
             rehash_body:\n\
             \x20 %ofp = getelementptr i8, ptr %old_flags, i64 %i\n\
             \x20 %of = load i8, ptr %ofp\n\
             \x20 %occ = icmp eq i8 %of, 1\n\
             \x20 br i1 %occ, label %rehash_insert, label %rehash_cont\n\
             rehash_insert:\n\
             \x20 %okp = getelementptr {elem_llvm}, ptr %old_keys, i64 %i\n\
             \x20 %ok = load {elem_llvm}, ptr %okp\n\
             \x20 %slot = call i64 @__yorum_set_find_slot__{suffix}(ptr %set, {elem_llvm} %ok, i64 %new_cap)\n\
             \x20 %nkp = getelementptr {elem_llvm}, ptr %new_keys, i64 %slot\n\
             \x20 store {elem_llvm} %ok, ptr %nkp\n\
             \x20 %nfp = getelementptr i8, ptr %new_flags, i64 %slot\n\
             \x20 store i8 1, ptr %nfp\n\
             \x20 br label %rehash_cont\n\
             rehash_cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %rehash_loop\n\
             rehash_done:\n\
             \x20 call void @free(ptr %old_keys)\n\
             \x20 call void @free(ptr %old_flags)\n\
             \x20 ret void\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
            elem_size = elem_size,
        ));

        // set_new — allocate 40-byte set struct with capacity 16
        self.body.push_str(&format!(
            "define ptr @set_new__{suffix}() {{\n\
             entry:\n\
             \x20 %set = call ptr @malloc(i64 40)\n\
             \x20 %kb = mul i64 16, {elem_size}\n\
             \x20 %keys = call ptr @malloc(i64 %kb)\n\
             \x20 %flags = call ptr @malloc(i64 16)\n\
             \x20 call ptr @memset(ptr %flags, i32 0, i64 16)\n\
             \x20 store ptr %keys, ptr %set\n\
             \x20 %fp = getelementptr i8, ptr %set, i64 8\n\
             \x20 store ptr %flags, ptr %fp\n\
             \x20 %cp = getelementptr i8, ptr %set, i64 16\n\
             \x20 store i64 16, ptr %cp\n\
             \x20 %sp = getelementptr i8, ptr %set, i64 24\n\
             \x20 store i64 0, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %set, i64 32\n\
             \x20 store i64 0, ptr %tp\n\
             \x20 ret ptr %set\n\
             }}\n\n",
            suffix = suffix,
            elem_size = elem_size,
        ));

        // set_add — insert element into set
        let key_copy = if elem_is_string {
            "\x20 %klen = call i64 @strlen(ptr %key)\n\
             \x20 %kbuf_sz = add i64 %klen, 1\n\
             \x20 %kbuf = call ptr @malloc(i64 %kbuf_sz)\n\
             \x20 call ptr @strcpy(ptr %kbuf, ptr %key)\n\
             \x20 %keys_p = load ptr, ptr %set\n\
             \x20 %kslot = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 store ptr %kbuf, ptr %kslot\n"
                .to_string()
        } else {
            format!(
                "\x20 %keys_p = load ptr, ptr %set\n\
                 \x20 %kslot = getelementptr {}, ptr %keys_p, i64 %slot\n\
                 \x20 store {} %key, ptr %kslot\n",
                elem_llvm, elem_llvm
            )
        };

        self.body.push_str(&format!(
            "define void @set_add__{suffix}(ptr %set, {elem_llvm} %key) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %set, i64 24\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %set, i64 32\n\
             \x20 %tombs = load i64, ptr %tp\n\
             \x20 %used = add i64 %size, %tombs\n\
             \x20 %cap_p = getelementptr i8, ptr %set, i64 16\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %s4 = mul i64 %used, 4\n\
             \x20 %c3 = mul i64 %cap, 3\n\
             \x20 %need_grow = icmp sge i64 %s4, %c3\n\
             \x20 br i1 %need_grow, label %grow, label %find\n\
             grow:\n\
             \x20 call void @__yorum_set_grow__{suffix}(ptr %set)\n\
             \x20 br label %find\n\
             find:\n\
             \x20 %cap2 = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_set_find_slot__{suffix}(ptr %set, {elem_llvm} %key, i64 %cap2)\n\
             \x20 %flags_pp = getelementptr i8, ptr %set, i64 8\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fslot = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fslot\n\
             \x20 %is_occupied = icmp eq i8 %flag, 1\n\
             \x20 br i1 %is_occupied, label %done, label %insert\n\
             insert:\n\
             {key_copy}\
             \x20 store i8 1, ptr %fslot\n\
             \x20 %new_size = add i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 br label %done\n\
             done:\n\
             \x20 ret void\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
            key_copy = key_copy,
        ));

        // set_has — check if element exists
        self.body.push_str(&format!(
            "define i1 @set_has__{suffix}(ptr %set, {elem_llvm} %key) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %set, i64 16\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_set_find_slot__{suffix}(ptr %set, {elem_llvm} %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %set, i64 8\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 ret i1 %found\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
        ));

        // set_remove — mark slot as tombstone
        let key_free = if elem_is_string {
            "\x20 %keys_p = load ptr, ptr %set\n\
             \x20 %kp = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 %k = load ptr, ptr %kp\n\
             \x20 call void @free(ptr %k)\n"
                .to_string()
        } else {
            String::new()
        };

        self.body.push_str(&format!(
            "define i1 @set_remove__{suffix}(ptr %set, {elem_llvm} %key) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %set, i64 16\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_set_find_slot__{suffix}(ptr %set, {elem_llvm} %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %set, i64 8\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 br i1 %found, label %remove, label %done\n\
             remove:\n\
             \x20 store i8 2, ptr %fp\n\
             \x20 %sp = getelementptr i8, ptr %set, i64 24\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %new_size = sub i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 %tomb_p = getelementptr i8, ptr %set, i64 32\n\
             \x20 %tombs = load i64, ptr %tomb_p\n\
             \x20 %new_tombs = add i64 %tombs, 1\n\
             \x20 store i64 %new_tombs, ptr %tomb_p\n\
             {key_free}\
             \x20 ret i1 1\n\
             done:\n\
             \x20 ret i1 0\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
            key_free = key_free,
        ));

        // set_size — load size field
        self.body.push_str(&format!(
            "define i64 @set_size__{suffix}(ptr %set) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %set, i64 24\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 ret i64 %size\n\
             }}\n\n",
            suffix = suffix,
        ));

        // set_values — collect all elements into [T] array
        self.body.push_str(&format!(
            "define ptr @set_values__{suffix}(ptr %set) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %set, i64 24\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %cap_p = getelementptr i8, ptr %set, i64 16\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %dbytes = mul i64 %size, {elem_size}\n\
             \x20 %data = call ptr @malloc(i64 %dbytes)\n\
             \x20 %fd = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %fd\n\
             \x20 %fl = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %fl\n\
             \x20 %fc = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %size, ptr %fc\n\
             \x20 %keys_p = load ptr, ptr %set\n\
             \x20 %flags_pp = getelementptr i8, ptr %set, i64 8\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %loop_cont ]\n\
             \x20 %out_idx = phi i64 [ 0, %entry ], [ %out_next, %loop_cont ]\n\
             \x20 %done = icmp sge i64 %i, %cap\n\
             \x20 br i1 %done, label %finish, label %check\n\
             check:\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %i\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %occ = icmp eq i8 %flag, 1\n\
             \x20 br i1 %occ, label %copy_key, label %skip\n\
             copy_key:\n\
             \x20 %kp = getelementptr {elem_llvm}, ptr %keys_p, i64 %i\n\
             \x20 %k = load {elem_llvm}, ptr %kp\n\
             \x20 %dp = getelementptr {elem_llvm}, ptr %data, i64 %out_idx\n\
             \x20 store {elem_llvm} %k, ptr %dp\n\
             \x20 %out_inc = add i64 %out_idx, 1\n\
             \x20 br label %loop_cont\n\
             skip:\n\
             \x20 br label %loop_cont\n\
             loop_cont:\n\
             \x20 %out_next = phi i64 [ %out_inc, %copy_key ], [ %out_idx, %skip ]\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             finish:\n\
             \x20 store i64 %out_idx, ptr %fl\n\
             \x20 ret ptr %fat\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
            elem_size = elem_size,
        ));

        // Register fn_ret_types for all mangled set names
        let elem_type = Self::yorum_name_to_ast_type(suffix);

        self.fn_ret_types.insert(
            format!("set_new__{}", suffix),
            Type::Generic("Set".to_string(), vec![elem_type.clone()]),
        );
        self.fn_ret_types
            .insert(format!("set_add__{}", suffix), Type::Unit);
        self.fn_ret_types
            .insert(format!("set_has__{}", suffix), Type::Bool);
        self.fn_ret_types
            .insert(format!("set_remove__{}", suffix), Type::Bool);
        self.fn_ret_types
            .insert(format!("set_size__{}", suffix), Type::Int);
        self.fn_ret_types.insert(
            format!("set_values__{}", suffix),
            Type::Array(Box::new(elem_type)),
        );
    }
}
