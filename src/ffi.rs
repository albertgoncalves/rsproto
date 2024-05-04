use std::convert::TryInto;
use std::ffi::{c_char, c_float, c_int, c_uint, c_void, CStr, CString};
use std::marker;
use std::ptr;
use std::slice::from_raw_parts;
use std::str::from_utf8_unchecked;
use std::time;

struct Defer<F: FnMut()>(F);

impl<F: FnMut()> Drop for Defer<F> {
    fn drop(&mut self) {
        (self.0)();
    }
}

macro_rules! defer {
    ($expr:expr) => {
        let _defer = Defer(|| {
            $expr;
        });
    };
}

macro_rules! opaque_struct {
    ($name:ident) => {
        // NOTE: See `https://doc.rust-lang.org/nomicon/ffi.html#representing-opaque-structs`.
        #[repr(C)]
        pub struct $name {
            _data: [u8; 0],
            _marker: marker::PhantomData<(*mut u8, marker::PhantomPinned)>,
        }
    };
}

opaque_struct!(GLFWwindow);
opaque_struct!(GLFWmonitor);

type GLenum = c_uint;
type GLbitfield = c_uint;
type GLint = c_int;
type GLuint = c_uint;
type GLsizei = c_int;
type GLclampf = c_float;
type GLchar = c_char;

type GLFWerrorfun = extern "C" fn(error_code: c_int, description: *const c_char);
type GLFWkeyfun = extern "C" fn(*mut GLFWwindow, c_int, c_int, c_int, c_int);

type GLDEBUGPROC = extern "C" fn(
    source: GLenum,
    r#type: GLenum,
    id: GLuint,
    severity: GLenum,
    length: GLsizei,
    message: *const GLchar,
    userParam: *const c_void,
);

extern "C" {
    fn glfwGetVersionString() -> *const c_char;

    fn glfwInit() -> c_int;
    fn glfwTerminate();

    fn glfwWindowHint(hint: c_int, value: c_int);

    fn glfwCreateWindow(
        width: c_int,
        height: c_int,
        title: *const c_char,
        monitor: *mut GLFWmonitor,
        share: *mut GLFWwindow,
    ) -> *mut GLFWwindow;
    fn glfwDestroyWindow(window: *mut GLFWwindow);

    fn glfwSetErrorCallback(callback: GLFWerrorfun) -> GLFWerrorfun;

    fn glfwSetKeyCallback(window: *mut GLFWwindow, callback: GLFWkeyfun) -> GLFWkeyfun;
    fn glfwSetWindowShouldClose(window: *mut GLFWwindow, value: c_int);

    fn glfwMakeContextCurrent(window: *mut GLFWwindow);
    fn glfwSwapInterval(interval: c_int);

    fn glfwWindowShouldClose(window: *mut GLFWwindow) -> c_int;
    fn glfwPollEvents();
    fn glfwSwapBuffers(window: *mut GLFWwindow);

    fn glGetError() -> GLenum;

    // NOTE: See `https://www.khronos.org/opengl/wiki/OpenGL_Error`.
    fn glDebugMessageCallback(callback: GLDEBUGPROC, userParam: *const c_void);
    fn glDebugMessageInsert(
        source: GLenum,
        r#type: GLenum,
        id: GLuint,
        severity: GLenum,
        length: GLsizei,
        buf: *const GLchar,
    );

    fn glViewport(x: GLint, y: GLint, width: GLsizei, height: GLsizei);

    fn glEnable(cap: GLenum);
    fn glBlendFunc(sfactor: GLenum, dfactor: GLenum);

    fn glClearColor(red: GLclampf, green: GLclampf, blue: GLclampf, alpha: GLclampf);
    fn glClear(mask: GLbitfield);

    fn glGenBuffers(n: GLsizei, buffers: *mut GLuint);
}

const GLFW_RESIZABLE: c_int = 0x0002_0003;
const GLFW_SAMPLES: c_int = 0x0002_100D;
const GLFW_CONTEXT_VERSION_MAJOR: c_int = 0x0002_2002;
const GLFW_CONTEXT_VERSION_MINOR: c_int = 0x0002_2003;
const GLFW_OPENGL_DEBUG_CONTEXT: c_int = 0x0002_2007;
const GLFW_OPENGL_PROFILE: c_int = 0x0002_2008;
const GLFW_OPENGL_CORE_PROFILE: c_int = 0x0003_2001;

const GLFW_PRESS: c_int = 1;
const GLFW_KEY_ESCAPE: c_int = 256;

const GL_DEBUG_TYPE_ERROR: GLenum = 0x824C;
const GL_DEBUG_OUTPUT: GLenum = 0x92E0;
const GL_DEBUG_OUTPUT_SYNCHRONOUS: GLenum = 0x8242;

const GL_DEPTH_BUFFER_BIT: GLbitfield = 0x0000_0100;
const GL_STENCIL_BUFFER_BIT: GLbitfield = 0x0000_0400;
const GL_COLOR_BUFFER_BIT: GLbitfield = 0x0000_4000;

const GL_BLEND: GLenum = 0x0BE2;
const GL_MULTISAMPLE: GLenum = 0x809D;

const GL_SRC_ALPHA: GLenum = 0x0302;
const GL_ONE_MINUS_SRC_ALPHA: GLenum = 0x0303;

const GL_DEBUG_SOURCE_APPLICATION: GLenum = 0x824A;
const GL_DEBUG_TYPE_OTHER: GLenum = 0x8251;
const GL_DEBUG_SEVERITY_NOTIFICATION: GLenum = 0x826B;

extern "C" fn callback_glfw_error(error_code: c_int, description: *const c_char) {
    let mut message = error_code.to_string();
    if !description.is_null() {
        message.push_str(&format!(
            ": {}",
            unsafe { CStr::from_ptr(description) }.to_str().unwrap(),
        ));
    }
    panic!("{}", message);
}

extern "C" fn callback_glfw_key(
    window: *mut GLFWwindow,
    key: c_int,
    _scancode: c_int,
    action: c_int,
    _mods: c_int,
) {
    if action != GLFW_PRESS {
        return;
    }
    if key == GLFW_KEY_ESCAPE {
        unsafe {
            glfwSetWindowShouldClose(window, 1);
        }
    }
}

static mut GL_DEBUG_MESSAGES: Vec<String> = Vec::new();

extern "C" fn callback_gl_debug(
    _source: GLenum,
    _type: GLenum,
    _id: GLuint,
    severity: GLenum,
    length: GLsizei,
    message: *const GLchar,
    _userParam: *const c_void,
) {
    assert!(0 < length);
    unsafe {
        let message: &str = from_utf8_unchecked(from_raw_parts(
            message.cast::<u8>(),
            length.try_into().unwrap(),
        ));
        if severity == GL_DEBUG_SEVERITY_NOTIFICATION {
            GL_DEBUG_MESSAGES.push(message.to_owned());
        } else {
            panic!("{}", message);
        }
    }
}

fn main() {
    unsafe {
        let version = CStr::from_ptr(glfwGetVersionString()).to_str().unwrap();
        println!("{version}");

        assert!(glfwInit() == 1);
        defer!(glfwTerminate());

        glfwSetErrorCallback(callback_glfw_error);

        glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, 1);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
        glfwWindowHint(GLFW_RESIZABLE, 0);
        glfwWindowHint(GLFW_SAMPLES, 16);

        let width = 1536;
        let height = 768;

        let window = glfwCreateWindow(
            width,
            height,
            CString::new(std::module_path!())
                .unwrap()
                .as_bytes_with_nul()
                .as_ptr()
                .cast::<c_char>(),
            ptr::null_mut::<GLFWmonitor>(),
            ptr::null_mut::<GLFWwindow>(),
        );
        assert!(!window.is_null());
        defer!(glfwDestroyWindow(window));

        glfwMakeContextCurrent(window);
        glfwSwapInterval(1);
        glfwSetKeyCallback(window, callback_glfw_key);

        glEnable(GL_DEBUG_OUTPUT);
        glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
        glDebugMessageCallback(callback_gl_debug, ptr::null::<c_void>());

        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glClearColor(0.1, 0.1, 0.1, 1.0);
        glEnable(GL_MULTISAMPLE);
        glViewport(0, 0, width, height);

        let mut now = time::Instant::now();
        let mut frames = 0;

        println!("\n");
        while glfwWindowShouldClose(window) != 1 {
            if 0 < now.elapsed().as_secs() {
                println!(
                    "\x1B[2A\
                     {:10} ns/f\n\
                     {frames:10} frames",
                    now.elapsed().as_nanos() / frames,
                );
                now = time::Instant::now();
                frames = 0;
            }

            glfwPollEvents();
            glClear(GL_COLOR_BUFFER_BIT);
            glfwSwapBuffers(window);

            frames += 1;
        }

        let message: &[u8] = b"Hello, world!";
        glDebugMessageInsert(
            GL_DEBUG_SOURCE_APPLICATION,
            GL_DEBUG_TYPE_OTHER,
            0,
            GL_DEBUG_SEVERITY_NOTIFICATION,
            message.len().try_into().unwrap(),
            message.as_ptr().cast::<i8>(),
        );

        #[allow(static_mut_refs)]
        for message in &GL_DEBUG_MESSAGES {
            eprintln!("{message}");
        }
    }
}
