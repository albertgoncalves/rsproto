use std::ffi::{c_char, c_int, CStr, CString};
use std::marker;
use std::ptr;
use std::time;

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

type GLFWerrorfun = extern "C" fn(error_code: c_int, description: *const c_char);
type GLFWkeyfun = extern "C" fn(*mut GLFWwindow, c_int, c_int, c_int, c_int);

extern "C" {
    fn glfwGetVersionString() -> *const c_char;

    fn glfwInit() -> c_int;
    fn glfwTerminate();

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

    fn glEnable(cap: u32);
    fn glBlendFunc(sfactor: u32, dfactor: u32);

    fn glClearColor(red: f32, green: f32, blue: f32, alpha: f32);
    fn glClear(mask: u32);
}

const GLFW_PRESS: c_int = 1;
const GLFW_KEY_ESCAPE: c_int = 256;

const GL_COLOR_BUFFER_BIT: u32 = 0x0000_4000;
const GL_DEPTH_BUFFER_BIT: u32 = 0x0000_0100;
const GL_STENCIL_BUFFER_BIT: u32 = 0x0000_0400;

const GL_BLEND: u32 = 0x0BE2;
const GL_SRC_ALPHA: u32 = 0x0302;
const GL_ONE_MINUS_SRC_ALPHA: u32 = 0x0303;

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

extern "C" fn callback_error(error_code: c_int, description: *const c_char) {
    let mut message = error_code.to_string();
    if !description.is_null() {
        message.push_str(&format!(
            ": {}",
            unsafe { CStr::from_ptr(description) }.to_str().unwrap(),
        ));
    }
    panic!("{}", message);
}

extern "C" fn callback_key(
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
        unsafe { glfwSetWindowShouldClose(window, 1) }
    }
}

fn main() {
    unsafe {
        let version = CStr::from_ptr(glfwGetVersionString()).to_str().unwrap();
        println!("{version}");

        assert!(glfwInit() == 1);
        defer!(glfwTerminate());

        glfwSetErrorCallback(callback_error);

        let window = glfwCreateWindow(
            1536,
            768,
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
        glfwSetKeyCallback(window, callback_key);

        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glClearColor(0.1, 0.1, 0.1, 1.0);

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
    }
}
