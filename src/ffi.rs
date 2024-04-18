use std::convert::TryInto;
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

    fn glfwSetKeyCallback(window: *mut GLFWwindow, callback: GLFWkeyfun) -> GLFWkeyfun;
    fn glfwSetWindowShouldClose(window: *mut GLFWwindow, value: c_int);

    fn glfwMakeContextCurrent(window: *mut GLFWwindow);
    fn glfwSwapInterval(interval: c_int);

    fn glfwWindowShouldClose(window: *mut GLFWwindow) -> c_int;
    fn glfwPollEvents();
    fn glfwSwapBuffers(window: *mut GLFWwindow);
}

const GLFW_PRESS: c_int = 1;
const GLFW_KEY_ESCAPE: c_int = 256;

struct Glfw {
    window: *mut GLFWwindow,
}

impl Glfw {
    fn new(width: u32, height: u32, callback_key: GLFWkeyfun) -> Self {
        let version = unsafe { CStr::from_ptr(glfwGetVersionString()) }
            .to_str()
            .unwrap();
        println!("{version}");

        assert!(unsafe { glfwInit() } == 1);

        let window = unsafe {
            glfwCreateWindow(
                width.try_into().unwrap(),
                height.try_into().unwrap(),
                CString::new(std::module_path!())
                    .unwrap()
                    .as_bytes_with_nul()
                    .as_ptr()
                    .cast::<c_char>(),
                ptr::null_mut::<GLFWmonitor>(),
                ptr::null_mut::<GLFWwindow>(),
            )
        };
        if window.is_null() {
            unsafe { glfwTerminate() };
            panic!();
        }

        unsafe {
            glfwMakeContextCurrent(window);
            glfwSwapInterval(1);
            glfwSetKeyCallback(window, callback_key);
        }

        Self { window }
    }

    fn r#loop<T>(&mut self, state: &mut T, update: fn(&mut T), draw: fn(&T)) {
        let mut now = time::Instant::now();
        let mut frames = 0;

        println!("\n");
        while unsafe { glfwWindowShouldClose(self.window) } != 1 {
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

            unsafe { glfwPollEvents() };

            update(state);
            draw(state);

            unsafe { glfwSwapBuffers(self.window) };

            frames += 1;
        }
    }
}

impl Drop for Glfw {
    fn drop(&mut self) {
        unsafe {
            glfwDestroyWindow(self.window);
            glfwTerminate();
        }
    }
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

struct State {}

fn update(_state: &mut State) {}

const fn draw(_state: &State) {}

fn main() {
    let mut glfw = Glfw::new(1536, 768, callback_key);
    let mut state = State {};
    glfw.r#loop(&mut state, update, draw);
}
