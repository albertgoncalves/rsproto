use std::ffi;
use std::marker;
use std::ptr;

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

type GLFWkeyfun = extern "C" fn(*mut GLFWwindow, ffi::c_int, ffi::c_int, ffi::c_int, ffi::c_int);

extern "C" {
    fn glfwGetVersionString() -> *const ffi::c_char;

    fn glfwInit() -> ffi::c_int;
    fn glfwTerminate();

    fn glfwCreateWindow(
        width: ffi::c_int,
        height: ffi::c_int,
        title: *const ffi::c_char,
        monitor: *mut GLFWmonitor,
        share: *mut GLFWwindow,
    ) -> *mut GLFWwindow;
    fn glfwDestroyWindow(window: *mut GLFWwindow);

    fn glfwSetKeyCallback(window: *mut GLFWwindow, callback: GLFWkeyfun) -> GLFWkeyfun;
    fn glfwSetWindowShouldClose(window: *mut GLFWwindow, value: ffi::c_int);

    fn glfwMakeContextCurrent(window: *mut GLFWwindow);
    fn glfwSwapInterval(interval: ffi::c_int);

    fn glfwWindowShouldClose(window: *mut GLFWwindow) -> ffi::c_int;
    fn glfwPollEvents();
}

const GLFW_PRESS: ffi::c_int = 1;
const GLFW_KEY_ESCAPE: ffi::c_int = 256;

extern "C" fn callback_key(
    window: *mut GLFWwindow,
    key: ffi::c_int,
    _scancode: ffi::c_int,
    action: ffi::c_int,
    _mods: ffi::c_int,
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
        let version = ffi::CStr::from_ptr(glfwGetVersionString())
            .to_str()
            .unwrap();
        println!("{version}");
        assert!(glfwInit() == 1);
        let window = glfwCreateWindow(
            1536,
            768,
            ffi::CString::new(std::module_path!())
                .unwrap()
                .as_bytes_with_nul()
                .as_ptr()
                .cast::<ffi::c_char>(),
            ptr::null_mut::<GLFWmonitor>(),
            ptr::null_mut::<GLFWwindow>(),
        );

        glfwMakeContextCurrent(window);
        glfwSwapInterval(1);

        glfwSetKeyCallback(window, callback_key);

        while glfwWindowShouldClose(window) != 1 {
            glfwPollEvents();
        }

        glfwDestroyWindow(window);
        glfwTerminate();
    }
}
