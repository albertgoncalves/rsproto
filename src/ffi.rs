use std::ffi;
use std::marker;
use std::ptr;
use std::thread;
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
}

fn main() {
    unsafe {
        let version = ffi::CStr::from_ptr(glfwGetVersionString())
            .to_str()
            .unwrap();
        println!("{version}\n{}", glfwInit());
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
        thread::sleep(time::Duration::from_millis(2500));
        glfwDestroyWindow(window);
        glfwTerminate();
    }
}
