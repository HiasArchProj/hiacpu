use core::fmt;
use std::ffi::{c_char, c_longlong, CString};
use std::sync::Mutex;

use crate::drive::Driver;
use crate::plusarg::PlusArgMatcher;
use crate::HiaArgs;
use svdpi::sys::dpi::{svBitVecVal, svLogic};
use svdpi::SvScope;

pub type SvBitVecVal = u32;

// --------------------------
// preparing data structures
// --------------------------

static DPI_TARGET: Mutex<Option<Box<Driver>>> = Mutex::new(None);

pub(crate) trait TestPayload {
  fn to_be_bytes(&self) -> Vec<u8>;
}

#[derive(Debug)]
pub(crate) struct AXIReadPayload {
  pub(crate) valid: svLogic,
  pub(crate) bits: SvBitVecVal,
}

impl TestPayload for AXIReadPayload {
  fn to_be_bytes(&self) -> Vec<u8> {
    (self.bits.to_be_bytes().iter()).chain(vec![self.valid].iter()).cloned().collect::<Vec<u8>>()
  }
}

#[derive(Debug)]
#[repr(C)]
pub(crate) struct AXIWritePayload {
  pub(crate) success: svLogic,
}

impl TestPayload for AXIWritePayload {
  fn to_be_bytes(&self) -> Vec<u8> {
    vec![self.success]
  }
}

unsafe fn fill_test_payload<T: TestPayload>(dst: *mut SvBitVecVal, payload: &T) {
  let data: &[u8] = &payload.to_be_bytes();
  let dst = std::slice::from_raw_parts_mut(dst as *mut u8, data.len());
  dst.copy_from_slice(data);
}

//----------------------
// dpi functions
//----------------------

#[no_mangle]
unsafe extern "C" fn hia_init() {
  let plusargs = PlusArgMatcher::from_args();
  let args = HiaArgs::from_plusargs(&plusargs);
  args.setup_logger().unwrap();
  let scope = SvScope::get_current().expect("failed to get scope in hia_init");
  let driver = Box::new(Driver::new(scope, &args));

  let mut dpi_target = DPI_TARGET.lock().unwrap();
  assert!(dpi_target.is_none(), "hia_init should be called only once");
  *dpi_target = Some(driver);

  if let Some(driver) = dpi_target.as_mut() {
    driver.init();
  }
}

#[no_mangle]
unsafe extern "C" fn hia_watchdog(reason: *mut c_char) {
  let mut driver = DPI_TARGET.lock().unwrap();
  if let Some(driver) = driver.as_mut() {
    *reason = driver.watchdog() as c_char;
  }
}

#[no_mangle]
unsafe extern "C" fn hia_instructionFetchAXI(addr: c_longlong, payload: *mut svBitVecVal) {
  let mut driver = DPI_TARGET.lock().unwrap();
  if let Some(driver) = driver.as_mut() {
    fill_test_payload(payload, &driver.instruction_fetch_axi(addr as u32));
  }
}

#[no_mangle]
unsafe extern "C" fn hia_loadStoreAXIR(addr: c_longlong, payload: *mut svBitVecVal) {
  let mut driver = DPI_TARGET.lock().unwrap();
  if let Some(driver) = driver.as_mut() {
    fill_test_payload(payload, &driver.load_store_axi_r(addr as u32));
  }
}

#[no_mangle]
unsafe extern "C" fn hia_loadStoreAXIW(addr: c_longlong, data: c_longlong, payload: *mut svBitVecVal) {
  let mut driver = DPI_TARGET.lock().unwrap();
  if let Some(driver) = driver.as_mut() {
    fill_test_payload(payload, &driver.load_store_axi_w(addr as u32, data as u32));
  }
}

//--------------------------------
// import functions and wrappers
//--------------------------------

mod dpi_export {
  use std::ffi::c_char;
  extern "C" {
    #[cfg(feature = "trace")]
    /// `export "DPI-C" function dump_wave(input string file)`
    pub fn dump_wave(path: *const c_char);
  }
}

#[cfg(feature = "trace")]
pub(crate) fn dump_wave(scope: SvScope, path: &str) {
  use svdpi::set_scope;

  let path_cstring = CString::new(path).unwrap();

  set_scope(scope);
  unsafe {
    dpi_export::dump_wave(path_cstring.as_ptr());
  }
}
