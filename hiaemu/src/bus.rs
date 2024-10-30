mod mem;

use mem::*;
use tracing::{debug, error, trace};

trait ShadowDevice: Send + Sync {
  fn new() -> Box<dyn ShadowDevice>
  where
    Self: Sized;
  /// addr: offset respect to the base of this device
  fn read_mem(&self, addr: usize, size: usize) -> &[u8];
  /// addr: offset respect to the base of this device
  fn write_mem(&mut self, addr: usize, data: u8);
  /// addr: offset respect to the base of this device
  /// strobe: signals which element in data is valid, None = all valid
  fn write_mem_chunk(&mut self, addr: usize, size: usize, strobe: Option<&[bool]>, data: &[u8]);
}

struct ShadowBusDevice {
  base: usize,
  size: usize,
  device: Box<dyn ShadowDevice>,
}

const MAX_DEVICES: usize = 4;

pub(crate) struct ShadowBus {
  devices: [ShadowBusDevice; MAX_DEVICES],
}

impl ShadowBus {
  /// Initiate the devices on the bus as specified in `tests/t1.ld`
  /// NOTE: For some reason DDR is not aligned in the address space
  pub fn new() -> Self {
    const MMIO_BASE: usize = 0x0000_0000;
    const SCALAR_BASE: usize = 0x2000_0000;
    const DDR_BASE: usize = 0x4000_0000;
    const SRAM_BASE: usize = 0xc000_0000;

    const MMIO_SIZE: usize = 0x2000_0000;
    const SCALAR_SIZE: usize = 0x2000_0000;
    const DDR_SIZE: usize = 0x8000_0000;
    const SRAM_SIZE: usize = 0x0040_0000;

    Self {
      devices: [
        ShadowBusDevice {
          base: MMIO_BASE,
          size: MMIO_SIZE,
          device: MemDevice::<MMIO_SIZE>::new(),
        },
        ShadowBusDevice {
          base: SCALAR_BASE,
          size: SCALAR_SIZE,
          device: MemDevice::<SCALAR_SIZE>::new(),
        },
        ShadowBusDevice {
          base: DDR_BASE,
          size: DDR_SIZE,
          device: MemDevice::<DDR_SIZE>::new(),
        },
        ShadowBusDevice {
          base: SRAM_BASE,
          size: SRAM_SIZE,
          device: MemDevice::<SRAM_SIZE>::new(),
        },
      ],
    }
  }

  pub fn read_mem(&self, addr: u32, size: u32) -> &[u8] {
    debug!("read mem addr={addr:#x}, size={size}");

    // find the device that contains the address
    let start = addr as usize;
    let end = (addr + size) as usize;
    let handler = self
      .devices
      .iter()
      .find(|d| match d {
        ShadowBusDevice { base, size, device: _ } => *base <= start && end < (*base + *size),
      })
      .unwrap_or_else(|| panic!("can't find device: addr={addr}, size={size}"));

    // read the data from the device
    let offset = start - handler.base;
    handler.device.read_mem(offset, size as usize)
  }

  pub fn write_mem(&mut self, addr: u32, data: u32) {
    debug!("write mem addr={addr:#x}, data={data:#x}");
    let data = data.to_le_bytes();
    let size = 32 / 8;

    // find the device that contains the address
    let start = addr as usize;
    let end = (addr + size) as usize;
    let handler = self
      .devices
      .iter_mut()
      .find(|d| match d {
        ShadowBusDevice { base, size, device: _ } => *base <= start && end <= (*base + *size),
      })
      .unwrap_or_else(|| panic!("address out: addr={addr}, data={data:?}"));

    // write the data to the device
    (0..size as usize).for_each(|i| {
      handler.device.write_mem(start + i, data[i]);
    });
  }

  // size: 1 << arsize
  // bus_size: AXI bus width in bytes
  // return: Vec<u8> with len=bus_size
  // if size < bus_size, the result is padded due to AXI narrow transfer rules
  pub fn read_mem_axi(&self, addr: u32, size: u32, bus_size: u32) -> Vec<u8> {
    assert!(
      addr % size == 0 && bus_size % size == 0,
      "unaligned access addr={addr:#x} size={size}B dlen={bus_size}B"
    );

    let start = addr as usize;
    let end = (addr + size) as usize;

    let handler = self.devices.iter().find(|d| match d {
      ShadowBusDevice { base, size, device: _ } => *base <= start && end <= (*base + *size),
    });

    match handler {
      Some(ShadowBusDevice { base, size: _, device }) => {
        let offset = start - *base;
        let data = device.read_mem(offset, size as usize);

        if size < bus_size {
          let mut data_padded = vec![0; bus_size as usize];
          let start = (addr % bus_size) as usize;
          let end = start + data.len();
          data_padded[start..end].copy_from_slice(data);

          data_padded
        } else {
          data.to_vec()
        }
      }
      None => {
        error!("read addr={addr:#x} size={size}B dlen={bus_size}B leads to nowhere!");
        vec![0; bus_size as usize]
      }
    }
  }

  // size: 1 << awsize
  // bus_size: AXI bus width in bytes
  // masks: write strobes, len=bus_size
  // data: write data, len=bus_size
  pub fn write_mem_axi(
    &mut self,
    addr: u32,
    size: u32,
    bus_size: u32,
    masks: &[bool],
    data: &[u8],
  ) {
    assert!(
      addr % size == 0 && bus_size % size == 0,
      "unaligned write access addr={addr:#x} size={size}B dlen={bus_size}B"
    );

    if !masks.iter().any(|x| *x) {
      trace!("Mask 0 write detected");
      return;
    }

    let start = (addr & ((!bus_size) + 1)) as usize;
    let end = start + bus_size as usize;

    let handler = self.devices.iter_mut().find(|d| match d {
      ShadowBusDevice { base, size, device: _ } => *base <= start && end <= (*base + *size),
    });

    match handler {
      Some(ShadowBusDevice { base, size: _, device }) => {
        let offset = start - *base;
        device.write_mem_chunk(offset, bus_size as usize, Option::from(masks), data);
      }
      None => {
        error!("write addr={addr:#x} size={size}B dlen={bus_size}B leads to nowhere!");
      }
    }
  }

  pub fn load_mem_seg(&mut self, vaddr: usize, data: &[u8]) {
    let handler = self
      .devices
      .iter_mut()
      .find(|d| match d {
        ShadowBusDevice { base, size, device: _ } => {
          *base <= vaddr as usize && (vaddr as usize + data.len()) <= (*base + *size)
        }
      })
      .unwrap_or_else(|| {
        panic!(
          "fail reading ELF into mem with vaddr={:#x}, len={}B: load memory to nowhere",
          vaddr,
          data.len()
        )
      });

    let offset = vaddr - handler.base;
    handler.device.write_mem_chunk(offset, data.len(), None, data)
  }
}
