use crate::{bus::*, dpi::*, HiaArgs};
use anyhow::*;
use core::panic;
use elf::{
  abi::{EM_RISCV, ET_EXEC, PT_LOAD, STT_FUNC},
  endian::LittleEndian,
  ElfStream,
};
use std::collections::HashMap;
use std::os::unix::fs::FileExt;
use std::{fs, path::Path};
use svdpi::{get_time, SvScope};
use tracing::{debug, error, info, trace};

#[derive(Debug)]
pub struct FunctionSym {
  #[allow(dead_code)]
  pub(crate) name: String,
  #[allow(dead_code)]
  pub(crate) info: u8,
}
pub type FunctionSymTab = HashMap<u64, FunctionSym>;

pub(crate) struct Driver {
  scope: SvScope,
  pub(crate) timeout: u64,
  pub(crate) clock_flip_time: u64,

  #[cfg(feature = "trace")]
  wave_path: String,
  #[cfg(feature = "trace")]
  dump_start: u64,
  #[cfg(feature = "trace")]
  dump_end: u64,
  #[cfg(feature = "trace")]
  dump_started: bool,

  last_input_cycle: u64,
  exit: bool,

  shadow_bus: ShadowBus,
}

impl Driver {
  fn get_tick(&self) -> u64 {
    get_time() / self.clock_flip_time
  }

  fn set_last_input_cycle(&mut self) {
    self.last_input_cycle = self.get_tick();
  }

  pub(crate) fn new(scope: SvScope, args: &HiaArgs) -> Self {
    let (_entry, shadow_bus, _fn_sym_tab) =
      Self::load_elf(Path::new(&args.elf_file)).expect("fail to load ELF file");

    Self {
      scope,

      #[cfg(feature = "trace")]
      wave_path: args.wave_path.to_owned(),
      #[cfg(feature = "trace")]
      dump_start: args.dump_start,
      #[cfg(feature = "trace")]
      dump_end: args.dump_end,
      #[cfg(feature = "trace")]
      dump_started: false,

      timeout: env!("DESIGN_TIMEOUT").parse().unwrap(),
      clock_flip_time: env!("CLOCK_FLIP_TIME").parse().unwrap(),
      last_input_cycle: 0,
      exit: false,

      shadow_bus,
    }
  }

  pub fn load_elf(path: &Path) -> anyhow::Result<(u64, ShadowBus, FunctionSymTab)> {
    info!("Loading ELF file: {:?}", path);
    let file = fs::File::open(path).with_context(|| "reading ELF file")?;
    let mut elf: ElfStream<LittleEndian, _> =
      ElfStream::open_stream(&file).with_context(|| "parsing ELF file")?;

    if elf.ehdr.e_machine != EM_RISCV {
      anyhow::bail!("ELF is not in RISC-V");
    }

    if elf.ehdr.e_type != ET_EXEC {
      anyhow::bail!("ELF is not an executable");
    }

    if elf.ehdr.e_phnum == 0 {
      anyhow::bail!("ELF has zero size program header");
    }

    debug!("ELF entry: 0x{:x}", elf.ehdr.e_entry);
    let mut mem = ShadowBus::new();
    let mut load_buffer = Vec::new();
    elf.segments().iter().filter(|phdr| phdr.p_type == PT_LOAD).for_each(|phdr| {
      let vaddr: usize = phdr.p_vaddr.try_into().expect("fail converting vaddr(u64) to usize");
      let filesz: usize = phdr.p_filesz.try_into().expect("fail converting p_filesz(u64) to usize");
      debug!(
        "Read loadable segments 0x{:x}..0x{:x} to memory 0x{:x}",
        phdr.p_offset,
        phdr.p_offset + filesz as u64,
        vaddr
      );

      // Load file start from offset into given mem slice
      // The `offset` of the read_at method is relative to the start of the file and thus independent from the current cursor.
      load_buffer.resize(filesz, 0);
      file.read_at(load_buffer.as_mut_slice(), phdr.p_offset).unwrap_or_else(|err| {
        panic!(
          "fail reading ELF into mem with vaddr={}, filesz={}, offset={}. Error detail: {}",
          vaddr, filesz, phdr.p_offset, err
        )
      });
      mem.load_mem_seg(vaddr, &load_buffer.as_mut_slice());
    });

    // FIXME: now the symbol table doesn't contain any function value
    let mut fn_sym_tab = FunctionSymTab::new();
    let symbol_table =
      elf.symbol_table().with_context(|| "reading symbol table(SHT_SYMTAB) from ELF")?;
    if let Some((parsed_table, string_table)) = symbol_table {
      parsed_table
        .iter()
        // st_symtype = symbol.st_info & 0xf (But why masking here?)
        .filter(|sym| sym.st_symtype() == STT_FUNC)
        .for_each(|sym| {
          let name = string_table
            .get(sym.st_name as usize)
            .unwrap_or_else(|_| panic!("fail to get name at st_name={}", sym.st_name));
          fn_sym_tab.insert(
            sym.st_value,
            FunctionSym { name: name.to_string(), info: sym.st_symtype() },
          );
        });
    } else {
      debug!("load_elf: symtab not found");
    };

    Ok((elf.ehdr.e_entry, mem, fn_sym_tab))
  }

  pub(crate) fn init(&mut self) {
    #[cfg(feature = "trace")]
    if self.dump_start == 0 {
      self.start_dump_wave();
      self.dump_started = true;
    }
  }

  pub(crate) fn instruction_fetch_axi(&mut self, addr: u32) -> AXIReadPayload {
    self.set_last_input_cycle();

    if self.exit {
      return AXIReadPayload { valid: false as u8, bits: 0 };
    }

    let inst = self.shadow_bus.read_mem(addr, 4);
    let bits = inst.iter().fold(0, |acc, &x| (acc << 8) | x as u32);
    debug!("instruction_fetch_axi: addr={:#x}, inst={:#x}", addr, bits);
    AXIReadPayload { valid: 1, bits }
  }

  pub(crate) fn load_store_axi_r(&mut self, addr: u32) -> AXIReadPayload {
    self.set_last_input_cycle();

    if self.exit {
      return AXIReadPayload { valid: false as u8, bits: 0 };
    }

    let inst = self.shadow_bus.read_mem(addr, 4);
    let bits = inst.iter().fold(0, |acc, &x| (acc << 8) | x as u32);
    debug!("load_store_axi_r: addr={:#x}, data={:#x}", addr, bits);
    AXIReadPayload { valid: true as u8, bits }
  }

  pub(crate) fn load_store_axi_w(&mut self, addr: u32, data: u32) -> AXIWritePayload {
    self.set_last_input_cycle();

    // exit code
    if self.exit {
      return AXIWritePayload { success: true as u8 };
    }

    if addr == 0x1145_1400 && data == 0xdead_beef {
      self.exit = true;
    }

    self.shadow_bus.write_mem(addr, data);
    debug!("load_store_axi_w: addr={:#x}, data={:#x}", addr, data);
    AXIWritePayload { success: true as u8 }
  }

  pub(crate) fn watchdog(&mut self) -> u8 {
    const WATCHDOG_CONTINUE: u8 = 0;
    const WATCHDOG_TIMEOUT: u8 = 1;
    const WATCHDOG_FINISH: u8 = 2;

    let tick = self.get_tick();
    if tick - self.last_input_cycle > self.timeout {
      error!(
        "[{}] watchdog timeout, last input tick = {}",
        tick, self.last_input_cycle
      );
      WATCHDOG_TIMEOUT
    } else if self.exit {
      info!("[{tick}] watchdog finish, exiting");
      WATCHDOG_FINISH
    } else {
      #[cfg(feature = "trace")]
      if self.dump_end != 0 && tick > self.dump_end {
        info!("[{tick}] run to dump end, exiting");
        return WATCHDOG_FINISH;
      }

      #[cfg(feature = "trace")]
      if !self.dump_started && tick >= self.dump_start {
        self.start_dump_wave();
        self.dump_started = true;
      }
      trace!("[{tick}] watchdog continue");
      WATCHDOG_CONTINUE
    }
  }

  #[cfg(feature = "trace")]
  fn start_dump_wave(&mut self) {
    dump_wave(self.scope, &self.wave_path);
  }
}
