/* Type for a 16-bit quantity.  */
pub type Half = u16;

/* Types for signed and unsigned 32-bit quantities.  */
pub type Word = u32;
pub type Sword = i32;

/* Type of addresses.  */
pub type Addr = u32;

/* Type of file offsets.  */
pub type Off = u32;

/* Type for section indices, which are 16-bit quantities.  */
pub type Section = u16;

/* Type for version symbol information.  */
pub type Versym = u32;


/* The ELF file header.  This appears at the start of every ELF file.  */

pub const EI_NIDENT: usize = 16;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Ehdr {
    pub e_ident: [u8; EI_NIDENT], /* Magic number and other info */
    pub e_type: Half, /* Object file type */
    pub e_machine: Half, /* Architecture */
    pub e_version: Word, /* Object file version */
    pub e_entry: Addr, /* Entry point virtual address */
    pub e_phoff: Off, /* Program header table file offset */
    pub e_shoff: Off, /* Section header table file offset */
    pub e_flags: Word, /* Processor-specific flags */
    pub e_ehsize: Half, /* ELF header size in bytes */
    pub e_phentsize: Half, /* Program header table entry size */
    pub e_phnum: Half, /* Program header table entry count */
    pub e_shentsize: Half, /* Section header table entry size */
    pub e_shnum: Half, /* Section header table entry count */
    pub e_shstrndx: Half, /* Section header string table index */
}

/* Fields in the e_ident array.  The EI_* macros are indices into the
   array.  The macros under each EI_* macro are the values the byte
   may have.  */

pub const EI_MAG0: usize = 0; /* File identification byte 0 index */
pub const ELFMAG0: u8 = 0x7f; /* Magic number byte 0 */

pub const EI_MAG_1: usize = 1; /* File identification byte 1 index */
pub const ELFMAG1: u8 = 'E' as u8; /* Magic number byte 1 */

pub const EI_MAG2: usize = 2; /* File identification byte 2 index */
pub const ELFMAG2: u8 = 'L' as u8; /* Magic number byte 2 */

pub const EI_MAG3: usize = 3; /* File identification byte 3 index */
pub const ELFMAG3: u8 = 'F' as u8; /* Magic number byte 3 */

pub const EI_CLASS: usize = 4; /* File class byte index */
pub const ELFCLASSNONE: u8 = 0; /* Invalid class */
pub const ELFCLASS32: u8 = 1; /* 32-bit objects */
pub const ELFCLASS64: u8 = 2; /* 64-bit objects */
pub const ELFCLASSNUM: u8 = 3;

pub const EI_DATA: usize = 5; /* Data encoding byte index */
pub const ELFDATANONE: u8 = 0; /* Invalid data encoding */
pub const ELFDATA2LSB: u8 = 1; /* 2's complement, little endian */
pub const ELFDATA2MSB: u8 = 2; /* 2's complement, big endian */
pub const ELFDATANUM: u8 = 3;

pub const EI_VERSION: usize = 6; /* File version byte index */
                /* Value must be EV_CURRENT */

pub const EI_OSABI: usize = 7; /* OS ABI identification */
pub const ELFOSABI_NONE: u8 = 0; /* UNIX System V ABI */
pub const ELFOSABI_SYSV: u8 = 0; /* Alias.  */
pub const ELFOSABI_GNU: u8 = 3; /* Object uses GNU ELF extensions.  */
pub const ELFOSABI_LINUX: u8 = ELFOSABI_GNU; /* Compatibility alias.  */

pub const EI_ABIVERSION: usize = 8;  /* ABI version */

pub const EI_PAD: usize = 9; /* Byte index of padding bytes */

/* Legal values for e_type (object file type).  */

pub const ET_NONE: Half = 0; /* No file type */
pub const ET_REL: Half = 1; /* Relocatable file */
pub const ET_EXEC: Half = 2; /* Executable file */
pub const ET_DYN: Half = 3; /* Shared object file */
pub const ET_CORE: Half = 4; /* Core file */
pub const ET_NUM: Half = 5; /* Number of defined types */

/* Legal values for e_machine (architecture).  */

pub const EM_NONE: Half = 0; /* No machine */
pub const EM_386: Half = 3; /* Intel 80386 */
pub const EM_X86_64: Half = 62; /* AMD x86-64 architecture */
pub const EM_NUM: Half = 259;

/* Legal values for e_version (version).  */

pub const EV_NONE: Word = 0; /* Invalid ELF version */
pub const EV_CURRENT: Word = 1; /* Current version */
pub const EV_NUM: Word = 2;

/* Section header.  */

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Shdr {
    pub sh_name: Word, /* Section name (string tbl index) */
    pub sh_type: Word, /* Section type */
    pub sh_flags: Word, /* Section flags */
    pub sh_addr: Addr, /* Section virtual addr at execution */
    pub sh_offset: Off, /* Section file offset */
    pub sh_size: Word, /* Section size in bytes */
    pub sh_link: Word, /* Link to another section */
    pub sh_info: Word, /* Additional section information */
    pub sh_addralign: Word, /* Section alignment */
    pub sh_entsize: Word, /* Entry size if section holds table */
}

/* Special section indices.  */

pub const SHN_UNDEF: Half = 0; /* Undefined section */
pub const SHN_LORESERVE: Half = 0xff00; /* Start of reserved indices */
pub const SHN_ABS: Half = 0xfff1; /* Associated symbol is absolute */
pub const SHN_COMMON: Half = 0xfff2; /* Associated symbol is common */
pub const SHN_HIRESERVE: Half = 0xffff; /* End of reserved indices */

/* Legal values for sh_type (section type).  */

pub const SHT_NULL: Word = 0; /* Section header table entry unused */
pub const SHT_PROGBITS: Word = 1; /* Program data */
pub const SHT_SYMTAB: Word = 2; /* Symbol table */
pub const SHT_STRTAB: Word = 3; /* String table */
pub const SHT_RELA: Word = 4; /* Relocation entries with addends */
pub const SHT_NOBITS: Word = 8; /* Program space with no data (bss) */
pub const SHT_REL: Word = 9; /* Relocation entries, no addends */
pub const SHT_NUM: Word = 20; /* Number of defined types.  */

/* Legal values for sh_flags (section flags).  */
pub const SHF_WRITE: Word = 1 << 0; /* Writable */
pub const SHF_ALLOC: Word = 1 << 1; /* Occupies memory during execution */
pub const SHF_EXECINSTR: Word = 1 << 2; /* Executable */
pub const SHF_MERGE: Word = 1 << 4; /* Might be merged */
pub const SHF_STRINGS: Word = 1 << 5; /* Contains nul-terminated strings */
pub const SHF_INFO_LINK: Word = 1 << 6; /* `sh_info' contains SHT index */

/* Symbol table entry.  */

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Sym {
    pub st_name: Word, /* Symbol name (string tbl index) */
    pub st_value: Addr, /* Symbol value */
    pub st_size: Word, /* Symbol size */
    pub st_info: u8, /* Symbol type and binding */
    pub st_other: u8, /* Symbol visibility */
    pub st_shndx: Section, /* Section index */
}

/* How to extract and insert information held in the st_info field.  */

macro_rules! st_bind {
    ($val:expr) => {
        $val as u8 >> 4
    };
}

macro_rules! st_type {
    ($val:expr) => {
        $val & 0x0f
    };
}

macro_rules! st_info {
    ($bind:expr, $type:expr) => {
        (($bind as u8) << 4) + ($type as u8 & 0xf)
    };
}

/* Legal values for ST_BIND subfield of st_info (symbol binding).  */

pub const STB_LOCAL: u8 = 0; /* Local symbol */
pub const STB_GLOBAL: u8 = 1; /* Global symbol */
pub const STB_WEAK: u8 = 2; /* Weak symbol */
pub const STB_NUM: u8 = 3; /* Number of defined types.  */

/* Legal values for ST_TYPE subfield of st_info (symbol type).  */

pub const STT_NOTYPE: u8 = 0; /* Symbol type is unspecified */
pub const STT_OBJECT: u8 = 1; /* Symbol is a data object */
pub const STT_FUNC: u8 = 2; /* Symbol is a code object */
pub const STT_SECTION: u8 = 3; /* Symbol associated with a section */
pub const STT_FILE: u8 = 4; /* Symbol's name is file name */
pub const STT_COMMON: u8 = 5; /* Symbol is a common data object */
pub const STT_TLS: u8 = 6; /* Symbol is thread-local data object*/
pub const STT_NUM: u8 = 7; /* Number of defined types.  */

/* Relocation table entry without addend (in section of type SHT_REL).  */

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Rel {
    pub r_offset: Addr, /* Address */
    pub r_info: Word, /* Relocation type and symbol index */
}

/* How to extract and insert information held in the r_info field.  */

macro_rules! r_sym {
    ($val:expr) => {
        $val >> 8
    };
}

macro_rules! r_type {
    ($val:expr) => {
        $val & 0xff
    };
}

macro_rules! r_info {
    ($sym:expr, $type:expr) => {
        (($sym as Word) << 8) + ($type as Word & 0xff)
    };
}

/* Intel 80386 specific definitions.  */

/* i386 relocs.  */

pub const R_386_NONE: u8 = 0; /* No reloc */
pub const R_386_32: u8 = 1; /* Direct 32 bit  */
pub const R_386_PC32: u8 = 2; /* PC relative 32 bit */
pub const R_386_GOT32: u8 = 3; /* 32 bit GOT entry */
pub const R_386_PLT32: u8 = 4; /* 32 bit PLT address */

/* Program segment header.  */

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Phdr {
    pub p_type: Word, /* Segment type */
    pub p_offset: Off, /* Segment file offset */
    pub p_vaddr: Addr, /* Segment virtual address */
    pub p_paddr: Addr, /* Segment physical address */
    pub p_filesz: Word, /* Segment size in file */
    pub p_memsz: Word, /* Segment size in memory */
    pub p_flags: Word, /* Segment flags */
    pub p_align: Word, /* Segment alignment */
}

/* Special value for e_phnum.  This indicates that the real number of
   program headers is too large to fit into e_phnum.  Instead the real
   value is in the field sh_info of section 0.  */

pub const PN_XNUM: Half = 0xffff;

/* Legal values for p_type (segment type).  */
pub const PT_NULL: Word = 0; /* Program header table entry unused */
pub const PT_LOAD: Word = 1; /* Loadable program segment */
pub const PT_DYNAMIC: Word = 2; /* Dynamic linking information */
pub const PT_INTERP: Word = 3; /* Program interpreter */
pub const PT_NOTE: Word = 4; /* Auxiliary information */
pub const PT_SHLIB: Word = 5; /* Reserved */
pub const PT_PHDR: Word = 6; /* Entry for header table itself */
pub const PT_TLS: Word = 7; /* Thread-local storage segment */
pub const PT_NUM: Word = 8; /* Number of defined types */

/* Legal values for p_flags (segment flags).  */

pub const PF_X: Word = 1 << 0; /* Segment is executable */
pub const PF_W: Word = 1 << 1; /* Segment is writable */
pub const PF_R: Word = 1 << 2; /* Segment is readable */
pub const PF_MASKOS: Word = 0x0ff00000; /* OS-specific */
pub const PF_MASKPROC: Word = 0xf0000000; /* Processor-specific */
