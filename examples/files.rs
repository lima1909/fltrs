use std::{error::Error, fs::read_dir};

fn main() -> Result<(), Box<dyn Error>> {
    for f in read_dir(".")? {
        let f = f?;
        let m = f.metadata()?;
        if m.is_file() {
            println!("{:?}: {} ", f.file_name(), m.len());
        }
    }

    Ok(())
}
