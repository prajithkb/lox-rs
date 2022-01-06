use cli_table::{print_stdout, Cell, Color, Style, Table};
use lox_rs::errors::*;
use std::{ffi::OsStr, fs, path::Path, process::Command, time::Instant};

// Temporarily hard-coded to my workspace
const TEST_CASE_PATH: &str = "/Users/kprajith/workspace/rust/lox-rs/tests/benchmark/";
const CLOX_PATH: &str = "/Users/kprajith/workspace/crafting-interpretors/craftinginterpreters/clox";
const VM_PATH: &str = "/Users/kprajith/workspace/rust/lox-rs/target/release/lox-rs";

/**
 * This test runs the bench mark tests and prints the timing information between clox and vm.
 * It does not assert on anything yet.
 */
#[test]
fn perf_timings() -> Result<()> {
    println!("This test runs the bench mark tests and compares the timing (performance) between clox and vm.\nIt does not assert on anything yet.\n");
    let dir_path = Path::new(TEST_CASE_PATH);
    let mut entries: Vec<_> = fs::read_dir(dir_path)?.collect();
    entries.sort_by(|a, b| {
        let a = a.as_ref().unwrap();
        let b = b.as_ref().unwrap();
        a.file_name().cmp(&b.file_name())
    });
    let mut table = vec![];
    // let _ = entries.split_off(1);
    for entry in entries {
        let e = entry?;
        if e.file_type()?.is_file() {
            let file_name = String::from(e.file_name().to_string_lossy());
            let path = e.path();
            println!("Benchmark for {:?}", path.as_os_str());
            let timed_taken_by_vm = run_vm(path.as_os_str())?;
            let timed_taken_by_clox = run_clox(path.as_os_str())?;
            let percentage_difference =
                ((timed_taken_by_vm / timed_taken_by_clox) * 100f64) - 100f64;
            let percentage_difference_styled = if percentage_difference < 0f64 {
                percentage_difference
                    .cell()
                    .background_color(Some(Color::Green))
            } else {
                percentage_difference.cell().bold(true)
            };
            println!("Timing for test = {}, time taken by clox ={}, time taken by vm = {}, difference = {} %", file_name, timed_taken_by_clox, timed_taken_by_vm, percentage_difference);
            table.push(vec![
                file_name.cell(),
                timed_taken_by_clox.cell(),
                timed_taken_by_vm.cell(),
                percentage_difference_styled,
            ]);
        }
    }
    let table = table
        .table()
        .title(vec![
            "Test".cell().bold(true),
            "Clox time in seconds".cell().bold(true),
            "Vm time in seconds".cell().bold(true),
            "Percentage difference".cell().bold(true),
        ])
        .bold(true);

    println!("Final results: Green is good (vm is faster than clox), bold is bad (vm is slower than clox");
    print_stdout(table)?;
    Ok(())
}

fn run_clox(path: &OsStr) -> Result<f64> {
    run(OsStr::new(CLOX_PATH), path, None)
}

fn run_vm(path: &OsStr) -> Result<f64> {
    run(OsStr::new(VM_PATH), path, Some(&["vm"]))
}

fn run(
    path_to_executable: &OsStr,
    path_to_file: &OsStr,
    additonal_arg: Option<&[&str]>,
) -> Result<f64> {
    let mut command = &mut Command::new(path_to_executable);
    if let Some(args) = additonal_arg {
        for &arg in args {
            command = command.arg(arg);
        }
    }
    command.arg(path_to_file);
    let start_time = Instant::now();
    let output = command.output()?;
    let stdout = std::str::from_utf8(&output.stdout).map_err(|_| ErrorKind::Msg("".into()))?;
    let stderr = std::str::from_utf8(&output.stderr).map_err(|_| ErrorKind::Msg("".into()))?;
    println!(
        "RAN  {:?}{}{:?}",
        path_to_executable,
        additonal_arg
            .map(|v| v.to_vec().join(" "))
            .unwrap_or_else(|| " ".to_string()),
        path_to_file
    );
    println!("STDOUT:{}", stdout);
    println!("STDERR:{}", stderr);
    println!("---------------------------------");
    Ok(start_time.elapsed().as_secs_f64())
}
