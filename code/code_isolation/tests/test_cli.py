import csv
import sys

from src.cli.main import main
from src.data_processing import load_csv_data_simple


def write_pressure_csv(path, rows):
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(["time", "pressure"])
        writer.writerows(rows)


def run_cli(monkeypatch, *args):
    monkeypatch.setattr(sys, "argv", ["piston-analyzer", *map(str, args)])
    return main()


def read_csv_rows(path):
    with path.open(newline="", encoding="utf-8") as handle:
        return list(csv.reader(handle))


def test_csv_loader_keeps_first_data_row_after_header(tmp_path):
    input_path = tmp_path / "pressure.csv"
    write_pressure_csv(input_path, [(0, 100), (1, 101), (2, 102)])

    timestamps, pressures = load_csv_data_simple(str(input_path))

    assert timestamps == [0.0, 1.0, 2.0]
    assert pressures == [100.0, 101.0, 102.0]


def test_zero_triggers_is_a_successful_analysis(monkeypatch, tmp_path):
    input_path = tmp_path / "stable.csv"
    output_path = tmp_path / "results.csv"
    write_pressure_csv(input_path, [(0, 100), (1, 100), (2, 100), (3, 100), (4, 100)])

    exit_code = run_cli(
        monkeypatch,
        "--csv",
        input_path,
        "--output",
        output_path,
        "--quiet",
    )

    assert exit_code == 0
    assert len(read_csv_rows(output_path)) == 1


def test_cli_applies_window_size(monkeypatch, tmp_path):
    input_path = tmp_path / "pressure.csv"
    output_path = tmp_path / "results.csv"
    write_pressure_csv(input_path, [(0, 0), (1, 10), (2, 10), (3, 10), (4, 10)])

    exit_code = run_cli(
        monkeypatch,
        "--csv",
        input_path,
        "--threshold",
        5,
        "--grace",
        0,
        "--window",
        2,
        "--output",
        output_path,
        "--quiet",
    )

    rows = read_csv_rows(output_path)
    assert exit_code == 0
    assert len(rows) == 2
    assert rows[1][1] == "1.00"


def test_csv_header_matches_documentation(monkeypatch, tmp_path):
    input_path = tmp_path / "stable.csv"
    output_path = tmp_path / "results.csv"
    write_pressure_csv(input_path, [(0, 100), (1, 100)])

    assert (
        run_cli(
            monkeypatch,
            "--csv",
            input_path,
            "--output",
            output_path,
            "--quiet",
        )
        == 0
    )

    assert read_csv_rows(output_path)[0] == [
        "Trigger Point",
        "Trigger Time (s)",
        "Data Index",
        "Average Slope (pressure/s)",
        "Backflush Start (s)",
        "Backflush End (s)",
        "Backflush Duration (s)",
    ]


def test_save_config_creates_parent_directory(monkeypatch, tmp_path):
    config_path = tmp_path / "nested" / "config.json"

    assert run_cli(monkeypatch, "--window", 7, "--save-config", config_path) == 0
    assert config_path.exists()
