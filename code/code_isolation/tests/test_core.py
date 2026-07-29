import pytest

from src.core import calculate_slope_window, detect_triggers


def test_calculate_slope_window_supports_configurable_lengths():
    assert calculate_slope_window([0.0, 2.0], [0.0, 1.0]) == pytest.approx(2.0)
    assert calculate_slope_window(
        [0.0, 2.0, 4.0, 6.0],
        [0.0, 1.0, 2.0, 3.0],
    ) == pytest.approx(2.0)


def test_calculate_slope_window_validates_input():
    with pytest.raises(ValueError, match="same length"):
        calculate_slope_window([1.0, 2.0], [0.0])

    with pytest.raises(ValueError, match="At least 2"):
        calculate_slope_window([1.0], [0.0])


def test_detect_triggers_uses_requested_window_size():
    triggers = detect_triggers(
        pressure_series=[0.0, 10.0, 10.0, 10.0, 10.0],
        timestamp_series=[0.0, 1.0, 2.0, 3.0, 4.0],
        slope_threshold=5.0,
        grace_period=0.0,
        window_size=2,
    )

    assert len(triggers) == 1
    assert triggers[0][0] == 1.0


def test_detect_triggers_rejects_too_small_window():
    with pytest.raises(ValueError, match="at least 2"):
        detect_triggers([0.0], [0.0], window_size=1)
