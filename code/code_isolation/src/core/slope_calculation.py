"""Sliding-window pressure slope calculation."""

from typing import List


def calculate_slope_window(pressures: List[float], timestamps: List[float]) -> float:
    """
    Calculate the average slope between consecutive points in a window.

    Args:
        pressures: Pressure values in raw sensor units
        timestamps: Corresponding timestamps in seconds

    Returns:
        Average slope (pressure/second)

    Raises:
        ValueError: When lengths differ or fewer than two points are provided

    Algorithm:
        Calculates each consecutive segment slope and returns their mean.
    """
    if len(pressures) != len(timestamps):
        raise ValueError("pressures and timestamps must have the same length")
    if len(pressures) < 2:
        raise ValueError("At least 2 points are required for slope calculation")

    slopes = []
    for i in range(1, len(pressures)):
        dt = timestamps[i] - timestamps[i - 1]
        if dt <= 1e-6:
            dt = 1e-6
        slope = (pressures[i] - pressures[i - 1]) / dt
        slopes.append(slope)

    return sum(slopes) / len(slopes)
