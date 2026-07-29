from pathlib import Path

from setuptools import find_packages, setup

ROOT = Path(__file__).parent


def read_requirements(filename):
    return [
        line.strip()
        for line in (ROOT / filename).read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.startswith("#")
    ]


source_packages = find_packages(where="src")
packages = ["adaptive_piston_algorithm"] + [
    f"adaptive_piston_algorithm.{package}" for package in source_packages
]

setup(
    name="adaptive-piston-algorithm",
    version="1.0.0",
    author="iEVIP authors",
    description="Intelligent algorithm for detecting clogging events in fluid delivery systems",
    long_description=(ROOT / "README.md").read_text(encoding="utf-8"),
    long_description_content_type="text/markdown",
    url="https://github.com/Adoczf11/iEVIP",
    license="GPL-3.0-only",
    packages=packages,
    package_dir={"adaptive_piston_algorithm": "src"},
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering",
        "Topic :: Scientific/Engineering :: Medical Science Apps",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Programming Language :: Python :: 3.13",
        "Programming Language :: Python :: 3.14",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.9",
    install_requires=read_requirements("requirements.txt"),
    extras_require={
        "dev": read_requirements("requirements-dev.txt"),
    },
    entry_points={
        "console_scripts": [
            "piston-analyzer=adaptive_piston_algorithm.cli.main:main",
        ],
    },
    include_package_data=True,
)
