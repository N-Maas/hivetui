#!/usr/bin/bash

target_folder=$1

mkdir -p $target_folder

BuildBinary() {
    # 1: target, 2: name
    cargo build --target $1 --release
    if [[ $? != 0 ]]; then
        echo "build failed: $2" 1>&2
        exit 1
    fi
    cp ./target/$1/release/hive $target_folder/$2_tmp
    zip $target_folder/$2 $target_folder/$2_tmp
    rm -r $target_folder/$2_tmp
}

BuildWindows() {
    # 1: target, 2: name
    cargo build --target $1 --release
    if [[ $? != 0 ]]; then
        echo "build failed: $2" 1>&2
        exit 1
    fi
    mkdir -p $target_folder/$2_tmp/bin
    cp ./target/$1/release/hive.exe $target_folder/$2_tmp/bin
    cp ./hive.bat $target_folder/$2_tmp
    zip -r $target_folder/$2 $target_folder/$2_tmp/*
    rm -r $target_folder/$2_tmp
}

BuildBinary x86_64-unknown-linux-gnu x86_64-linux
BuildWindows x86_64-pc-windows-gnu x86_64-windows
