import argparse
import os
import subprocess
import shutil


def create_folder_with_cleanup(folder, name):
    folder_path = os.path.join(folder, name)
    print(f"creating a folder with {folder_path=}")
    if os.path.exists(folder_path):
        # Clean up the folder by removing its contents
        for file_or_dir in os.listdir(folder_path):
            file_or_dir_path = os.path.join(folder_path, file_or_dir)

            # Remove files and directories
            if os.path.isfile(file_or_dir_path):
                os.remove(file_or_dir_path)
            elif os.path.isdir(file_or_dir_path):
                shutil.rmtree(file_or_dir_path)
    else:
        # Create the new folder if it does not exist
        os.makedirs(folder_path, exist_ok=True)


def process_files(folder):
    allowed_extensions = ['.l1', '.l2', '.l3', '.l4']
    compiler_path = 'compiler/bin/c0c'

    try:
        files = os.listdir(folder)
        print(
            f"Processing files in the folder '{folder}' with extensions {allowed_extensions}:")
        for file in files:
            if os.path.isfile(os.path.join(folder, file)) and any(file.endswith(ext) for ext in allowed_extensions):
                file_path = os.path.join(folder, file)
                print(f"Processing: {file_path}")
                subprocess.run([compiler_path, '-e', 'llvm', '-l',
                               'runtime/15411-l4.h0', file_path], stderr=subprocess.PIPE)

    except FileNotFoundError:
        print(f"Error: Folder '{folder}' not found.")
    except Exception as e:
        print(f"Error: {e}")


llc_path = "/opt/homebrew/opt/llvm/bin/llc"


def process_ll_files(folder):
    try:
        files = os.listdir(folder)
        print(
            f"Processing .ll files in the folder '{folder}'")
        for file in files:
            if os.path.isfile(os.path.join(folder, file)):
                file_path = os.path.join(folder, file)
                print(f"Processing: {file_path}")
                subprocess.run([llc_path, file_path])

    except FileNotFoundError:
        print(f"Error: Folder '{folder}' not found.")
    except Exception as e:
        print(f"Error: {e}")


def compile_s_files(folder):
    allowed_extensions = ['.s']
    try:
        files = os.listdir(folder)
        print(
            f"Processing files in the folder '{folder}' with extensions {allowed_extensions}:")
        for file in files:
            if os.path.isfile(os.path.join(folder, file)) and any(file.endswith(ext) for ext in allowed_extensions):
                file_path = os.path.join(folder, file)
                print(f"Processing: {file_path}")
                subprocess.run(['clang', file_path,
                               'runtime/run411.c', '-o', file_path + ".exe"])

    except FileNotFoundError:
        print(f"Error: Folder '{folder}' not found.")
    except Exception as e:
        print(f"Error: {e}")


def main():
    parser = argparse.ArgumentParser(
        description="Run the 'c0c' compiler with the '-e llvm' option on files with specific extensions in a given folder")
    parser.add_argument(
        "folder", help="Name of the folder to process files from")
    args = parser.parse_args()
    create_folder_with_cleanup(os.getcwd(), "llvm")
    process_files(args.folder)
    process_ll_files(os.path.join(os.getcwd(), "llvm"))
    compile_s_files(os.path.join(os.getcwd(), "llvm"))


if __name__ == "__main__":
    main()
