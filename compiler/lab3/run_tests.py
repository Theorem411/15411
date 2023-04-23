import argparse
import os
import subprocess
import shutil


verbose = False


def custom_print(*args, **kwargs):
    if verbose:
        print(*args, **kwargs)


def create_folder_with_cleanup(folder, name):
    folder_path = os.path.join(folder, name)
    custom_print(f"creating a folder with {folder_path=}")
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
    counter = 0
    try:
        files = os.listdir(folder)
        custom_print(
            f"Processing files in the folder '{folder}' with extensions {allowed_extensions}:")
        for file in files:
            if os.path.isfile(os.path.join(folder, file)) and any(file.endswith(ext) for ext in allowed_extensions):
                counter += 1
                file_path = os.path.join(folder, file)
                custom_print(f"Processing: {file_path}")
                try:
                    out = subprocess.run([compiler_path, '-e', 'llvm', '-l',
                                    'runtime/15411-l4.h0', file_path], stderr=subprocess.PIPE, timeout=25)
                    if (out.returncode !=0):
                        print(f"{file},ret={out.returncode}, out='Could be typeerror'")
                except subprocess.TimeoutExpired:
                    print("FAIL:" ^ file_path ^ "TIMED OUT")

    except FileNotFoundError:
        custom_print(f"Error: Folder '{folder}' not found.")
    except Exception as e:
        custom_print(f"Error: {e}")
    return counter


llc_path = "/opt/homebrew/opt/llvm/bin/llc"


def process_ll_files(folder):
    counter = 0
    try:
        files = os.listdir(folder)
        custom_print(
            f"Processing .ll files in the folder '{folder}'")
        for file in files:
            if os.path.isfile(os.path.join(folder, file)):
                counter += 1
                file_path = os.path.join(folder, file)
                custom_print(f"Processing: {file_path}")
                out = subprocess.run(
                    [llc_path, file_path], stderr=subprocess.PIPE)
                custom_print(out.stderr.decode())

    except FileNotFoundError:
        custom_print(f"Error: Folder '{folder}' not found.")
    except Exception as e:
        custom_print(f"Error: {e}")
    return counter


def execute_s_files(folder):
    allowed_extensions = ['.s']
    try:
        files = os.listdir(folder)
        custom_print(
            f"Processing files in the folder '{folder}' with extensions {allowed_extensions}:")
        for file in files:
            if os.path.isfile(os.path.join(folder, file)) and any(file.endswith(ext) for ext in allowed_extensions):
                file_path = os.path.join(folder, file)
                exe_file = file_path + ".exe"
                custom_print(f"Processing: {file_path}")
                subprocess.run(['clang', file_path,
                               'runtime/run411.c', '-o', exe_file])
                custom_print(f"running:{file=}")
                out = subprocess.run([exe_file], stdout=subprocess.PIPE)
                print(
                    f"{file},ret={out.returncode}, out='{out.stdout.decode()[:-1]}'")

    except FileNotFoundError:
        custom_print(f"Error: Folder '{folder}' not found.")
    except Exception as e:
        custom_print(f"Error: {e}")


def main():
    parser = argparse.ArgumentParser(
        description="Run the 'c0c' compiler with the '-e llvm' option on files with specific extensions in a given folder")
    parser.add_argument("-v", "--verbose",
                        help="Verbose custom_print", action='store_true')
    parser.add_argument(
        "folder", help="Name of the folder to process files from")
    args = parser.parse_args()
    global verbose
    verbose = args.verbose
    create_folder_with_cleanup(os.getcwd(), "llvm")
    print("folder created")
    counter = process_files(args.folder)
    print(f"{counter} ll files are generated")
    counter = process_ll_files(os.path.join(os.getcwd(), "llvm"))
    print(f"{counter} .s files are generated")
    execute_s_files(os.path.join(os.getcwd(), "llvm"))


if __name__ == "__main__":
    main()
