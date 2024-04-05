import bz2
import os
from urllib.parse import urljoin

import requests
from bs4 import BeautifulSoup


def HttpsScan(url):
    """
    Function to scan the directory of the input url using Beautiful Soup

    :param url: The URL of the directory to be scanned
    :type url: str
    :return: list of urls, file names to the target file if successfully accessed, otherwise an empty list
    :rtype: List[str]
    """

    try:
        response = requests.get(url)
        if response.status_code == 200:
            soup = BeautifulSoup(response.content, 'html.parser')
            links = soup.find_all('a', href=True)
            urls = [urljoin(url, link['href']) for link in links]
            files = [link['href'] for link in links]
            return urls, files
        else:
            print(f"Failed to scan directory: {response.status_code}")
            return []
    except Exception as e:
        print(f"Error: {e}")
        return []


def RequestWebFile(file_url, fname, directory):
    r"""
    Function to request the target file from remote server

    :param file_url: The URL of the file to be requested.
    :type file_url: str
    :param fname: The filename to save the downloaded file as.
    :type fname: str
    :param directory: Local target directory of the downloaded file
    :type directory: str
    :return: True if the file was successfully downloaded and extracted, False otherwise.
    :rtype: bool
    """

    # Data consistency check
    assert type(file_url) == type(fname) == type(directory) == str, "Input should be string"

    # Define the header setup for downloading the files
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36',
        'Referer': 'https://example.com'
    }

    target_path = os.path.join(directory, fname)

    response = requests.get(file_url, headers)
    if response.status_code == 200:
        with open(target_path, 'wb') as f:
            f.write(response.content)

        # Check if the downloaded file is a bz2 file
        if target_path.endswith('.bz2'):
            try:
                # Open the downloaded file and decompress it
                with open(target_path, 'rb') as f_bz2:
                    decompressed_content = bz2.decompress(f_bz2.read())
                # Write the decompressed content back to the file
                with open(target_path[:-4], 'wb') as f_decompressed:
                    f_decompressed.write(decompressed_content)
                # Remove the bz2 file
                os.remove(target_path)
                return True
            except Exception as e:
                print(f"Error decompressing file: {e}")
                return False
        else:
            return True
    else:
        return False


def ModelLoad(model, parameters_dict):
    r"""Entry point of the wget routine of the
    :param model:
    :return:
    """

    env_dict = parameters_dict

    vnames = [x for x in env_dict.__getattribute__((model + '_variable'))]
    for vname in vnames:
        local_storage_path = os.path.join(env_dict.local_directory[model], vname)
        url = urljoin(env_dict.model_url[model],
                      env_dict.__getattribute__((model + '_variable'))[vname])
        fileurls, filenames = HttpsScan(url)
        for i in range(len(fileurls)):
            status = RequestWebFile(fileurls[i], filenames[i], local_storage_path)
            assert status == True, "Web request failed"
