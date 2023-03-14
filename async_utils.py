"""Async utils."""
import asyncio
from typing import Any, Coroutine, List


def run_async_tasks(tasks: List[Coroutine]) -> List[Any]:
    """Run a list of async tasks."""

    async def _gather() -> List[Any]:
        return await asyncio.gather(*tasks)

    outputs: List[Any] = asyncio.run(_gather())
    return outputs


import os
import json
import asyncio
from typing import List, Dict


async def index_file(file_path: str) -> Dict[str, List[int]]:
    """
    Index a file and return a dictionary containing the words and their positions.

    Args:
        file_path (str): Path to the file to index.

    Returns:
        dict: Dictionary containing the words and their positions in the file.
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    words = content.split()
    index = {}
    for i, word in enumerate(words):
        if word not in index:
            index[word] = [i]
        else:
            index[word].append(i)
    
    return index


async def construct_index(data_dir: str) -> Dict[str, Dict[str, List[int]]]:
    """
    Construct an index of all files in a directory.

    Args:
        data_dir (str): Path to the directory containing the files to index.

    Returns:
        dict: Dictionary containing the file names and their indices.
    """
    tasks = []
    for file_name in os.listdir(data_dir):
        file_path = os.path.join(data_dir, file_name)
        if os.path.isfile(file_path):
            tasks.append(index_file(file_path))

    indices = await asyncio.gather(*tasks)
    index = {}
    for i, file_name in enumerate(os.listdir(data_dir)):
        file_path = os.path.join(data_dir, file_name)
        if os.path.isfile(file_path):
            index[file_name] = indices[i]

    return index

