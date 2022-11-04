#!/usr/bin/env node

import { fileURLToPath } from 'node:url';
import { dirname, join, parse } from 'node:path';
import { mkdir, writeFile } from "node:fs/promises";
import decompress from 'decompress';
import decompressUnzip from 'decompress-unzip';
import decompressTar from 'decompress-tar';
import plzma from 'plzmasdk';
const __dirname = dirname(fileURLToPath(import.meta.url));
const tag = process.argv.slice(2).at(0) || 'dev';

let packages = {
    'wizer-darwin-arm64': {
        releaseAsset: `wizer-${tag}-aarch64-macos.tar.xz`,
        binaryAsset: 'wizer',
        description: 'The macOS 64-bit binary for Wizer, the WebAssembly Pre-Initializer',
        os: 'darwin',
        cpu: 'arm64',
    },
    'wizer-darwin-x64': {
        releaseAsset: `wizer-${tag}-x86_64-macos.tar.xz`,
        binaryAsset: 'wizer',
        description: 'The macOS 64-bit binary for Wizer, the WebAssembly Pre-Initializer',
        os: 'darwin',
        cpu: 'x64',
    },
    'wizer-linux-x64': {
        releaseAsset: `wizer-${tag}-x86_64-linux.tar.xz`,
        binaryAsset: 'wizer',
        description: 'The Linux 64-bit binary for Wizer, the WebAssembly Pre-Initializer',
        os: 'darwin',
        cpu: 'x64',
    },
    'wizer-win32-x64': {
        releaseAsset: `wizer-${tag}-x86_64-windows.zip`,
        binaryAsset: 'wizer.exe',
        description: 'The Windows 64-bit binary for Wizer, the WebAssembly Pre-Initializer',
        os: 'win32',
        cpu: 'x64',
    },
}

const response = await fetch(`https://api.github.com/repos/bytecodealliance/wizer/releases/tags/${tag}`)
if (!response.ok) {
    console.error(`Response from https://api.github.com/repos/bytecodealliance/wizer/releases/tags/${tag} was not ok`, response)
    console.error(await response.text())
    process.exit(1)
}
response = await response.json()
const id = response.id
const assets = await fetch(`https://api.github.com/repos/bytecodealliance/wizer/releases/${id}/assets`)
if (!assets.ok) {
    console.error(`Response from https://api.github.com/repos/bytecodealliance/wizer/releases/${id}/assets was not ok`, assets)
    console.error(await response.text())
    process.exit(1)
}
assets = await assets.json()

for (const [packageName, info] of Object.entries(packages)) {
    const asset = assets.find(asset => asset.name === info.releaseAsset)
    if (!asset) {
        console.error(`Can't find an asset named ${info.releaseAsset} for the release https://github.com/bytecodealliance/wizer/releases/tag/${tag}`)
        process.exit(1)
    }
    const packageDirectory = join(__dirname, '../', packageName.split('/').pop())
    await mkdir(packageDirectory, { recursive: true })
    await writeFile(join(packageDirectory, 'package.json'), packageJson(packageName, tag, info.description, info.os, info.cpu))
    await writeFile(join(packageDirectory, 'index.js'), indexJs(info.binaryAsset))
    const browser_download_url = asset.browser_download_url;
    const archive = await fetch(browser_download_url)
    if (!archive.ok) {
        console.error(`Response from ${browser_download_url} was not ok`, archive)
        console.error(await response.text())
        process.exit(1)
    }
    let buf = await archive.arrayBuffer()

    // Need to decompress into the original tarball format for later use in the `decompress` function
    if (info.releaseAsset.endsWith('.xz')) {
        const archiveDataInStream = new plzma.InStream(buf);
        const decoder = new plzma.Decoder(archiveDataInStream, plzma.FileType.xz);
        decoder.open();

        // We know the xz archive only contains 1 file, the tarball
        // We extract the tarball in-memory, for later use in the `decompress` function
        const selectedItemsToStreams = new Map();
        selectedItemsToStreams.set(decoder.itemAt(0), plzma.OutStream());

        decoder.extract(selectedItemsToStreams);
        for (const value of selectedItemsToStreams.values()) {
            buf = value.copyContent()
        }
    }
    await decompress(Buffer.from(buf), packageDirectory, {
        // Remove the leading directory from the extracted file.
        strip:1,
        plugins: [
            decompressUnzip(),
            decompressTar()
        ],
        // Only extract the binary file and nothing else
        filter: file => parse(file.path).base === info.binaryAsset
    })
}

function indexJs(binaryAsset) {
    return `
import { fileURLToPath } from 'node:url'
import { dirname, join } from 'node:path'
const __dirname = dirname(fileURLToPath(import.meta.url))
let location = join(__dirname, '${binaryAsset}')
export default location
`
}
function packageJson(name, version, description, os, cpu) {
    return JSON.stringify({
        name: `@bytecode-alliance/${name}`,
        bin: {
            [name]: "wizer"
        },
        type: "module",
        version,
        main: "index.js",
        description,
        license: "Apache-2.0",
        preferUnplugged: false,
        os: [os],
        cpu: [cpu],
    }, null, 4);
}
