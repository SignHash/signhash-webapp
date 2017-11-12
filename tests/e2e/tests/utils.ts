import { readFileSync } from 'fs';
import * as got from 'got';
import * as path from 'path';


const dataDir = path.join(__dirname, '../../data');
const dataPath = (name: string) => path.join(dataDir, name);


export const readDataFile = (name: string) => readFileSync(dataPath(name));

export const accounts = readDataFile('accounts.txt').toString()
  .trim()
  .split('\n')


export interface TestFile {
  path: string;
  name: string;
  hash: string;
  signer?: string;
}


export function buildTestFile(name: string, signer?: string): TestFile {
  const path = dataPath(name);
  return {
    path,
    name,
    signer,
    hash: readFileSync(path + '.sha256').toString().trim(),
  }
}


export function waitForPage(url: string, timeout: number) {
  return async () => {
    for (var i = 0; i < timeout; i++) {
      try {
        await got(url, { timeout: 1000 });
        break;
      } catch (err) {
        console.log('Fetching page failed, retrying');
        await sleep(1000);
      }
    }
  }
}


function sleep(ms: number) {
  return new Promise(resolve => {
    setTimeout(resolve, ms)
  })
}
