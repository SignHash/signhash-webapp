import { readFileSync } from 'fs';
import * as got from 'got';
import * as path from 'path';


const dataDir = path.join(__dirname, '../../data');
const dataPath = (name: string, ...dirs: string[]) =>
  path.join(dataDir, ...dirs, name);


export const readDataFile = (name: string) => readFileSync(dataPath(name));

export const accounts = readDataFile('accounts.txt').toString()
  .trim()
  .split('\n')


export interface TestFile {
  path: string;
  name: string;
  checksum: string;
  signers: number[];
}


export const buildTestFile =
  (accounts: string[]) => (name: string): TestFile => {
    const path = dataPath(name, 'files');
    const meta = JSON.parse(readFileSync(path + '.json').toString());
    return {
      path,
      name,
      signers: meta.signers,
      checksum: meta.checksum,
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
