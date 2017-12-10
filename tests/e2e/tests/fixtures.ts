import { readFileSync } from 'fs';
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


export const fileBuilder = buildTestFile(accounts);


const accountFile = (i: number) => fileBuilder(`acc${i}.txt`);


export const signerFileWithoutGithub = accountFile(0);
export const signerFileWithMissingGithubProof = accountFile(1);
export const signerFileWithValidGithub = accountFile(2);
export const signerFileWithInvalidGithubProofValue = accountFile(3);
export const signerFileWithXSSGithubProofValue = accountFile(4);
