import { Selector } from 'testcafe';
import * as got from 'got';
import { TestFile, buildTestFile, waitForPage, accounts } from './utils';


const maxAppWarmup = 15;
const rootURL = process.env.SIGNHASH_URL || 'http://localhost:8080';


const signedFile = buildTestFile('signed.txt', accounts[0]);
const unsignedFile = buildTestFile('unsigned.txt')


fixture`SignHash`
  .page(rootURL)
  .before(waitForPage(rootURL, maxAppWarmup));


test('Site is available', async t => {
  await t
    .expect(Selector('body').textContent)
    .contains('SignHash');
});


test('Verifying not signed file', async t => {
  const file = unsignedFile;
  const text = Selector('body').textContent;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(text).contains(file.name)
    .expect(text).contains(file.hash)
    .expect(text).contains("No signers");
});


test('Verifying signed file', async t => {
  const file = signedFile;
  const text = Selector('body').textContent;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(text).contains(file.name)
    .expect(text).contains(file.hash)
    .expect(text).contains(file.signer);
});
