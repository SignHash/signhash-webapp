import { Selector } from 'testcafe';
import * as got from 'got';
import { TestFile, buildTestFile, waitForPage, accounts } from './utils';


const maxAppWarmup = 45;
const rootURL = process.env.SIGNHASH_URL || 'http://localhost:8080';


const fileBuilder = buildTestFile(accounts);
const signedFile = fileBuilder('signed.txt');
const unsignedFile = fileBuilder('unsigned.txt')


fixture`SignHash`
  .page(rootURL)
  .before(waitForPage(rootURL, maxAppWarmup));


test('Site is available', async t => {
  await t
    .expect(Selector('body').textContent)
    .contains('SignHash');
});


test('Contract is loaded', async t => {
  await t
    .expect(Selector('[data-qa=contract-address]').textContent).ok();
})


test('Verifying not signed file', async t => {
  const file = unsignedFile;
  const text = Selector('body').textContent;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(text).contains(file.name)
    .expect(text).contains(file.checksum)
    .expect(text).contains("No signers");
});


test('Verifying signed file', async t => {
  const file = signedFile;
  const text = Selector('body').textContent;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(text).contains(file.name)
    .expect(text).contains(file.checksum)
    .expect(text).contains(file.signers[0]);
});
