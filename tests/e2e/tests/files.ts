import { Selector } from 'testcafe';
import { rootFixture } from './utils';
import { fileBuilder } from './fixtures';


const signedFile = fileBuilder('signed.txt');
const unsignedFile = fileBuilder('unsigned.txt')


const f = fixture(`Files`)
rootFixture(f);


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
