import { Selector } from 'testcafe';
import { rootFixture } from './utils';
import { fileBuilder } from './fixtures';


const signedFile = fileBuilder('signed.txt');
const unsignedFile = fileBuilder('unsigned.txt')


const f = fixture(`Files`)
rootFixture(f);


test('Verifying not signed file', async t => {
  const file = unsignedFile;

  await t.setFilesToUpload('#file-upload', file.path)
  await assertChecksum(t, file.checksum);
  await t
    .expect(bodyTextSelector()).contains(file.name)
    .expect(bodyTextSelector()).contains("has not been signed.");
});


test('Verifying signed file', async t => {
  const file = signedFile;

  await t.setFilesToUpload('#file-upload', file.path)
  await assertChecksum(t, file.checksum);
  await t
    .expect(bodyTextSelector()).contains(file.name)
    .expect(bodyTextSelector()).contains(file.signers[0]);
});


interface ChecksumSelector extends Selector {
  fullChecksum: Promise<any>;
}


const bodyTextSelector = () => Selector('body').textContent;


const checksumSelector = () =>
  <ChecksumSelector>Selector('[data-qa=checksum]').addCustomDOMProperties({
    fullChecksum: el => el.getAttribute('title'),
  });


function assertChecksum(t: TestController, checksum: string) {
  return t
    .expect(checksumSelector().textContent).contains(checksum.slice(0, 6))
    .expect(checksumSelector().fullChecksum).contains(checksum)
}
