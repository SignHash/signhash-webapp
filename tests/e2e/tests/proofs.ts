import { Selector } from 'testcafe';
import { rootFixture } from './utils';
import { fileBuilder, signerFileWithoutGithub } from './fixtures';


const f = fixture(`Proofs`)
rootFixture(f);


test('Signer without github source', async t => {
  const file = signerFileWithoutGithub;
  const text = Selector('body').textContent;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(Selector('[data-qa=proof-details-github]').textContent)
    .contains('No proof defined');
});
