import { Selector } from 'testcafe';
import { rootFixture } from './utils';
import * as fixtures from './fixtures';


const f = fixture(`Proofs`)
rootFixture(f);


const githubProofResult =
  () => Selector('[data-qa=proof-details-github]').textContent;


test('Signer without github source', async t => {
  const file = fixtures.signerFileWithoutGithub;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(githubProofResult())
    .contains('No proof defined');
});


test('Signer with invalid github source', async t => {
  const file = fixtures.signerFileWithInvalidGithub;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(githubProofResult())
    .contains('Verification failed');
})


test('Signer with valid github source', async t => {
  const file = fixtures.signerFileWithValidGithub;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(githubProofResult())
    .contains('Verified: foobar32167');
})
