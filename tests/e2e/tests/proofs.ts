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
  const file = fixtures.signerFileWithMissingGithubProof;

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


test('Signer with illegal github username', async t => {
  const file = fixtures.signerFileWithInvalidGithubProofValue;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(githubProofResult())
    .contains('Verification failed')
    .expect(githubProofResult())
    .contains('InvalidProofValue');
})


test.only('Signer with XSS proof value', async t => {
  const file = fixtures.signerFileWithXSSGithubProofValue;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(githubProofResult())
    // Note: This tests the text content of the selector.
    // If it is present, than it means that XSS attempt has been rendered
    // just as a safe, encoded text.
    .contains("<script>alert('f');</script>")
})
