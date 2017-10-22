import { Selector } from 'testcafe';
import { readFileSync } from 'fs';
import * as path from 'path';
import * as got from 'got';


const maxAppWarmup = 15;
const rootURL = process.env.SIGNHASH_URL || 'http://localhost:8080';


const knownFile = {
  path: 'foo.txt',
  name: 'foo.txt',
  hash: readFileSync(path.join(__dirname, 'foo.hash')).toString().trim(),
};


fixture`SignHash`
  .page(rootURL)
  .before(waitForPage(maxAppWarmup));


test('Site is available', async t => {
  await t
    .expect(Selector('body').textContent)
    .contains('SignHash');
});


test('Verifying known hash', async t => {
  const file = knownFile;
  const text = Selector('body').textContent;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(text).contains(file.name)
    .expect(text).contains(file.hash);
});


function waitForPage(timeout: number) {
  return async () => {
    for (var i = 0; i < timeout; i++) {
      try {
        await got(rootURL, { timeout: 1000 });
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
