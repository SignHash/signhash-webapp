import { Selector } from 'testcafe';
import { readFileSync } from 'fs';
import * as path from 'path';


const knownFile = {
  path: 'foo.txt',
  name: 'foo.txt',
  hash: readFileSync(path.join(__dirname, 'foo.hash')).toString()
};


fixture`SignHash`
  .page`http://localhost:8080`;


test('Verifying known hash', async t => {
  const file = knownFile;
  const text = Selector('body').textContent

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(text).contains(file.name)
    .expect(text).contains(file.hash)
    ;
});
