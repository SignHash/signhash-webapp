import { Selector } from 'testcafe';
import { rootFixture } from './utils';
import * as fixtures from './fixtures';


const f = fixture(`Signing`)
rootFixture(f, '/sign');


test('My account details are visible', async t => {
  const file = fixtures.someFile;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(Selector('[data-qa=my-eth-address]').textContent)
    .contains(fixtures.myAccount);
});
