import { Selector } from 'testcafe';
import { rootFixture, pages } from './utils';
import * as fixtures from './fixtures';


const f = fixture(`Signing`)
rootFixture(f, '/sign');


test('My account details are visible', async t => {
  const file = fixtures.someFile;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(Selector('[data-qa=my-id] [data-qa=proof-details-eth]').textContent)
    .contains(fixtures.myAccount);
});


test('File becomes signed after page is reloaded', async t => {
  const file = fixtures.unsignedFile;

  await t
    .setFilesToUpload('#file-upload', file.path)
    .click('[data-qa=sign]')
    .expect(Selector('[data-qa=tx-status]').textContent)
    .contains('Successful');

  await t
    .navigateTo(pages.verify)
    .eval(() => location.reload(true));

  await t
    .setFilesToUpload('#file-upload', file.path)
    .expect(signersSelector())
    .contains(fixtures.myAccount);
});


const signersSelector = () => Selector('body').textContent;
