import { Selector } from 'testcafe';
import { rootFixture } from './utils';


const f = fixture`Site`
rootFixture(f);


test('Site is available', async t => {
  await t
    .expect(Selector('body').textContent)
    .contains('SignHash');
});


test('Contract is loaded', async t => {
  await t
    .expect(Selector('[data-qa=contract-address]').textContent).ok();
})
