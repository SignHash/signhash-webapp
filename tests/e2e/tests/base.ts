import { Selector } from 'testcafe';
import { rootFixture } from './utils';


const f = fixture`Site`
rootFixture(f);


test('Site is available', async t => {
  await t
    .expect(Selector('body').textContent)
    .contains('SignHash');
});


test('Contracts are loaded', async t => {
  await t
    .expect(Selector('[data-qa=signhash-address]').textContent).ok()
    .expect(Selector('[data-qa=signproof-address]').textContent).ok();
})
