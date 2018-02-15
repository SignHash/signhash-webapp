import { Selector } from 'testcafe';
import { rootFixture, pages } from './utils';
import * as fixtures from './fixtures';


const f = fixture(`Identity`)
rootFixture(f, '/account');


const proofMethods = ['github', 'http'];


test('No identity is displayed', async t => {
  for (const method of proofMethods) {
    await expectNoProofValue(t, method)
  }
});


const expectNoProofValue = (t: TestController, method: string) =>
  t.expect(proofValueSelector(method)).notOk()


const proofValueSelector = (method: string) =>
  Selector(`[data-qa=identity-${method}-content]`).value;
