import { Selector } from 'testcafe';
import { rootFixture, pages, reload } from './utils';
import * as fixtures from './fixtures';


const f = fixture(`Identity`)
rootFixture(f, '/account');


const proofMethods = ['github', 'http'];


test('No identity is displayed', async t => {
  for (const method of proofMethods) {
    await expectNoProofValue(t, method);
  }
});


test('Identity can be added', async t => {
  const method = 'github';
  const newProof = 'foobar';

  await t
    .click(addProofSelector(method))
    .typeText(proofInputSelector(method), newProof)
    .click(updateProofSelector(method))
    ;

  await expectValueUpdated(t, method, newProof);
  await reload(t);
  await expectValueUpdated(t, method, newProof);
});


const expectNoProofValue = (t: TestController, method: string) =>
  t
    .expect(proofValueSelector(method))
    .notOk(`Proof ${method} expected to be empty`)


const expectValueUpdated = (
  t: TestController,
  method: string,
  expectedValue: string,
) =>
  t
    .expect(proofInputSelector(method).value)
    .contains(expectedValue)
  ;


const proofValueSelector = (method: string) =>
  Selector(`[data-qa=identity-${method}-content]`).value;


const addProofSelector = (method: string) =>
  Selector(`[data-qa=identity-${method}-add]`);


const proofInputSelector = (method: string) =>
  Selector(`[data-qa=identity-${method}-input]`);


const updateProofSelector = (method: string) =>
  Selector(`[data-qa=identity-${method}-update]`);
