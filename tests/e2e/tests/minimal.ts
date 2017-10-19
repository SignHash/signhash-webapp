import { Selector } from 'testcafe';


fixture`FooBar`
  .page`http://app:8080`;


test('Test', async t => {
  const text = Selector('body').textContent

  await t
    .expect(text).contains('foobar')
    ;
});
