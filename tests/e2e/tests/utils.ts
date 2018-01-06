import { ClientFunction } from 'testcafe';
import * as got from 'got';


const maxAppWarmup = 45;
export const rootURL = process.env.SIGNHASH_URL || 'http://localhost:8080';


function sendRpc(web3: any, method: string, params?: any) {
  return new Promise((resolve) => {
    web3.currentProvider.sendAsync({
      jsonrpc: '2.0',
      method,
      params: params || [],
      id: new Date().getTime(),
    }, (err: any, res: any) => { resolve(res); });
  });
};


const snapshot = ClientFunction(() => {
  const web3: any = (window as any).web3;
  return sendRpc(web3, 'evm_snapshot');
}, { dependencies: { sendRpc } })


const restore = ClientFunction(() => {
  const web3: any = (window as any).web3;
  return sendRpc(web3, 'evm_restore');
}, { dependencies: { sendRpc } })


const boundToTest = (func: ClientFunction) => (t: TestController) =>
  func.with({ boundTestRun: t })()


export function rootFixture(fixture: FixtureFn, endpoint?: string) {
  var snapshotInitialized = false;

  return fixture
    .page(rootURL + (endpoint ? '#' + endpoint : ''))
    .before(waitForPage(rootURL, maxAppWarmup))
    .beforeEach(async t => {
      if (!snapshotInitialized) {
        await boundToTest(snapshot)(t);
        snapshotInitialized = true;
      }
    })
    .afterEach(boundToTest(restore));
}


export function waitForPage(url: string, timeout: number) {
  return async () => {
    for (var i = 0; i < timeout; i++) {
      try {
        await got(url, { timeout: 1000 });
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
