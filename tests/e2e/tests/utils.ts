import { ClientFunction } from 'testcafe';
import * as got from 'got';


const maxAppWarmup = 45;
export const rootURL = process.env.SIGNHASH_URL || 'http://localhost:8080';


function sendRpc(web3: any, method: string, params?: any) {
  return new Promise((resolve, reject) => {
    web3.currentProvider.sendAsync({
      jsonrpc: '2.0',
      method,
      params: params || [],
      id: new Date().getTime(),
    }, (err: any, res: any) => {
      if (err) {
        reject(err);
      } else {
        resolve(res);
      }
    });
  });
};


const snapshot = ClientFunction(() => {
  const web3: any = (window as any).web3;
  return sendRpc(web3, 'evm_snapshot');
}, { dependencies: { sendRpc } })


const revert = ClientFunction((id: string) => {
  const web3: any = (window as any).web3;
  return sendRpc(web3, 'evm_revert', [id]);
}, { dependencies: { sendRpc } })


const boundToTest =
  (func: ClientFunction, ...args: any[]) =>
    (t: TestController) =>
      func.with({ boundTestRun: t })(...args)


export const changeAccount = ClientFunction((account: string) => {
  const web3: any = (window as any).web3;
  web3.accounts = () => Promise.resolve([account]);
  return new Promise(resolve => setTimeout(resolve, 3000));
});



export function rootFixture(fixture: FixtureFn, endpoint?: string) {
  let lastSnapshotId: string;

  return fixture
    .page(rootURL + (endpoint ? '#' + endpoint : ''))
    .before(waitForPage(rootURL, maxAppWarmup))
    .beforeEach(async t => {
      const response = await boundToTest(snapshot)(t);
      lastSnapshotId = response.result;
    })
    .afterEach((t) => boundToTest(revert, lastSnapshotId)(t));
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


export const pages = {
  verify: '#/',
}


export const reload = (t: TestController) =>
  t.eval(() => location.reload(true));


export const clearInput = (t: TestController, sel: Selector) =>
  t.click(sel).pressKey('ctrl+a delete')
