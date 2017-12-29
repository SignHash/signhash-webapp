import * as got from 'got';


const maxAppWarmup = 45;
export const rootURL = process.env.SIGNHASH_URL || 'http://localhost:8080';


export function rootFixture(fixture: FixtureFn, endpoint?: string) {
  return fixture
    .page(rootURL + (endpoint ? '#' + endpoint : ''))
    .before(waitForPage(rootURL, maxAppWarmup));
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
