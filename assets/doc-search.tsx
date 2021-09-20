import {
  DocSearch,
  DocSearchProps as DocSearchComponentProps,
  version,
} from '@docsearch/react';
import React from 'react';
import ReactDOM from 'react-dom';

import '@docsearch/css';

function getHTMLElement(
  value: string | HTMLElement,
  environment: DocSearchProps['environment'] = window
): HTMLElement {
  if (typeof value === 'string') {
    return environment.document.querySelector<HTMLElement>(value)!;
  }

  return value;
}

interface DocSearchProps extends DocSearchComponentProps {
  container?: string | HTMLElement;
  environment?: typeof window;
}

const docsearch = <DocSearch
  indexName='erlang' appId="PX36FSFH8Z" apiKey="efb7fd38dc2b5ec3bbeb664d1b9588e5"
  transformSearchClient={(searchClient) => {
    searchClient.addAlgoliaAgent('docsearch.js', version);
    return searchClient;
  }}
/>;

const button = getHTMLElement('#docsearch')
ReactDOM.render(docsearch, button);

const mobile = getHTMLElement('#docsearch-mobile');
if (mobile) {
  mobile.addEventListener('click',() => {
    button.getElementsByTagName('button')[0].click();
  })
}
