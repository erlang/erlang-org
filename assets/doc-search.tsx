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
  appId='LUYTU1J2MB'
  indexName='erlang' apiKey="86152ba1d4a9d7e179d537b8060a4c31"
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
