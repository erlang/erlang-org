import type { AutocompleteState } from "@algolia/autocomplete-core"
import {
  DocSearch,
  DocSearchProps as DocSearchComponentProps,
  version,
} from '@docsearch/react';
import type { InternalDocSearchHit, StoredDocSearchHit } from "@docsearch/react/dist/esm/";
import React from 'react';
import ReactDOM from 'react-dom';

import '@docsearch/css';

declare var _docsearch_version: string;

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

function Footer({ state }: {
  state: AutocompleteState<InternalDocSearchHit>;
}): JSX.Element {
  return <>
    <footer>
      <a href={"/doc/search?v="+_docsearch_version+"&q=" + state.query}>
        Show all {state.context.nbHits as number} results
      </a>
    </footer>
  </>;
}

interface HitProps {
  hit: InternalDocSearchHit | StoredDocSearchHit;
  children: React.ReactNode;
}

function Hit({ hit }: HitProps): JSX.Element {
  let myHit = hit as InternalDocSearchHit & { isModule: boolean} | StoredDocSearchHit & { isModule: boolean};
  let path = hit.hierarchy.lvl1;
  if (hit.hierarchy.lvl2) {
    path += myHit.isModule ? ':' : ' → ';
    path += hit.hierarchy.lvl2;
  }
  return <a href={hit.url}>
    <div className="DocSearch-Hit-Container">
      <div className="DocSearch-Hit-icon">
        <svg width="20" height="20" viewBox="0 0 20 20">
          <path d="M17 5H3h14zm0 5H3h14zm0 5H3h14z" stroke="currentColor" fill="none" fill-rule="evenodd" stroke-linejoin="round"></path>
        </svg>
      </div>
      <div className="DocSearch-Hit-content-wrapper">
        <span className="DocSearch-Hit-title">{hit.content}</span>
        <span className="DocSearch-Hit-path">{path}</span>
      </div>
      <div className="DocSearch-Hit-action">
        <svg className="DocSearch-Hit-Select-Icon" width="20" height="20" viewBox="0 0 20 20">
          <g stroke="currentColor" fill="none" fill-rule="evenodd" stroke-linecap="round" stroke-linejoin="round">
            <path d="M18 3v4c0 2-2 4-4 4H2"></path><path d="M8 17l-6-6 6-6"></path>
          </g>
        </svg>
      </div>
    </div>
  </a>;
}

const docsearch = <DocSearch
  appId='LUYTU1J2MB'
  indexName='erlang' apiKey="86152ba1d4a9d7e179d537b8060a4c31"
  searchParameters={{
    filters: 'version:'+_docsearch_version,
    attributesToRetrieve: [
      'hierarchy.lvl0',
      'hierarchy.lvl1',
      'hierarchy.lvl2',
      'hierarchy.lvl3',
      'hierarchy.lvl4',
      'hierarchy.lvl5',
      'hierarchy.lvl6',
      'content',
      'type',
      'url',
      'isModule']
  }}
  resultsFooterComponent={Footer}
  hitComponent={Hit}
  transformSearchClient={(searchClient) => {
    searchClient.addAlgoliaAgent('docsearch.js', version);
    return searchClient;
  }}
/>;

const button = getHTMLElement('#docsearch')
ReactDOM.render(docsearch, button);

const mobile = getHTMLElement('#docsearch-mobile');
if (mobile) {
  mobile.addEventListener('click', () => {
    button.getElementsByTagName('button')[0].click();
  })
}
