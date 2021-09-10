import docsearch from '@docsearch/js/dist/umd';

import '@docsearch/css';

docsearch({
    container: '#docsearch',
    indexName: 'erlang_org',
    appId: 'T71IQF5OFV',
    apiKey: 'a2c3c6c0120e6eb2574bb328bd316967',
    searchParameters: {
        hitsPerPage: 10
    }
});

docsearch({
    container: '#docsearch-mobile',
    indexName: 'erlang_org',
    appId: 'T71IQF5OFV',
    apiKey: 'a2c3c6c0120e6eb2574bb328bd316967',
    searchParameters: {
        hitsPerPage: 10
    }
});
