import docsearch from '@docsearch/js/dist/umd';

// import './app.css';
import '@docsearch/css';

docsearch({
  container: '#docsearch',
  indexName: 'erlang_org',
  appId: 'T71IQF5OFV',
  apiKey: 'a2c3c6c0120e6eb2574bb328bd316967',
});

docsearch({
    container: '#docsearch-mobile',
    indexName: 'erlang_org',
    appId: 'T71IQF5OFV',
    apiKey: 'a2c3c6c0120e6eb2574bb328bd316967',
  });

export function toggleSearch() {
    var leftnav = document.getElementById('search');
    var topbar = document.getElementsByClassName('search-expand')[0];
    if (leftnav.classList.contains('show')) {
        leftnav.classList.remove('show');
        topbar.classList.remove('show');
        leftnav.classList.add('overflow-hidden');
    } else {
        leftnav.classList.add('show');
        topbar.classList.add('show');
        setTimeout(() => {
            leftnav.classList.remove('overflow-hidden');
        }, 350);
    }
}