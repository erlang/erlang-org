import docsearch from '@docsearch/js';

(function() {
    var a = document.getElementById('docsearch'),
        b;
    document.addEventListener('keydown', function(b) {
        b.ctrlKey && b.key === 'k' &&
            (b.preventDefault(), a.focus())
    });
    docsearch({
        appId: 'T71IQF5OFV',
        apiKey: 'a2c3c6c0120e6eb2574bb328bd316967',
        indexName: 'erlang_org',
        container: '#docsearch',
        // Set debug to true to inspect the dropdown
        debug: false,
        algoliaOptions: {
            hitsPerPage: 10
        }
    });
})();

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