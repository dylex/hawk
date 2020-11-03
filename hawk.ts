namespace _HaWK__ {

  function initDocument(doc: HTMLDocument): void {
    /* maybe should be click: */
    doc.addEventListener('focusin', function focusin(event) {
      if (event.target instanceof HTMLInputElement && (event.target.type === 'text' || event.target.type === 'password') || 
          event.target instanceof HTMLTextAreaElement) {
        (<any>window).webkit.messageHandlers.hawk.postMessage('input');
      }
    }, false);
  }

  initDocument(document);

  const links = document.getElementsByTagName("a");

  var linkSelected = -1;

  function linkFocus(n: number): void {
    linkSelected = n;
    links[n].focus();
  }

  export function linkSelect(link: string, match: RegExp): void {
    const el = document.querySelector('link[rel=' + link + ']');
    if (el instanceof HTMLLinkElement) {
      location.assign(el.href);
      return;
    }
    for (var i = linkSelected+1; i < links.length; i++) {
      if (match.test(links[i].text)) {
        linkFocus(i);
        break;
      }
    }
    linkSelected = -1;
  }

}
