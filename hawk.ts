namespace _HaWK__ {

  export var block: undefined|string[];
  export var blockSrc: undefined|RegExp;

  type LoadedElement = HTMLElement&{src?:string,href?:string};

  function blockTest(type: string, src: string|undefined): boolean {
    let b = block ? block.includes(type) : false;
    if (!b && blockSrc && src)
      b = blockSrc.test(src);
    console.log((b ? "blocking" : "allowing") + " " + type + " " + src);
    return b;
  }

  function initDocument(doc: HTMLDocument): void {
    /* maybe should be click: */
    doc.addEventListener('focusin', function focusin(event) {
      if (event.target instanceof HTMLInputElement && (event.target.type === 'text' || event.target.type === 'password') || 
          event.target instanceof HTMLTextAreaElement) {
        (<any>window).webkit.messageHandlers.hawk.postMessage('input');
      }
    }, false);
    doc.addEventListener('beforeload', function beforeload(event) {
      const el = <LoadedElement>event.target;
      if (!(el instanceof HTMLElement))
        return;
      if (blockTest(el.tagName, el.src || el.href)) {
        event.preventDefault();
        if (el.parentNode)
          el.parentNode.removeChild(el);
      } else if (el instanceof HTMLIFrameElement) {
        if (el.contentDocument)
          /* this does not work, possibly because the document hasn't started loading yet */
          initDocument(el.contentDocument);
      }
    }, true);
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

  console.log("hawk loaded");
}
