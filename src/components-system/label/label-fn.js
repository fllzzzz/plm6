import { LABEL_HTML, PRE_LABEL_STYLE, MINI_LABEL_STYLE } from './label-constant'

export function getPreviewLabelHtml({ productType, labelType, labelData, printConfig }) {
  console.log(labelType, 'labelType')
  const html = LABEL_HTML[productType][labelType]({ ...labelData, printConfig })
  const { style, fClass } = PRE_LABEL_STYLE[productType][labelType]
  return `
    <div class="${fClass}">
      ${html}
    </div>
    ${style}
  `
}

export function getMiniLabelHtml({ productType, labelType, labelData, printConfig }) {
  const html = LABEL_HTML[productType][labelType]({ ...labelData, printConfig })
  const { style, fClass } = MINI_LABEL_STYLE[productType][labelType]
  return `
    <div class="${fClass}">
     ${html}
    </div>
    ${style}
  `
}
