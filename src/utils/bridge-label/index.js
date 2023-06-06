import { LABEL_HTML, PRE_LABEL_STYLE, MINI_LABEL_STYLE, PRINT_LABEL_STYLE } from './constant'
import { combineHtml } from '@/utils/print/base'

// 产品标签预览样式
export function getPreviewLabelHtml({ productType, labelType, labelData }) {
  const html = LABEL_HTML[productType][labelType]({ ...labelData })
  const { style, fClass, qrPosition } = PRE_LABEL_STYLE[productType][labelType]
  const showHtml = `
    <div class="${fClass}">
      ${html}
    </div>
    ${style}
  `
  return { showHtml, qrPosition }
}

// 产品标签打印配置预览样式
export function getMiniLabelHtml({ productType, labelType, labelData }) {
  const html = LABEL_HTML[productType][labelType]({ ...labelData })
  const { style, fClass } = MINI_LABEL_STYLE[productType][labelType]
  return `
    <div class="${fClass}">
     ${html}
    </div>
    ${style}
  `
}

// 产品标签打印样式
export function getPrintLabelHtml({ productType, labelType, component, productionLineName, logo, manufacturerPhone, manufacturerURL, manufacturerName, printConfig }) {
  const html = LABEL_HTML[productType][labelType]({ component, productionLineName, logo, manufacturerPhone, manufacturerURL, manufacturerName, printConfig })
  const { style, fClass } = PRINT_LABEL_STYLE[productType][labelType]
  const body = `
    <div class="${fClass}">
      ${html}
    </div>`
  return combineHtml(style, body)
}
