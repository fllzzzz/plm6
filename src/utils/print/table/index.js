/**
 * 因打印控件原因导致部分功能无法实现，因此“剑走偏锋”，通过其他方式实现部分功能
 * 若日后打印控件部分功能升级，可尝试使用table-disable.js文件
 * 当前文件有很多功能通过取巧来实现，请勿参考
 * @author duhh
 */
// 备注：增加了每项内容的margin
import { toThousand } from '@/utils/data-type/number'
import { isBlank, isNotBlank } from '@data-type/index'
import { emptyTextFormatter } from '@/utils/data-type'
import { convertUnits } from '@/utils/convert/unit'

import { projectNameFormatter } from '@/utils/project'
import { matClsEnum } from '@enum-ms/classification'

import { orientEnum, amountUnitEnum, dataSourceEnum, pageFormatEnum, alignEnum, verticleAlignEnum, fieldTypeEnum, printModeEnum as PrintMode } from '../enum'
import { convertColumns, delNotDisplayed, getLastColumns } from '../page-handle'

import { projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'
import EO from '@/utils/enum'
import { getMaterialTypeUnit, getMaterialListTypeUnit } from '@/utils/unit'
import enumAll from '@/utils/enum/all'
import { MIN_UNIT } from '@/settings/config'
import moment from 'moment'
import { getLODOP, printByMode } from '../base'
import * as lodash from 'lodash'

let LODOP

/**
 * 打印表格
 * @param {object} header 表头信息
 * @param {object} footer 底部信息
 * @param {object} table 表格信息
 * @param {object} qrCode 二维码
 * @param {object} config 配置信息
 * @param {number} printMode 打印模式（enum）
 * @param {number} intCopies 打印数量
 * @author duhh
 */
async function printTable({ header, table, footer, qrCode, config, printMode = PrintMode.QUEUE.V } = {}, intCopies = 1) {
  if (isBlank(config)) {
    throw new Error('打印未配置')
  }
  let result = false
  try {
    setColumns(config.table)
    setStyle(config) // 设置各模块样式
    const headHtml = getTitleHtml(config.title) // 拼接标题
    const tableHtml = getTableHtml({ header, footer, table, globalConfig: config }) // 拼接表格
    const footerHtml = getFooterHtml(footer, config) // 拼接底部信息
    const pageNumberHtml = getPageHtml(config.page) // 拼接页码信息
    const logoHtml = getLogoHtml(config.logo) // logo信息
    let prevHeight = config.paddingTB // 设置上边距
    let tbOffset2Top = 0 // 表格次页TOP偏移
    const notHeaderSpacing = 3 // header不存在时，增加表格与table之间的间距（设置table的margin-top无效，因此在此设置）

    LODOP = await getLODOP()

    const loadHandler = async () => {
      // 打印方向
      const orient = config.orient || orientEnum.LONGITUDINAL.V
      // 设置纸张大小
      LODOP.SET_PRINT_PAGESIZE(orient, config.width + config.unit, config.height + config.unit, 'CreateCustomPage')
      // 标题
      LODOP.ADD_PRINT_HTM(`${prevHeight}${config.unit}`, 0, '100%', `${config.title.height}${config.unit}`, headHtml)
      LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1) // 设置标题每页显示
      if (isNotBlank(config.title) && config.title.show) {
        // 若不是每页显示，则只有第一页显示
        prevHeight += config.title.height
        if (!config.title.allPage) {
          // 如果title不是每页都显示（只有第一页显示），则增加table次页偏移距离
          LODOP.SET_PRINT_STYLEA(0, 'PageIndex', 'first')
          tbOffset2Top += config.title.height
        }
      }
      if (isNotBlank(config.header) && config.header.show && !config.header.allPage) {
        tbOffset2Top += config.header.height
      }
      // title显示且表头信息不显示的情况
      if (isNotBlank(config.title) && config.title.show && (isBlank(config.header) || !config.header.show)) {
        prevHeight += notHeaderSpacing // 增加表格与table之间的间距
        if (!config.title.allPage) {
          // 如果title不是每页都显示，则增加table次页偏移距离
          tbOffset2Top += notHeaderSpacing
        }
      }
      /**
         * LODOP——bug，BottomMargin， table有bug
         * 例如：BM为10mm，剩余高度为12mm（实际可用高度2mm）。单元格高度为11mm，这样的话不会换页
         */
      const extraBottom = convertUnits(8, 'mm', config.unit)
      // 表格 8mm
      LODOP.ADD_PRINT_TABLE(`${prevHeight}${config.unit}`, 0, '100%', `BottomMargin:${config.paddingTB + extraBottom}${config.unit}`, tableHtml)
      LODOP.SET_PRINT_STYLEA(0, 'TableHeightScope', 1) // 设置TABLE高度是否包含页头页尾，0-代表不包含（默认），1-代表包含头和尾 2-只包含页头 3-只包含页尾
      LODOP.SET_PRINT_STYLEA(0, 'TableRowThickNess', '30px') // 设置TABLE高度是否包含页头页尾，0-代表不包含（默认），1-代表包含头和尾 2-只包含页头 3-只包含页尾
      if (tbOffset2Top) {
        LODOP.SET_PRINT_STYLEA(0, 'Offset2Top', `${-tbOffset2Top}${config.unit}`) // 从次页开始的上边距偏移量
      }
      // 当底部信息“显示且只在尾页显示”的情况，每页显示则再table的tfoot中加入代码
      if (isNotBlank(config.footer) && config.footer.show && !config.footer.allPage) {
        LODOP.ADD_PRINT_HTM(0, 0, '100%', '100%', footerHtml)
        LODOP.SET_PRINT_STYLEA(0, 'LinkedItem', -1) // 关联上一个table对象，在table的末尾显示
      }
      if (isNotBlank(config.page) && config.page.show) {
        LODOP.ADD_PRINT_HTM(0, 0, '100%', '100%', pageNumberHtml)
        LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1) // 设置标题每页显示
      }
      if (isNotBlank(config.logo) && config.logo.show && config.logo.url) {
        LODOP.ADD_PRINT_HTM(`${config.logo.top}${config.unit}`, `${config.logo.left}${config.unit}`, '100%', '100%', logoHtml)
        LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1) // 设置标题每页显示
        if (!config.logo.allPage) {
          // 如果logo不是每页都显示（只有第一页显示）
          LODOP.SET_PRINT_STYLEA(0, 'PageIndex', 'first')
        }
      }
      if (isNotBlank(config.qrCode) && config.qrCode.show && isNotBlank(qrCode)) {
        const tempLeft = convertUnits(1, 'mm', config.unit)
        LODOP.ADD_PRINT_BARCODE(
          `${config.qrCode.top}${config.unit}`,
          `${config.qrCode.left + tempLeft}${config.unit}`,
          `${config.qrCode.width}${config.unit}`,
          `${config.qrCode.height}${config.unit}`,
          'QRCode',
          qrCode
        )
        LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 5)
        LODOP.SET_PRINT_STYLEA(0, 'QRCodeErrorLevel', 'M')
        LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1) // 设置标题每页显示
        if (!config.qrCode.allPage) {
          // 如果logo不是每页都显示（只有第一页显示）
          LODOP.SET_PRINT_STYLEA(0, 'PageIndex', 'first')
        }
      }
      LODOP.SET_PRINT_COPIES(intCopies) // 打印份数
      result = await printByMode(printMode)
    }
    if (isNotBlank(config.logo) && config.logo.show && config.logo.url) {
      var img = new Image()
      img.addEventListener('load', await loadHandler)
      img.src = config.logo.url
    } else {
      await loadHandler()
    }
    return result
  } catch (error) {
    throw new Error(error)
  }
}

/**
 * 设置表格columns
 * @param {object} config 表格配置对象
 */
function setColumns(config) {
  if (!config) return
  // 设置column
  config.columns = delNotDisplayed(JSON.parse(JSON.stringify(config.fields)))
  config.columnRows = convertColumns(config.columns)
  config.lastColumns = getLastColumns(config.columnRows)
}

/**
 * 标题html
 * @param {object} config 标题的配置信息
 */
function getTitleHtml(config) {
  if (isBlank(config) || !config.show) {
    return ''
  }
  let html = TITLE_STYLE + config.style
  html += `<span class="title-content">${config.title || ''}</span>`
  return html
}

/**
 * 表格头部摘要信息html（标题之下，表格之上的内容）
 * @param {object} data 表头信息数据
 * @param {object} config 表头信息的配置信息
 */
function getHeadAbstractHtml(data, config) {
  if (!config) {
    return ''
  }
  let html = ''
  if (config.show && config.fields) {
    html += HEADER_STYLE + config.style
    html += `<div class="header-content">`
    for (const field of config.fields) {
      if (field.show) {
        html += `<div style="${field.style}">`
        if (field.title) {
          html += `<span style="font-weight:${config.bold}">${field.title}</span>`
        }
        if (data && field.key) {
          const _val = dataFormat({ row: data, field, emptyVal: config.emptyVal })
          html += isNotBlank(_val) ? `<span>${_val}</span>` : ''
        }
        html += `</div>`
      }
    }
    html += `</div>`
  }
  return html
}

function getFooterHtml(data, globalConfig) {
  const config = globalConfig.footer
  if (!config || !config.show) {
    return ''
  }
  const tipCfg = config.tip
  let html = ''
  html += FOOTER_STYLE + config.style
  html += `<div class="footer-content">`
  if (tipCfg && tipCfg.show && tipCfg.above) {
    html += `<span class="tip" style="font-size:${tipCfg.size}${globalConfig.fontUnit};font-weight:${tipCfg.bold};text-align:${textAlign(
      tipCfg.align
    )}">${isNotBlank(tipCfg.text) ? tipCfg.text : ''}</span>`
  }
  if (config.fields) {
    for (const field of config.fields) {
      if (field.show) {
        html += `<div style="${field.style}">`
        if (field.title) {
          html += `<span style="font-weight:${config.bold}">${field.title}</span>`
        }
        if (data && field.key) {
          const _val = dataFormat({ row: data, field, emptyVal: config.emptyVal })
          html += isNotBlank(_val) ? `<span>${_val}</span>` : ''
        }
        html += `</div>`
      }
    }
  }
  if (tipCfg && tipCfg.show && !tipCfg.above) {
    html += `<span class="tip" style="font-size:${tipCfg.size}${globalConfig.fontUnit};font-weight:${tipCfg.bold};text-align:${textAlign(
      tipCfg.align
    )}">${isNotBlank(tipCfg.text) ? tipCfg.text : ''}</span>`
  }
  html += `</div>`
  return html
}

/**
 * 标题html
 * @param {object} config 页码的配置信息
 */
function getPageHtml(config) {
  if (isBlank(config) || !config.show) {
    return ''
  }

  let html = PAGE_STYLE + config.style
  html += `<div class="page-content">${pageFormat(config.format)}</div>`
  return html
}

function getLogoHtml(config) {
  if (isBlank(config) || !config.show) {
    return ''
  }
  let html = LOGO_STYLE + config.style
  html += `<div class="logo-content"><img src=${config.url} class="logo-img" /></div>`
  // html += `<img class="logo-img" />`
  return html
}

/**
 * 表头处理
 * @param {object} config 表格的配置信息
 * @param {object} needBlankColumn 需要空列
 */
function getTHeadHtml(config, needBlankColumn) {
  const columnRows = config.columnRows
  let html = ''
  columnRows.forEach((cr, i) => {
    html += '<tr>'
    if (i === 0 && config.index && config.index.show) {
      const _style = config.index.style
      html += `<td class="th" rowspan="${columnRows.length}" style="${_style}"><div style="${_style}">${config.index.title || '序号'}</div></td>`
    }
    cr.forEach(c => {
      if (c.show) {
        html += `<td class="th" colspan="${c.colSpan}" rowspan="${c.rowSpan}" style="${c.style}"><div style="${c.style}">${c.title || ''}</div></td>`
      }
    })
    if (i === 0 && needBlankColumn) {
      html += `<td class="th blank-column" scope="col"><div></div></td>`
    }
    html += '</tr>'
  })
  return html
}

/**
 * 表格信息html
 * @param {object} data 表格数据
 * @param {object} globalConfig 表格的配置信息
 */
function getTableHtml({ header, footer, table, globalConfig }) {
  if (!globalConfig) {
    return ''
  }
  let html = getStyle(globalConfig) // 加载所有样式
  const config = globalConfig.table
  const headAbstractHtml = getHeadAbstractHtml(header, globalConfig.header)
  const footerHtml = getFooterHtml(footer, globalConfig)
  let thColspan = config.fields.filter(f => f.show).length // th需要跨行的值
  if (config.index && config.index.show) {
    ++thColspan
  }
  html += `
    <div class="table-content" >
      <table class="print-table" border="0">`

  if (isNotBlank(globalConfig.header) && globalConfig.header.show && !globalConfig.header.allPage) {
    html += `<caption>${headAbstractHtml}</caption>`
  }
  html += `<thead>`
  if (isNotBlank(globalConfig.header) && globalConfig.header.show && globalConfig.header.allPage) {
    html += `<tr><th colspan="${thColspan}">${headAbstractHtml}</th></tr>`
  }
  // 是否需要最后空列（虚假的）
  const needBlankColumn = config.index && config.index.show && isNotBlank(config.index.width) && config.lastColumns.every(f => isNotBlank(f.width)) // 字段都是固定宽度
  html += `${getTHeadHtml(config, needBlankColumn)}`
  html += `</thead>
        <tbody>
        `
  // if (isNotBlank(table)) {
  // body
  let index = 0
  for (const row of table) {
    html += `<tr>`
    // 序号
    if (config.index && config.index.show) {
      const _style = config.index.style
      html += `<td class="td" style="${_style}"><div style="${_style}">${index + 1}</div></td>`
    }
    // 表格内容
    for (const column of config.lastColumns) {
      // 打印问题处理 needParse
      // let _content = row && column && column.key && isNotBlank(row[column.key]) ? row[column.key] : ''
      // 格式转换
      const _content = dataFormat({ row, field: column, emptyVal: config.emptyVal })
      html += `<td class="td" style="${column.style}"><div style="${column.style}">${_content}</div></td>`
    }
    if (needBlankColumn) {
      html += `<td class="td blank-column"><div></div></td>`
    }
    html += `</tr>`
    ++index
  }
  // 是否需要合计
  if (config.summary && config.summary.show) {
    html += spliceSummary(table, config, needBlankColumn)
  }
  // }

  html += `
        </tbody>
        <tfoot>`
  if (isNotBlank(globalConfig.footer) && globalConfig.footer.show && globalConfig.footer.allPage) {
    html += `<tr><th colspan="${thColspan}" style="text-align:left">${footerHtml}</th></tr>`
  }
  html += `</tfoot>
      </table>
    </div>
    `
  return html
}

/**
 * 表格的合计信息html
 * @param {object} data 表格数据
 * @param {object} config 表格的配置信息
 */
function spliceSummary(data, config, needBlankColumn) {
  let html = `<tr>`
  if (config.index && config.index.show) {
    const _style = config.index.style
    html += `<td class="td" style="${_style}"><div style="${_style}">${config.summary.title || '合计'}</div></td>`
  }
  for (const column of config.lastColumns) {
    if (column.show) {
      // 单元格填充内容
      let sum = ''
      if (column && column.sum) {
        // 判断字段是否需要合计
        sum = 0 // 打印时汇总数据默认显示0
        const columns = data.map(d => keyParse(d, column.key)) // 列数据 字段解析
        if (!columns.every(value => isNaN(+value))) {
          // 判断是否为数字类型,此处允许空格或空字符串，因此可用isNaN，否则使用正则表达式
          sum = columns.reduce((prev, curr) => {
            const value = Number(curr)
            if (!isNaN(value)) {
              return prev + curr
            } else {
              return prev
            }
          }, 0)
          sum = dataFormat({ val: sum, field: column })
        }
      }
      html += `<td class="td" style="${column.style}"><div style="${column.style}">${sum}</div></td>`
    }
  }
  if (needBlankColumn) {
    html += `<td class="td blank-column"><div></div></td>`
  }
  html += `</tr>`
  return html
}

function getStyle(config) {
  let style = HEADER_STYLE + TABLE_STYLE + COMMON_STYLE
  if (isNotBlank(config.header) && config.header.show) {
    style += config.header.style
  }
  style += config.table.style
  return style
}

function setStyle(config) {
  setTitleStyle(config)
  setHeaderStyle(config)
  setFooterStyle(config)
  setPageStyle(config)
  setLogoStyle(config)
  setTableStyle(config)
}

/**
 * 设置标题的样式
 * @param {object} config 打印表格的全局配置
 */
function setTitleStyle(config) {
  const itemConfig = config.title
  if (config && isNotBlank(itemConfig) && itemConfig.show) {
    let _style = `<style> 
        .title-content {`
    if (isNotBlank(itemConfig.size)) {
      _style += `font-size:${itemConfig.size}${config.fontUnit};`
    }
    if (isNotBlank(itemConfig.bold)) {
      _style += `font-weight:${itemConfig.bold};`
    }
    if (isNotBlank(itemConfig.height)) {
      _style += `height:${itemConfig.height}${config.unit};`
    }
    if (isNotBlank(itemConfig.width)) {
      _style += `width:${itemConfig.width}${config.unit};`
    }
    if (isNotBlank(itemConfig.align)) {
      _style += `justify-content:${flexAlign(itemConfig.align)};`
    }
    if (isNotBlank(itemConfig.verticleAlign)) {
      _style += `align-items:${verticleAlign(itemConfig.verticleAlign)};`
    }
    if (isNotBlank(config.paddingLR)) {
      _style += `padding:0 ${config.paddingLR}${config.unit};`
    }
    _style += `}</style>`
    itemConfig.style = _style
  }
}

/**
 * 设置表头信息的样式
 * @param {object} config 打印表格的全局配置
 */
function setHeaderStyle(config) {
  const itemConfig = config.header
  if (config && isNotBlank(itemConfig) && itemConfig.show) {
    setHeaderFieldStyle(config) // 设置字段信息
    let _style = `<style> 
        .header-content {`
    // if (isNotBlank(config.paddingLR)) {
    //   _style += `padding:0 ${config.paddingLR}${config.unit};`
    // }
    if (isNotBlank(itemConfig.width)) {
      _style += `width: ${itemConfig.width}${config.unit};`
    }
    if (isNotBlank(itemConfig.height)) {
      _style += `height: ${itemConfig.height}${config.unit};`
    }
    if (isNotBlank(itemConfig.size)) {
      _style += `font-size: ${itemConfig.size}${config.fontUnit};`
    }
    if (isNotBlank(itemConfig.align)) {
      _style += `justify-content: ${flexAlign(itemConfig.align)};`
    }
    if (isNotBlank(itemConfig.verticleAlign)) {
      _style += `align-items: ${verticleAlign(itemConfig.verticleAlign)};`
    }
    _style += `}</style>`
    itemConfig.style = _style
  }
}

/**
 * 设置底部信息的样式
 * @param {object} config 打印表格的全局配置
 */
function setFooterStyle(config) {
  const itemConfig = config.footer
  if (config && isNotBlank(itemConfig) && itemConfig.show) {
    setFooterFieldStyle(config) // 设置字段信息
    let _style = `<style> 
        .footer-content {`
    if (isNotBlank(config.footer) && config.footer.show && !config.footer.allPage && isNotBlank(config.paddingLR)) {
      // footer 不在tfoot中显示的情况
      _style += `padding:0 ${config.paddingLR}${config.unit};`
    }
    if (isNotBlank(itemConfig.width)) {
      _style += `width:${itemConfig.width}${config.unit};`
    }
    if (isNotBlank(itemConfig.height)) {
      _style += `height:${itemConfig.height}${config.unit};`
    }
    if (isNotBlank(itemConfig.size)) {
      _style += `font-size:${itemConfig.size}${config.fontUnit};`
    }
    if (isNotBlank(itemConfig.align)) {
      _style += `justify-content:${flexAlign(itemConfig.align)};`
    }
    if (isNotBlank(itemConfig.verticleAlign)) {
      _style += `align-items: ${verticleAlign(itemConfig.verticleAlign)};`
    }
    _style += `}</style>`
    itemConfig.style = _style
  }
}

/**
 * 设置页码的样式
 * @param {object} config 打印表格的全局配置
 */
function setPageStyle(config) {
  const itemConfig = config.page
  if (config && isNotBlank(itemConfig) && itemConfig.show) {
    let _style = `<style> 
        .page-content {`
    if (isNotBlank(config.paddingLR)) {
      _style += `padding-left: ${config.paddingLR || 0}${config.unit};`
      _style += `padding-right: ${config.paddingLR || 0}${config.unit};`
    }
    if (isNotBlank(itemConfig.bottom)) {
      _style += `bottom: ${itemConfig.bottom}${config.unit};`
    }
    if (isNotBlank(itemConfig.size)) {
      _style += `font-size: ${itemConfig.size}${config.fontUnit};`
    }
    if (isNotBlank(itemConfig.bold)) {
      _style += `font-weight: ${itemConfig.bold};`
    }
    if (isNotBlank(itemConfig.align)) {
      _style += `text-align: ${textAlign(itemConfig.align)};`
    }
    _style += `}</style>`
    itemConfig.style = _style
  }
}

/**
 * 设置底部信息的样式
 * @param {object} config 打印表格的全局配置
 */
function setLogoStyle(config) {
  const itemConfig = config.logo
  if (config && isNotBlank(itemConfig) && itemConfig.show) {
    let _style = `<style> 
    .logo-content {`
    if (isNotBlank(itemConfig.width)) {
      _style += `width:${itemConfig.width}${config.unit};`
    }
    if (isNotBlank(itemConfig.height)) {
      _style += `height:${itemConfig.height}${config.unit};`
    }
    _style += `}
        .logo-img {`
    if (isNotBlank(itemConfig.width)) {
      _style += `width:${itemConfig.width}${config.unit};`
    }
    // if (isNotBlank(itemConfig.height)) {
    //   _style += `height:${itemConfig.height}${config.unit};`
    // }
    _style += `}</style>`
    itemConfig.style = _style
  }
}

/**
 * 设置表格信息的样式
 * @param {object} globalConfig 打印表格的全局配置
 */
function setTableStyle(globalConfig) {
  const config = globalConfig.table
  if (globalConfig && isNotBlank(config)) {
    setTableColumnsStyle(globalConfig) // 设置表格样式
    let _style = `<style> 
        .table-content {`
    if (isNotBlank(globalConfig.width)) {
      _style += `width:${globalConfig.width}${globalConfig.unit};`
    }
    if (isNotBlank(globalConfig.paddingLR)) {
      _style += `padding:0 ${globalConfig.paddingLR}${globalConfig.unit};`
    }
    _style += `}`
    if (isNotBlank(config.th)) {
      _style += `
      .print-table .th {`
      if (isNotBlank(config.th.size)) {
        _style += ` font-size: ${config.th.size}${globalConfig.fontUnit};`
      }
      if (isNotBlank(config.th.bold)) {
        _style += ` font-weight: ${config.th.bold};`
      }
      if (isNotBlank(config.th.lineHeight)) {
        _style += ` line-height: ${config.th.lineHeight}${globalConfig.fontUnit}!important;`
      }
      // PS: 打印控件存在tfoot，并有表格内容的情况下，必须有一行td，若无，表头的td（即当前样式）会多1px，因此在使用tfoot时，将该行的border-bottom设为none
      if (isNotBlank(globalConfig.footer) && globalConfig.footer.show && globalConfig.footer.allPage) {
        _style += `border-bottom: none;`
      }
      _style += `}`
      _style += `
      .print-table .th >div{`
      if (isNotBlank(config.th.paddingTB)) {
        _style += ` padding-top: ${config.th.paddingTB}${globalConfig.unit}!important;`
        _style += ` padding-bottom: ${config.th.paddingTB}${globalConfig.unit}!important;`
      }
      _style += `}`
    }
    if (isNotBlank(config.td)) {
      _style += `
      .print-table .td {`
      if (isNotBlank(config.td.size)) {
        _style += ` font-size: ${config.td.size}${globalConfig.fontUnit};`
      }
      if (isNotBlank(config.td.bold)) {
        _style += ` font-weight: ${config.td.bold};`
      }
      if (isNotBlank(config.td.lineHeight)) {
        _style += ` line-height: ${config.td.lineHeight}${globalConfig.fontUnit}!important;`
      }
      _style += `}`
      _style += `
      .print-table .td >div{`
      if (isNotBlank(config.td.paddingTB)) {
        _style += ` padding-top: ${config.td.paddingTB}${globalConfig.unit}!important;`
        _style += ` padding-bottom: ${config.td.paddingTB}${globalConfig.unit}!important;`
      }
      _style += `}`
    }
    _style += `</style>`
    config.style = _style
  }
}

/**
 * 设置表格字段的样式
 * TODO: 考虑宽度手动分配，否则在横向打印时会出现问题
 * @param {array} fields 配置信息
 */
function setTableColumnsStyle(globalConfig) {
  const config = globalConfig.table
  if (globalConfig && isNotBlank(config)) {
    // const fields = config.fields
    // isNotBlank(fields) && fields.forEach(field => {
    //   let _style = ''
    //   if (isNotBlank(field.align)) {
    //     _style += `text-align:${textAlign(field.align)};`
    //   }
    //   if (isNotBlank(field.width)) {
    //     _style += `width:${field.width}${globalConfig.unit};`
    //   } else if (isNotBlank(field.minWidth)) {
    //     _style += `min-width:${field.minWidth}${globalConfig.unit};`
    //   }
    //   field.style = _style
    // })
    const columnRows = config.columnRows
    columnRows.forEach(row => {
      row.forEach(column => {
        let _style = ''
        if (isNotBlank(column.align)) {
          _style += `text-align:${textAlign(column.align)};`
        }
        if (isNotBlank(column.width)) {
          _style += `width:${column.width}${globalConfig.unit};`
        } else if (isNotBlank(column.minWidth)) {
          _style += `min-width:${column.minWidth}${globalConfig.unit};`
        }
        column.style = _style
      })
    })
    const index = config.index
    if (isNotBlank(config.index)) {
      let _style = ''
      if (isNotBlank(index.align)) {
        _style += `text-align:${textAlign(index.align)};`
      }
      if (isNotBlank(index.width)) {
        _style += `width:${index.width}${globalConfig.unit};`
      } else if (isNotBlank(index.minWidth)) {
        _style += `min-width:${index.minWidth}${globalConfig.unit};`
      }
      index.style = _style
    }
  }
}

/**
 * 设置表头信息字段的样式
 * @param {array} fields 配置信息
 */
function setHeaderFieldStyle(globalConfig) {
  const config = globalConfig.header
  const fields = config.fields
  isNotBlank(fields) &&
    fields.forEach(field => {
      let _style = ''
      if (isNotBlank(field.width)) {
        _style += `width:${field.width}${globalConfig.unit};`
      } else if (isNotBlank(field.maxWidth)) {
        _style += `max-width:${field.maxWidth}${globalConfig.unit};`
      }
      field.style = _style
    })
}

/**
 * 设置表头信息字段的样式
 * @param {array} fields 配置信息
 */
function setFooterFieldStyle(globalConfig) {
  const config = globalConfig.footer
  const fields = config.fields
  isNotBlank(fields) &&
    fields.forEach(field => {
      let _style = ''
      if (isNotBlank(field.width)) {
        _style += `width:${field.width}${globalConfig.unit};`
      } else if (isNotBlank(field.maxWidth)) {
        _style += `max-width:${field.maxWidth}${globalConfig.unit};`
      }
      if (isNotBlank(field.align)) {
        _style += `text-align:${textAlign(field.align)};`
      }
      field.style = _style
    })
}

/**
 * 数据格式转换
 * @param {*} val 数据
 * @param {object} field 字段信息
 * @param {string} emptyVal | default: "/" 当值未空时，代替的值
 */
function dataFormat({ row = {}, val, field, emptyVal = '' }) {
  if (field.source === dataSourceEnum.CUSTOMIZE.V) {
    emptyVal = ''
  }
  const needParse = isBlank(val) && isNotBlank(row) && isNotBlank(field)
  if (needParse) {
    val = keyParse(row, field.key)
  }
  switch (field.type) {
    case fieldTypeEnum.PROJECT.K:
      return emptyTextFormatter(projectNameFormat(val, field.format), emptyVal)
    case fieldTypeEnum.DATE.K:
      return emptyTextFormatter(dateFormat(val, field.format), emptyVal)
    case fieldTypeEnum.DATES.K:
      return emptyTextFormatter(dateFormat(val, field.format), emptyVal)
    case fieldTypeEnum.AMOUNT.K:
      return emptyTextFormatter(amountFormat(val, field.format), emptyVal)
    case fieldTypeEnum.WEIGHT.K:
      return emptyTextFormatter(weightFormat(val, field.format), emptyVal)
    case fieldTypeEnum.LENGTH.K:
      return emptyTextFormatter(lengthFormat(val, field.format), emptyVal)
    case fieldTypeEnum.RATE.K:
      return emptyTextFormatter(rateFormat(val, field.format), emptyVal)
    case fieldTypeEnum.THICKNESS.K:
      return emptyTextFormatter(thicknessFormat(val, field.format), emptyVal)
    case fieldTypeEnum.METE.K:
      return emptyTextFormatter(
        meteFormat({
          val,
          format: field.format,
          basicClass: row.basicClass,
          materialType: row.materialType,
          materialListType: row.materialListType,
          unit: row.unit,
          checkUnit: row.checkUnit
        }),
        emptyVal
      )
    case fieldTypeEnum.QUANTITY.K:
      return emptyTextFormatter(quantityFormat(val, field.format), emptyVal)
    case fieldTypeEnum.ENUM.K:
      return emptyTextFormatter(enumFormat(val, field.format), emptyVal)
    default:
      return emptyTextFormatter(val, emptyVal)
  }
}

/**
 * 枚举数据格式转换
 * @param {*} val 数据
 * @param {object} format 格式
 */
function enumFormat(val, format) {
  const flag = isNotBlank(format) && isNotBlank(format.enum) && isNotBlank(enumAll[format.enum]) && isNotBlank(val)
  if (flag) {
    const key = format.key || 'L'
    const enumK = enumAll[format.enum]
    if (format.bit) {
      // 位运算的值
      const enums = EO.toArr(enumK)
      const res = []
      enums.forEach(e => {
        if (e.V & val) {
          res.push(e[key] || e['L'])
        }
      })
      return res.join('/')
    } else {
      const enumV = EO.key2val(enumK)
      return isNotBlank(enumV) && isNotBlank(enumV[val]) ? enumV[val][key] || enumV[val]['L'] : ''
    }
  }
  return
}

/**
 * “项目”数据格式转换
 * @param {*} val 数据
 * @param {object} format 格式
 * @return {string} 项目名称
 */
function projectNameFormat(val, format = {}) {
  if (isBlank(format)) {
    // 默认只显示项目简称
    format = { showProjectFullName: false, showSerialNumber: false, projectNameShowConfig: projectNameArrangementModeEnum.SERIAL_NUMBER_START.V }
  }
  return projectNameFormatter(val, format, format.lineBreak)
}

/**
 * “日期”数据格式转换
 * @param {*} val 数据
 * @param {string} format | default: 'YY/MM/DD' 格式
 * @return {string} 日期
 */
function dateFormat(val, format = 'YY/MM/DD') {
  const filterDate = val => {
    if (isNotBlank(val)) {
      return moment(+val).format(format)
    }
  }
  if (typeof val === 'string') {
    // 'xx,xx,xx'
    val = val.split(',')
    if (val instanceof Array) {
      if (val.length === 1) {
        return filterDate(val[0])
      }
      if (val.length > 1) {
        return val
          .map(t => {
            return filterDate(t)
          })
          .join('，')
      }
    }
  }
  if (val instanceof Array) {
    if (val.length === 1) {
      return filterDate(val[0])
    }
    if (val.length === 2) {
      const _startDate = filterDate(val[0])
      const _endDate = filterDate(val[1])
      return `${_startDate} ~ ${_endDate}`
    }
  }
  return filterDate(val)
}

/**
 * “金额”数据格式转换
 * @param {*} val 数据
 * @param {object} format
 * @return {string|number} 金额
 */
function amountFormat(val, format = {}) {
  let _val = val
  if (isNotBlank(_val)) {
    // 单位转换
    if (isNotBlank(format.unit)) {
      if (format.unit === amountUnitEnum.WAN.V) {
        _val /= 10000
      }
    }
    // 小数精度
    if (isNotBlank(format.precision)) {
      _val = (+_val).toFixed(format.precision)
    }
    // 1000 => 1,000
    if (format.toThousand) {
      _val = toThousand(_val)
    }
  }
  return _val
}

/**
 * “重量”数据格式转换
 * @param {*} val 数据
 * @param {object} format
 * @return {string|number} 重量
 */
function weightFormat(val, format = {}) {
  let _val = val
  if (isNotBlank(_val)) {
    // 单位转换
    if (isNotBlank(format.unit)) {
      _val = convertUnits(_val, MIN_UNIT.WEIGHT, format.unit)
    }
    // 小数精度
    if (isNotBlank(format.precision)) {
      _val = (+_val).toFixed(format.precision)
    }
    // 1000 => 1,000
    if (format.toThousand) {
      _val = toThousand(_val)
    }
  }
  return _val
}

/**
 * “长度”数据格式转换
 * @param {*} val 数据
 * @param {object} format
 * @return {string|number} 长度
 */
function lengthFormat(val, format = {}) {
  let _val = val
  if (isNotBlank(_val)) {
    if (isNotBlank(format.unit)) {
      _val = convertUnits(_val, MIN_UNIT.LENGTH, format.unit)
    }
    // 小数精度
    if (isNotBlank(format.precision)) {
      _val = (+_val).toFixed(format.precision)
    }
    // 1000 => 1,000
    if (format.toThousand) {
      _val = toThousand(_val)
    }
  }
  return _val
}

/**
 * “厚度”数据格式转换
 * @param {*} val 数据
 * @param {object} format
 * @return {string|number} 厚度
 */
function thicknessFormat(val, format = {}) {
  let _val = val
  if (isNotBlank(_val)) {
    if (isNotBlank(format.unit)) {
      _val = convertUnits(_val, MIN_UNIT.THICKNESS, format.unit)
    }
    // 小数精度
    if (isNotBlank(format.precision)) {
      _val = (+_val).toFixed(format.precision)
    }
    // 1000 => 1,000
    if (format.toThousand) {
      _val = toThousand(_val)
    }
  }
  return _val
}

/**
 * “量”数据格式转换
 * @param {*} val 数据
 * @param {object} format
 * @param {number} basicClass 基础类型（enum）
 * @return {string|number} 量
 */
function meteFormat({ val, unit, checkUnit, format = {}, basicClass, materialType, materialListType }) {
  let _val = val
  if (isNotBlank(_val)) {
    // 小数精度
    if (isNotBlank(format.precision)) {
      _val = (+_val).toFixed(format.precision)
    }
    // 1000 => 1,000
    if (format.toThousand) {
      _val = toThousand(_val)
    }
    // 是否显示单位
    if (format.showUnit) {
      let _unit
      if (isNotBlank(basicClass)) {
        if (checkUnit) {
          _unit = checkUnit
        } else {
          _unit = matClsEnum(basicClass)
        }
      }
      if (isNotBlank(materialType)) {
        if (unit) {
          _unit = unit
        } else {
          _unit = getMaterialTypeUnit(materialType)
        }
      }
      if (isNotBlank(materialListType)) {
        if (unit) {
          _unit = unit
        } else {
          _unit = getMaterialListTypeUnit(materialListType)
        }
      }
      if (_unit) {
        _val += ` ${_unit}`
      }
    }
  }
  return _val
}

/**
 * “数量”数据格式转换
 * @param {*} val 数据
 * @param {object} format
 * @return {string|number} 量
 */
function quantityFormat(val, format = {}) {
  let _val = val
  if (isNotBlank(_val)) {
    // 小数精度
    if (isNotBlank(format.precision)) {
      _val = (+_val).toFixed(format.precision)
    }
    // 1000 => 1,000
    if (format.toThousand) {
      _val = toThousand(_val)
    }
  }
  return _val
}

/**
 * “比例/比率”数据格式转换
 * @param {*} val 数据
 * @param {object} format
 * @return {string|number} 量
 */
function rateFormat(val, format = {}) {
  let _val = val
  // 小数精度
  if (isNotBlank(format.precision)) {
    _val = (+_val).toFixed(format.precision)
  }
  return _val + '%'
}

/**
 * flex对齐方式
 * @param {*} align
 */
function flexAlign(align) {
  switch (align) {
    case alignEnum.LEFT.V:
      return 'flex-start'
    case alignEnum.RIGHT.V:
      return 'flex-end'
    case alignEnum.CENTER.V:
      return 'center'
    default:
      return 'flex-start'
  }
}

/**
 * flex垂直对齐方式（align-item）
 * @param {*} align
 */
function verticleAlign(align) {
  switch (align) {
    case verticleAlignEnum.TOP.V:
      return 'flex-start'
    case verticleAlignEnum.BOTTOM.V:
      return 'flex-end'
    case verticleAlignEnum.CENTER.V:
      return 'center'
    default:
      return 'flex-start'
  }
}

/**
 * text对齐方式
 * @param {*} align
 */
function textAlign(align) {
  switch (align) {
    case alignEnum.LEFT.V:
      return 'left'
    case alignEnum.RIGHT.V:
      return 'right'
    case alignEnum.CENTER.V:
      return 'center'
    default:
      return 'left'
  }
}

/**
 * 页码格式转换
 */
function pageFormat(val) {
  let html = ''
  switch (val) {
    case pageFormatEnum.DEFAULT.V:
      html = "<span tdata='pageNO'>##</span> / <span tdata='pageCount'>##</span>"
      break
    case pageFormatEnum.ONE.V:
      html = "<span tdata='pageNO'>第##页</span> / <span tdata='pageCount'>共##页</span>"
      break
    case pageFormatEnum.TWO.V:
      html = "<span tdata='pageNO' format='ChineseNum'>第##页</span> / <span tdata='pageCount' format='ChineseNum'>共##页</span>"
      break
    default:
      html = "<span tdata='pageNO'>##</span> / <span tdata='pageCount'>##</span>"
  }
  return html
}

/**
 * key解析，处理key为'xxx.xx'的情况
 * @param {object} data 数据
 * @param {string} key 字段名
 */
function keyParse(data, key) {
  const parseable = isNotBlank(data) && isNotBlank(key) && typeof key === 'string'
  if (parseable) {
    const keys = key.split('.')
    if (keys.length === 1) {
      return data[keys[0]]
    } else {
      return keys.reduce((cur, key) => {
        return lodash.isPlainObject(cur) ? cur[key] : undefined
      }, data)
    }
  }
  return
}

const COMMON_STYLE = `
<style>
body {
    /* font-feature-settings: 'onum' 1; */
    color: #333;
   /*  overflow: hidden;*/
    text-rendering: optimizeLegibility;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    font-family: AvenirNext,Avenir,-apple-system,BlinkMacSystemFont,Roboto Slab,Droid Serif,Segoe UI,Oxygen-Sans,Ubuntu,Cantarell,Georgia,serif;
    line-height: 1.15;
}
</style>
`

const HEADER_STYLE =
  COMMON_STYLE +
  `
<style>
.header-content {
  font-family: lucida sans unicode,lucida grande,Sans-Serif;
  width: 100%;
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: center;
  flex-wrap: wrap;
  box-sizing: border-box;
  overflow: hidden;
  line-height: 1.15;
  margin-bottom: 1mm;
}
.header-content >div {
  margin: 1mm 0;
  text-align: left;
  display: inline-block;
  /* padding-right: 5mm; */
  box-sizing: border-box;
  overflow : hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.header-content :first-child {
  margin-top: 0;
}
</style>
`
const TITLE_STYLE =
  COMMON_STYLE +
  `
<style>
.title-content {
  display: flex;
  justify-content: center;
  align-items: flex-start;
  width: 100%;
  box-sizing: border-box;
  text-align: center;
  font-size: 17pt;
  font-weight: bold;
}
</style>
`

// 表格演示
const TABLE_STYLE =
  COMMON_STYLE +
  `
<style>
.table-content {
  word-break: break-all;
  box-sizing: border-box;
  width: fit-content;
  margin: 1mm 0;
}
td{ 
  border:1px solid #000;
}
.print-table {
    font-family: lucida sans unicode,lucida grande,Sans-Serif;
    font-size: 9pt;
    border-collapse: collapse;
    /* border-top: 1px solid #9baff1; */
    /* border-bottom: 1px solid #9baff1; */
    width: 100%;
}
.print-table .blank-column {
  min-width: 0;
  border: none;
  display: table-column;
}
.print-table .blank-column >div{
  display: inline-table;
  min-width: 0;
}
.print-table tbody .td {
  /* font-size: 9pt; */
  /* border-right: 1px solid #aabcfe; */
  /* border-left: 1px solid #aabcfe; */
  /* color: #669; */
  padding: 0;
  line-height:13pt;
  white-space: pre-wrap;
}
.print-table tbody .td >div {
  padding: 0 1mm;
  box-sizing: border-box;
  min-height: 3mm;
}
.print-table .th {
    /* font-size: 10pt; */
    font-weight: 400;
    line-height: 15pt;
    /* background: #e8edff; */
    /* border-right: 1px solid #9baff1; */
    /* border-left: 1px solid #9baff1; */
    /* color: #039; */
    padding: 0;
}
.print-table .th >div {
  padding: 0 1mm;
  box-sizing: border-box;
  min-height: 3mm;
}
tbody tr:last-child{
  border-bottom:none
}
</style>
`

const FOOTER_STYLE =
  COMMON_STYLE +
  `
<style>
.tip {
  margin: 1mm 0;
  white-space: pre-line;
  display: inline-block;
  width: 100%;
  font-size: 9pt;
  color: red;
}
.footer-content{
  font-family: lucida sans unicode,lucida grande,Sans-Serif;
  overflow: hidden;
  width: 100%;
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: center;
  flex-wrap: wrap;
  line-height: 1.15;
  /* margin-bottom: 3mm;*/
}
.footer-content >div {
  margin: 1mm 0;
  display: inline-block;
  /* padding-right: 5mm; */
  box-sizing: border-box;
  overflow : hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
</style>
`

const PAGE_STYLE =
  COMMON_STYLE +
  `
<style>
.page-content {
  box-sizing: border-box;
  border: none;
  width: 100%;
  position: absolute;
  bottom: 0;
  left: 0;
}
</style>
`

// 无法使用object-fit 和 通过背景图实现，所以用盒子嵌套居中
const LOGO_STYLE = `
<style>
.logo-content {
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
}
</style>
`

export { printTable }
