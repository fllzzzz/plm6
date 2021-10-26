/**
 * TODO: 不可使用当前打印table文件
 * 在打印控件支持 “ ItemType与Offset2Top同时使用 ” 或允许修改次页样式（列如：次页高度）时，方可使用该文件
 * @author duhh
 */
import { emptyTextFormatter, isNotBlank } from '@data-type/index'
import { toThousandFilter } from '@data-type/number'
import { projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'
import { projectNameFormatter } from '@/utils/project'
import { alignEnum, fieldTypeEnum as typeEnum, printModeEnum as PrintMode } from '../enum'
import { getLODOP } from '../base'
import moment from 'moment'

let LODOP

/**
   * 打印表格
   * @param {object} extra 额外信息
   * @param {object} footer 底部信息
   * @param {object} table 表格信息
   * @param {object} config 配置信息
   * @param printMode 打印模式
   * @author duhh
   */
async function printTable({ extra, table, footer, config, printMode = PrintMode.QUEUE.V } = {}) {
  if (!isNotBlank(config)) {
    throw new Error('打印未配置')
  }
  if (!isNotBlank(table)) {
    throw new Error('无数据')
  }
  // eslint-disable-next-line prefer-const
  let result = false
  try {
    setStyle(config) // 设置各模块样式
    const headHtml = getTitleHtml(config.title) // 拼接标题
    const extraHtml = getExtraHtml(extra, config.extra) // 拼接额外信息
    const tableHtml = getTableHtml(table, config.table, config.unit) // 拼接表格
    const footerHtml = getFooterHtml(footer, config.footer) // 拼接底部信息
    // let titleHeight = config.paddingTB + config.title.height
    let prevHeight = config.paddingTB
    let offset2Top = 0
    let tableBottomMargin = config.paddingTB

    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, config.width + config.unit, config.height + config.unit, '') /* 纸张大小*/
    // 标题
    LODOP.ADD_PRINT_HTM(`${config.paddingTB}${config.unit}`, 0, '100%', `${config.title.height}${config.unit}`, headHtml)
    LODOP.SET_PRINT_STYLEA(0, 'Offset2Top', `10mm`) // 从次页开始的上边距偏移量
    LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1)
    if (!config.title.everyPage) { // 若不是每页显示，则只有第一页显示
      offset2Top += config.title.height
      LODOP.SET_PRINT_STYLEA(0, 'PageIndex', 'first')
    }
    prevHeight += config.title.height
    // 额外信息（标题之下，表格之上）
    if (config.extra.show) {
      LODOP.ADD_PRINT_HTM(`${prevHeight}${config.unit}`, 0, '100%', `${config.extra.height}${config.unit}`, extraHtml)
      LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1)
      LODOP.SET_PRINT_STYLEA(0, 'VOrient', 0)
      if (!config.extra.everyPage) { // 若不是每页显示，则只有第一页显示
        offset2Top += config.extra.height
        LODOP.SET_PRINT_STYLEA(0, 'PageIndex', 'first')
      } else if (offset2Top) { // 次页需显示该模块且标题隐藏的情况
        // console.log('offset2Top', offset2Top)
        LODOP.SET_PRINT_STYLEA(0, 'Offset2Top', `${-offset2Top}${config.unit}`) // TODO: 打印控件暂不支持与“ItemType同时使用” 从次页开始的上边距偏移量
      }
      prevHeight += config.extra.height
    }
    // 底部信息
    if (config.footer.show) {
      tableBottomMargin += config.footer.height
      LODOP.ADD_PRINT_HTM(`${config.height - tableBottomMargin}${config.unit}`, 0, '100%', `BottomMargin:${config.paddingTB}${config.unit}`, footerHtml)
      LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1)
      LODOP.SET_PRINT_STYLEA(0, 'PageIndex', 'first')
    }
    // 表格
    LODOP.ADD_PRINT_TABLE(`${prevHeight}${config.unit}`, 0, '100%', `BottomMargin:${tableBottomMargin}${config.unit}`, tableHtml)
    LODOP.SET_PRINT_STYLEA(0, 'TableHeightScope', 1) // 设置TABLE高度是否包含页头页尾，0-代表不包含（默认），1-代表包含头和尾 2-只包含页头 3-只包含页尾
    if (offset2Top) {
      LODOP.SET_PRINT_STYLEA(0, 'Offset2Top', `${-offset2Top}${config.unit}`) // 从次页开始的上边距偏移量
    }
    // LODOP.SET_PRINT_STYLEA(0, 'Offset2Height', `BottomMargin:-${config.footer.height}${config.unit}`)// 用转义字
    // LODOP.SET_PRINT_STYLEA(0, 'TableRowThickNess', 25)

    LODOP.PREVIEW()/* 打印预览*/
    // LODOP.SET_PRINT_COPIES(intCopies); // 打印份数
    // result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * 标题html
 * @param {object} config 标题的配置信息
 */
function getTitleHtml(config) {
  if (!config) {
    return ''
  }
  // setTitleStyle()
  let html = TITLE_STYLE + config.style
  html += `<span class="title">${config.title}</span>`
  return html
}

/**
 * 额外信息html（标题之下，表格之上的内容）
 * @param {object} data 额外信息数据
 * @param {object} config 额外信息的配置信息
 */
function getExtraHtml(data, config) {
  if (!config) {
    return ''
  }
  let html = ''
  if (config.show && config.fields) {
    html += EXTRA_INFO_STYLE + config.style
    html += `<div class="extra-info">`
    for (const field of config.fields) {
      html += `<div style="${field.style}">`
      if (field.title) {
        html += `<span style="font-weight:${config.bold}">${field.title}</span>`
      }
      if (field.name) {
        const _val = dataFormat(data[field.name], field, config.emptyVal)
        html += `<span>${_val}</span>`
      }
      html += `</div>`
    }
    html += `</div>`
  }
  return html
}

function getFooterHtml(data, config) {
  if (!config) {
    return ''
  }
  let html = ''
  if (config.show && config.fields) {
    html += FOOTER_STYLE + config.style
    html += `<div class="footer-info" style="font-size:${config.size};justify-content:${flexAlign(config.align)}">`
    for (const field of config.fields) {
      html += `<div style="${field.style}">`
      if (field.title) {
        html += `<span style="font-weight:${config.bold}">${field.title}</span>`
      }
      if (field.name) {
        const _val = dataFormat(data[field.name], field, config.emptyVal)
        html += `<span>${_val}</span>`
      }
      html += `</div>`
    }
    if (config.tip && config.tip.show) {
      html += `<span class="tip" style="font-size:${config.tip.size};font-weight:${config.tip.bold}">${config.tip.text}</span>`
    }
    html += `</div>`
  }
  return html
}

/**
 * 表格信息html
 * @param {object} data 表格数据
 * @param {object} config 表格的配置信息
 */
function getTableHtml(data, config, unit) {
  if (!config) {
    return ''
  }
  let html = TABLE_STYLE + config.style
  console.log('html', html)
  html += `
    <div class="table-box">
      <table class="print-table" border="0">
       <thead>`
  html +=
          `<tr>
          `
  if (config.index && config.index.show) {
    // 拼接表头-序号
    html += `<td class="th" scope="col"><div style="text-align:${textAlign(config.index.align)};width:${config.index.width}${unit}">${config.index.title}</div></td>`
  }
  // 拼接表头
  for (const field of config.fields) {
    html += `<td class="th" scope="col" ><div style="${field.style}">${field.title}</div></td>`
  }

  html += `
          </tr>
        </thead>
        <tbody>
      </div>
        `
  // body
  let index = 0
  for (const row of data) {
    html += `<tr>`
    // 序号
    if (config.index && config.index.show) {
      html += `<td><div style="text-align:${textAlign(config.index.align)};width:${config.index.width}${unit}">${index + 1}</div></td>`
    }
    // 表格内容
    for (const field of config.fields) {
      let _content = row && field && field.name && row[field.name] ? row[field.name] : ''
      // 格式转换
      _content = dataFormat(_content, field, config.emptyVal)
      html += `<td><div style="${field.style}">${_content}</div></td>`
    }
    html += `</tr>`
    ++index
  }
  // 是否需要合计
  if (config.summary && config.summary.show) {
    html += spliceSummary(data, config, unit)
  }
  html += `
        </tbody>
      </table>
      `
  return html
}

/**
 * 表格的合计信息html
 * @param {object} data 表格数据
 * @param {object} config 表格的配置信息
 */
function spliceSummary(data, config, unit) {
  let html = `<tr>`
  if (config.index && config.index.show) {
    html += `<td><div style="text-align:${textAlign(config.index.align)};width:${config.index.width}${unit}">${config.summary.title}</div></td>`
  }
  for (const field of config.fields) {
    // 单元格填充内容
    let sum = ''
    if (field && field.sum) { // 判断字段是否需要合计
      const columns = data.map(d => d[field.name]) // 列数据
      if (!columns.every(value => isNaN(+value))) { // 判断是否为数字类型,此处允许空格或空字符串，因此可用isNaN，否则使用正则表达式
        sum = columns.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sum = amountFormat(sum, field.format) // 数据格式转换
      }
    }
    html += `<td><div style="${field.style}">${sum}</div></td>`
  }
  html += `</tr>`
  return html
}

function setStyle(config) {
  setTitleStyle(config)
  setExtraStyle(config)
  setFooterStyle(config)
  setTableStyle(config)
}

function setTitleStyle(config) {
  const itemConfig = config.title
  if (config && isNotBlank(itemConfig)) {
    let _style = `<style> 
        .title {`
    if (isNotBlank(itemConfig.size)) {
      _style += `font-size:${itemConfig.size};`
    }
    if (isNotBlank(itemConfig.bold)) {
      _style += `font-weight:${itemConfig.bold};`
    }
    if (isNotBlank(itemConfig.height)) {
      _style += `height:${itemConfig.height}${config.unit};`
    }
    if (isNotBlank(config.width)) {
      _style += `width:${config.width}${config.unit};`
    }
    if (isNotBlank(config.paddingLR)) {
      _style += `padding:0 ${config.paddingLR}${config.unit};`
    }
    _style += `}</style>`
    itemConfig.style = _style
  }
}

function setExtraStyle(config) {
  const itemConfig = config.extra
  if (config && isNotBlank(itemConfig) && itemConfig.show) {
    setExtraFieldStyle(config) // 设置字段信息
    let _style = `<style> 
        .extra-info {`
    if (isNotBlank(config.paddingLR)) {
      _style += `padding:0 ${config.paddingLR}${config.unit};`
    }
    if (isNotBlank(itemConfig.width)) {
      _style += `width:${itemConfig.width}${config.unit};`
    }
    if (isNotBlank(itemConfig.height)) {
      _style += `height:${itemConfig.height}${config.unit};`
    }
    if (isNotBlank(itemConfig.size)) {
      _style += `font-size:${itemConfig.size};`
    }
    if (isNotBlank(itemConfig.align)) {
      _style += `justify-content:${flexAlign(itemConfig.align)};`
    }
    _style += `}</style>`
    itemConfig.style = _style
  }
}

function setFooterStyle(config) {
  const itemConfig = config.footer
  if (config && isNotBlank(itemConfig) && itemConfig.show) {
    setFooterFieldStyle(config) // 设置字段信息
    let _style = `<style> 
        .footer-info {`
    if (isNotBlank(config.paddingLR)) {
      _style += `padding:0 ${config.paddingLR}${config.unit};`
    }
    if (isNotBlank(itemConfig.width)) {
      _style += `width:${itemConfig.width}${config.unit};`
    }
    if (isNotBlank(itemConfig.height)) {
      _style += `height:${itemConfig.height}${config.unit};`
    }
    if (isNotBlank(itemConfig.size)) {
      _style += `font-size:${itemConfig.size};`
    }
    if (isNotBlank(itemConfig.align)) {
      _style += `justify-content:${flexAlign(itemConfig.align)};`
    }
    _style += `}</style>`
    itemConfig.style = _style
  }
}

function setTableStyle(config) {
  const itemConfig = config.table
  if (config && isNotBlank(itemConfig)) {
    setTableColumnsStyle(config) // 设置表格样式
    let _style = `<style> 
        .table-box {`
    if (isNotBlank(config.width)) {
      _style += `width:${config.width}${config.unit};`
    }
    if (isNotBlank(config.paddingLR)) {
      _style += `padding:0 ${config.paddingLR}${config.unit};`
    }
    _style += `}`
    if (isNotBlank(itemConfig.th)) {
      _style += `
      .print-table .th {`
      if (isNotBlank(itemConfig.th.size)) {
        _style += ` font-size: ${itemConfig.th.size};`
      }
      if (itemConfig.th.bold) {
        _style += ` font-weight: bold;`
      }
      _style += `}`
    }
    if (isNotBlank(itemConfig.tdFS)) {
      _style += `
      .print-table td {`
      if (isNotBlank(itemConfig.td.size)) {
        _style += ` font-size: ${itemConfig.td.size};`
      }
      if (itemConfig.td.bold) {
        _style += ` font-weight: bold};`
      }
      _style += `}`
    }
    _style += `</style>`
    itemConfig.style = _style
  }
}

/**
 * 设置表格字段的样式
 * @param {array} fields 配置信息
 */
function setTableColumnsStyle(config) {
  const itemConfig = config.table
  const fields = itemConfig.fields
  isNotBlank(fields) && fields.forEach(field => {
    let _style = ''
    if (isNotBlank(field.align)) {
      _style += `text-align:${textAlign(field.align)};`
    }
    if (isNotBlank(field.minWidth)) {
      _style += `min-width:${field.minWidth}${config.unit};`
    }
    if (isNotBlank(field.width)) {
      _style += `width:${field.width}${config.unit};`
    }
    field.style = _style
  })
}

/**
 * 设置额外信息字段的样式
 * @param {array} fields 配置信息
 */
function setExtraFieldStyle(config) {
  const itemConfig = config.extra
  const fields = itemConfig.fields
  isNotBlank(fields) && fields.forEach(field => {
    let _style = ''
    if (isNotBlank(field.minWidth)) {
      _style += `min-width:${field.minWidth}${config.unit};`
    }
    if (isNotBlank(field.maxWidth)) {
      _style += `max-width:${field.maxWidth}${config.unit};`
    }
    if (isNotBlank(field.width)) {
      _style += `width:${field.width}${config.unit};`
    }
    field.style = _style
  })
}

/**
 * 设置额外信息字段的样式
 * @param {array} fields 配置信息
 */
function setFooterFieldStyle(config) {
  const itemConfig = config.footer
  const fields = itemConfig.fields
  isNotBlank(fields) && fields.forEach(field => {
    let _style = ''
    if (isNotBlank(field.minWidth)) {
      _style += `min-width:${field.minWidth}${config.unit};`
    }
    if (isNotBlank(field.maxWidth)) {
      _style += `max-width:${field.maxWidth}${config.unit};`
    }
    if (isNotBlank(field.width)) {
      _style += `width:${field.width}${config.unit};`
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
function dataFormat(val, field, emptyVal = '/') {
  switch (field.type) {
    case typeEnum.PROJECT.V: return emptyTextFormatter(projectNameFormat(val, field.format), emptyVal)
    case typeEnum.DATE.V: return emptyTextFormatter(dateFormat(val, field.format), emptyVal)
    case typeEnum.AMOUNT.V: return emptyTextFormatter(amountFormat(val, field.format), emptyVal)
    default: return emptyTextFormatter(val, emptyVal)
  }
}

/**
 * “项目”数据格式转换
 * @param {*} val 数据
 * @param {object} format 格式
 * @return {string} 项目名称
 */
function projectNameFormat(val, format) {
  if (!isNotBlank(format)) {
    // 默认只显示项目简称
    format = { showProjectFullName: false, showContractNo: false, projectNameShowConfig: projectNameArrangementModeEnum.CONTRACT_NO_START.V }
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
  if (val instanceof Array) {
    if (val.length === 1) {
      return moment(val[0]).format(format)
    }
    if (val.length === 2) {
      const _startDate = moment(val[0]).format(format)
      const _endDate = moment(val[1]).format(format)
      return `${_startDate} ~ ${_endDate}`
    }
  }
  return moment(val).format(format)
}

/**
 * “金额”数据格式转换
 * @param {*} val 数据
 * @param {object} format
 * @return {string|number} 金额
 */
function amountFormat(val, format) {
  let _val = val
  // 小数精度
  if (isNotBlank(format.decimal)) {
    _val = (+val).toFixed(format.decimal)
  }
  // 1000 => 1,000
  if (format.toThousandFilter) {
    _val = toThousandFilter(val)
  }
  return _val
}

/**
 * flex对齐方式
 * @param {*} align
 */
function flexAlign(align) {
  switch (align) {
    case alignEnum.LEFT.V: return 'flex-start'
    case alignEnum.RIGHT.V: return 'flex-end'
    case alignEnum.CENTER.V: return 'center'
    default: return 'flex-start'
  }
}

/**
 * text对齐方式
 * @param {*} align
 */
function textAlign(align) {
  switch (align) {
    case alignEnum.LEFT.V: return 'left'
    case alignEnum.RIGHT.V: return 'right'
    case alignEnum.CENTER.V: return 'center'
    default: return 'left'
  }
}

const COMMON_STYLE = `
<style>
body {
    /* font-feature-settings: 'onum' 1; */
    color: #333;
    overflow: hidden;
    text-rendering: optimizeLegibility;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    font-family: AvenirNext,Avenir,-apple-system,BlinkMacSystemFont,Roboto Slab,Droid Serif,Segoe UI,Oxygen-Sans,Ubuntu,Cantarell,Georgia,serif;
}
</style>
`

const EXTRA_INFO_STYLE = COMMON_STYLE + `
<style>
.extra-info {
  width: 100%;
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: center;
  flex-wrap: wrap;
  /* margin-bottom: 3mm;*/
}
.extra-info >div {
  display: inline-block;
  box-sizing: border-box;
  margin-bottom: 3mm;
  padding-right: 5mm;
  overflow : hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
</style>
`
const TITLE_STYLE = COMMON_STYLE + `
<style>
.title {
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  box-sizing: border-box;
  text-align: center;
  font-size: 17pt;
  font-weight: bold;
}
</style>
`

// 表格演示
const TABLE_STYLE = `
<style>
.table-box {
  box-sizing:border-box;
  width:100%;
}
td{ border:1px solid #000; }
.print-table {
    font-family: lucida sans unicode,lucida grande,Sans-Serif;
    font-size: 9pt;
    border-collapse: collapse;
    /* border-top: 1pt solid #9baff1; */
    /* border-bottom: 1pt solid #9baff1; */
    width: 100%;
}
.print-table th {
    font-size: 10pt;
    font-weight: 400;
    /* background: #e8edff; */
    /* border-right: 1pt solid #9baff1; */
    /* border-left: 1pt solid #9baff1; */
    /* color: #039; */
    padding: 1mm;
}
.print-table td {
    font-size: 9pt;
    /* border-right: 1pt solid #aabcfe; */
    /* border-left: 1pt solid #aabcfe; */
    /* color: #669; */
    padding: 1mm;
    line-height:13pt;
    white-space: pre-wrap;
}
.print-table tr {
  /* border-bottom: 1pt solid #ddd; */
}
tbody tr:last-child{
  /* border-bottom:none */
}
</style>
`

const FOOTER_STYLE = `
<style>
.tip {
  margin-top: 3mm;
  font-size: 9pt;
  color: red;
}
.footer-info{
  width: 100%;
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: center;
  flex-wrap: wrap;
  /* margin-bottom: 3mm;*/
}
.footer-info >div {
  display: inline-block;
  box-sizing: border-box;
  margin-top: 3mm;
  padding-right: 5mm;
  overflow : hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
</style>
`

export {
  printTable
}
