import { MIN_UNIT, DEF_UNIT } from '@/settings/config'
import { emptyTextFormatter, isBlank, isNotBlank } from '@data-type/index'
import { getDP, toThousand } from '@data-type/number'
import { convertUnits } from '@/utils/convert/unit'
import { projectNameFormatter } from '@/utils/project'
import { amountUnitEnum, alignEnum, verticleAlignEnum, dataSourceEnum, fieldTypeEnum, pageFormatEnum } from '@/utils/print/enum'
import EO from '@/utils/enum'
import { projectNameArrangementModeEnum } from '@enum-ms/contract'
import enumAll from '@/utils/enum/all'
import moment from 'moment'
import _ from 'lodash'

// TODO: 待修改
const pageFormatEnumV = EO.key2val(pageFormatEnum)
const defaultPrecision = 2

// 获取所有列
const getAllColumns = (columns) => {
  const result = []
  columns.forEach((column) => {
    result.push(column)
    if (column.children) {
      result.push.apply(result, getAllColumns(column.children))
    }
  })
  return result
}

// 删除不显示的列
const delNotDisplayed = (originColumns) => {
  const filters = (_originColumns) => {
    let index = 0
    while (index < _originColumns.length) {
      const currentColum = _originColumns[index]

      if (!currentColum.show) {
        _originColumns.splice(index, 1)
        continue
      }
      if (currentColum.children) {
        filters(currentColum.children)
        if (currentColum.children.length === 0) {
          _originColumns.splice(index, 1)
          continue
        }
      }
      index++
    }
  }
  filters(originColumns)
  return originColumns
}

// 设置colum的row和col
const convertColumns = (originColumns) => {
  // const filterColumns = delNotDisplayed(JSON.parse(JSON.stringify(originColumns)))
  let maxLevel = 1
  const traverse = (column, parent) => {
    if (parent) {
      column.level = parent.level + 1
      if (maxLevel < column.level) {
        maxLevel = column.level
      }
    }
    if (column.children) {
      let colSpan = 0
      column.children.forEach((subColumn) => {
        traverse(subColumn, column)
        colSpan += subColumn.colSpan
      })
      column.colSpan = colSpan
    } else {
      column.colSpan = 1
    }
  }

  originColumns.forEach((column) => {
    column.level = 1
    traverse(column)
  })

  const rows = []
  for (let i = 0; i < maxLevel; i++) {
    rows.push([])
  }

  const allColumns = getAllColumns(originColumns)

  allColumns.forEach((column) => {
    if (!column.children) {
      column.rowSpan = maxLevel - column.level + 1
    } else {
      column.rowSpan = 1
    }
    rows[column.level - 1].push(column)
  })

  return rows
}

// 获取末级列（即最别最小的表头）
const getLastColumns = (originColumnRows) => {
  const lastColumns = []
  const maxLevel = originColumnRows.length
  originColumnRows.forEach((r) => {
    r.forEach((c) => {
      // 末级
      if (c.level === maxLevel || c.rowSpan > 1) {
        lastColumns.push(c)
      }
    })
  })
  return lastColumns
}

const setting = {
  // 纠正config-JSON
  correctJSON(config) {
    if (!config) {
      config = {}
    }
    if (!config.fontUnit) {
      config.fontUnit = 'pt'
    }
    if (!config.logo) {
      config.logo = { show: false }
    }
    if (!config.title) {
      config.title = { show: true }
    }
    if (!config.header) {
      config.header = { show: true }
    }
    if (!config.header.fields) {
      config.header.fields = []
    }
    if (!config.footer) {
      config.footer = { show: true }
    }
    if (!config.footer.fields) {
      config.footer.fields = []
    }
    if (!config.footer.tip) {
      config.footer.tip = {}
    }
    if (!config.page) {
      config.page = { show: true }
    }
    if (!config.table) {
      config.table = { show: true }
    }
    if (!config.table.th) {
      config.table.th = {}
    }
    if (!config.table.td) {
      config.table.td = {}
    }
    if (!config.table.index) {
      config.table.index = {}
    }
    if (!config.table.summary) {
      config.table.summary = {}
    }
    if (!config.table.fields) {
      config.table.fields = []
    }
    return config
  },
  /**
   * 标题html
   * @param {object} config 标题的配置信息
   */
  getTitleHtml(config) {
    if (!isNotBlank(config)) {
      return ''
    }
    const html = `<span>${config.title || ''}</span>`
    return html
  },

  /**
   * 标题html
   * @param {object} config 标题的配置信息
   */
  getPageHtml(config) {
    if (!isNotBlank(config)) {
      return ''
    }
    const html = `<span>${this.pageFormat(config.format)}</span>`
    return html
  },

  /**
   * 表头信息html（标题之下，表格之上的内容）
   * @param {object} data 表头信息数据
   * @param {object} config 表头信息的配置信息
   */
  getHeaderHtml(data, config) {
    if (!config) {
      return ''
    }
    let html = ''
    if (config.fields) {
      for (const field of config.fields) {
        if (field.show) {
          html += `<div style="${field.style}">`
          if (field.title) {
            html += `<span style="font-weight:${config.bold}">${field.title}</span>`
          }
          if (data && field.key) {
            const _val = this.dataFormat({ row: data, field, emptyVal: config.emptyVal })
            html += isNotBlank(_val) ? `<span>${_val}</span>` : ''
          }
          html += `</div>`
        }
      }
    }
    return html
  },

  getFooterHtml(data, globalConfig) {
    const config = globalConfig.footer
    if (!config) {
      return ''
    }
    const tipCfg = config.tip
    let html = ''
    if (tipCfg && tipCfg.show && tipCfg.above) {
      html += `<span class="tip" style="font-size:${tipCfg.size}${globalConfig.fontUnit};font-weight:${
        tipCfg.bold
      };text-align:${this.textAlign(tipCfg.align)}">${isNotBlank(tipCfg.text) ? tipCfg.text : ''}</span>`
    }
    if (config.fields) {
      for (const field of config.fields) {
        if (field.show) {
          html += `<div style="${field.style}">`
          if (field.title) {
            html += `<span style="font-weight:${config.bold}">${field.title}</span>`
          }
          if (data && field.key) {
            const _val = this.dataFormat({ row: data, field, emptyVal: config.emptyVal })
            html += isNotBlank(_val) ? `<span>${_val}</span>` : ''
          }
          html += `</div>`
        }
      }
    }
    if (tipCfg && tipCfg.show && !tipCfg.above) {
      html += `<span class="tip" style="font-size:${tipCfg.size}${globalConfig.fontUnit};font-weight:${
        tipCfg.bold
      };text-align:${this.textAlign(tipCfg.align)}">${isNotBlank(tipCfg.text) ? tipCfg.text : ''}</span>`
    }
    return html
  },
  /**
   * 表头处理
   * @param {object} config 表格的配置信息
   * @param {object} needBlankColumn 需要空列
   */
  getTHeadHtml(config, columnCfg, needBlankColumn) {
    const columnRows = columnCfg.columnRows
    let html = '<thead>'
    columnRows.forEach((cr, i) => {
      html += '<tr>'
      if (i === 0 && config.index && config.index.show) {
        const _style = config.index.style
        html += `<th rowspan="${columnRows.length}" style="${_style}"><div style="${_style}">${config.index.title || '序号'}</div></th>`
      }
      cr.forEach((c) => {
        if (c.show) {
          html += `<th colspan="${c.colSpan}" rowspan="${c.rowSpan}" style="${c.style}"><div style="${c.style}">${c.title || ''}</div></th>`
        }
      })
      if (i === 0 && needBlankColumn) {
        html += `<th scope="col" class="blank-column"><div></div></th>`
      }
      html += '</tr>'
    })
    html += '</thead>'
    return html
  },

  /**
   * 表格信息html
   * @param {object} data 表格数据
   * @param {object} config 表格的配置信息
   */
  getTableHtml(data, config, columnCfg) {
    if (!config) {
      return ''
    }
    let html = ''
    // 需要空列
    const needBlankColumn =
      config.index && config.index.show && isNotBlank(config.index.width) && columnCfg.lastColumns.every((f) => isNotBlank(f.width)) // 字段都是固定宽度
    html += `
      <table class="preview-table" border="1">
       ${this.getTHeadHtml(config, columnCfg, needBlankColumn)}
        <tbody>
        `
    // body
    let index = 0
    for (const row of data) {
      html += `<tr>`
      // 序号
      if (config.index && config.index.show) {
        const _style = config.index.style
        html += `<td style="${_style}"><div style="${_style}">${index + 1}</div></td>`
      }
      // 表格内容
      for (const field of columnCfg.lastColumns) {
        // let _content = row && field && field.key && isNotBlank(row[field.key]) ? row[field.key] : ''
        // 格式转换
        const _content = this.dataFormat({ row, field, emptyVal: config.emptyVal })
        html += `<td style="${field.style}"><div style="${field.style}">${_content}</div></td>`
      }
      if (needBlankColumn) {
        html += `<td class="blank-column"><div></div></td>`
      }
      html += `</tr>`
      ++index
    }
    // 是否需要合计
    if (config.summary && config.summary.show) {
      html += this.spliceSummary({
        data,
        config,
        needBlankColumn,
        columnCfg
      })
    }
    html += `
        </tbody>
      </table>
      `
    return html
  },

  /**
   * 表格的合计信息html
   * @param {object} data 表格数据
   * @param {object} config 表格的配置信息
   */
  spliceSummary({ data, config, needBlankColumn, columnCfg }) {
    let html = `<tr>`
    if (config.index && config.index.show) {
      const _style = config.index.style
      html += `<td style="${_style}"><div style="${_style}">${config.summary.title || '合计'}</div></td>`
    }
    for (const field of columnCfg.lastColumns) {
      // 单元格填充内容
      let sum = ''
      if (field && field.sum) {
        // 判断字段是否需要合计
        const columns = data.map((d) => this.keyParse(d, field.key)) // 列数据
        let dp = 0
        const dpArr = []
        if (!columns.every((value) => isNaN(+value))) {
          // 判断是否为数字类型,此处允许空格或空字符串，因此可用isNaN，否则使用正则表达式
          sum = columns.reduce((prev, curr) => {
            const value = Number(curr)
            if (!isNaN(value)) {
              dpArr.push(getDP(curr))
              return prev + Number(curr)
            } else {
              return prev
            }
          }, 0)
          // 获取最大的小数精度位数
          dp = dpArr.getMax()
          dp = dp > 5 ? 5 : dp
          const cloneField = JSON.parse(JSON.stringify(field))
          if (!cloneField.format) {
            cloneField.format = {}
          }
          if (isBlank(cloneField.format.precision)) {
            cloneField.format.precision = dp
          }

          sum = this.dataFormat({ val: sum, field: cloneField })
        }
      }
      html += `<td style="${field.style}"><div style="${field.style}">${sum}</div></td>`
    }
    if (needBlankColumn) {
      html += `<td class="blank-column"><div></div></td>`
    }
    html += `</tr>`
    return html
  },

  /**
   * 设置表格内容样式
   * @param {object} globalConfig 表格配置信息
   */
  setTableStyle(globalConfig, columnCfg) {
    const config = globalConfig.table
    if (globalConfig && isNotBlank(config)) {
      this.setTableColumnsStyle(globalConfig, columnCfg) // 设置表格样式
      let _style = `<style>`
      if (isNotBlank(config.th)) {
        _style += `
      .preview-table th {`
        if (isNotBlank(config.th.size)) {
          _style += ` font-size: ${config.th.size}${globalConfig.fontUnit};`
        }
        if (isNotBlank(config.th.bold)) {
          _style += ` font-weight: ${config.th.bold};`
        }
        if (isNotBlank(config.th.lineHeight)) {
          _style += ` line-height: ${config.th.lineHeight}${globalConfig.fontUnit}!important;`
        }
        _style += `}`
        _style += `
      .preview-table th >div{`
        if (isNotBlank(config.th.paddingTB)) {
          _style += ` padding-top: ${config.th.paddingTB}${globalConfig.unit}!important;`
          _style += ` padding-bottom: ${config.th.paddingTB}${globalConfig.unit}!important;`
        }
        _style += `}`
      }
      if (isNotBlank(config.td)) {
        _style += `
      .preview-table td {`
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
      .preview-table td >div{`
        if (isNotBlank(config.td.paddingTB)) {
          _style += ` padding-top: ${config.td.paddingTB}${globalConfig.unit}!important;`
          _style += ` padding-bottom: ${config.td.paddingTB}${globalConfig.unit}!important;`
        }
        _style += `}`
      }
      _style += `</style>`
      config.style = _style
    }
  },

  /**
   * 设置表格字段的样式
   * @param {object} globalConfig 表格配置信息
   */
  setTableColumnsStyle(globalConfig, columnCfg) {
    const config = globalConfig.table
    if (globalConfig && isNotBlank(config)) {
      // 列样式
      const columnRows = columnCfg.columnRows
      columnRows.forEach((row) => {
        row.forEach((column) => {
          let _style = ''
          if (isNotBlank(column.align)) {
            _style += `text-align:${this.textAlign(column.align)};`
          }
          if (isNotBlank(column.width)) {
            _style += `width:${column.width}${globalConfig.unit};`
          } else if (isNotBlank(column.minWidth)) {
            _style += `min-width:${column.minWidth}${globalConfig.unit};`
          }
          column.style = _style
        })
      })
      // 序号样式
      const index = config.index
      if (isNotBlank(config.index)) {
        let _style = ''
        if (isNotBlank(index.align)) {
          _style += `text-align:${this.textAlign(index.align)};`
        }
        if (isNotBlank(index.width)) {
          _style += `width:${index.width}${globalConfig.unit};`
        } else if (isNotBlank(index.minWidth)) {
          _style += `min-width:${index.minWidth}${globalConfig.unit};`
        }
        index.style = _style
      }
    }
  },

  /**
   * 设置表头信息字段的样式
   * @param {object} globalConfig 表格配置信息
   */
  setHeaderFieldStyle(globalConfig) {
    const config = globalConfig.header
    if (globalConfig && isNotBlank(config)) {
      const fields = config.fields
      isNotBlank(fields) &&
        fields.forEach((field) => {
          let _style = ''
          if (isNotBlank(field.width)) {
            _style += `width:${field.width}${globalConfig.unit};`
          } else if (isNotBlank(field.maxWidth)) {
            _style += `max-width:${field.maxWidth}${globalConfig.unit};`
          }
          field.style = _style
        })
    }
  },

  /**
   * 设置表头信息字段的样式
   * @param {object} globalConfig 表格配置信息
   */
  setFooterFieldStyle(globalConfig) {
    const config = globalConfig.footer
    if (globalConfig && isNotBlank(config)) {
      const fields = config.fields
      isNotBlank(fields) &&
        fields.forEach((field) => {
          let _style = ''
          if (isNotBlank(field.width)) {
            _style += `width:${field.width}${globalConfig.unit};`
          } else if (isNotBlank(field.maxWidth)) {
            _style += `max-width:${field.maxWidth}${globalConfig.unit};`
          }
          if (isNotBlank(field.align)) {
            _style += `text-align:${this.textAlign(field.align)};`
          }
          field.style = _style
        })
    }
  },

  /**
   * 数据格式转换
   * @param {*} val 数据
   * @param {object} field 字段信息
   * @param {string} emptyVal | default: "/" 当值未空时，代替的值
   */
  dataFormat({ row = {}, val, field, emptyVal = '' }) {
    if (field.source === dataSourceEnum.CUSTOMIZE.V) {
      emptyVal = ''
    }
    const needParse = !isNotBlank(val) && isNotBlank(row) && isNotBlank(field)
    if (needParse) {
      val = this.keyParse(row, field.key)
    }
    switch (field.type) {
      case fieldTypeEnum.PROJECT.K:
        return emptyTextFormatter(this.projectNameFormat(val, field.format), emptyVal)
      case fieldTypeEnum.DATE.K:
        return emptyTextFormatter(this.dateFormat(val, field.format), emptyVal)
      case fieldTypeEnum.DATES.K:
        return emptyTextFormatter(this.dateFormat(val, field.format), emptyVal)
      case fieldTypeEnum.AMOUNT.K:
        return emptyTextFormatter(this.amountFormat(val, field.format), emptyVal)
      case fieldTypeEnum.WEIGHT.K:
        return emptyTextFormatter(this.weightFormat(val, field.format), emptyVal)
      case fieldTypeEnum.LENGTH.K:
        return emptyTextFormatter(this.lengthFormat(val, field.format), emptyVal)
      case fieldTypeEnum.RATE.K:
        return emptyTextFormatter(this.rateFormat(val, field.format), emptyVal)
      case fieldTypeEnum.THICKNESS.K:
        return emptyTextFormatter(this.thicknessFormat(val, field.format), emptyVal)
      case fieldTypeEnum.METE.K:
        return emptyTextFormatter(
          this.meteFormat({
            val,
            row,
            format: field.format
          }),
          emptyVal
        )
      case fieldTypeEnum.QUANTITY.K:
        return emptyTextFormatter(this.quantityFormat(val, field.format), emptyVal)
      case fieldTypeEnum.ENUM.K:
        return emptyTextFormatter(this.enumFormat(val, field.format), emptyVal)
      default:
        return emptyTextFormatter(val, emptyVal)
    }
  },
  /**
   * key解析，处理key为'xxx.xx'的情况
   * @param {object} data 数据
   * @param {string} key 字段名
   */
  keyParse(data, key) {
    const parseable = isNotBlank(data) && isNotBlank(key) && typeof key === 'string'
    if (parseable) {
      const keys = key.split('.')
      if (keys.length === 1) {
        return data[keys[0]]
      } else {
        return keys.reduce((cur, key) => {
          return _.isPlainObject(cur) ? cur[key] : undefined
        }, data)
      }
    }
    return
  },
  /**
   * 枚举数据格式转换
   * @param {*} val 数据
   * @param {object} format 格式
   */
  enumFormat(val, format) {
    const flag = isNotBlank(format) && isNotBlank(format.enum) && isNotBlank(enumAll[format.enum]) && isNotBlank(val)
    if (flag) {
      const key = format.key || 'L'
      const enumK = enumAll[format.enum]
      if (format.bit) {
        // 位运算的值
        const enums = EO.toArr(enumK)
        const res = []
        enums.forEach((e) => {
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
  },

  /**
   * “项目”数据格式转换
   * @param {*} val 数据
   * @param {object} format 格式
   * @return {string} 项目名称
   */
  projectNameFormat(val, format = {}) {
    if (!isNotBlank(format)) {
      // 默认只显示项目简称
      format = {
        showProjectFullName: false,
        showSerialNumber: false,
        projectNameShowConfig: projectNameArrangementModeEnum.SERIAL_NUMBER_START.V
      }
    }
    return projectNameFormatter(val, format, format.lineBreak)
  },

  /**
   * “日期”数据格式转换
   * @param {number/string/array} val 数据
   * @param {string} format | default: 'YY/MM/DD' 格式
   * @return {string} 日期
   */
  dateFormat(val, format = 'YY/MM/DD') {
    const filterDate = (val) => {
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
            .map((t) => {
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
  },

  /**
   * “金额”数据格式转换
   * @param {*} val 数据
   * @param {object} format
   * @return {string|number} 金额
   */
  amountFormat(val, format = {}) {
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
        _val = toThousand(_val, format.precision ?? defaultPrecision)
      }
    }
    return _val
  },

  /**
   * “重量”数据格式转换
   * @param {*} val 数据
   * @param {object} format
   * @return {string|number} 重量
   */
  weightFormat(val, format = {}) {
    let _val = val
    if (isNotBlank(_val)) {
      // 单位转换
      if (isNotBlank(format.unit)) {
        _val = convertUnits(_val, DEF_UNIT.WEIGHT, format.unit)
      }
      // 小数精度
      if (isNotBlank(format.precision)) {
        _val = (+_val).toFixed(format.precision)
      }
      // 1000 => 1,000
      if (format.toThousand) {
        _val = toThousand(_val, format.precision ?? defaultPrecision)
      }
    }
    return _val
  },

  /**
   * “长度”数据格式转换
   * @param {*} val 数据
   * @param {object} format
   * @return {string|number} 长度
   */
  lengthFormat(val, format = {}) {
    let _val = val
    if (isNotBlank(_val)) {
      if (isNotBlank(format.unit)) {
        _val = convertUnits(_val, DEF_UNIT.LENGTH, format.unit)
      }
      // 小数精度
      if (isNotBlank(format.precision)) {
        _val = (+_val).toFixed(format.precision)
      }
      // 1000 => 1,000
      if (format.toThousand) {
        _val = toThousand(_val, format.precision ?? defaultPrecision)
      }
    }
    return _val
  },

  /**
   * “厚度”数据格式转换
   * @param {*} val 数据
   * @param {object} format
   * @return {string|number} 厚度
   */
  thicknessFormat(val, format = {}) {
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
        _val = toThousand(_val, format.precision ?? defaultPrecision)
      }
    }
    return _val
  },

  /**
   * “量”数据格式转换
   * @param {*} val 数据
   * @param {object} format
   * @return {string|number} 量
   */
  meteFormat({ val, row, format = {}}) {
    let _val = val
    if (isNotBlank(_val)) {
      // 小数精度
      if (isNotBlank(format.precision)) {
        _val = (+_val).toFixed(format.precision)
      }
      // 1000 => 1,000
      if (format.toThousand) {
        _val = toThousand(_val, format.precision ?? defaultPrecision)
      }
      // 是否显示单位
      if (format.showUnit) {
        if (format.unit) {
          _val += ` ${format.unit}`
        }
        if (row?.[format.rowUnit]) {
          _val += ` ${row[format.rowUnit]}`
        }
      }
    }
    return _val
  },

  /**
   * “数量”数据格式转换
   * @param {*} val 数据
   * @param {object} format
   * @return {string|number} 量
   */
  quantityFormat(val, format = {}) {
    let _val = val
    if (isNotBlank(_val)) {
      // 小数精度
      if (isNotBlank(format.precision)) {
        _val = (+_val).toFixed(format.precision)
      }
      // 1000 => 1,000
      if (format.toThousand) {
        _val = toThousand(_val, format.precision ?? defaultPrecision)
      }
    }
    return _val
  },

  /**
   * “比例/比率”数据格式转换
   * @param {*} val 数据
   * @param {object} format
   * @return {string|number} 量
   */
  rateFormat(val, format = {}) {
    let _val = val
    // 小数精度
    if (isNotBlank(format.precision)) {
      _val = (+_val).toFixed(format.precision)
    }
    return _val + '%'
  },

  /**
   * flex对齐方式
   * @param {*} align
   */
  flexAlign(align) {
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
  },

  /**
   * flex垂直对齐方式（align-item）
   * @param {*} align
   */
  verticleAlign(align) {
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
  },

  /**
   * text对齐方式
   * @param {*} align
   */
  textAlign(align) {
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
  },

  pageFormat(val) {
    return pageFormatEnumV[val] ? pageFormatEnumV[val].L : '1 / 1'
  }
}

export { convertColumns, delNotDisplayed, getLastColumns, setting }
