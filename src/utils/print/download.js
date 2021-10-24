import { emptyTextFormatter, isNotBlank } from '@/utils/data-type'
import { toThousandFilter } from '@/utils/data-type/number'
import { createUniqueString } from '@/utils/data-type/string'
import { projectNameFormatter, getBasicClassUnit, getMaterialTypeUnit, getMaterialListTypeUnit } from '@/utils/other'
import { px2emu, convertUnits, lengthUnit2px, pt2px } from '@/utils/unit'
import { imgLoaded, img2Uint8Array, base64ToUint8Array } from '@/utils/image'
import { getFileType } from '@/utils/file'
import { amountUnitEnum, dataSourceEnum, alignEnum, verticleAlignEnum, fieldTypeEnum } from './enum'
import { convertColumns, delNotDisplayed, getLastColumns } from './page-handle'
import enumOperate, { projectNameArrangementModeEnum } from '@/utils/enum'
import enumAll from '@/utils/enum/all'
import { minUnit } from '@/utils/constant'
import moment from 'moment'
import _ from 'lodash'

let XLSX
// MDW can control the column width, but don't understand what this number is
const MDW = 8.1

if (typeof require !== 'undefined') XLSX = require('xlsx-styleable')

// Do not use font attribute： shadow, vertAlign
// Cell border line style
const borderLineStyle = { style: 'thin', color: 'e10602FF' }
// Cell border style
const borderStyle = {
  left: borderLineStyle,
  right: borderLineStyle,
  top: borderLineStyle,
  bottom: borderLineStyle
}

/**
   * Export Excel
   * @param {object} header header data
   * @param {object} footer footer data
   * @param {object} table table data
   * @param {object} qrCode QR Code
   * @param {object} config config
   * @author duhh
   */
async function download({ header, table, footer, qrCode, config }) {
  if (!isNotBlank(config)) {
    throw new Error('未配置')
  }
  try {
    if (isNotBlank(config.logo) && config.logo.show && config.logo.url) {
      const _img = await imgLoaded(config.logo.url)
      if (_img) config.logo.binary = img2Uint8Array(_img)
    }
    // Each module configuration
    const titleCfg = config.title
    const headerCfg = config.header
    const tableCfg = config.table
    const footerCfg = config.footer
    const logoCfg = config.logo
    const qrCodeCfg = config.qrCode

    // base configuration
    const baseCfg = {
      height: config.height,
      width: config.width,
      paddingTB: config.paddingTB,
      paddingLR: config.paddingLR,
      aclHeight: config.height - config.paddingTB * 2,
      aclWidth: config.width - config.paddingLR * 2,
      unit: config.unit
    }

    // sr:starting row
    const sr = {
      title: void 0,
      header: void 0,
      tHeader: void 0,
      tBody: void 0,
      footer: void 0,
      footerTip: void 0,
      footerExtra: void 0,
      all: void 0
    }

    // rn:row number
    const rn = {
      title: 0,
      header: 0,
      tHeader: 0,
      tBody: 0,
      footer: 0,
      footerTip: 0,
      footerExtra: 0
    }

    const headerRows = []
    const footerRows = []

    // File (workbook) name
    const filename = config.name + '.xlsx'

    // Create workbook object
    const wb = XLSX.utils.book_new()

    // Worksheet options
    var wsopts = { WTF: true, cellStyles: true, dateNF: 'yyyy/mm/dd' }

    // Worksheet data
    const wsData = []

    // worksheet name
    const ws_name = titleCfg.title

    // Converts an array of arrays of JS data to a worksheet
    var ws = XLSX.utils.aoa_to_sheet(wsData, wsopts)
    ws['!rows'] = []
    ws['!cols'] = []

    // Set the contents of the worksheet, example: worksheet width and height
    setBase({ config, baseCfg, ws })
    setColumns(tableCfg)
    sr.header = setTitle({ config: titleCfg, baseCfg, ws, sr, rn })
    sr.tHeader = setHeader({ data: header, config: headerCfg, baseCfg, ws, sr, rn, headerRows })
    // FIXME: Project / Multiple lines, Line height problem (Not high enough )
    // TODO: Date format problem, whether to use the date format of SHEET
    // TODO: tHeader cell line break
    sr.footer = setTable({ data: table, config: tableCfg, baseCfg, ws, sr, rn })
    sr.all = setFooter({ data: footer, config: footerCfg, baseCfg, ws, sr, rn, footerRows })

    // Merger cells (May contain Settings for row height and column width)
    mergerCells({ ws, headerCfg, tableCfg, footerCfg, baseCfg, sr, rn, headerRows, footerRows })

    // Image Settings are related to column width and row height, so they are set after merging cells
    setLogo({ baseCfg, config: logoCfg, ws })

    setQRCode({ baseCfg, qrCode, config: qrCodeCfg, ws })

    // Add worksheet to workbook
    XLSX.utils.book_append_sheet(wb, ws, ws_name)

    XLSX.write(wb, { file: filename, bookType: 'xlsx', bookSST: false, type: 'file', cellStyles: false })
    return true
  } catch (e) {
    throw new Error(e)
  }
}

/**
 * set table columns
 * @param {*} config table config
 */
function setColumns(config) {
  if (!config) return
  // 设置column
  config.columns = delNotDisplayed(JSON.parse(JSON.stringify(config.fields)))
  config.columnRows = convertColumns(config.columns)
  config.lastColumns = getLastColumns(config.columnRows)
}

/**
 * Set logo
 * @param {object} baseCfg base config
 * @param {object} config logo config
 * @param {object} ws worksheet
 */
function setLogo({ baseCfg, config, ws }) {
  if (!config || !config.binary) {
    return
  }
  setImage(ws, baseCfg, {
    name: `LOGO_${createUniqueString()}.${getFileType(config.url) || 'png'}`,
    data: config.binary,
    width: config.width,
    height: config.height,
    top: config.top,
    left: config.left
  })
}

/**
 * Set QR Code
 * @param {object} baseCfg base config
 * @param {object} qrCode qr code
 * @param {object} config logo config
 * @param {object} ws worksheet
 */
function setQRCode({ baseCfg, qrCode, config, ws }) {
  if (!config || !qrCode) {
    return
  }

  const jrQrcode = require('jr-qrcode')

  /**
   * Get the QR Code of base64 encoding
   * @param: options: {
   *            padding       : 10,   // padding(default: 10px)
   *            width         : 256,  // width(default: 256px)
   *            height        : 256,  // height(default: 256px)
   *            correctLevel  : 0-3,    // Fault tolerance level(default: 2)
   *            reverse       : false,        // Reverse color, color is the background color of the upper container
   *            background    : "#ffffff",    // background(default: white)
   *            foreground    : "#000000"     // QR Code color(default: black)
   *         }
   */
  const imgBase64 = jrQrcode.getQrBase64(qrCode, {
    padding: 0,
    correctLevel: 1,
    width: lengthUnit2px(config.width, baseCfg.unit),
    height: lengthUnit2px(config.height, baseCfg.unit)
  })

  setImage(ws, baseCfg, {
    name: createUniqueString() + '.png',
    data: base64ToUint8Array(imgBase64),
    width: config.width,
    height: config.height,
    top: config.top,
    left: config.left
  })
}

/**
 * Set drawing
 * @param {object} ws worksheet
 * @param {object} baseCfg base config
 * @param {object} imageInfo { name, data, type, width, height, top, left }
 */
function setImage(ws, baseCfg, { name, data, type, width, height, top, left }) {
  if (!ws['!drawing']) ws['!drawing'] = []
  const _width = lengthUnit2px(width, baseCfg.unit)
  const _height = lengthUnit2px(height, baseCfg.unit)
  // Deal with margin
  let _top = lengthUnit2px(top - baseCfg.paddingTB, baseCfg.unit)
  let _left = lengthUnit2px(left - baseCfg.paddingLR, baseCfg.unit)
  const _right = lengthUnit2px(baseCfg.aclWidth, baseCfg.unit) - _width - left
  const _bottom = lengthUnit2px(baseCfg.aclHeight, baseCfg.unit) - _height - top
  if (_right < 0) _left += _right
  if (_bottom < 0) _top += _bottom
  if (_top < 0) _top = 0
  if (_left < 0) _left = 0
  // Distance from image right to excel left
  let imgR2excelL = _left + _width
  // Distance from image bottom to excel top
  const imgB2excelT = _top + _height

  const cols = ws['!cols'].map(c => c.wpx)
  const colsSum = cols.reduce((sum, cur) => sum + cur, 0)
  // 1.33, The actual height is different from the calculation
  const rows = ws['!rows'].map(c => (c.hpx || pt2px(c.hpt)) * 1.33)

  // Print beyond the width doesn't show, at this point the image moves to the left
  const beyondWidth = imgR2excelL - colsSum
  if (beyondWidth > 0) {
    _left -= beyondWidth
    if (_left < 0) _left = 0
    imgR2excelL = _left + _width
  }

  const posFrom = { col: void 0, row: void 0, colOff: void 0, rowOff: void 0 }
  const posTo = { col: void 0, row: void 0, colOff: void 0, rowOff: void 0 }
  let colsWidth = 0
  for (let i = 0; i < cols.length; i++) {
    colsWidth += cols[i]
    if (!isNotBlank(posFrom.col) && colsWidth >= _left) {
      posFrom.col = i
      // Offset relative to the cell
      posFrom.colOff = px2emu(_left - (colsWidth - cols[i]))
    }
    if (colsWidth >= imgR2excelL) {
      posTo.col = i
      posTo.colOff = px2emu(imgR2excelL - (colsWidth - cols[i]))
      break
    }
  }

  // End with the last column when the table exceeds its width
  if (posTo.colOff === void 0) {
    const _i = cols.length - 1
    posTo.col = _i
    posTo.colOff = px2emu(cols[_i])
  }

  let rowsHeight = 0
  for (let i = 0; i < rows.length; i++) {
    rowsHeight += rows[i]
    if (!isNotBlank(posFrom.row) && rowsHeight >= _top) {
      posFrom.row = i
      // Offset relative to the cell
      posFrom.rowOff = px2emu(_top - (rowsHeight - rows[i]))
    }
    if (rowsHeight >= imgB2excelT) {
      posTo.row = i
      posTo.rowOff = px2emu(imgB2excelT - (rowsHeight - rows[i]))
      break
    }
  }

  // Rows are added when the table exceeds its height
  if (posTo.rowOff === void 0) {
    const _h = imgB2excelT - rowsHeight
    ws['!rows'].push({ hpx: _h })
    posTo.row = rows.length // length + 1 - 1
    posTo.rowOff = px2emu(_h)
  }

  // Set image position
  ws['!drawing'].push({
    name: name,
    data: data,
    opts: { base64: false, binary: true, optimizedBinaryString: false },
    type: type,
    // editAs: oneCell and absolute => Nothing changes after setting
    attrs: { editAs: 'oneCell' },
    spPr: {
      // xfrm：Nothing changes after setting
      xfrm: {
        off: { x: 0, y: 0 },
        ext: { cx: 0, cy: 0 }
      }
    },
    /**
     * unit => https://blog.lindexi.com/post/Office-Open-XML-%E7%9A%84%E6%B5%8B%E9%87%8F%E5%8D%95%E4%BD%8D.html
     * EMUs (English Metric Unit)
     * 1 in = 914400 EMUs
     * 1 cm = 360000 EMUs
     * colOff and rowOff should not exceed 1 cell
     * Example1:
     * cellHeight: 10mm; cellWidth: 10mm
     * from : { col: 0, row: 0, colOff: 0, rowOff:0 }
     * to: { col: 5, row: 5, colOff: 0, rowOff: 0}
     * actual: col => 0 - 4, row => 0 - 4
     *
     * Example2:
     * cellHeight: 10mm; cellWidth: 10mm
     * from : { col: 0, row: 0, colOff: 2000000, rowOff:2000000 } more than 10mm
     * to: { col: 5, row: 5, colOff: 0, rowOff: 0}
     * actual: col => 1 - 4, row => 1 - 4
     *
     * Example3:
     * cellHeight: 10mm; cellWidth: 10mm
     * from : { col: 0, row: 0, colOff: 0, rowOff:0 }
     * to: { col: 5, row: 5, colOff: 2000000, rowOff: 2000000} more than 10mm
     * actual: col => 0 - 5, row => 0 - 5
     */
    position: {
      type: 'twoCellAnchor',
      from: posFrom,
      to: posTo
    }
  })
}

/**
 * Set basic information for worksheet
 * @param {object} baseCfg base config
 * @param {object} ws worksheet
 */
function setBase({ baseCfg, ws }) {
  // unit inches
  ws['!margins'] = {
    left: convertUnits(baseCfg.paddingLR, baseCfg.unit, 'in'),
    right: convertUnits(baseCfg.paddingLR, baseCfg.unit, 'in'),
    top: convertUnits(baseCfg.paddingTB, baseCfg.unit, 'in'),
    bottom: convertUnits(baseCfg.paddingTB, baseCfg.unit, 'in'),
    header: 0,
    footer: 0.393 // 1 cm = 0.3937008 in
  }
}

/**
 * merger cells
 * @param {object} ws worksheet
 * @param {object} headerCfg header config
 * @param {object} footerCfg footer config
 * @param {object} baseCfg base config
 * @param {object} sr starting row
 * @param {object} rn row number
 * @param {object} headerRows header rows data
 * @param {object} footerRows footer rows data
 */
function mergerCells({ ws, headerCfg, tableCfg, footerCfg, baseCfg, sr, rn, headerRows, footerRows }) {
  // No cell (haven't any data)
  // TODO: There is a problem with table style when content does not exit
  if (!ws['!ref']) {
    return
  }
  const wsRef = ws['!ref'].split(':')
  const firstCell = XLSX.utils.decode_cell(wsRef[0])
  // need to determine if there is only one cell
  const lastCell = XLSX.utils.decode_cell(wsRef.length > 1 ? wsRef[1] : wsRef[0])
  const wsMerges = []

  // Processing columns width when table data is empty
  if (!rn.tHeader && !rn.tBody) {
    // If there only one column then the column width is equal to the length
    if (lastCell.c === 0) {
      ws['!rows'].push({ hpx: lengthUnit2px(baseCfg.aclHeight, baseCfg.unit) })
    } else { // lastCell.c > 0
      // Gets the rows with the most fields
      const headerRowsLengthArr = headerRows.map(r => r.length)
      const footerRowsLengthArr = footerRows.map(r => r.length)
      const headerMaxFieldsI = headerRowsLengthArr.maxIndex()
      const footerMaxFieldsI = footerRowsLengthArr.maxIndex()
      const headerMax = headerRowsLengthArr[headerMaxFieldsI]
      const footerMax = footerRowsLengthArr[footerMaxFieldsI]

      // At present, when columns are greater than 1, they're not all going to be 0, so the comment determines
      // if (rowMaxFields > 0 || footerMaxFields > 0)
      let _startIndex
      let _endIndex
      let _fields
      if (headerMax >= footerMax) {
        _startIndex = _.sum(headerRowsLengthArr.slice(0, headerMaxFieldsI))
        _endIndex = _startIndex + headerMax - 1
        _fields = headerCfg.fields.filter(f => f.show)
      } else {
        _startIndex = _.sum(footerRowsLengthArr.slice(0, footerMaxFieldsI))
        _endIndex = _startIndex + footerMax - 1
        _fields = footerCfg.fields.filter(f => f.show)
      }
      const _maxFields = JSON.parse(JSON.stringify(_fields.slice(_startIndex, _endIndex + 1)))

      // Calculate the columns width based on the fields width
      const _widthSum = _.sum(_maxFields.map(f => {
        f.realWidth = isNotBlank(f.width) ? f.width : f.maxWidth
        return f.realWidth
      }))
      const _percentWidth = _maxFields.map(f => Math.floor((f.realWidth / _widthSum) * 100) / 100)
      _percentWidth.forEach(p => {
        const _w = Math.floor((p * baseCfg.aclWidth) * 100) / 100
        ws['!cols'].push({ wpx: lengthUnit2px(_w, baseCfg.unit), MDW })
      })
      // }
    }
    // FIXME: table fields number < header/footer fields number
  }

  const wsCols = ws['!cols'].map(c => c.wpx)

  if (rn.title) {
    wsMerges.push({ s: { r: firstCell.r, c: firstCell.c }, e: { r: firstCell.r, c: lastCell.c }})
  }
  if (rn.footerTip) {
    wsMerges.push({ s: { r: sr.footerTip, c: firstCell.c }, e: { r: sr.footerTip, c: lastCell.c }})
  }
  if (rn.header) {
    const fields = headerCfg.fields.filter(f => f.show)
    const newHeaderRows = []
    const rowIndex = sr.header
    const rowEndIndex = rowIndex + rn.header
    for (let ri = rowIndex; ri < rowEndIndex; ri++) {
      const _row = headerRows[ri - rowIndex]
      const _fields = fields.splice(0, _row.length)
      // get the current row fields width
      const _rw = _fields.map(v => {
        const width = isNotBlank(v.width) ? v.width : v.maxWidth
        return lengthUnit2px(width, baseCfg.unit)
      })

      /**
       * FIXME: To optimize：Better allocation of cells
       * if don't compute the difference: the footer Each row should be no more than three fields
       */
      // The number of columns each field（left to right）
      const _wsCols = [...wsCols]
      const _cn = _rw.map((w, i) => {
        let flag = true
        let acc = 0
        let columnNum = 0
        while (flag && _wsCols.length > 0) {
          acc += _wsCols[0]
          if (acc >= w && columnNum !== 0) {
            // Calculate the difference. If diff2 is greater than diff2 then column add one
            const diff1 = Math.abs(acc - w)
            const diff2 = Math.abs(acc - _wsCols[0] - w)
            if (diff1 < diff2) {
              columnNum += 1
            }
            flag = false
          } else {
            _wsCols.shift()
            columnNum += 1
          }
        }
        return columnNum
      })
      //  Adjust the columns number of fields to 0 ( 0 => 1) 【In the case of multiple distributions】
      for (let i = 0; i < _cn.length; i++) {
        if (!_cn[i]) {
          _cn[i] = 1
          _cn[_cn.maxIndex()] -= 1
        }
      }
      let _colIndex = 0
      const currentRowData = new Array(lastCell.c + 1).fill('')
      _cn.forEach((n, i) => {
        let _range = {}
        if (i === _cn.length - 1) {
          _range = { s: { r: ri, c: _colIndex }, e: { r: ri, c: lastCell.c }}
        } else {
          _range = { s: { r: ri, c: _colIndex }, e: { r: ri, c: _colIndex + n - 1 }}
        }
        currentRowData[_colIndex] = _row[i]
        _colIndex += n
        wsMerges.push(_range)
      })
      newHeaderRows.push(currentRowData)
    }
    XLSX.utils.sheet_add_aoa(ws, newHeaderRows, { skipHeader: true, origin: rowIndex, sheetStubs: true })

    for (let r = rowIndex; r < rowEndIndex; r++) {
      const row = newHeaderRows[r - rowIndex]
      for (let c = 0; c < row.length; c++) {
        const cellAddress = XLSX.utils.encode_cell({ c, r })
        // Set the header cell style
        ws[cellAddress].s = {
          font: {
            name: 'Microsoft YaHei',
            sz: pt2px(headerCfg.size), color: { rgb: '000000' },
            bold: headerCfg.bold === 'bold', underline: false,
            outline: false, italic: false, strike: false
          },
          alignment: {
            vertical: verticleAlign(headerCfg.verticleAlign),
            horizontal: horizontalAlign(headerCfg.align),
            wrapText: false
          }
        }
        // Line height varies with font size
      }
      ws['!rows'].splice(rowIndex, 0, ({ hpt: headerCfg.size + 10 }))
    }
  }

  if (isNotBlank(rn.footerExtra)) {
    const fields = footerCfg.fields.filter(f => f.show)
    const newFooterRows = []
    const rowIndex = sr.footerExtra
    const rowEndIndex = rowIndex + rn.footerExtra
    for (let ri = rowIndex; ri < rowEndIndex; ri++) {
      const _row = footerRows[ri - rowIndex]
      const _fields = fields.splice(0, _row.length)
      // get the current row fields width
      const _rw = _fields.map(v => {
        const width = isNotBlank(v.width) ? v.width : v.maxWidth
        return lengthUnit2px(width, baseCfg.unit)
      })

      /**
       * FIXME: To optimize：Better allocation of cells
       * if don't compute the difference: the footer Each row should be no more than three fields
       */
      // The number of columns each field（left to right）
      const _wsCols = [...wsCols]
      const _cn = _rw.map((w, i) => {
        let flag = true
        let acc = 0
        let columnNum = 0
        while (flag && _wsCols.length > 0) {
          acc += _wsCols[0]
          if (acc >= w && columnNum !== 0) {
            // Calculate the difference. If diff2 is greater than diff2 then column add one
            const diff1 = Math.abs(acc - w)
            const diff2 = Math.abs(acc - _wsCols[0] - w)
            if (diff1 < diff2) {
              columnNum += 1
            }
            flag = false
          } else {
            _wsCols.shift()
            columnNum += 1
          }
        }
        return columnNum
      })
      //  Adjust the columns number of fields to 0 ( 0 => 1) 【In the case of multiple distributions】
      for (let i = 0; i < _cn.length; i++) {
        if (!_cn[i]) {
          _cn[i] = 1
          _cn[_cn.maxIndex()] -= 1
        }
      }
      let _colIndex = 0
      const currentRowData = new Array(lastCell.c + 1).fill('')
      _cn.forEach((n, i) => {
        let _range = {}
        if (i === _cn.length - 1) {
          _range = { s: { r: ri, c: _colIndex }, e: { r: ri, c: lastCell.c }}
        } else {
          _range = { s: { r: ri, c: _colIndex }, e: { r: ri, c: _colIndex + n - 1 }}
        }
        currentRowData[_colIndex] = _row[i]
        _colIndex += n
        wsMerges.push(_range)
      })
      newFooterRows.push(currentRowData)
    }
    XLSX.utils.sheet_add_aoa(ws, newFooterRows, { skipHeader: true, origin: rowIndex, sheetStubs: true })

    for (let r = rowIndex; r < rowEndIndex; r++) {
      const row = newFooterRows[r - rowIndex]
      for (let c = 0; c < row.length; c++) {
        const cellAddress = XLSX.utils.encode_cell({ c, r })
        // Set the footer cell style
        ws[cellAddress].s = {
          font: {
            name: 'Microsoft YaHei',
            sz: pt2px(footerCfg.size), color: { rgb: '000000' },
            bold: footerCfg.bold === 'bold', underline: false,
            outline: false, italic: false, strike: false
          },
          alignment: {
            vertical: verticleAlign(footerCfg.verticleAlign),
            horizontal: horizontalAlign(footerCfg.align),
            wrapText: false
          }
        }
        // Line height varies with font size
      }
      ws['!rows'].splice(rowIndex, 0, ({ hpt: footerCfg.size + 10 }))
    }
  }

  // Merge header
  if (rn.tHeader > 1) {
    const columnRows = tableCfg.columnRows
    const _sr = sr.tHeader
    if (tableCfg.index.show) {
      wsMerges.push({ s: { r: _sr, c: 0 }, e: { r: _sr + columnRows.length - 1, c: 0 }})
    }
    columnRows.forEach((row, i) => {
      const r = _sr + i
      let c = 1
      row.forEach(col => {
        const range = { s: { r: r, c: c }, e: { r: r + col.rowSpan - 1, c: c + col.colSpan - 1 }}
        // col.colSpan - 1 + 1
        c += col.colSpan
        wsMerges.push(range)
      })
    })
  }

  ws['!merges'] = wsMerges
}

/**
 * Title information
 * @param {object} config title config
 * @param {object} baseCfg base config
 * @param {object} ws worksheet
 * @param {object} sr starting row
 * @param {object} rn row number
 * @returns
 */
function setTitle({ config, baseCfg, ws, sr, rn }) {
  let _sr = sr.title
  if (!config) {
    return _sr
  }
  if (config.show && config.title) {
    const rows = [[config.title]]
    _sr = sr.title = _sr || 0
    rn.title = 1
    XLSX.utils.sheet_add_aoa(ws, rows, { skipHeader: true, origin: _sr })

    const titleStyle = {
      font: {
        name: '宋体',
        sz: pt2px(config.size), color: { rgb: '000000' },
        bold: config.bold === 'bold', underline: false,
        outline: false, italic: false, strike: false
      },
      alignment: {
        vertical: verticleAlign(config.verticleAlign),
        horizontal: horizontalAlign(config.align),
        wrapText: true
      }
    }

    // 获取单元格地址
    const cellAddress = XLSX.utils.encode_cell({ c: 0, r: _sr })

    // 设置单元格样式 及 行高
    ws[cellAddress].s = titleStyle
    ws['!rows'].push({ hpx: lengthUnit2px(config.height, baseCfg.unit) })

    return ++_sr
  }
  return _sr
}

/**
 * Header extra information
 * @param {object} data header data
 * @param {object} config header config
 * @param {object} headerRows header rows data
 * @param {object} ws worksheet
 * @param {object} sr starting row
 * @param {object} rn row number
 * @returns
 */
function setHeader({ data, config, ws, sr, rn, headerRows }) {
  let _sr = sr.header
  if (!config) {
    return _sr
  }
  const rows = []
  if (config.show && config.fields) {
    const rowMaxW = config.width
    let currentRowW = 0 // The width of the current row is used
    let rowI = -1
    for (const field of config.fields) {
      if (field.show) {
        let _v = ''
        if (isNotBlank(field.title)) {
          _v += field.title
        }
        if (data && field.key) {
          _v += dataFormat({ row: data, field, emptyVal: config.emptyVal })
        }
        let wPlaceholder = 0 // The width of the current field
        // priority: width maxWidth
        if (isNotBlank(field.width)) {
          wPlaceholder += field.width
        } else if (isNotBlank(field.maxWidth)) {
          wPlaceholder += field.maxWidth
        }

        currentRowW += wPlaceholder
        // Whether the width exceeds the current row
        if (currentRowW > rowMaxW || rowI === -1) {
          rowI++ // Beyond, line break
          currentRowW = wPlaceholder // Sets the current row width to the current field width
        }
        if (!rows[rowI]) {
          rows[rowI] = []
        }
        rows[rowI].push(_v)
      }
    }
    if (rowI > -1) {
      _sr = sr.header = _sr || 0
      rn.header = rowI + 1
      Object.assign(headerRows, rows)
      // Adds additional header information to the sheet
      // The header style is set in the method: mergerCells
      XLSX.utils.sheet_add_aoa(ws, rows, { skipHeader: true, origin: _sr })
      // const headerStyle = {
      //   font: {
      //     sz: pt2px(config.size), color: { rgb: '000000' },
      //     bold: config.bold === 'bold', underline: false,
      //     outline: false, italic: false, strike: false
      //   },
      //   alignment: {
      //     vertical: verticleAlign(config.verticleAlign),
      //     horizontal: horizontalAlign(config.align),
      //     wrapText: false
      //   }
      // }
      // // new starting row
      // const _nsr = _sr + rowI + 1
      // for (let r = _sr; r < _nsr; r++) {
      //   const row = rows[r - _sr]
      //   for (let c = 0; c < row.length; c++) {
      //     const cellAddress = XLSX.utils.encode_cell({ c, r })
      //     ws[cellAddress].s = headerStyle
      //     ws['!rows'].push({ hpx: 25 })
      //   }
      // }
      return _sr + rn.header
    }
  }
  return _sr
}

/**
 * 多表头处理
 * @param {object} ws
 * @param {object} config
 * @param {number} sr
 */
function setTHeader(ws, sr, config) {
  const rows = [] // thead
  let { columnRows = [] } = config
  columnRows = JSON.parse(JSON.stringify(columnRows))

  // NO. displayed
  if (config.index.show) {
    columnRows[0].unshift({
      ...config.index, colSpan: 1, rowSpan: columnRows.length
    })
  }

  columnRows.forEach(v => rows.push([]))

  // columnRows.for
  columnRows.forEach((rowCols, i) => {
    const row = rows[i]
    rowCols.forEach(col => {
      row.push(col.title)
      row.push.apply(row, new Array(col.colSpan - 1).fill(''))

      for (let j = 0; j < col.rowSpan - 1; j++) {
        rows[i + 1].push.apply(rows[i + 1], new Array(col.colSpan).fill(''))
      }
    })
  })

  XLSX.utils.sheet_add_aoa(ws, rows, { skipHeader: true, origin: sr.tHeader })
  const scMap = {}
  columnRows.forEach((cr, i) => { scMap[i + sr.tHeader] = 0 })
  columnRows.forEach((rowCols, i) => {
    rowCols.forEach(col => {
      for (let r = 0; r < col.rowSpan; r++) {
        const _r = sr.tHeader + i + r
        for (let c = 0; c < col.colSpan; c++) {
          const _c = scMap[_r] + c
          const cellAddress = XLSX.utils.encode_cell({ c: _c, r: _r })
          ws[cellAddress].s = { // Header style
            font: {
              name: 'Microsoft YaHei',
              sz: pt2px(config.th.size), color: { rgb: '000000' },
              bold: config.th.bold === 'bold', underline: false,
              outline: false, italic: false, strike: false
            },
            alignment: {
              vertical: 'center',
              horizontal: horizontalAlign(col.align),
              wrapText: true
            },
            border: borderStyle
          }
        }
        scMap[_r] += col.colSpan
      }
    })
  })
  sr.tHeader = sr.tHeader || 0
  return columnRows.length
}

/**
 * Table information
 * @param {object} data table data
 * @param {object} config table config
 * @param {object} baseCfg base config
 * @param {object} ws worksheet
 * @param {object} sr starting row
 * @param {object} rn row number
 * @returns
 */
function setTable({ data, config, baseCfg, ws, sr, rn }) {
  let _sr = sr.tHeader
  if (!config) {
    return _sr
  }
  // Headers to set
  if (isNotBlank(config.lastColumns)) {
    let columns = [] // Columns displayed
    const keys = [] // the keys of Columns displayed
    const colNames = [] // the column names of Columns displayed

    // Multiple header process
    rn.tHeader = setTHeader(ws, sr, config)
    _sr = sr.tBody = sr.tHeader + rn.tHeader

    columns = JSON.parse(JSON.stringify(config.lastColumns))
    if (config.index.show) {
      columns.unshift({ ...config.index, key: '__index' })
    }

    // Data is not set when the number of columns is 0
    if (columns.length === 0) {
      return _sr
    }
    columns.forEach(f => {
      if (f.show) {
        keys.push(f.key)
        colNames.push(f.title)
      }
    })
    const needSummary = config.summary && config.summary.show
    const summary = {}
    const summaryKeys = columns.filter(f => {
      summary[f.key] = ''
      return f.sum
    }).map(f => {
      summary[f.key] = 0
      return f.key
    })
    summary['__index'] = config.summary.title || '合计'

    // Data to rewrite and calculated summary data
    const _data = data.map((v, i) => {
      const _v = {}
      columns.forEach(f => {
        if (needSummary && summaryKeys.includes(f.key)) {
          summary[f.key] += keyParse(v, f.key)
        }
        _v[f.key] = dataFormat({ row: v, field: f, emptyVal: config.emptyVal })
      })
      if (config.index.show) {
        _v['__index'] = i + 1
      }
      return _v
    })

    // Summary Data to rewrite
    columns.forEach(f => {
      if (f.sum) {
        // 下载时汇总数据默认显示0
        summary[f.key] = dataFormat({ val: summary[f.key] || 0, field: f, emptyVal: '', kParse: false })
      }
    })

    // Put the summary data into _data
    if (needSummary && config.index.show) _data.push(summary)

    ws['!rows'].push({ hpx: pt2px(config.th.lineHeight) + lengthUnit2px(config.th.paddingTB * 2, baseCfg.unit) }) // line height + padding-top + padding-bottom

    rn.tBody = _data.length

    // Whether there is any remaining width. If there is, add empty column
    const widthRemain = setColumnWidth({ baseCfg, columns: columns, ws })

    // Line increase and set table body json to the sheet
    XLSX.utils.sheet_add_json(ws, _data, { header: widthRemain > 0 ? [...keys, `blankColumn_${Math.random}`] : keys, skipHeader: true, origin: sr.tBody })

    // New starting row(footer starting row)
    const _nsr = sr.tBody + _data.length
    for (let r = sr.tBody; r < _nsr; r++) {
      for (let c = 0; c < columns.length; c++) {
        // Current column
        const column = columns[c]
        const cellAddress = XLSX.utils.encode_cell({ c, r })
        ws[cellAddress].s = {
          font: {
            name: 'Microsoft YaHei',
            sz: pt2px(config.td.size), color: { rgb: '000000' },
            bold: config.td.bold === 'bold', underline: false,
            outline: false, italic: false, strike: false
          },
          alignment: {
            vertical: 'center',
            horizontal: horizontalAlign(column.align),
            wrapText: true
          },
          border: borderStyle
        }
      }
      ws['!rows'].push({ hpx: pt2px(config.td.lineHeight) + lengthUnit2px(config.td.paddingTB * 2, baseCfg.unit) }) // line height + padding-top + padding-bottom
    }

    return _nsr
  }
  return _sr
}

/**
 * Footer information
 * @param {object} data footer data
 * @param {object} config footer config
 * @param {object} baseCfg base config
 * @param {object} footerRows footer rows data
 * @param {object} ws worksheet
 * @param {object} sr starting row
 * @param {object} rn row number
 * @returns
 */
function setFooter({ data, config, baseCfg, ws, sr, rn, footerRows }) {
  const _sr = sr.all = sr.footer
  if (!config || !config.show) {
    return _sr
  }
  const tipCfg = config.tip
  if (tipCfg && tipCfg.show && tipCfg.above) {
    sr.all = setFooterTip({ config: tipCfg, footerCfg: config, baseCfg, ws, sr, rn })
  }

  sr.all = setFooterExtra({ data, config, baseCfg, ws, sr, rn, footerRows })

  if (tipCfg && tipCfg.show && !tipCfg.above) {
    sr.all = setFooterTip({ config: tipCfg, footerCfg: config, baseCfg, ws, sr, rn })
  }
  return sr.all
}

/**
 * Footer tip information
 * @param {object} config footer tip config
 * @param {object} footerCfg footer config
 * @param {object} baseCfg base config
 * @param {object} ws worksheet
 * @param {object} sr starting row
 * @param {object} rn row number
 * @returns
 */
function setFooterTip({ config, footerCfg, baseCfg, ws, sr, rn }) {
  let _sr = sr.all
  if (!config) {
    return _sr
  }
  if (config.show && config.text) {
    const rows = [[config.text]]
    // Prompt to Join the sheet
    _sr = sr.footerTip = _sr || 0
    rn.footerTip = 1
    XLSX.utils.sheet_add_aoa(ws, rows, { skipHeader: true, origin: _sr })

    // Get the cell address
    const cellAddress = XLSX.utils.encode_cell({ c: 0, r: sr.footerTip })
    // Set the cell style and row height
    ws[cellAddress].s = {
      font: {
        name: 'Microsoft YaHei',
        sz: pt2px(config.size), color: { rgb: 'FFFF0000' },
        bold: config.bold === 'bold', underline: false,
        outline: false, italic: false, strike: false
      },
      alignment: {
        vertical: 'center',
        // vertical: verticleAlign(config.verticleAlign),
        horizontal: horizontalAlign(config.align),
        wrapText: true
      }
    }

    let _tipHeight = lengthUnit2px(footerCfg.height, baseCfg.unit)
    if (footerCfg.fields) {
      const rowMaxW = footerCfg.width
      let currentRowW = 0 // The width of the current row is used
      let rowI = -1
      for (const field of footerCfg.fields) {
        if (field.show) {
          let wPlaceholder = 0 // The width of the current field
          // priority: width maxWidth
          if (isNotBlank(field.width)) {
            wPlaceholder += field.width
          } else if (isNotBlank(field.maxWidth)) {
            wPlaceholder += field.maxWidth
          }

          currentRowW += wPlaceholder
          // Whether the width exceeds the current row
          if (currentRowW > rowMaxW || rowI === -1) {
            rowI++ // Beyond, line break
            currentRowW = wPlaceholder // Sets the current row width to the current field width
          }
        }
      }
      if (rowI > -1) {
        _tipHeight -= pt2px(footerCfg.size) * (rowI + 1)
      }
    }
    // TODO: Row height should be configurable
    ws['!rows'].push({ hpx: _tipHeight })
    // ws['!rows'].push({ hpx: lengthUnit2px(config.height + 8, unit) })

    return ++_sr
  }
}

/**
 * Footer information
 * @param {object} data footer data
 * @param {object} config footer config
 * @param {object} baseCfg base config
 * @param {object} footerRows footer rows data
 * @param {object} ws worksheet
 * @param {object} sr starting row
 * @param {object} rn row number
 * @returns
 */
function setFooterExtra({ data, config, ws, sr, rn, footerRows }) {
  let _sr = sr.all
  if (!config) {
    return _sr
  }
  const rows = []
  if (config.show && config.fields) {
    const rowMaxW = config.width
    let currentRowW = 0 // The width of the current row is used
    let rowI = -1
    for (const field of config.fields) {
      if (field.show) {
        let _v = ''
        if (isNotBlank(field.title)) {
          _v += field.title
        }
        if (data && field.key) {
          _v += dataFormat({ row: data, field, emptyVal: config.emptyVal })
        }
        let wPlaceholder = 0 // The width of the current field
        // priority: width maxWidth
        if (isNotBlank(field.width)) {
          wPlaceholder += field.width
        } else if (isNotBlank(field.maxWidth)) {
          wPlaceholder += field.maxWidth
        }

        currentRowW += wPlaceholder
        // Whether the width exceeds the current row
        if (currentRowW > rowMaxW || rowI === -1) {
          rowI++ // Beyond, line break
          currentRowW = wPlaceholder // Sets the current row width to the current field width
        }
        if (!rows[rowI]) {
          rows[rowI] = []
        }
        rows[rowI].push(_v)
      }
    }
    if (rowI > -1) {
      _sr = sr.footerExtra = _sr || 0
      rn.footerExtra = rowI + 1
      Object.assign(footerRows, rows)
      // Adds additional footer information to the sheet
      // The footer style is set in the method: mergerCells
      XLSX.utils.sheet_add_aoa(ws, rows, { skipHeader: true, origin: _sr })
      // const headerStyle = {
      //   font: {
      //     sz: pt2px(config.size), color: { rgb: '000000' },
      //     bold: config.bold === 'bold', underline: false,
      //     outline: false, italic: false, strike: false
      //   },
      //   alignment: {
      //     vertical: verticleAlign(config.verticleAlign),
      //     horizontal: horizontalAlign(config.align),
      //     wrapText: false
      //   }
      // }
      // // new starting row
      // const _nsr = _sr + rowI + 1
      // for (let r = _sr; r < _nsr; r++) {
      //   const row = rows[r - _sr]
      //   for (let c = 0; c < row.length; c++) {
      //     const cellAddress = XLSX.utils.encode_cell({ c, r })
      //     ws[cellAddress].s = headerStyle
      //   }
      //   ws['!rows'].push({ hpx: 25 })
      // }
      return _sr + rn.footerExtra
    }
  }
  return _sr
}

/**
 * set column width
 * @param {object} baseCfg base config
 * @param {object} fields column
 * @param {object} ws worksheet
 * @return {number} widthRemain / The remaining width
 */
function setColumnWidth({ baseCfg, columns, ws }) {
  const _fieldsW = []
  let totalFieldWidth = 0
  let avgWNum = 0
  columns.forEach(col => {
    let fieldWidth = 0
    let distributable = false
    // Priority：1.width 2.minWidth
    if (isNotBlank(col.width)) {
      fieldWidth += col.width
    } else if (isNotBlank(col.minWidth)) {
      fieldWidth += col.minWidth
      distributable = true
      avgWNum++
    }
    totalFieldWidth += fieldWidth

    _fieldsW.push({
      distributable,
      key: col,
      width: fieldWidth
    })
  })

  // Distributable width [need minus border => (fields.length + 1) / 2, Think of tow borders as 1mm]
  const dWidth = baseCfg.aclWidth - totalFieldWidth - convertUnits((columns.length + 1) / 2, 'mm', baseCfg.unit)
  // FIXME: To optimize：column width
  // Average distribution width
  let argDWidth = 0
  if (dWidth > 0 && avgWNum) {
    argDWidth = dWidth / avgWNum
  }
  _fieldsW.forEach(f => {
    let _w = f.width
    if (argDWidth && f.distributable) {
      _w += argDWidth
    }
    // TODO: MDW can control the column width, but don't understand what this number is
    ws['!cols'].push({ wpx: lengthUnit2px(_w, baseCfg.unit), MDW })
  })

  // Whether there is any remaining width. If there is, add empty column
  if (dWidth > 0 && !avgWNum) {
    ws['!cols'].push({ wpx: lengthUnit2px(dWidth, baseCfg.unit), MDW })
    return dWidth
  }
  return 0
}

// ---------------------------------------------------------------------------------------------------------------------------

/**
 * flex垂直对齐方式（align-item）
 * @param {*} align
 */
function verticleAlign(align) {
  switch (align) {
    case verticleAlignEnum.TOP.V: return 'top'
    case verticleAlignEnum.BOTTOM.V: return 'bottom'
    case verticleAlignEnum.CENTER.V: return 'center'
    default: return 'top'
  }
}

/**
 * text对齐方式
 * @param {*} align
 */
function horizontalAlign(align) {
  switch (align) {
    case alignEnum.LEFT.V: return 'left'
    case alignEnum.RIGHT.V: return 'right'
    case alignEnum.CENTER.V: return 'center'
    default: return 'left'
  }
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
  const needParse = !isNotBlank(val) && isNotBlank(row) && isNotBlank(field)
  if (needParse) {
    val = keyParse(row, field.key)
  }
  switch (field.type) {
    case fieldTypeEnum.PROJECT.K: return emptyTextFormatter(projectNameFormat(val, field.format), emptyVal)
    case fieldTypeEnum.DATE.K: return emptyTextFormatter(dateFormat(val, field.format), emptyVal)
    case fieldTypeEnum.DATES.K: return emptyTextFormatter(dateFormat(val, field.format), emptyVal)
    case fieldTypeEnum.AMOUNT.K: return emptyTextFormatter(amountFormat(val, field.format), emptyVal)
    case fieldTypeEnum.WEIGHT.K: return emptyTextFormatter(weightFormat(val, field.format), emptyVal)
    case fieldTypeEnum.LENGTH.K: return emptyTextFormatter(lengthFormat(val, field.format), emptyVal)
    case fieldTypeEnum.RATE.K: return emptyTextFormatter(rateFormat(val, field.format), emptyVal)
    case fieldTypeEnum.THICKNESS.K: return emptyTextFormatter(thicknessFormat(val, field.format), emptyVal)
    case fieldTypeEnum.METE.K: return emptyTextFormatter(meteFormat({
      val, format: field.format, basicClass: row.basicClass,
      materialType: row.materialType, materialListType: row.materialListType,
      unit: row.unit, checkUnit: row.checkUnit
    }), emptyVal)
    case fieldTypeEnum.QUANTITY.K: return emptyTextFormatter(quantityFormat(val, field.format), emptyVal)
    case fieldTypeEnum.ENUM.K: return emptyTextFormatter(enumFormat(val, field.format), emptyVal)
    default: return emptyTextFormatter(val, emptyVal)
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
    if (format.bit) { // 位运算的值
      const enums = enumOperate.toArr(enumK)
      const res = []
      enums.forEach(e => {
        if (e.V & val) {
          res.push(e[key] || e['L'])
        }
      })
      return res.join('/')
    } else {
      const enumV = enumOperate.key2val(enumK)
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
  if (!isNotBlank(format)) {
    // 默认只显示项目简称
    format = { showProjectFullName: false, showContractNo: false, projectNameShowConfig: projectNameArrangementModeEnum.CONTRACT_NO_START.V, lineBreak: true }
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
  const filterDate = (val) => {
    if (isNotBlank(val)) {
      return moment(+val).format(format)
    }
  }
  if (typeof val === 'string') { // 'xx,xx,xx'
    val = val.split(',')
    if (val instanceof Array) {
      if (val.length === 1) {
        return filterDate(val[0])
      }
      if (val.length > 1) {
        return val.map(t => {
          return filterDate(t)
        }).join('，')
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
    if (format.toThousandFilter) {
      _val = toThousandFilter(_val)
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
      _val = convertUnits(_val, minUnit.WEIGHT, format.unit)
    }
    // 小数精度
    if (isNotBlank(format.precision)) {
      _val = (+_val).toFixed(format.precision)
    }
    // 1000 => 1,000
    if (format.toThousandFilter) {
      _val = toThousandFilter(_val)
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
      _val = convertUnits(_val, minUnit.LENGTH, format.unit)
    }
    // 小数精度
    if (isNotBlank(format.precision)) {
      _val = (+_val).toFixed(format.precision)
    }
    // 1000 => 1,000
    if (format.toThousandFilter) {
      _val = toThousandFilter(_val)
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
      _val = convertUnits(_val, minUnit.THICKNESS, format.unit)
    }
    // 小数精度
    if (isNotBlank(format.precision)) {
      _val = (+_val).toFixed(format.precision)
    }
    // 1000 => 1,000
    if (format.toThousandFilter) {
      _val = toThousandFilter(_val)
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
    if (format.toThousandFilter) {
      _val = toThousandFilter(_val)
    }
    // 是否显示单位
    if (format.showUnit) {
      let _unit
      if (isNotBlank(basicClass)) {
        if (checkUnit) {
          _unit = checkUnit
        } else {
          _unit = getBasicClassUnit(basicClass)
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
    if (format.toThousandFilter) {
      _val = toThousandFilter(_val)
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
        return _.isPlainObject(cur) ? cur[key] : void 0
      }, data)
    }
  }
  return
}

export {
  download
}
