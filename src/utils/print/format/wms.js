import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

function preparesCustomSummary({ header, table = [], footer, qrCode }) {
  const _table = table.map((row) => {
    row = numFmtByBasicClass({
      data: row,
      basicClass: row.basicClass,
      precision: 5,
      field: ['value'],
      unit: ['unit'],
      returnNewObj: true,
      toNum: true
    })
    return row
  })
  return {
    header,
    table: _table,
    footer,
    qrCode
  }
}

// 普通数据格式化转换
async function dataFormat({ header, table = [], footer, qrCode }) {
  await setSpecInfoToList(table)
  await numFmtByBasicClass(table, undefined, {
    mete: ['mete', 'rejectMete'],
    quantity: ['quantity', 'rejectQuantity']
  })
  return {
    header,
    table,
    footer,
    qrCode
  }
}

// 调拨数据格式转化
async function transferDataFormat({ header = {}, table = [], footer, qrCode }) {
  const cloneHeader = JSON.parse(JSON.stringify(header))

  // 调拨来源数据转换
  const source = cloneHeader.source
  let sourceStr = ''
  let sourceStrArr = []
  if (source && source.length > 0) {
    source.forEach((sInfo, sIndex) => {
      let str = ''
      if (sInfo && sInfo.project) {
        str += sInfo.project.shortName
      } else {
        str += '公共库'
      }
      if (sInfo.factory) {
        str += `（${sInfo.factory.name}）`
      }
      sourceStrArr.push(str)
    })
    sourceStrArr = Array.from(new Set(sourceStrArr))
    sourceStr = sourceStrArr.join('　/　')
  }
  if (cloneHeader.boolBorrowReturnNotSelf && cloneHeader.borrowProject) {
    sourceStr += '　▶　'
    sourceStr += cloneHeader.borrowProject.shortName
  }
  cloneHeader.source = sourceStr

  // 调拨目的数据转换
  const direction = cloneHeader.direction
  let directionStr = ''
  if (direction) {
    if (direction.project) {
      directionStr += direction.project.shortName
    } else {
      directionStr += '公共库'
    }
    // 工厂-仓库位置
    let fwStr = ''
    if (direction.factory) {
      fwStr += `${direction.factory.name} - `
    }
    if (direction.warehouse) {
      fwStr += `${direction.warehouse.name}`
    }
    directionStr += `（${fwStr}）`
  }
  cloneHeader.direction = directionStr

  await setSpecInfoToList(table)
  await numFmtByBasicClass(table)
  return {
    header: cloneHeader,
    table,
    footer,
    qrCode
  }
}

function checkUnitFormat({ header, table = [], footer, qrCode }) {
  const _table = table.map((row) => {
    row = numFmtByBasicClass({
      data: row,
      basicClass: row.basicClass,
      field: ['value'],
      returnNewObj: true,
      toNum: true
    })
    // row.checkUnit = getBasicClassUnit(row.basicClass, false)
    return row
  })
  return {
    header,
    table: _table,
    footer,
    qrCode
  }
}

export default {
  preparesCustomSummary,
  checkUnitFormat,
  dataFormat,
  transferDataFormat
}
