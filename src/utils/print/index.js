import { printModeEnum as PrintMode } from './enum'
import { getLODOP, printByMode, combineHtml } from './base'
import { projectNameFormatter } from '@/utils/project'
import { labelTypeEnum, packTypeEnum, componentTypeEnum } from '@enum-ms/mes'
import { getPrintLabelHtml } from '@/utils/label/index'
import { getPrintLabelHtml as getBridgePrintLabelHtml } from '@/utils/bridge-label/index.js'
import { isNotBlank } from '@data-type/index'

let LODOP

/**
   * 桥梁
   * 打印分段,直发件
   * @param {object}
   * @param component 构件信息
   * @param manufacturerName 制造商
   * @param qrCode 构件二维码
   * @param printMode 打印模式
   * @author duhh
   */
async function printBox({ productType, labelType, component, productionLineName, logo, manufacturerPhone, manufacturerURL, manufacturerName, printConfig, qrCode, printMode = PrintMode.QUEUE.V }) {
  const strHtml = getBridgePrintLabelHtml({ productType, labelType, component, productionLineName, logo, manufacturerPhone, manufacturerURL, manufacturerName, printConfig })
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 1030, 2080, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('7mm', '2mm', '98mm', '203mm', strHtml)
    LODOP.ADD_PRINT_BARCODE('15mm', '70mm', '36mm', '36mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 7)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeErrorLevel', 'M')
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
   * 建钢
   * 打印构件
   * @param {object}
   * @param component 构件信息
   * @param manufacturerName 制造商
   * @param qrCode 构件二维码
   * @param printMode 打印模式
   * @author duhh
   */
async function printArtifact({ productType, labelType, component, manufacturerName, printConfig, qrCode, printMode = PrintMode.QUEUE.V }) {
  const strHtml = getPrintLabelHtml({ productType, labelType, component, manufacturerName, printConfig })
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 1030, 680, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('0.5mm', '3mm', '98mm', '68mm', strHtml)
    LODOP.ADD_PRINT_BARCODE('40mm', '72mm', '28mm', '28mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 7)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeErrorLevel', 'M')
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
   * 打印围护
   * @param {object}
   * @param component 构件信息
   * @param productionLineName 生产线名称
   * @param manufacturerName 制造商
   * @param qrCode 构件二维码
   * @param printMode 打印模式
   * @author duhh
   */
async function printEnclosure({ productType, labelType, component, productionLineName, manufacturerName, printConfig, qrCode, printMode = PrintMode.QUEUE.V }) {
  const strHtml = getPrintLabelHtml({ productType, labelType, component, productionLineName, manufacturerName, printConfig })
  console.log(strHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    if (labelType === labelTypeEnum.COMMON.V) {
      LODOP.SET_PRINT_PAGESIZE(1, 1030, 300, '1') /* 纸张大小*/
      LODOP.ADD_PRINT_HTM('2mm', '3mm', '100%', '100%', strHtml)
      LODOP.ADD_PRINT_BARCODE('4mm', '3mm', '25mm', '25mm', 'QRCode', qrCode)
    }
    if (labelType === labelTypeEnum.CUSTOM.V) {
      LODOP.SET_PRINT_PAGESIZE(1, 1030, 500, '1') /* 纸张大小*/
      LODOP.ADD_PRINT_HTM('2mm', '3mm', '100%', '100%', strHtml)
      LODOP.ADD_PRINT_BARCODE('38mm', '130mm', '25mm', '25mm', 'QRCode', qrCode)
    }
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 7)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeErrorLevel', 'M')
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
   * 打印辅材
   * @param {object}
   * @param component 箱体、构件信息
   * @param manufacturerName 制造商
   * @param qrCode 箱体、构件二维码
   * @param printMode 打印模式
   * @author duhh
   */
async function printAuxiliaryMaterial({ productType, labelType, component, manufacturerName, qrCode, printConfig, printMode = PrintMode.QUEUE.V }) {
  const strHtml = getPrintLabelHtml({ productType, labelType, component, manufacturerName, printConfig })
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 1030, 680, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('2mm', '3mm', '98mm', '68mm', strHtml)
    LODOP.ADD_PRINT_BARCODE('32mm', '72mm', '28mm', '28mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 7)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeErrorLevel', 'M')
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
   * 打印桥梁辅材
   * @param {object}
   * @param component 箱体、构件信息
   * @param manufacturerName 制造商
   * @param qrCode 箱体、构件二维码
   * @param printMode 打印模式
   * @author duhh
   */
async function printBridgeAuxiliaryMaterial({ productType, labelType, component, manufacturerName, qrCode, printConfig, printMode = PrintMode.QUEUE.V }) {
  const strHtml = getBridgePrintLabelHtml({ productType, labelType, component, manufacturerName, printConfig })
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 1030, 680, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('2mm', '3mm', '98mm', '68mm', strHtml)
    LODOP.ADD_PRINT_BARCODE('32mm', '72mm', '28mm', '28mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 7)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeErrorLevel', 'M')
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
   * 建钢、桥梁：打包清单
   * @param {object}
   * @param packageInfo 打包信息
   * @param qrCode 清单二维码
   * @param printMode 打印模式
   * @author duhh
   */
async function printPackageLabel({ packageInfo, qrCode, printMode = PrintMode.QUEUE.V }) {
  const pageHtml = `<div style="text-align:center;"><span tdata='pageNO'>##</span> / <span tdata='pageCount'>##</span></div>`
  let headHtml = ''
  // const theadHtml = {
  //   [packTypeEnum.STRUCTURE.V]: `<div class="flex">
  //       <div class="row-0 w-1 col border-r">编号</div>
  //       <div class="row-0 w-1 col border-r">材质</div>
  //       <div class="row-0 w-1 col border-r">数量</div>
  //       <div class="row-0 w-1 col">重量(kg)</div>
  //       </div>`,
  //   // [packTypeEnum.ENCLOSURE.V]: `<div class="flex">
  //   //     <div class="row-0 w-1 col border-r">编号</div>
  //   //     <div class="row-0 w-1 col border-r">板型</div>
  //   //     <div class="row-0 w-1 col border-r">长度</div>
  //   //     <div class="row-0 w-1 col">数量</div>
  //   //   </div>`,
  //   [packTypeEnum.AUXILIARY_MATERIAL.V]: ''
  // }
  headHtml = `<div style="font-weight: bold; font-size: 12pt;color: #333;padding-bottom: 2pt;">${packageInfo.project.shortName}</div>
    <div class="package-label">
      <div class="row border-b">
        <div class="row row-2 w-1 col border-r">
          <div class="qr-content"></div>
        </div>
        <div class="row w-3 row-1 col border-b" style="font-weight:bold;font-size: 10pt;">打包单</div>
        <div class="row w-3">
          <div style="width:33.4%;" class="row-1 col border-r">包单号</div>
          <div style="width:66%;" class="row-1 col" style="font-weight:bold">${packageInfo.serialNumber}</div>
        </div>
      </div>
`

  const structureHtml = `
  <div class="row">
  <div class="row-0 col border-r" style="width:34%;">
  <div class="col-div">编号</div>
  </div>
  <div class="row-0 w-1 col border-r">
  <div class="col-div">材质</div>
  </div>
  <div class="row-0 col border-r" style="width:14%;">
  <div class="col-div">数量</div>
  </div>
  <div class="row-0 w-1 col">
  <div class="col-div">重量(kg)</div>
  </div>
</div>
</div>`
  const enclosureHtml = `
<div class="row">
<div class="row-0 col border-r" style="width:25%;">
<div class="col-div">编号</div>
</div>
<div class="row-0 col border-r" style="width: 24%;">
<div class="col-div">板型</div>
</div>
<div class="row-0 col border-r" style="width:14%;">
<div class="col-div">单长</div>
</div>
<div class="row-0 col border-r" style="width: 23%">
<div class="col-div">单面积</div>
</div>
<div class="row-0 col" style="width: 12%">
<div class="col-div">数量</div>
</div>
</div>
</div>
`
  const auxHtml = `
  <div class="row">
  <div class="row-0 col border-r" style="width:26%;">
  <div class="col-div">名称</div>
  </div>
  <div class="row-0 col border-r" style="width:14%;">
  <div class="col-div">单位</div>
  </div>
  <div class="row-0 col border-r" style="width:35%;">
  <div class="col-div">规格</div>
  </div>
  <div class="row-0 col" style="width:23%;">
  <div class="col-div">${packageInfo.unitValue === 1 ? '核算量' : '数量'}</div>
  </div>
</div>
</div>`
  if (packageInfo.productType === packTypeEnum.STRUCTURE.V || packageInfo.productType === packTypeEnum.MACHINE_PART.V || packageInfo.productType === componentTypeEnum.MACHINE_PART.V || packageInfo.productType === componentTypeEnum.ASSEMBLE.V) {
    headHtml += structureHtml
  } else if (packageInfo.productType === packTypeEnum.ENCLOSURE.V) {
    headHtml += enclosureHtml
  } else if (packageInfo.productType === packTypeEnum.AUXILIARY_MATERIAL.V) {
    headHtml += auxHtml
  }
  let bodyHtml = '<div class="package-label" style="border-top:none;border-bottom:none;">'
  // const tbodyHtml = {
  //   [packTypeEnum.STRUCTURE.V]: function (item) {
  //     return `
  //       <div class="flex">
  //         <div class="row-0 w-1 col border-b border-r">${item.serialNumber}</div>
  //         <div class="row-0 w-1 col border-b border-r">${item.material}</div>
  //         <div class="row-0 w-1 col border-b border-r">${item.quantity}</div>
  //         <div class="row-0 w-1 col border-b">${item.totalWeight}</div>
  //       </div>
  //     `
  //   },
  //   // [packTypeEnum.ENCLOSURE.V]: function (item) {
  //   //   return `
  //   //     <div class="flex">
  //   //       <div class="row-0 w-1 col border-b border-r">${item.serialNumber}</div>
  //   //       <div class="row-0 w-1 col border-b border-r">${item.plate}</div>
  //   //       <div class="row-0 w-1 col border-b border-r">${item.length}</div>
  //   //       <div class="row-0 w-1 col border-b">${item.quantity}</div>
  //   //     </div>
  //   //   `
  //   // },
  //   [packTypeEnum.AUXILIARY_MATERIAL.V]: ''
  // }
  for (let x = 0; x < packageInfo.list.length; x++) {
    const item = packageInfo.list[x]
    if (packageInfo.productType === packTypeEnum.STRUCTURE.V || packageInfo.productType === packTypeEnum.MACHINE_PART.V || packageInfo.productType === componentTypeEnum.MACHINE_PART.V || packageInfo.productType === componentTypeEnum.ASSEMBLE.V) {
      bodyHtml += `
      <div class="row border-b">
      <div class="row-0 col border-r" style="width:34%;">
      <div class="col-div">${item.serialNumber}</div>
      </div>
      <div class="row-0 col w-1 border-r">
      <div class="col-div">${item.material}</div>
      </div>
      <div class="row-0 col border-r" style="width:14%;">
      <div class="col-div">${item.quantity}</div>
      </div>
      <div class="row-0 col w-1">
      <div class="col-div">${item.totalWeight}</div>
      </div>
    </div>
    `
    } else if (packageInfo.productType === packTypeEnum.ENCLOSURE.V) {
      bodyHtml += `
      <div class="row border-b">
      <div class="row-0 col border-r" style="width:25%;">
      <div class="col-div">${item.serialNumber}</div>
      </div>
      <div class="row-0 col border-r" style="width: 24%;">
      <div class="col-div">${item.plate}</div>
      </div>
      <div class="row-0 col border-r" style="width:14%;">
      <div class="col-div">${item.length}</div>
      </div>
      <div class="row-0 col border-r" style="width: 23%">
      <div class="col-div">${item.surfaceArea}</div>
      </div>
      <div class="row-0 col" style="width: 12%">
      <div class="col-div">${item.quantity}</div>
      </div>
    </div>
    `
    } else if (packageInfo.productType === packTypeEnum.AUXILIARY_MATERIAL.V) {
      bodyHtml += `
      <div class="row border-b">
      <div class="row-0 col border-r" style="width:26%;">
      <div class="col-div">${item.name}</div>
      </div>
      <div class="row-0 col border-r" style="width:14%;">
      <div class="col-div">${packageInfo.unitValue === 1 ? item.accountingUnit : item.measureUnit}</div>
      </div>
      <div class="row-0 col border-r" style="width:35%;">
      <div class="col-div">${item.specification}</div>
      </div>
      <div class="row-0 col" style="width:23%;">
      <div class="col-div">${packageInfo.unitValue === 1 ? item.packageMete : item.quantity}</div>
      </div>
    </div>
    `
    }
  }
  bodyHtml += '</div>'
  const strHtml = combineHtml(PACKAGE_STYLE, bodyHtml)
  const headStrHtml = combineHtml(PACKAGE_STYLE, headHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(2, 1030, 680, '1') /* 纸张大小*/ // 100mm* 75mm
    LODOP.ADD_PRINT_HTM('3mm', '1.5mm', '67mm', '29.5mm', headStrHtml)
    LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1)
    LODOP.ADD_PRINT_HTM('31.7mm', '1.5mm', '67mm', '61.5mm', strHtml)
    LODOP.ADD_PRINT_HTM('96mm', '1.5mm', '67mm', '5mm', pageHtml)
    LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1)
    LODOP.ADD_PRINT_BARCODE('8.8mm', '1.8mm', '16.4mm', '16.4mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 3)
    LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1)
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
   * 建钢：零件工单-分拣单
   * @param {object}
   * @param separateOrderInfo 分拣单信息
   * @param productionLineList 生产线信息
   * @param printMode 打印模式
   * @author duhh
   */
export async function printSeparateOrderLabel({ taskNumberOrder = '', separateOrderInfo, printMode = PrintMode.QUEUE.V } = {}) {
  const marginNum = '5mm'
  const pageHtml = `<div style="text-align:center;"><span tdata='pageNO'>##</span> / <span tdata='pageCount'>##</span></div>`
  const headHtml = `
  <div style="font-size: 17pt; font-weight: bold; height: 10mm; text-align:center;">零件分拣单</div>
  <div style="font-size: 12pt; padding:0 ${marginNum};margin-top: 3mm;margin-bottom: 1mm;">任务单：${taskNumberOrder}</div>
  `
  let listHtml = ``
  const piWidth = '25mm' // 零件信息宽度
  const imgHeight = '20mm'// 零件图片-高度
  for (let o = 0; o < separateOrderInfo.length; o++) {
    const s = separateOrderInfo[o]

    // 产线信息
    let pHeadHtml = ``
    let pConHtml = ``

    for (let i = 0; i < s.productionLineList?.length; i++) {
      const p = s.productionLineList[i]
      pHeadHtml += `
      <td>
        ${isNotBlank(p) ? `<span>${p.productionLineName}>${p.groupName}</span>` : '<span>\\</span>'}
      </td>
      `
      pConHtml += `
      <td>
        ${isNotBlank(p) ? `<span>${p.quantity}</span>` : '<span>\\</span>'}
      </td>
      `
    }
    listHtml += `
        <tr class="separate-thead">
          <td style="width: ${piWidth}">${s.serialNumber}</td>
          ${pHeadHtml}
        </tr>
        <tr>
          <td class="separate-td-img">
            ${s.picturePath ? `<img style="width: 95%;height: 95%;vertical-align: middle;" src='${s.picturePath}' />` : '<span>\\</span>'}
          </td>
          ${pConHtml}
        </tr>
     
    `
  }

  const bodyHtml = `<table class="separate-table" cellspacing="0" cellpadding="0" border="1">
        <tbody>
          ${listHtml}  
        </tbody>
      </table>`

  const separate_style = `
  <style>
    .separate-table {
      width: 100%;
      table-layout: fixed;
      border-collapse: collapse;  
    }
    .separate-table td {
      text-align: center;
    }
    .separate-table .separate-thead {
      background-color: #d9d9d9;
    }
    .separate-table .separate-thead td {
      font-size: 10pt;
    }
    .separate-table .separate-td-img {
      height: ${imgHeight};
      text-align: center;
    }
  </style>`
  const strHtml = combineHtml(separate_style, bodyHtml)
  let result = false
  try {
    const headHeight = 25
    const bodyHeight = 255
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(3, 0, 0, 'A4') /* 纸张大小*/ // 100mm* 75mm
    LODOP.ADD_PRINT_HTM('5mm', '0mm', '100%', `${headHeight}mm`, headHtml)
    LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1)
    LODOP.ADD_PRINT_TABLE(`${headHeight}mm`, '5mm', '200mm', `${bodyHeight}mm`, strHtml)
    LODOP.ADD_PRINT_HTM(`${headHeight + bodyHeight + 5}mm`, '0', '100%', '5mm', pageHtml)
    LODOP.SET_PRINT_STYLEA(0, 'ItemType', 1)
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * 物料仓 --钢板打印
 */
export async function printSteelPlateLabel({ secondClassName, projectName, thirdClassName, specification, qrCode, number = 1, printMode = PrintMode.QUEUE.V }) {
  const bodyHtml =
    `<table cellspacing="0" cellpadding="0" border="0" frame="void">
      <tbody>
        <tr>
            <td style="padding: 0 4pt;;box-sizing:border-box;height: 5.5mm;width: 68mm;font-size: 10pt;" overflow: hidden; text-overflow: ellipsis;white-space: nowrap;>[${secondClassName}/${thirdClassName}]${specification}</td>
            <td style="padding: 0 2pt;;box-sizing:border-box;height: 6.5mm;width: 16mm;" rowspan="2">
            </td>
        </tr>
        <tr>
           <td style="padding: 0 4pt;;box-sizing:border-box;height: 4.2mm;width: 68mm;overflow: hidden; text-overflow: ellipsis;white-space: nowrap;">${projectName}</td>
        </tr>
      </tbody>
    </table>`
  const strHtml = combineHtml(LABEL_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 800, 120, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('1.1mm', '3mm', '100%', '100%', strHtml)
    LODOP.ADD_PRINT_BARCODE('1.27mm', '68mm', '14mm', '14mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 5)
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * 物料仓 --型材打印
 */
export async function printSectionSteelLabel({ secondClassName, projectName, thirdClassName, specification, brand, qrCode, number = 1, printMode = PrintMode.QUEUE.V }) {
  const bodyHtml =
    `<table cellspacing="0" cellpadding="0" border="0" frame="void">
    <tbody>
      <tr>
          <td style="padding: 0 4pt;;box-sizing:border-box;height: 5.5mm;width: 68mm;font-size: 10pt;overflow: hidden; text-overflow: ellipsis;white-space: nowrap;">[${secondClassName}/${thirdClassName}]${specification}</td>
          <td style="padding: 0 2pt;;box-sizing:border-box;height: 6.5mm;width: 16mm;" rowspan="2">
          </td>
      </tr>
      <tr>
         <td style="padding: 0 4pt;;box-sizing:border-box;height: 4.2mm;width: 68mm;overflow: hidden; text-overflow: ellipsis;white-space: nowrap;">${projectName}</td>
      </tr>
    </tbody>
  </table>`
  const strHtml = combineHtml(LABEL_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 800, 120, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('1.1mm', '3mm', '100%', '100%', strHtml)
    LODOP.ADD_PRINT_BARCODE('1.27mm', '68mm', '14mm', '14mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 5)
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * 物料仓 --钢卷打印
 */
export async function printSteelCoilLabel({ secondClassName, projectName, thirdClassName, specification, color, brand, qrCode, number = 1, printMode = PrintMode.QUEUE.V }) {
  const bodyHtml =
    `<table cellspacing="0" cellpadding="0" border="0" frame="void">
    <tbody>
      <tr>
          <td style="padding: 0 4pt;;box-sizing:border-box;height: 5.5mm;width: 68mm;font-size: 10pt;overflow: hidden; text-overflow: ellipsis;white-space: nowrap;">[${secondClassName}/${thirdClassName}]${specification}</td>
          <td style="padding: 0 2pt;;box-sizing:border-box;height: 6.5mm;width: 16mm;" rowspan="2">
          </td>
      </tr>
      <tr>
         <td style="padding: 0 4pt;;box-sizing:border-box;height: 4.2mm;width: 68mm;overflow: hidden; text-overflow: ellipsis;white-space: nowrap;">${projectName}</td>
      </tr>
    </tbody>
  </table>`
  const strHtml = combineHtml(LABEL_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 800, 120, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('1.1mm', '3mm', '100%', '100%', strHtml)
    LODOP.ADD_PRINT_BARCODE('1.27mm', '68mm', '14mm', '14mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 5)
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * 物料仓 -- 成品围护
 */
export async function printWarehouseEnclosure({ projectName, thirdClassName, specification, qrCode, number = 1, color, printMode = PrintMode.QUEUE.V }) {
  const bodyHtml =
    `<table cellspacing="0" cellpadding="0" border="0" frame="void">
      <tbody>
        <tr>
            <td style="padding: 0 4pt;;box-sizing:border-box;height: 5.5mm;width: 68mm;font-size: 10pt;overflow: hidden; text-overflow: ellipsis;white-space: nowrap;">[${thirdClassName}]${specification} ${color}</td>
            <td style="padding: 0 2pt;;box-sizing:border-box;height: 6.5mm;width: 16mm;" rowspan="2">
            </td>
        </tr>
        <tr>
           <td style="padding: 0 4pt;;box-sizing:border-box;height: 4.2mm;width: 68mm;overflow: hidden; text-overflow: ellipsis;white-space: nowrap;">${projectName}</td>
        </tr>
      </tbody>
    </table>`
  const strHtml = combineHtml(LABEL_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 800, 120, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('1.1mm', '3mm', '100%', '100%', strHtml)
    LODOP.ADD_PRINT_BARCODE('1.27mm', '68mm', '14mm', '14mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 5)
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * 物料仓 --辅材打印
 */
export async function printWarehouseMaterial({ secondClassName, projectName, thirdClassName, specification, qrCode, number = 1, color, printMode = PrintMode.QUEUE.V }) {
  const bodyHtml =
    `<table cellspacing="0" cellpadding="0" border="0" frame="void">
      <tbody>
        <tr>
            <td style="padding: 0 4pt;;box-sizing:border-box;height: 5.5mm;width: 68mm;font-size: 10pt;overflow: hidden; text-overflow: ellipsis;white-space: nowrap;">[${secondClassName}/${thirdClassName}]${specification} ${color}</td>
            <td style="padding: 0 2pt;;box-sizing:border-box;height: 6.5mm;width: 16mm;" rowspan="2">
            </td>
        </tr>
        <tr>
           <td style="padding: 0 4pt;;box-sizing:border-box;height: 4.2mm;width: 68mm;overflow: hidden; text-overflow: ellipsis;white-space: nowrap;">${projectName}</td>
        </tr>
      </tbody>
    </table>`
  const strHtml = combineHtml(LABEL_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 800, 120, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('1.1mm', '3mm', '100%', '100%', strHtml)
    LODOP.ADD_PRINT_BARCODE('1.27mm', '68mm', '14mm', '14mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 5)
    // LODOP.PRINT_DESIGN()/* 打印设计*/
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * 气体打印
 */
export async function printGasLabel({ firstName, secondName, thirdName, unit, weight, companyName, qrCode, number, printMode = PrintMode.NORMAL.V }) {
  const bodyHtml = `
        <table border="1" bordercolor="#000000">
            <tr>
                <td style="font-size: 18pt;font-weight:bold;text-align:center;" colspan="2">${firstName}-${secondName}-${thirdName}</td>
            </tr>
            <tr>
                <td>单位： ${unit}</td>
                <td>重量： ${weight}</td>
            </tr>
            <tr>
                <td style="height:32mm;">${companyName}</td>
                <td >
                    <div class="qr-content"></div>
                </td>
            </tr>
        </table>`
  const strHtml = combineHtml(GAS_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 1030, 680, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('8mm', '3mm', '100%', '100%', strHtml)
    LODOP.SET_PRINT_COPIES(number)
    LODOP.ADD_PRINT_BARCODE('29mm', '60mm', '40mm', '40mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 10)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeErrorLevel', 'L')
    // LODOP.PREVIEW()/* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * 打印出库单
 * @param {string} date 出库日期
 * @param {string} handler 办理人
 * @param {string} orderNo 出库编号
 * @param {array} list 出库单列表
 * @param {number} number 打印数量
 */
export async function printDetailOutboundOrder({ date, handler, orderNo, list, number = 1, printMode = PrintMode.QUEUE.V }) {
  if (!list || !list.length) return
  let bodyHtml = `
    <div class="list-content">
      <span class="list-header">出库（领料）单</span>
      <div class="list-other-title">
        <span>出库日期：${date}</span>
        <span>出库办理人：${handler}</span>
        <span>NO：${orderNo}</span>
      </div>
      <table border="1">
        <tbody>
          <tr>
            <th style="width: 5%;" scope="row">序号</th>
            <th style="width: 20%;" scope="row">物料种类</th>
            <th style="width: 12%;" scope="row">规格</th>
            <th style="width: 5%;" scope="row">单位</th>
            <th style="width: 5%;" scope="row">出库量</th>
            <th scope="row">所属项目</th>
            <th style="width: 10%;" scope="row">领用人</th>
          </tr>`
  for (const i in list) {
    const project = projectNameFormatter(list[i].project)
    bodyHtml += `
          <tr>
            <td>${Number(i) + 1}</td>
            <td>${list[i].firstClassName}/${list[i].secondClassName}/${list[i].thirdClassName}</td>
            <td>${list[i].specification}</td>
            <td>${list[i].unit}</td>
            <td>${list[i].value}</td>
            <td>${project}</td>
            <td>${list[i].receiverUserName}</td>
          </tr>`
  }
  bodyHtml += `
        </tbody>
      </table>
      <div class="list-footer">
        <span>领料人（签字）：</span>
        <span>审核人（签字）：</span>
      </div>
    </div>`
  const strHtml = combineHtml(OUTBOUND_LIST_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(3, 0, 0, 'A4')
    LODOP.ADD_PRINT_HTM(0, 0, '100%', '100%', strHtml)
    LODOP.SET_PRINT_COPIES(number)
    LODOP.SET_PRINT_MODE('PRINT_PAGE_PERCENT', 'Full-Width')
    // LODOP.PREVIEW()
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

const GAS_STYLE = `
<style>
    table {
        font-family:'微软雅黑';
        border-collapse:collapse;
        text-align: left;
        font-size: 9pt;
        color: black;
    }
    table tr td {
        box-sizing: border-box;
        padding: 0 1mm;
        height: 10mm;
        width: 48mm;
        word-break: break-all;
    }
</style>`

const PACKAGE_STYLE = `
  <style>
  .package-label {
    float: left;
    font-family: "微软雅黑";
    font-size: 9pt;
    color: black;
    width: 100%;
    border: 1px solid #000;
  }

  .row {
    width: 100%;
    float: left;
  }

  .w-1 {
    width: 25%;
  }

  .w-2 {
    width: 50%;
  }

  .w-3 {
    width: 74.5%;
  }

  .row-0 {
    height: 5.85mm;
    position: relative;
  }

  .col-div {
    width: 100%;
    text-align: center;
    position: absolute;
    left: 0;
    top: 50%;
    margin: -0.5em 0 0 0;
  }

  .row-1 {
    height: 7.9mm;
    line-height: 7mm;
  }

  .row-2 {
    height: 16mm;
    line-height: 16mm;
  }

  .border-r {
    border-right: 1px solid #000;
  }

  .border-t {
    border-top: 1px solid #000;
  }

  .border-b {
    border-bottom: 1px solid #000;
  }

  .col {
    float: left;
    word-break: break-all;
    text-align: center;
  }
</style>`

const OUTBOUND_LIST_STYLE = `
<style>
  .list-content {
    width:210mm;
    padding: 5mm;
    box-sizing: border-box;
    margin:0 auto;
    font-size: 11pt;
  }
  .list-header {
    display: inline-block;
    font-size: 16pt;
    margin-bottom: 3mm;
    width: 100%;
    text-align: center;
  }
  .list-project-name {
    display: inline-block;
    width: 100%;
    text-align: left;
    margin-bottom: 3mm;
  }
  .list-other-title {
    width: 100%;
  }
  .list-other-title >span {
    display: inline-block;
    width: 30%;
    text-align: left;
  }
  .list-footer >span{
    display: inline-block;
    width: 40%;
    text-align: left;
  }
  table {
    margin:  3mm 0;
    width: calc(100% - 5pt);
    border-collapse: collapse;
    font-size: 8pt;
  }
  table th{
    padding:5pt 0;
    text-align: center;
    vertical-align: middle;
  }
  table td{
    padding:5pt 0;
    text-align: center;
    vertical-align: middle;
  }
</style>`

const LABEL_STYLE = `
<style type="text/css">
  table {
    border-collapse: collapse;
    width:100%;
    font-family:'微软雅黑';
    border-collapse:collapse;
    table-layout:fixed;
    text-align: left;
    vertical-align:middle;
    font-size: 8pt;
    font-weight:bold;
    color: black;
  }
  // table td {
  //   border: solid 0.5pt black;
  // }
</style>
`

export {
  printArtifact,
  printEnclosure,
  printAuxiliaryMaterial,
  printPackageLabel,
  printBox,
  printBridgeAuxiliaryMaterial
}
