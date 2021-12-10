import { printModeEnum as PrintMode } from './enum'
import { getLODOP, printByMode, combineHtml } from './base'
import { projectNameFormatter } from '@/utils/project'

let LODOP

/**
   * 建钢
   * 打印构件
   * @param {object}
   * @param component 构件信息
   * @param productionLineName 生产线名称
   * @param manufacturerName 制造商
   * @param qrCode 构件二维码
   * @param printMode 打印模式
   * @author duhh
   */
async function printArtifact({ component, productionLineName, manufacturerName, qrCode, printMode = PrintMode.QUEUE.V }) {
  const bodyHtml = `
    <table border="1" bordercolor="#000000">
        <tr>
            <td class="col-2" colspan="2">${component.projectName}</td>
            <td>${component.monomerName}</td>
        </tr>
        <tr>
            <td class="col-3" style="font-size: 16pt;font-weight:bold" colspan="3">${component.serialNumber}</td>
        </tr>
        <tr>
            <td>名称： ${component.name}</td>
            <td class="col-2" colspan="2">规格：${component.specification}</td>
        </tr>
        <tr>
            <td>数量：${component.quantity}</td>
            <td>长度(m): ${component.length}</td>
            <td>单重(kg): ${component.weight}</td>
        </tr>
        <tr>
            <td class="col-2" colspan="2">区域： ${component.areaName}</td>
            <td rowspan="3">
                <div class="qr-content"></div>
            </td>
        </tr>
        <tr>
            <td class="col-2" colspan="2">${productionLineName}</td>
        </tr>
        <tr>
            <td class="col-2" colspan="2">${manufacturerName}</td>
        </tr>
    </table>`
  const strHtml = combineHtml(COMPONENT_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 1030, 680, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('2mm', '3mm', '100%', '90%', strHtml)
    LODOP.ADD_PRINT_BARCODE('39.5mm', '71mm', '34mm', '34mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 7)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeErrorLevel', 'M')
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
async function printEnclosure({ component, productionLineName, manufacturerName, qrCode, printMode = PrintMode.QUEUE.V }) {
  const bodyHtml = `
      <table border="1" bordercolor="#000000">
          <tr>
              <td class="col-2" colspan="2">${component.projectName}</td>
              <td>${component.monomerName}</td>
          </tr>
          <tr>
              <td class="col-3" style="font-size: 18pt;font-weight:bold" colspan="3">${component.name}-${component.serialNumber}</td>
          </tr>
          <tr>
              <td>颜色： ${component.color}</td>
              <td class="col-2" colspan="2">版型：${component.plateType}</td>
          </tr>
          <tr>
              <td>厚度(mm): ${component.thickness}</td>
              <td>长度(mm): ${component.length}</td>
              <td>数量：${component.quantity}</td>
          </tr>
          <tr>
              <td class="col-2" colspan="2">区域： ${component.areaName}</td>
              <td rowspan="3">
                  <div class="qr-content"></div>
              </td>
          </tr>
          <tr>
              <td class="col-2" colspan="2">${productionLineName}</td>
          </tr>
          <tr>
              <td class="col-2" colspan="2">${manufacturerName}</td>
          </tr>
      </table>`
  const strHtml = combineHtml(COMPONENT_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 1030, 680, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('2mm', '3mm', '100%', '90%', strHtml)
    LODOP.ADD_PRINT_BARCODE('39.5mm', '71mm', '34mm', '34mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 7)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeErrorLevel', 'M')
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
async function printAuxiliaryMaterial({ component, manufacturerName, qrCode, printMode = PrintMode.QUEUE.V }) {
  const bodyHtml = `
        <table border="1" bordercolor="#000000">
            <tr>
                <td class="col-2" colspan="2">${component.projectName}</td>
                <td>${component.monomerName}</td>
            </tr>
            <tr>
                <td class="col-3" style="font-size: 18pt;font-weight:bold" colspan="3">${component.thirdName}</td>
            </tr>
            <tr>
                <td>编号： ${component.serialNumber}</td>
                <td>颜色：${component.color}</td>
                <td>规格：${component.specification}</td>
            </tr>
            <tr>
              <td>分类： ${component.firstName}</td>
              <td>种类： ${component.secondName}</td>
              <td>单位： ${component.unit}</td>
            </tr>
            <tr>
                <td class="col-2" colspan="2">数量： ${component.quantity}</td>
                <td rowspan="3">
                    <div class="qr-content"></div>
                </td>
            </tr>
            <tr>
              <td class="col-2" colspan="2">区域： ${component.areaName}</td>
            </tr>
            <tr>
            <td class="col-2" colspan="2">${manufacturerName}</td>
          </tr>
      </table>`
  const strHtml = combineHtml(COMPONENT_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 1030, 680, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('2mm', '3mm', '100%', '90%', strHtml)
    LODOP.ADD_PRINT_BARCODE('39.5mm', '71mm', '34mm', '34mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 7)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeErrorLevel', 'M')
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
  // 备注最大长度255
  if (packageInfo.remark && (packageInfo.remark.length > 91)) {
    packageInfo.remark = packageInfo.remark.substr(0, 91) + '...'
  }
  const bodyHtml = `
        <table border="1" bordercolor="#000000">
            <tr>
                <td class="col-3" colspan="3">${packageInfo.projectName}</td>
            </tr>
            <tr>
                <td class="col-3" colspan="3">
                  <span style="font-size: 18pt;font-weight:bold">${packageInfo.serialNumber}</span>
                  <span style="font-size: 10pt;float: right;margin-top: 5pt;">${packageInfo.materialTypeNames}</span>
                </td>
            </tr>
            <tr>
                <td class="col-2" colspan="2">总重(kg): ${packageInfo.totalWeight}</td>
                <td>数量：${packageInfo.quantity}</td>
            </tr>
            <tr>
              <td class="col-2" colspan="2">打包：${packageInfo.packerName} ${packageInfo.createTime}</td>
              <td rowspan="3">
                <div class="qr-content"></div>
              </td>
            </tr>
            <tr>
              <td class="col-2" colspan="2" rowspan="2">备注：${packageInfo.remark}</td>
            </tr>
        </table>`
  const strHtml = combineHtml(PACKAGE_STYLE, bodyHtml)
  let result = false
  try {
    LODOP = await getLODOP()
    LODOP.SET_PRINT_PAGESIZE(1, 1030, 680, '1') /* 纸张大小*/
    LODOP.ADD_PRINT_HTM('1mm', '3mm', '100%', '100%', strHtml)
    LODOP.ADD_PRINT_BARCODE('38.5mm', '71mm', '30mm', '30mm', 'QRCode', qrCode)
    LODOP.SET_PRINT_STYLEA(0, 'QRCodeVersion', 7)
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
 * 物料仓 --型钢打印
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

const COMPONENT_STYLE = `
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
        height: 9mm;
        width: 32mm;
        word-break: break-all;
    }
    table tr td.col-2 {
      width: 64mm;
    }
    table tr td.col-3 {
      width: 96mm;
    }
</style>`

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
    table {
        font-family:'微软雅黑';
        border-collapse:collapse;
        border-spacing: 0;
        text-align: left;
        font-size: 9pt;
        color: black;
    }
    table tr td {
        box-sizing: border-box;
        padding: 0 1mm;
        height: 10.9mm;
        width: 32mm;
        word-break: break-all;
    }
    table tr td.col-2 {
      width: 64mm;
    }
    table tr td.col-3 {
      width: 96mm;
    }
    table .qr-content {
      height: 32mm;
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
  printPackageLabel
}
