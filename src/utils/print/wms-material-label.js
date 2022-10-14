/* eslint-disable no-irregular-whitespace */
import { printModeEnum as PrintMode } from './enum'
import { getLODOP, printByMode, combineHtml } from './base'
import { spliceMaterialSpec, spliceSteelSize } from '@/utils/wms/spec-format'
import { rawMatClsEnum } from '../enum/modules/classification'
import { isNotBlank } from '../data-type'
// import { projectNameFormatter } from '@/utils/project'
// import { packTypeEnum, labelTypeEnum } from '@enum-ms/mes'
// import { getPrintLabelHtml } from '@/utils/label/index'

let LODOP
/**
 * 1---纵(正)向打印，固定纸张；
 * 2---横向打印，固定纸张；
 * 3---纵(正)向打印，宽度固定，高度按打印内容的高度自适应；
 * 0(或其它)----打印方向由操作者自行选择或按打印机缺省设置；
 */
const intOrient = 1
const pageWidth = 1000
const pageHeight = 120

/**
 * 物料仓 - 标签打印
 */
export async function printMaterialLabel({ material, copies = 1, printMode = PrintMode.QUEUE.V } = {}) {
  switch (material.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return await printSteelPlateLabel({ material, copies, printMode })
    case rawMatClsEnum.SECTION_STEEL.V:
      return await printSectionSteelLabel({ material, copies, printMode })
    case rawMatClsEnum.STEEL_COIL.V:
      return await printSteelCoilLabel({ material, copies, printMode })
    default:
      return false
  }
}

/**
 * 物料仓 - 钢板标签打印
 */
export async function printSteelPlateLabel({ material, copies = 1, printMode = PrintMode.QUEUE.V } = {}) {
  let result = false

  try {
    // 首行：科目名 + 材质 等信息
    let firstLineStr = `名称: ${material.classifyName}`
    firstLineStr = `${firstLineStr}　${spliceMaterialSpec(material)}`
    // 第二行：钢材尺寸
    const secondLineStr = `规格: ${spliceSteelSize(material)}`
    // 拼接主体信息
    const contentHtml = `
    <div class="material-info">
      <div class="first-line ellipsis-text">
        ${firstLineStr}
      </div>
      <div class="second-line ellipsis-text">
        ${secondLineStr}
      </div>
    </div>
    `
    // 拼接css及页面信息其他html
    const strHtml = combineHtml(MATERIAL_LABEL_STYLE, contentHtml)

    LODOP = await getLODOP()
    // 设置打印纸张大小
    LODOP.SET_PRINT_PAGESIZE(intOrient, pageWidth, pageHeight, '1')
    LODOP.ADD_PRINT_HTM(0, 0, '100%', '100%', strHtml)
    LODOP.ADD_PRINT_BARCODE('1mm', '72mm', '28mm', '10mm', '128C', material.barcode)
    LODOP.SET_PRINT_COPIES(copies)
    // LODOP.PRINT_DESIGN() /* 打印设计*/
    // LODOP.PREVIEW() /* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * 物料仓 - 型材标签打印
 */
export async function printSectionSteelLabel({ material, copies = 1, printMode = PrintMode.QUEUE.V } = {}) {
  let result = false

  try {
    // 首行：科目名 + 材质 等信息
    let firstLineStr = `名称: ${material.classifyName}`
    firstLineStr = `${firstLineStr}　${spliceMaterialSpec(material)}`
    // 第二行：钢材尺寸
    const secondLineStr = `长度: ${material.length}　规格: ${spliceSteelSize(material)}`
    // 拼接主体信息
    const contentHtml = `
    <div class="material-info">
      <div class="first-line ellipsis-text">
        ${firstLineStr}
      </div>
      <div class="second-line ellipsis-text">
        ${secondLineStr}
      </div>
    </div>
    `
    // 拼接css及页面信息其他html
    const strHtml = combineHtml(MATERIAL_LABEL_STYLE, contentHtml)

    LODOP = await getLODOP()
    // 设置打印纸张大小
    LODOP.SET_PRINT_PAGESIZE(intOrient, pageWidth, pageHeight, '1')
    LODOP.ADD_PRINT_HTM(0, 0, '100%', '100%', strHtml)
    LODOP.ADD_PRINT_BARCODE('1mm', '72mm', '28mm', '10mm', '128C', material.barcode)
    LODOP.SET_PRINT_COPIES(copies)
    // LODOP.PRINT_DESIGN() /* 打印设计*/
    // LODOP.PREVIEW() /* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * 物料仓 - 钢卷标签打印
 */
export async function printSteelCoilLabel({ material, copies = 1, printMode = PrintMode.QUEUE.V, emptyText = '-' } = {}) {
  let result = false

  try {
    // 首行：科目名 + 材质 等信息
    const firstLineStr = `名称: ${material.classifyName}　品牌：${isNotBlank(material.brand) ? material.brand : emptyText}`
    // 第二行：钢材尺寸
    const secondLineStr = `规格: ${spliceSteelSize(material)}　颜色：${isNotBlank(material.color) ? material.color : emptyText}`
    // 拼接主体信息
    const contentHtml = `
    <div class="material-info">
      <div class="first-line ellipsis-text">
        ${firstLineStr}
      </div>
      <div class="second-line ellipsis-text">
        ${secondLineStr}
      </div>
    </div>
    `
    // 拼接css及页面信息其他html
    const strHtml = combineHtml(MATERIAL_LABEL_STYLE, contentHtml)

    LODOP = await getLODOP()
    // 设置打印纸张大小
    LODOP.SET_PRINT_PAGESIZE(intOrient, pageWidth, pageHeight, '1')
    LODOP.ADD_PRINT_HTM(0, 0, '100%', '100%', strHtml)
    LODOP.ADD_PRINT_BARCODE('1mm', '72mm', '28mm', '10mm', '128C', material.barcode)
    LODOP.SET_PRINT_COPIES(copies)
    // LODOP.PRINT_DESIGN() /* 打印设计*/
    // LODOP.PREVIEW() /* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

const MATERIAL_LABEL_STYLE = `
<style type="text/css">
  body {
    margin:0;
    padding:0;
    font-size:10pt;
    font-family:'SimSun, Microsoft Yahei';
  }
  .material-info {
    margin-left: 2mm;
    width: 71mm;
    height: 12mm;
    font-weight: bold;
  }
  .first-line {
    height: 6mm;
    line-height: 6.5mm;
    width: inherit;
  }
  .second-line {
    height: 6mm;
    line-height: 5.5mm;
    width: inherit;
  }
  .ellipsis-text {
    vertical-align: middle;
    overflow : hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
</style>
`
