import { dataSourceEnum, alignEnum, verticleAlignEnum, fieldTypeEnum as typeEnum, cssUnitEnum, cssUnitPrecisionEnum, pageFormatEnum, amountUnitEnum } from '@/utils/print/enum'
import { projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'

// 通常
// 手机号列宽：25mm
// 项目列宽：25mm
// 签订日期: YY/MM/DD 18mm
// 其他：18mm

// 发运信息
const SHIP_INFO = {
  fontUnit: 'pt', // 字体单位
  unit: cssUnitEnum.MM.V, // 长度单位
  unitPrecision: cssUnitPrecisionEnum.ZERO.V, // 长度单位精度
  type: 'SHIP_INFO', // 表格类型 KEY
  name: '发运信息（平台）', // 表格名称
  width: 210, // 打印纸的宽度
  height: 297, // 打印纸的高度
  paddingLR: 10, // 左右内边距
  paddingTB: 10, // 上下内边距
  /**
    * logo
    * @param {boolean} show 是否显示
    * @param {boolean} allPage true:每页显示，false: 第一页显示
    * @param {number} height 盒子高度
    * @param {number} width 盒子宽度
    * @param {number} top 盒子距离顶边距离
    * @param {number} left 盒子距离左侧距离
    */
  logo: {
    show: false,
    allPage: false,
    height: 20,
    width: 20,
    top: 10,
    left: 10,
    url: ''
  },
  /**
    * 页码
    * @param {boolean} show 是否显示
    * @param {enum} format 页码格式
    * @param {enum} align 水平对齐方式
    * @param {number} size 字体大小
    * @param {string} bold // 是否加粗 'unset' || 'bold'
    * @param {number} bottom 距离底边距离
    */
  page: {
    show: true,
    format: pageFormatEnum.DEFAULT.V,
    align: alignEnum.CENTER.V,
    size: 11,
    bold: 'unset',
    bottom: 2
  },
  /**
    * 标题
    * @param {boolean} show 是否显示
    * @param {boolean} allPage true:每页显示，false: 第一页显示
    * @param {string} title // 标题名称
    * @param {enum} align 水平对齐方式
    * @param {enum} verticleAlign 垂直对齐方式
    * @param {number} size 字体大小
    * @param {string} bold // 是否加粗 'unset' || 'bold'
    * @param {number} height 盒子高度
    */
  title: {
    show: true,
    allPage: false,
    title: '发运信息',
    align: alignEnum.CENTER.V,
    verticleAlign: verticleAlignEnum.CENTER.V,
    size: 17,
    bold: 'bold',
    height: 10
  },
  /**
    * 表头
    * @param {boolean} show 是否显示
    * @param {boolean} allPage true:每页显示，false: 第一页显示
    * @param {enum} align 水平对齐方式
    * @param {enum} verticleAlign 垂直对齐方式
    * @param {number} size 字体大小
    * @param {string} bold // 是否加粗 'unset' || 'bold'
    * @param {number} height 盒子高度
    * @param {number} width 盒子宽度
    * @param {string} emptyVal // 空值显示
    * @param {array} fields // 字段
    */
  header: {
    show: true,
    allPage: false,
    align: alignEnum.LEFT.V,
    verticleAlign: verticleAlignEnum.CENTER.V,
    size: 10,
    bold: 'bold',
    height: 6,
    width: 190,
    emptyVal: '',
    /**
     * 表格列
     * @param {boolean} show 是否显示
     * @param {string} key 字段名
     * @param {string} title 列名
     * @param {enum} source 数据来源 系统/自定义
     * @param {number} maxWidth 最大列宽
     * @param {number} width 列宽
     * @param {enum} type 数据类型
     * @param {*} format 格式转换
     */
    fields: [ // 字段内容
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'statisticsDate', title: '统计日期：', width: 100, type: typeEnum.DATE.K, format: 'YYYY' },
      { show: false, source: dataSourceEnum.SYSTEM.V, key: 'printDate', title: '打印时间：', width: 55, type: typeEnum.DATE.K, format: 'YY/MM/DD kk:mm:ss' },
      { show: false, source: dataSourceEnum.SYSTEM.V, key: 'printer', title: '打印人：', width: 35, type: typeEnum.USER_NAME.K }
    ]
  },
  /**
    * 表尾
    * @param {boolean} show 是否显示
    * @param {boolean} allPage true:每页显示，false: 最后一页显示
    * @param {enum} align 水平对齐方式
    * @param {enum} verticleAlign 垂直对齐方式
    * @param {number} size 字体大小
    * @param {string} bold // 是否加粗 'unset' || 'bold'
    * @param {number} height 盒子高度
    * @param {number} width 盒子宽度
    * @param {string} emptyVal // 空值显示
    * @param {object} tip // 提示
    * @param {array} fields // 字段
    */
  footer: {
    show: false,
    allPage: false,
    align: alignEnum.LEFT.V,
    verticleAlign: verticleAlignEnum.CENTER.V,
    size: 10,
    bold: 'bold',
    height: 15,
    width: 190,
    emptyVal: '',
    /**
     * 提示
     * @param {boolean} show 是否显示
     * @param {string} text 提示内容
     * @param {string} bold // 是否加粗 'unset' || 'bold'
     * @param {number} size 字体大小
     * @param {enum} align 水平对齐方式
     * @param {boolean} above 是否显示在字段上方
     */
    tip: { show: false, text: '', bold: 'unset', size: 9, align: alignEnum.LEFT.V, above: true },
    /**
     * 表格列
     * @param {boolean} show 是否显示
     * @param {string} key 字段名
     * @param {string} title 列名
     * @param {enum} source 数据来源 系统/自定义
     * @param {number} maxWidth 最大列宽
     * @param {number} width 列宽
     * @param {enum} type 数据类型
     * @param {*} format 格式转换
     */
    fields: []
  },
  table: {
    /**
     * th
     * @param {number} size 字体大小
     * @param {string} bold 是否加粗 'unset' || 'bold'
     * @param {number} lineHeight 行高
     */
    th: { size: 10, bold: 'bold', lineHeight: 15, paddingTB: 1 },
    /**
     * td
     * @param {number} size 字体大小
     * @param {string} bold 是否加粗 'unset' || 'bold'
     * @param {number} lineHeight 行高
     */
    td: { size: 9, bold: 'unset', lineHeight: 13, paddingTB: 1 },
    emptyVal: '/', // string 空值显示
    /**
     * 表格序号
     * @param {boolean} show 是否显示
     * @param {string} title 序号名称
     * @param {number} width 列宽
     * @param {enum} align 水平对齐方式
     */
    index: { show: true, title: '序号', width: 10, align: alignEnum.CENTER.V },
    /**
     * 合计行
     * @param {boolean} show 是否显示
     * @param {string} title 合计名称
     */
    summary: { show: true, title: '合计' },
    /**
     * 表格列
     * @param {boolean} show 是否显示
     * @param {string} key 字段名
     * @param {string} title 列名
     * @param {enum} source 数据来源 系统/自定义
     * @param {enum} align 水平对齐方式
     * @param {number} minWidth 最小列宽
     * @param {number} width 列宽
     * @param {enum} type 数据类型
     * @param {*} format 格式转换
     * @param {boolean} sum 列需要合计
     */
    fields: [
      { show: true, key: 'project.contractNo', title: '合同编号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 28, type: typeEnum.CONTRACT_NO.K },
      { show: true, key: 'project', title: '项目名称', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 28, type: typeEnum.PROJECT.K, format: { showProjectFullName: false, showSerialNumber: true, projectNameShowConfig: projectNameArrangementModeEnum.SERIAL_NUMBER_START.V }},
      { show: true, key: 'orderType', title: '订单类型', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.ORDER_TYPE.K },
      { show: true, key: 'structureType', title: '结构类型', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.STRUCTURE_TYPE.K },
      { show: true, key: 'settlementType', title: '计量方式', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.ENUM.K, format: { enum: 'weightMeasurementModeEnum', key: 'SL', bit: true }},
      { show: true, key: 'supplierName', title: '物流单位', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, width: 18, type: typeEnum.COMPANY_NAME.K },
      { show: true, key: 'licensePlate', title: '车牌号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.LICENSE_PLATE.K },
      { show: true, key: 'senderUserName', title: '发货人', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.USER_NAME.K },
      { show: true, key: 'checkUserName', title: '复核人', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.USER_NAME.K },
      { show: true, key: 'auditUserName', title: '审批人', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.USER_NAME.K },
      { show: true, key: 'name', title: '名称', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.STRUCTURE_NAME.K },
      { show: true, key: 'serialNumber', title: '编号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.SERIAL_NUMBER.K },
      { show: true, key: 'length', title: '长度', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 14, type: typeEnum.LENGTH.K },
      { show: true, key: 'specification', title: '规格', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.SPECIFICATION.K },
      { show: true, key: 'checkUnit', title: '核算单位', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, width: 12, type: typeEnum.ACCOUNTING_UNIT.K },
      { show: true, key: 'mete', title: '单量', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 14, type: typeEnum.METE.K, format: { toThousand: true, precision: 3 }, sum: false },
      { show: true, key: 'totalMete', title: '总量', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.METE.K, format: { toThousand: false, precision: 3 }, sum: true },
      { show: true, key: 'price', title: '单价', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 14, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }},
      { show: true, key: 'totalPrice', title: '总计', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }, sum: true },
      { show: true, key: 'tripNumber', title: '出库单号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, width: 18, type: typeEnum.COMPANY_NAME.K },
      { show: true, key: 'monomerName', title: '单体', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.MONOMER_NAME.K },
      { show: true, key: 'areaName', title: '区域', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.AREA_NAME.K }
    ]
  }
}

export default {
  SHIP_INFO // 发运信息
}
