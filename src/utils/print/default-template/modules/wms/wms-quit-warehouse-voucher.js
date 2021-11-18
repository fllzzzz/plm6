import { dataSourceEnum, alignEnum, verticleAlignEnum, fieldTypeEnum as typeEnum, cssUnitEnum, cssUnitPrecisionEnum, pageFormatEnum } from '@/utils/print/enum'
import { projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'
import { amountUnitEnum } from '../../../enum'

// 退库单
const WMS_QUIT_WAREHOUSE_VOUCHER = {
  fontUnit: 'pt', // 字体单位
  unit: cssUnitEnum.MM.V, // 长度单位
  unitPrecision: cssUnitPrecisionEnum.ZERO.V, // 长度单位精度
  type: 'WMS_QUIT_WAREHOUSE_VOUCHER', // 表格类型 KEY
  name: '退库单（平台）', // 表格名称
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
    title: '退库单',
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
    height: 25,
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
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'orderNo', title: '订单号：', width: 120, type: typeEnum.GUID.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'measurementType', title: '计量方式：', width: 55, type: typeEnum.ENUM.K, format: { enum: 'weightMeasurementModeEnum', key: 'SL' }},
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'returnWarehouseNo', title: '退库单号：', width: 120, type: typeEnum.GUID.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'licensePlate', title: '车牌号：', width: 55, type: typeEnum.LICENSE_PLATE.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'storageNo', title: '入库单号：', width: 120, type: typeEnum.CONTRACT_NO.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'supplierName', title: '供应商：', width: 55, type: typeEnum.COMPANY_NAME.K },
      { show: false, source: dataSourceEnum.SYSTEM.V, key: 'project.contractNo', title: '合同编号：', width: 70, type: typeEnum.CONTRACT_NO.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'projectName', title: '项目：', width: 120, type: typeEnum.PROJECT.K, format: { showProjectFullName: false, showContractNo: true, projectNameShowConfig: projectNameArrangementModeEnum.CONTRACT_NO_START.V }},
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'returnWarehouseTime', title: '退库时间：', width: 55, type: typeEnum.DATE.K, format: 'YY/MM/DD kk:mm:ss' },
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
    show: true,
    allPage: false,
    align: alignEnum.LEFT.V,
    verticleAlign: verticleAlignEnum.CENTER.V,
    size: 10,
    bold: 'bold',
    height: 10,
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
    fields: [
      { show: true, source: dataSourceEnum.CUSTOMIZE.V, key: 'createUserName', title: '退库：', width: 85 },
      { show: true, source: dataSourceEnum.CUSTOMIZE.V, key: 'checkUserName', title: '审核：', width: 85 }
    ]
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
    td: { size: 9, bold: 'unset', lineHeight: 13, paddingTB: 2 },
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
    extraFields: [
      { key: 'basicClass', title: '基础类型', type: typeEnum.ENUM.K, format: { enum: 'materialBasicClassSetEnum' }}
    ],
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
      { show: true, key: 'serialNumber', title: '编号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.SERIAL_NUMBER.K },
      { show: true, key: 'className', title: '物料类别', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.MATERIAL_CLASS_FULL_NAME.K },
      { show: true, key: 'specification', title: '规格', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.SPECIFICATION.K },
      { show: true, key: 'color', title: '颜色', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.COLOR.K },
      { show: true, key: 'brand', title: '品牌', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.BRAND.K },
      { show: false, key: 'unit', title: '计量单位', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.MEASUREMENT_UNIT.K },
      { show: false, key: 'quantity', title: '退库数', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.QUANTITY.K, format: { toThousandFilter: false, precision: 0 }},
      { show: true, key: 'checkUnit', title: '核算单位', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, width: 10, type: typeEnum.ACCOUNTING_UNIT.K },
      { show: true, key: 'value', title: '退库量', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.METE.K, format: { toThousandFilter: false, precision: 3 }, sum: true },
      { show: false, key: 'unitPrice', title: '采购单价', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.AMOUNT.K, format: { toThousandFilter: true, precision: 2, unit: amountUnitEnum.YUAN.V }, sum: false },
      { show: true, key: 'costPrice', title: '总额', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.AMOUNT.K, format: { toThousandFilter: true, precision: 2, unit: amountUnitEnum.YUAN.V }, sum: true },
      { show: false, key: 'warehouse', title: '存储位置', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.WAREHOUSE_NAME.K }
      // { show: true, key: 'freightPrice', title: '运费', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.AMOUNT.K, format: { toThousandFilter: true, precision: 2, unit: amountUnitEnum.YUAN.V }, sum: true },
      // { show: true, key: 'entruckPrice', title: '装车费', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.AMOUNT.K, format: { toThousandFilter: true, precision: 2, unit: amountUnitEnum.YUAN.V }, sum: true }
      // { show: true, key: 'transportType', title: '运输方式', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 16, type: typeEnum.ENUM.K, format: { enum: 'pickUpModeEnum', key: 'L' }}
    ]
  }
}

export default {
  WMS_QUIT_WAREHOUSE_VOUCHER // 退库单
}
