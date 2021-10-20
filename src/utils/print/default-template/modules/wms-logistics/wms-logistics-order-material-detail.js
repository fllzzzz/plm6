import { dataSourceEnum, alignEnum, verticleAlignEnum, fieldTypeEnum as typeEnum, cssUnitEnum, cssUnitPrecisionEnum, pageFormatEnum } from '@/utils/print/enum'
import { amountUnitEnum } from '../../../enum'

// 订单收货详情
const WMS_LOGISTICS_ORDER_MATERIAL_DETAIL = {
  fontUnit: 'pt', // 字体单位
  unit: cssUnitEnum.MM.V, // 长度单位
  unitPrecision: cssUnitPrecisionEnum.ZERO.V, // 长度单位精度
  type: 'WMS_LOGISTICS_ORDER_MATERIAL_DETAIL', // 表格类型 KEY
  name: '订单收货详情（平台）', // 表格名称
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
    title: '订单收货详情',
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
    height: 12,
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
      { show: true, key: 'orderNo', title: '订单号：', source: dataSourceEnum.SYSTEM.V, width: 130, type: typeEnum.GUID.K },
      { show: true, key: 'supplierName', title: '供应商：', source: dataSourceEnum.SYSTEM.V, width: 60, align: alignEnum.LEFT.V, type: typeEnum.COMPANY_NAME.K },
      { show: true, key: 'inWarehouseDate', title: '入库日期：', source: dataSourceEnum.SYSTEM.V, width: 130, type: typeEnum.DATES.K, format: 'YY/MM/DD' },
      { show: true, key: 'measurementType', title: '计量方式：', source: dataSourceEnum.SYSTEM.V, width: 60, align: alignEnum.CENTER.V, type: typeEnum.ENUM.K, format: { enum: 'engineerSettlementTypeEnum', key: 'SL' }},
      { show: false, key: 'printDate', title: '打印日期：', source: dataSourceEnum.SYSTEM.V, width: 55, type: typeEnum.DATE.K, format: 'YY/MM/DD kk:mm:ss' },
      { show: false, key: 'printer', title: '打印人：', source: dataSourceEnum.SYSTEM.V, width: 35, type: typeEnum.USER_NAME.K }
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
    /**
     * 额外字段，不展示，用于做部分字段的处理，例如：basicClass处理mete的单位
     * @param {string} key 字段名
     * @param {string} title 名称
     * @param {enum} type 数据类型
     * @param {*} format 格式转换
     */
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
      { show: true, key: 'serialNumber', title: '编号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SERIAL_NUMBER.K },
      { show: true, key: 'className', title: '物料类别', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.MATERIAL_CLASS_FULL_NAME.K },
      { show: true, key: 'specification', title: '规格', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SPECIFICATION.K },
      { show: false, key: 'unit', title: '计量单位', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, width: 17, type: typeEnum.MEASUREMENT_UNIT.K },
      { show: false, key: 'number', title: '入库数', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 14, type: typeEnum.QUANTITY.K, format: { toThousandFilter: false, precision: 0 }},
      { show: true, key: 'checkUnit', title: '核算单位', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, width: 17, type: typeEnum.ACCOUNTING_UNIT.K },
      { show: true, key: 'mete', title: '入库量', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.METE.K, format: { toThousandFilter: true, precision: 3 }, sum: false },
      { show: false, key: 'brand', title: '品牌', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 20, type: typeEnum.BRAND.K },
      { show: false, key: 'unitPrice', title: '采购单价', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 20, type: typeEnum.AMOUNT.K, format: { toThousandFilter: true, precision: 2, unit: amountUnitEnum.YUAN.V }, sum: false },
      { show: true, key: 'totalAmount', title: '总金额', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 20, type: typeEnum.AMOUNT.K, format: { toThousandFilter: true, precision: 2, unit: amountUnitEnum.YUAN.V }, sum: true },
      { show: false, key: 'storageNo', title: '入库单号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 20, type: typeEnum.SERIAL_NUMBER.K },
      { show: true, key: 'licensePlate', title: '车牌号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.LICENSE_PLATE.K },
      { show: false, key: 'handlerName', title: '办理人', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.USER_NAME.K }
    ]
  }
}

export default {
  WMS_LOGISTICS_ORDER_MATERIAL_DETAIL // 订单收货详情
}
