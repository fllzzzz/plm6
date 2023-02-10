import { projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'
import {
  dataSourceEnum,
  alignEnum,
  verticleAlignEnum,
  fieldTypeEnum as typeEnum,
  cssUnitEnum,
  cssUnitPrecisionEnum,
  pageFormatEnum,
  amountUnitEnum
} from '@/utils/print/enum'

// 调拨单
const wmsRmTransferReceipt = {
  fontUnit: 'pt', // 字体单位
  unit: cssUnitEnum.MM.V, // 长度单位
  unitPrecision: cssUnitPrecisionEnum.ZERO.V, // 长度单位精度
  type: 'wmsRmTransferReceipt', // 表格类型 KEY
  name: '调拨单（平台）', // 表格名称
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
    title: '调拨单',
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
    fields: [
      // 字段内容
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'transferSN', title: '调拨单号：', width: 135, type: typeEnum.GUID.K },
      {
        show: true,
        source: dataSourceEnum.SYSTEM.V,
        key: 'transferType',
        title: '调拨类型：',
        width: 55,
        type: typeEnum.ENUM.K,
        format: { enum: 'transferTypeEnum' }
      },
      {
        show: true,
        source: dataSourceEnum.SYSTEM.V,
        key: 'source',
        title: '调拨来源：',
        width: 135,
        type: typeEnum.OTHER.K
      },
      {
        show: true,
        source: dataSourceEnum.SYSTEM.V,
        key: 'transferTime',
        title: '调拨时间：',
        width: 55,
        type: typeEnum.DATE.K,
        format: 'YY/MM/DD kk:mm'
      }, // 创建时间
      {
        show: true,
        source: dataSourceEnum.SYSTEM.V,
        key: 'direction',
        title: '调拨目的：',
        width: 135,
        type: typeEnum.OTHER.K
      },
      {
        show: false,
        source: dataSourceEnum.SYSTEM.V,
        key: 'printDate',
        title: '打印时间：',
        width: 55,
        type: typeEnum.DATE.K,
        format: 'YY/MM/DD kk:mm'
      },
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
    height: 8,
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
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'applicant', title: '申请人（签字）：', width: 85, type: typeEnum.BLANK.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'reviewer', title: '审核人（签字）：', width: 85, type: typeEnum.BLANK.K }
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
      { key: 'basicClass', title: '基础类型', type: typeEnum.ENUM.K, format: { enum: 'rawMatClsEnum' }},
      { key: 'accountingUnit', title: '核算单位', type: typeEnum.ACCOUNTING_UNIT.K },
      { key: 'accountingPrecision', title: '核算单位小数精度', type: typeEnum.DP.K },
      { key: 'measureUnit', title: '计量单位', type: typeEnum.MEASUREMENT_UNIT.K },
      { key: 'measurePrecision', title: '计量单位小数精度', type: typeEnum.DP.K }
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
      {
        show: false,
        key: 'serialNumber',
        title: '编号',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.LEFT.V,
        minWidth: 18,
        type: typeEnum.SERIAL_NUMBER.K
      },
      {
        show: true,
        key: 'classifyName',
        title: '物料类别',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.LEFT.V,
        minWidth: 18,
        type: typeEnum.MATERIAL_CLASS_NAME.K
      },
      {
        show: true,
        key: 'specMerge',
        title: '规格',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.LEFT.V,
        minWidth: 18,
        type: typeEnum.SPECIFICATION.K
      },
      {
        show: false,
        key: 'brand',
        title: '品牌',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.LEFT.V,
        minWidth: 18,
        type: typeEnum.BRAND.K
      },
      {
        show: false,
        key: 'measureUnit',
        title: '计量单位',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.CENTER.V,
        minWidth: 18,
        type: typeEnum.MEASUREMENT_UNIT.K
      },
      {
        show: false,
        key: 'quantity',
        title: '调拨数',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.RIGHT.V,
        minWidth: 18,
        type: typeEnum.QUANTITY.K,
        format: { toThousand: false, rowUnit: 'measureUnit' }
      },
      {
        show: true,
        key: 'accountingUnit',
        title: '核算单位',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.CENTER.V,
        width: 10,
        type: typeEnum.ACCOUNTING_UNIT.K
      },
      {
        show: true,
        key: 'mete',
        title: '调拨量',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.RIGHT.V,
        minWidth: 18,
        type: typeEnum.METE.K,
        format: { toThousand: false, rowUnit: 'accountingUnit' },
        sum: true
      },
      {
        show: false,
        key: 'unitPriceExcludingVAT',
        title: '不含税单价',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.RIGHT.V,
        minWidth: 18,
        type: typeEnum.AMOUNT.K,
        format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V },
        sum: false
      },
      {
        show: true,
        key: 'unitPrice',
        title: '含税单价',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.RIGHT.V,
        minWidth: 18,
        type: typeEnum.AMOUNT.K,
        format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V },
        sum: false
      },
      {
        show: false,
        key: 'amountExcludingVAT',
        title: '不含税金额',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.RIGHT.V,
        minWidth: 18,
        type: typeEnum.AMOUNT.K,
        format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V },
        sum: true
      },
      {
        show: true,
        key: 'amount',
        title: '含税金额',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.RIGHT.V,
        minWidth: 18,
        type: typeEnum.AMOUNT.K,
        format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V },
        sum: true
      },
      {
        show: false,
        key: 'workshop.name',
        title: '车间',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.LEFT.V,
        minWidth: 18,
        type: typeEnum.WORKSHOP.K
      },
      {
        show: false,
        key: 'warehouse.name',
        title: '存储位置',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.LEFT.V,
        minWidth: 18,
        type: typeEnum.WAREHOUSE_NAME.K
      },
      { show: false, key: 'remark', title: '备注', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20 },
      {
        show: true,
        key: 'project',
        title: '所属项目',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.LEFT.V,
        minWidth: 25,
        type: typeEnum.PROJECT.K,
        format: {
          showProjectFullName: false,
          showSerialNumber: true,
          projectNameShowConfig: projectNameArrangementModeEnum.SERIAL_NUMBER_START.V
        }
      },
      {
        show: false,
        key: 'monomerName',
        title: '单体',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.LEFT.V,
        minWidth: 18,
        type: typeEnum.MONOMER_NAME.K
      },
      {
        show: false,
        key: 'areaName',
        title: '区域',
        source: dataSourceEnum.SYSTEM.V,
        align: alignEnum.LEFT.V,
        minWidth: 18,
        type: typeEnum.AREA_NAME.K
      }
    ]
  }
}

export default {
  wmsRmTransferReceipt // 调拨单
}
