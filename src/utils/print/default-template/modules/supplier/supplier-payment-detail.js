import { dataSourceEnum, alignEnum, verticleAlignEnum, fieldTypeEnum as typeEnum, cssUnitEnum, cssUnitPrecisionEnum, pageFormatEnum } from '@/utils/print/enum'
import { amountUnitEnum } from '../../../enum'

// 供应商付款明细
const SUPPLIER_PAYMENT_DETAIL = {
  fontUnit: 'pt', // 字体单位
  unit: cssUnitEnum.MM.V, // 长度单位
  unitPrecision: cssUnitPrecisionEnum.ZERO.V, // 长度单位精度
  type: 'SUPPLIER_PAYMENT_DETAIL', // 表格类型 KEY
  name: '供应商付款明细表（平台）', // 表格名称
  width: 297, // 打印纸的宽度
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
    title: '供应商付款明细',
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
    width: 277,
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
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'orderNo', title: '订单号：', width: 187, type: typeEnum.GUID.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'contractAmount', title: '合同金额：', width: 50, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }},
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'statisticsDate', title: '统计日期：', width: 40, type: typeEnum.DATE.K, format: 'YY/MM/DD' },
      { show: false, source: dataSourceEnum.SYSTEM.V, key: 'printDate', title: '打印日期：', width: 55, type: typeEnum.DATE.K, format: 'YY/MM/DD kk:mm:ss' },
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
    width: 277,
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
      { show: true, key: 'applicationDate', title: '申请日期', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.DATE.K, format: 'YY/MM/DD' },
      { show: false, key: 'applicationDept', title: '申请部门', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.DEPT.K },
      { show: true, key: 'applicantName', title: '申请人', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 17, type: typeEnum.USER_NAME.K },
      { show: true, key: 'supplierFeeType', title: '费用种类', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.SUPPLIER_FEE_TYPE.K },
      { show: true, key: 'applicationAmount', title: '申请金额', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 20, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }, sum: false },
      { show: true, key: 'paymentAmount', title: '实付金额', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 20, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }, sum: true },
      { show: true, key: 'payer', title: '付款方', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 22, type: typeEnum.COMPANY_NAME.K },
      { show: true, key: 'paymentDepositBank', title: '付款开户行', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 22, type: typeEnum.BANK.K },
      { show: true, key: 'paymentBankAccount', title: '付款账号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 22, type: typeEnum.BANK_ACCOUNT.K },
      { show: true, key: 'payee', title: '收款方', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 22, type: typeEnum.COMPANY_NAME.K },
      { show: true, key: 'collectionDepositBank', title: '收款开户行', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 22, type: typeEnum.BANK.K },
      { show: true, key: 'collectionBankAccount', title: '收款账号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 22, type: typeEnum.BANK_ACCOUNT.K },
      { show: false, key: 'handlerName', title: '填报人', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 17, type: typeEnum.USER_NAME.K },
      { show: false, key: 'paymentVoucher', title: '付款凭证号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 20, type: typeEnum.GUID.K }
    ]
  }
}

export default {
  SUPPLIER_PAYMENT_DETAIL // 供应商付款明细
}
