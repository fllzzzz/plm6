import { dataSourceEnum, alignEnum, verticleAlignEnum, fieldTypeEnum as typeEnum, cssUnitEnum, cssUnitPrecisionEnum, pageFormatEnum, amountUnitEnum } from '@/utils/print/enum'

// 项目台账
const contractLedger = {
  fontUnit: 'pt', // 字体单位
  unit: cssUnitEnum.MM.V, // 长度单位
  unitPrecision: cssUnitPrecisionEnum.ZERO.V, // 长度单位精度
  type: 'contractLedger', // 表格类型 KEY
  name: '项目台账（平台）', // 表格名称
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
    title: '项目台账',
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
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'year', title: '统计日期：', width: 100, type: typeEnum.OTHER.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'printDate', title: '打印时间：', width: 55, type: typeEnum.DATE.K, format: 'YY/MM/DD kk:mm:ss' },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'printer', title: '打印人：', width: 35, type: typeEnum.USER_NAME.K }
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
    height: 0,
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
    summary: { show: false, title: '合计' },
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
      { show: false, key: 'projectType', title: '项目类型', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.ENUM.K, format: { enum: 'projectTypeEnum', key: 'L' }},
      { show: true, key: 'shortName', title: '项目简称', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 28, type: typeEnum.OTHER.K },
      { show: false, key: 'businessType', title: '业务类型', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.ENUM.K, format: { enum: 'businessTypeEnum', key: 'L' }},
      { show: false, key: 'projectManagerName', title: '业务负责人', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.USER_NAME.K },
      { show: false, key: 'signingDate', title: '签订日期', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.DATE.K, format: 'YY/MM/DD' },
      { show: true, key: 'contractAmount', title: '合同金额', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }},
      { show: true, key: 'settlementAmount', title: '结算额', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }},
      { show: true, key: 'collectionAmount', title: '累计收款', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }},
      { show: true, key: 'collectionRate', title: '收款比例', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, width: 17, type: typeEnum.RATE.K },
      { show: true, key: 'invoiceAmount', title: '累计开票', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }},
      { show: true, key: 'invoiceRate', title: '开票比例', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, width: 17, type: typeEnum.RATE.K },
      { show: true, key: 'deliverInstallAmount', title: '累计发生额', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, width: 20, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }},
      { show: true, key: 'availableBalance', title: '可用余额', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.AMOUNT.K, format: { toThousand: true, precision: 2, unit: amountUnitEnum.YUAN.V }},
      { show: false, key: 'status', title: '状态', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.ENUM.K, format: { enum: 'projectStatusEnum', key: 'L' }}
    ]
  }
}

export default {
  contractLedger // 项目台账
}
