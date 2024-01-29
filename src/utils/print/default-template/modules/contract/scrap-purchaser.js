import { dataSourceEnum, alignEnum, verticleAlignEnum, fieldTypeEnum as typeEnum, cssUnitEnum, cssUnitPrecisionEnum, pageFormatEnum } from '@/utils/print/enum'
// import { projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'

// 收款记录
const scrapPurchaser = {
  fontUnit: 'pt', // 字体单位
  unit: cssUnitEnum.MM.V, // 长度单位
  unitPrecision: cssUnitPrecisionEnum.ZERO.V, // 长度单位精度
  type: 'scrapPurchaser', // 表格类型 KEY
  name: '废料台账-购买方（平台）', // 表格名称
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
    title: '废料台账-购买方',
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
      // { show: true, source: dataSourceEnum.SYSTEM.V, key: 'project', title: '项目：', width: 140, type: typeEnum.PROJECT.K, format: { showProjectFullName: false, showSerialNumber: true, projectNameShowConfig: projectNameArrangementModeEnum.SERIAL_NUMBER_START.V, lineBreak: false }},
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'date', title: '统计日期：', width: 60, type: typeEnum.OTHER.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'printDate', title: '打印日期：', width: 60, type: typeEnum.DATE.K, format: 'YY/MM/DD' },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'printer', title: '打印人：', width: 60, type: typeEnum.USER_NAME.K }
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
      { show: true, key: 'serialNumber', title: '合同编号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 28, type: typeEnum.CONTRACT_NO.K },
      { show: true, key: 'purchaser', title: '购买单位', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.COMPANY_NAME.K },
      { show: true, key: 'branchCompanyName', title: '出售单位', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.COMPANY_NAME.K },
      { show: false, key: 'totalAmount', title: '累计出售金额', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.AMOUNT.K },
      { show: true, key: 'collectionAmount', title: '累计收款', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 22, type: typeEnum.AMOUNT.K },
      { show: true, key: 'collectionRate', title: '收款率', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 22, type: typeEnum.RATE.K },
      { show: true, key: 'invoiceAmount', title: '累计开票', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 22, type: typeEnum.AMOUNT.K },
      { show: true, key: 'invoiceRate', title: '开票率', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 22, type: typeEnum.RATE.K }
    ]
  }
}

export default {
  scrapPurchaser // 按购买方查列表
}
