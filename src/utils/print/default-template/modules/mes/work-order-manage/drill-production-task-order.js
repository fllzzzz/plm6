import { dataSourceEnum, alignEnum, verticleAlignEnum, fieldTypeEnum as typeEnum, cssUnitEnum, weightUnitEnum, cssUnitPrecisionEnum, pageFormatEnum } from '@/utils/print/enum'
import { projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'

// 工单管理：钻孔生产任务单
const mesDrillProductionTaskOrder = {
  fontUnit: 'pt', // 字体单位
  unit: cssUnitEnum.MM.V, // 长度单位
  unitPrecision: cssUnitPrecisionEnum.ZERO.V, // 长度单位精度
  type: 'mesDrillProductionTaskOrder', // 表格类型 KEY
  name: '钻孔生产任务单（平台）', // 表格名称
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
  qrCode: {
    show: true,
    allPage: false,
    height: 20.5,
    width: 20.5,
    top: 5,
    left: 180
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
  page:
    {
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
    title: '钻孔生产任务单',
    align: alignEnum.CENTER.V,
    verticleAlign: verticleAlignEnum.TOP.V,
    size: 17,
    bold: 'bold',
    height: 15
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
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'serialNumber', title: '任务单：', width: 150, type: typeEnum.SERIAL_NUMBER.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'process', title: '方式：', width: 40, type: typeEnum.OTHER.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'workshopAndLine', title: '产线：', width: 150, type: typeEnum.OTHER.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'scheduleUserName', title: '排产人：', width: 40, type: typeEnum.USER_NAME.K },
      { show: false, source: dataSourceEnum.SYSTEM.V, key: 'year', title: '统计日期：', width: 100, type: typeEnum.OTHER.K },
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
      { show: true, key: 'project', title: '项目', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 28, type: typeEnum.PROJECT.K, format: { showProjectFullName: false, showSerialNumber: true, projectNameShowConfig: projectNameArrangementModeEnum.SERIAL_NUMBER_START.V }},
      { show: true, key: 'monomerName', title: '单体', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.STRUCTURE_NAME.K },
      { show: true, key: 'serialNumber', title: '编号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.SERIAL_NUMBER.K },
      { show: true, key: 'specification', title: '规格', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.SPECIFICATION.K },
      { show: true, key: 'material', title: '材质', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.MATERIAL.K },
      { show: true, key: 'quantity', title: '数量', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 14, type: typeEnum.QUANTITY.K, format: { toThousand: false, precision: 0 }, sum: true },
      { show: true, key: 'netWeight', title: '单净重', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.WEIGHT.K, format: { toThousand: false, precision: 2, unit: weightUnitEnum.KG.V }},
      { show: false, key: 'grossWeight', title: '单毛重', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.WEIGHT.K, format: { toThousand: false, precision: 2, unit: weightUnitEnum.KG.V }},
      { show: true, key: 'totalNetWeight', title: '总净重', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.WEIGHT.K, format: { toThousand: false, precision: 2, unit: weightUnitEnum.KG.V }, sum: true },
      { show: false, key: 'totalGrossWeight', title: '总毛重', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.WEIGHT.K, format: { toThousand: false, precision: 2, unit: weightUnitEnum.KG.V }, sum: true },
      { show: true, key: 'picturePath', title: '图片', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, width: 20, height: 20, type: typeEnum.IMAGE.K }
    ]
  }
}

export default {
  mesDrillProductionTaskOrder // 钻孔生产任务单
}
