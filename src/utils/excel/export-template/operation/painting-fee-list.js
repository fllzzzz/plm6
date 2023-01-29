import { alignEnum, verticleAlignEnum, fieldTypeEnum as typeEnum, cssUnitEnum, cssUnitPrecisionEnum, pageFormatEnum } from '@/utils/print/enum'
import { projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import { getPaintingFeeListFn } from '@/api/operation/painting-fee'

// 涂装费清单导出
const paintingFeeListETmpl = {
  fontUnit: 'pt', // 字体单位
  unit: cssUnitEnum.MM.V, // 长度单位
  unitPrecision: cssUnitPrecisionEnum.ZERO.V, // 长度单位精度
  type: 'PAINTING_FEE_LIST_IMPL', // 表格类型 KEY
  name: '涂装费清单', // 表格名称
  width: 210, // 打印纸的宽度
  height: 297, // 打印纸的高度
  paddingLR: 10, // 左右内边距
  paddingTB: 10, // 上下内边距
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
    title: '涂装费清单',
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
    emptyVal: ''
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
    extraFields: [

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
      { show: true, key: 'project', title: '项目', minWidth: 50, type: typeEnum.PROJECT.K, format: { showProjectFullName: false, showSerialNumber: true, projectNameShowConfig: projectNameArrangementModeEnum.SERIAL_NUMBER_START.V, lineBreak: false }},
      { show: true, key: 'mete', title: '累计产量（吨）', align: alignEnum.CENTER.V, minWidth: 30, type: typeEnum.METE.K, format: { toThousand: false }, sum: true },
      { show: true, key: 'area', title: '油漆面积（㎡）', align: alignEnum.CENTER.V, minWidth: 30, type: typeEnum.OTHER.K, format: { toThousand: false }, sum: true },
      { show: true, key: 'paintingUnitPrice', title: '涂装单价', align: alignEnum.CENTER.V, minWidth: 30, type: typeEnum.AMOUNT.K },
      { show: true, key: 'price', title: '应付工程款（元）', align: alignEnum.CENTER.V, minWidth: 35, type: typeEnum.AMOUNT.K, format: { toThousand: false }, sum: true },
      { show: true, key: 'averageUnitPrice', title: '平均单价（元/吨）', align: alignEnum.CENTER.V, minWidth: 35, type: typeEnum.AMOUNT.K, sum: true }
    ]
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
  fetch: async (params) => {
    const data = await getPaintingFeeListFn(params)
    data.table?.forEach(v => {
      v.mete = convertUnits(v.mete, 'kg', 't', DP.COM_WT__T)
      v.area = convertUnits(v.area, 'mm2', 'm2', DP.COM_AREA__M2)
      v.paintingUnitPrice = v.price && v.area ? (v.price / v.area).toFixed(2) : 0
      v.averageUnitPrice = v.price && v.mete ? (v.price / v.mete).toFixed(2) : 0
    })
    return data
  }
}

export default paintingFeeListETmpl
