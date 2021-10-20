import { dataSourceEnum, alignEnum, verticleAlignEnum, fieldTypeEnum as typeEnum, cssUnitEnum, cssUnitPrecisionEnum, pageFormatEnum, weightUnitEnum, lengthUnitEnum } from '@/utils/print/enum'
import { projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'

// 发货清单
const BRIDGE_MES_PACK_SHIPMENT_DETAIL = {
  fontUnit: 'pt', // 字体单位
  unit: cssUnitEnum.MM.V, // 长度单位
  unitPrecision: cssUnitPrecisionEnum.ZERO.V, // 长度单位精度
  type: 'BRIDGE_MES_PACK_SHIPMENT_DETAIL', // 表格类型 KEY
  name: '发货清单（平台）', // 表格名称
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
    top: 10,
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
    title: '发货清单',
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
    height: 36,
    width: 190,
    emptyVal: '',
    /**
     * 表格列
     * @param {boolean} show 是否显示
     * @param {string} key 字段名
     * @param {string} title 列名
     * @param {enum} source 数据来源 系统/自定义
     * @param {number} minWidth 最小列宽
     * @param {number} width 列宽
     * @param {enum} type 数据类型
     * @param {*} format 格式转换
     */
    fields: [ // 字段内容
      { show: false, source: dataSourceEnum.SYSTEM.V, key: 'project.contractNo', title: '合同编号：', width: 70, type: typeEnum.CONTRACT_NO.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'project', title: '项目：', width: 190, type: typeEnum.PROJECT.K, format: { showProjectFullName: false, showContractNo: true, projectNameShowConfig: projectNameArrangementModeEnum.CONTRACT_NO_START.V, lineBreak: false }},
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'address', title: '项目地址：', width: 190, type: typeEnum.OTHER.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'supplierName', title: '物流单位：', width: 190, type: typeEnum.COMPANY_NAME.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'company', title: '收货单位：', width: 135, type: typeEnum.COMPANY_NAME.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'deliveryTime', title: '发货时间：', width: 55, type: typeEnum.DATE.K, format: 'YY/MM/DD kk:mm:ss' },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'consignee', title: '收货人：', width: 45, type: typeEnum.USER_NAME.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'tripNumber', title: '车次：', width: 90, type: typeEnum.GUID.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'leaderPhone', title: '联系电话：', width: 55, type: typeEnum.PHONE.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'carrier', title: '承运人：', width: 45, type: typeEnum.USER_NAME.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'licensePlate', title: '车牌号：', width: 90, type: typeEnum.LICENSE_PLATE.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'driverPhone', title: '司机电话：', width: 55, type: typeEnum.PHONE.K }
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
    height: 24,
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
    tip: { show: true, text: `特别说明：\n货物到达工地后，请及时清点数量并与驾驶员当面交接签字验收，如有异议及时与相关人员联系，否则本公司不承担责任。收货方承担运费项目以驾驶员签字为准，本公司承担运费项目驾驶员必须将收货方签字的发货单邮寄或扫描回传到公司后方可结算运费。`, bold: 'unset', size: 9, align: alignEnum.LEFT.V, above: false },
    /**
     * 表格列
     * @param {boolean} show 是否显示
     * @param {string} key 字段名
     * @param {string} title 列名
     * @param {enum} source 数据来源 系统/自定义
     * @param {number} minWidth 最小列宽
     * @param {number} width 列宽
     * @param {enum} type 数据类型
     * @param {*} format 格式转换
     */
    fields: [
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'shipper', title: '发货人：', width: 63, type: typeEnum.USER_NAME.K },
      { show: true, source: dataSourceEnum.SYSTEM.V, key: 'auditor', title: '审核：', width: 63, type: typeEnum.USER_NAME.K },
      { show: true, source: dataSourceEnum.CUSTOMIZE.V, key: 'approver', title: '签收：', width: 63 }
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
      { show: true, key: 'name', title: '产品名称', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.STRUCTURE_NAME.K },
      { show: true, key: 'serialNumber', title: '编号/型号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.SERIAL_NUMBER.K },
      { show: true, key: 'unit', title: '单位', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 18, type: typeEnum.ACCOUNTING_UNIT.K },
      { show: true, key: 'length', title: '长度', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.LENGTH.K, format: { toThousandFilter: false, precision: 3, unit: lengthUnitEnum.M.V }},
      { show: true, key: 'quantity', title: '数量', source: dataSourceEnum.SYSTEM.V, align: alignEnum.CENTER.V, minWidth: 14, type: typeEnum.QUANTITY.K, format: { toThousandFilter: false, precision: 0 }, sum: true },
      { show: true, key: 'weight', title: '单重', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.WEIGHT.K, format: { toThousandFilter: false, precision: 3, unit: weightUnitEnum.KG.V }},
      { show: true, key: 'totalWeight', title: '总重', source: dataSourceEnum.SYSTEM.V, align: alignEnum.RIGHT.V, minWidth: 18, type: typeEnum.WEIGHT.K, format: { toThousandFilter: false, precision: 3, unit: weightUnitEnum.KG.V }, sum: true },
      { show: true, key: 'monomer', title: '单体', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.MONOMER_NAME.K }
    ]
  }
}

export default {
  BRIDGE_MES_PACK_SHIPMENT_DETAIL //  发货清单
}
