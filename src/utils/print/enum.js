import { constantize, key2val, toArr, getBits, setEnumValue } from '../enum/base'
/**
 * @author dhh
 * L:label 名称,
 * SL: short label 简称
 * K:key 键,
 * V:value 值
 */
// 重量类型
const weightTypeEnum = {
  NET: { L: '净重', K: 'NET', V: 0 },
  GROSS: { L: '毛重', K: 'GROSS', V: 1 },
  NONE: { L: '不显示', K: 'NONE', V: -1 }
}
constantize(weightTypeEnum)

// 打印方向
const orientEnum = {
  LONGITUDINAL: { L: '纵向', K: 'LONGITUDINAL', V: 1 },
  TRANSVERSE: { L: '横向', K: 'TRANSVERSE', V: 2 },
  LONGITUDINAL_NO_LIMIT: { L: '纵向（不限高度）', K: 'LONGITUDINAL_NO_LIMIT', V: 3 }
}
constantize(orientEnum)

// 打印模式
const printModeEnum = {
  NORMAL: { L: '普通', K: 'NORMAL', V: 0 }, // 普通模式，可能会发生连续打印顺序不正确的问题（调用打印方法成功，视为打印成功）
  QUEUE: { L: '队列', K: 'QUEUE', V: 1 }, // 队列模式，顺序进入打印机队列（进入打印机打印队列，视为打印成功） ps：连续打印通常使用该模式
  RESULT: { L: '结果', K: 'RESULT', V: 2 }, // 结果模式，打印成功后下一条打印才会进入打印机队列（打印机打印完成，视为打印成功）
  PREVIEW: { L: '预览', K: 'PREVIEW', V: 3 }, // 预览模式
  PRINT_DESIGN: { L: '设计', K: 'PRINT_DESIGN', V: 4 } // 设计模式
}
constantize(printModeEnum)

// 金额单位
const amountUnitEnum = {
  YUAN: { L: '元', K: 'YUAN', V: 'y' },
  WAN: { L: '万元', K: 'WAN', V: 'w' }
}
constantize(amountUnitEnum)

// 长度/宽度/高度单位
const lengthUnitEnum = {
  MM: { L: '毫米', K: 'MM', V: 'mm' },
  M: { L: '米', K: 'M', V: 'm' }
}
constantize(lengthUnitEnum)

// 厚度单位
const thicknessUnitEnum = {
  MM: { L: '毫米', K: 'MM', V: 'mm' }
}
constantize(thicknessUnitEnum)

// 重量单位
const weightUnitEnum = {
  G: { L: '克', K: 'G', V: 'g' },
  KG: { L: '千克', K: 'KG', V: 'kg' },
  T: { L: '吨', K: 'T', V: 't' }
}
constantize(weightUnitEnum)

// 打印配置-字段类型
const fieldTypeEnum = {
  OTHER: { L: '其他', K: 'OTHER' },
  DICT: { L: '字典', K: 'DICT' },
  ENUM: { L: '枚举', K: 'ENUM' },
  DATE: { L: '日期', K: 'DATE' },
  DATES: { L: '时间段', K: 'DATES' },
  AMOUNT: { L: '金额', K: 'AMOUNT' },
  QUANTITY: { L: '数量', K: 'QUANTITY' },
  LENGTH: { L: '长度', K: 'LENGTH' },
  WEIGHT: { L: '重量', K: 'WEIGHT' },
  THICKNESS: { L: '厚度', K: 'THICKNESS' },
  METE: { L: '量', K: 'METE' }, // 含面积等
  RATE: { L: '百分比', K: 'RATE' },
  GUID: { L: '编号', K: 'GUID' },

  CONTRACT_NO: { L: '合同编号', K: 'CONTRACT_NO' },
  PROJECT: { L: '项目名称', K: 'PROJECT' },
  FACTORY_NAME: { L: '工厂名称', K: 'FACTORY_NAME' },
  MONOMER_NAME: { L: '单体名称', K: 'MONOMER_NAME' },
  AREA_NAME: { L: '区域名称', K: 'AREA_NAME' },
  AREA_DIVIDE: { L: '区域划分', K: 'AREA_DIVIDE' },
  AREA_AXIS: { L: '轴线/标高', K: 'AREA_AXIS' },
  WAREHOUSE_NAME: { L: '仓库', K: 'WAREHOUSE_NAME' },

  COMPONENT_PROCESS: { L: '工序', K: 'COMPONENT_PROCESS' },
  WORKSHOP: { L: '车间', K: 'WORKSHOP' },
  PRODUCTION_LINE: { L: '生产线', K: 'PRODUCTION_LINE' },
  TEAM_NAME: { L: '班组名称', K: 'TEAM_NAME' },

  SUBJECT_NAME: { L: '科目名称', K: 'SUBJECT_NAME' },
  SUBJECT_CODE: { L: '科目编号', K: 'SUBJECT_CODE' },
  SERIAL_NUMBER: { L: '物料编号', K: 'SERIAL_NUMBER' },
  STRUCTURE_NAME: { L: '构件名称', K: 'STRUCTURE_NAME' },
  ENCLOSURE_NAME: { L: '围护名称', K: 'ENCLOSURE_NAME' },
  MATERIAL_CLASS_FULL_NAME: { L: '物料种类全称', K: 'MATERIAL_CLASS_FULL_NAME' },
  MATERIAL_CLASS_NAME: { L: '物料种类', K: 'MATERIAL_CLASS_NAME' },
  MATERIAL: { L: '材质', K: 'MATERIAL' },
  PLATE_TYPE: { L: '板型', K: 'PLATE_TYPE' },
  SPECIFICATION: { L: '规格', K: 'SPECIFICATION' },
  COLOR: { L: '颜色', K: 'COLOR' },
  UNIT: { L: '单位', K: 'UNIT' },
  MEASUREMENT_UNIT: { L: '计量单位', K: 'MEASUREMENT_UNIT' },
  ACCOUNTING_UNIT: { L: '核算单位', K: 'ACCOUNTING_UNIT' },
  BRAND: { L: '品牌', K: 'BRAND' },

  PRODUCT_TYPE: { L: '产品类型', K: 'PRODUCT_TYPE' },
  ORDER_TYPE: { L: '订单类型', K: 'ORDER_TYPE' },
  STRUCTURE_TYPE: { L: '结构类型', K: 'STRUCTURE_TYPE' },
  REIMBURSEMENT_TYPE: { L: '报销类型', K: 'REIMBURSEMENT_TYPE' },
  SUPPLIER_FEE_TYPE: { L: '供应商费用类型', K: 'SUPPLIER_FEE_TYPE' },
  PAYMENT_REASON: { L: '付款事由', K: 'PAYMENT_REASON' },

  USER_NAME: { L: '用户名称', K: 'USER_NAME' },
  COMPANY_NAME: { L: '公司名称', K: 'COMPANY_NAME' },
  DEPT: { L: '部门', K: 'DEPT' },
  PHONE: { L: '手机号', K: 'PHONE' },
  TEL: { L: '电话', K: 'TEL' },
  LICENSE_PLATE: { L: '车牌号', K: 'LICENSE_PLATE' },
  ADDRESS: { L: '地址', K: 'ADDRESS' },

  INVOICE_NO: { L: '发票编号', K: 'INVOICE_NO' },
  BANK: { L: '银行', K: 'BANK' },
  BANK_ACCOUNT: { L: '银行账号', K: 'BANK_ACCOUNT' } // 非银行卡号
}
constantize(fieldTypeEnum)

// 数据来源
const dataSourceEnum = {
  SYSTEM: { L: '系统', K: 'SYSTEM', V: 1 },
  CUSTOMIZE: { L: '自定义', K: 'CUSTOMIZE', V: 2 }
}
constantize(dataSourceEnum)

// 打印配置-对齐方式
const alignEnum = {
  LEFT: { L: '左对齐', K: 'LEFT', V: 1 },
  CENTER: { L: '居中对齐', K: 'CENTER', V: 2 },
  RIGHT: { L: '右对齐', K: 'RIGHT', V: 3 }
}
constantize(alignEnum)

// 打印配置-垂直对齐方式
const verticleAlignEnum = {
  TOP: { L: '顶端对齐', K: 'TOP', V: 1 },
  BOTTOM: { L: '垂直居中', K: 'BOTTOM', V: 2 },
  CENTER: { L: '底端对齐', K: 'CENTER', V: 3 }
}
constantize(verticleAlignEnum)

// 表格配置项
const tableConfigItemEnum = {
  BASE: { L: '基础', K: 'BASE', V: 1 },
  TITLE: { L: '标题', K: 'TITLE', V: 2 },
  TABLE: { L: '表格', K: 'TABLE', V: 3 },
  HEADER: { L: '表头', K: 'HEADER', V: 4 },
  FOOTER: { L: '表尾', K: 'FOOTER', V: 5 }
}
constantize(tableConfigItemEnum)

// 日期格式
const dateFormatEnum = {
  DEFAULT: { L: 'YY/MM/DD', K: 'DEFAULT', V: 'YY/MM/DD' },
  ONE: { L: 'YYYY/MM/DD', K: 'ONE', V: 'YYYY/MM/DD' },
  TWO: { L: 'YY/M/D', K: 'TWO', V: 'YYYY/M/D' }
}
constantize(dateFormatEnum)

// css单位
const cssUnitEnum = {
  MM: { L: '毫米', K: 'MM', V: 'mm' },
  CM: { L: '厘米', K: 'CM', V: 'cm' },
  IN: { L: '英寸', K: 'IN', V: 'in' }
  // PT: { L: '磅', K: 'PT', V: 'pt' },
  // PX: { L: '像素', K: 'PX', V: 'px' }
}
constantize(cssUnitEnum)

// css单位小数精度
const cssUnitPrecisionEnum = {
  ZERO: { L: '0', K: 'ZERO', V: 0 },
  ONE: { L: '1', K: 'ONE', V: 1 },
  TWO: { L: '2', K: 'TWO', V: 2 }
}
constantize(cssUnitPrecisionEnum)

// 金额小数精度
const amountPrecisionEnum = {
  ZERO: { L: '0', K: 'ZERO', V: 0 },
  ONE: { L: '1', K: 'ONE', V: 1 },
  TWO: { L: '2', K: 'TWO', V: 2 },
  THREE: { L: '3', K: 'THREE', V: 3 }
}
constantize(amountPrecisionEnum)

// 页码格式
const pageFormatEnum = {
  DEFAULT: { L: '1 / 1', K: 'DEFAULT', V: 1 },
  ONE: { L: '第1页 / 共1页', K: 'ONE', V: 2 },
  TWO: { L: '第一页 / 共一页', K: 'TWO', V: 3 }
}
constantize(pageFormatEnum)

export {
  orientEnum, // 打印方向
  cssUnitEnum, // css单位
  cssUnitPrecisionEnum, // css单位小数精度
  amountPrecisionEnum, // 金额小数精度
  alignEnum, // 打印配置-对齐方式
  verticleAlignEnum, // 打印配置-垂直对齐方式
  fieldTypeEnum, // 打印配置-字段类型
  dataSourceEnum, // 数据来源
  dateFormatEnum, // 日期格式
  pageFormatEnum, // 页码格式
  weightTypeEnum, // 重量类型
  tableConfigItemEnum, // 表格配置项
  printModeEnum, // 打印模式
  amountUnitEnum, // 金额单位
  lengthUnitEnum, // 长度单位
  weightUnitEnum, // 重量单位
  thicknessUnitEnum // 厚度单位
}

export default {
  key2val,
  toArr,
  getBits,
  setEnumValue
}

