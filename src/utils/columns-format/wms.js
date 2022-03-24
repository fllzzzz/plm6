export * from './common'
import { reviewTimeColumns } from './common'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { rawMatClsEnum } from '@enum-ms/classification'

// 物料信息
export const materialColumns = [
  ['project', ['parse-project', { onlyShortName: true }]],
  ['projectFullName', 'parse-project', { source: 'project' }],
  ['quantity', ['to-fixed-field', 'measurePrecision']],
  ['mete', ['to-fixed-field', 'accountingPrecision']]
]

// 物料信息，嵌套
export const materialNestedColumns = [
  ['material.project', ['parse-project', { onlyShortName: true }]],
  ['material.projectFullName', 'parse-project', { source: 'material.project' }],
  ['material.quantity', ['to-fixed-field', 'measurePrecision']],
  ['material.mete', ['to-fixed-field', 'accountingPrecision']]

]

// 物料信息-带可操作数量
export const materialOperateColumns = [
  ...materialColumns,
  ['operableQuantity', ['to-fixed-field', 'measurePrecision']],
  ['operableMete', ['to-fixed-field', 'accountingPrecision']]
]

// 物料信息-带金额
export const materialHasAmountColumns = [
  ...materialColumns,
  // 金额相关
  ['invoiceType', ['parse-enum', invoiceTypeEnum, { f: 'SL' }]],
  ['taxRate', ['suffix', '%']],
  ['unitPrice', 'to-thousand'],
  ['amount', 'to-thousand'],
  ['amountExcludingVAT', 'to-thousand'],
  ['inputVAT', 'to-thousand']
]

// wms单据信息
export const wmsReceiptColumns = [
  ...reviewTimeColumns,
  ['projects', ['parse-project', { onlyShortName: true }]],
  ['projectsFullName', 'parse-project', { source: 'projects' }],
  ['basicClass', ['parse-enum', rawMatClsEnum, { bit: true, split: ' | ' }]]
]

// ['operableQuantity', 'to-fixed-field', 'measurePrecision'],
// ['operableMete', 'to-fixed-field', 'accountingPrecision'],
