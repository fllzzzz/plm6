export * from './common'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'

// 物料信息
export const materialColumns = [
  ['project', ['parse-project', { onlyShortName: true }]],
  ['quantity', ['to-fixed-field', 'measurePrecision']],
  ['mete', ['to-fixed-field', 'accountingPrecision']]
]

// 物料信息-带金额
export const materialHasAmountColumns = [
  ['project', ['parse-project', { onlyShortName: true }]],
  ['quantity', ['to-fixed-field', 'measurePrecision']],
  ['mete', ['to-fixed-field', 'accountingPrecision']],

  // 金额相关
  ['invoiceType', ['parse-enum', invoiceTypeEnum, { f: 'SL' }]],
  ['taxRate', ['suffix', '%']],
  ['unitPrice', 'to-thousand'],
  ['amount', 'to-thousand'],
  ['amountExcludingVAT', 'to-thousand'],
  ['inputVAT', 'to-thousand']
]

// ['operableQuantity', 'to-fixed-field', 'measurePrecision'],
// ['operableMete', 'to-fixed-field', 'accountingPrecision'],
