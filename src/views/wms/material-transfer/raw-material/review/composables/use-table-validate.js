import { ElMessage } from 'element-plus'
import { validate, wrongCellMask, cleanUpData } from '@/composables/form/use-table-validate'
import { partyAMatTransferEnum } from '@/utils/enum/modules/wms'

/**
 * 买入的甲供物料金额填写校验
 * @param {array} list 校验列表
 * @param {object} rules 校验规则
 */
export default function useTableValidate() {
  // 价格校验
  const validatePrice = (value, row) => {
    if (row.boolPartyA && row.partyATransferType === partyAMatTransferEnum.BUY_IN.V) {
      if (!value) return false
      return true
    } else {
      return true
    }
  }

  const rules = {
    unitPrice: [{ validator: validatePrice, message: '请填写单价', trigger: 'blur' }],
    amount: [{ validator: validatePrice, message: '请填写金额', trigger: 'blur' }]
  }
  return {
    tableValidate: (list) => tableValidate(list, rules),
    wrongCellMask: (tableInfo) => wrongCellMask(tableInfo, rules),
    cleanUpData: (list) => cleanUpData(list)
  }
}

function tableValidate(list, rules) {
  let flag = true
  // 筛选需要校验的行
  const partyABuyInList = list.filter((row) => {
    return row.boolPartyA && row.partyATransferType === partyAMatTransferEnum.BUY_IN.V
  })
  // 不存在买入的甲供物料直接返回true
  if (partyABuyInList.length === 0) return { dealList: list, validResult: flag }

  const message = '请填写“买入甲供物料”的金额'
  for (const i in list) {
    const row = list[i]
    delete row.verify // 删除验证字段，避免切换科目级别产生规则混淆，以及进行空行处理

    row.verify = {}
    for (const rule in rules) {
      row.verify[rule] = validate(rule, rules[rule], row)
      if (!row.verify[rule]) {
        flag = false
      }
    }
  }

  if (!flag) {
    ElMessage({ message, type: 'error' })
  }

  return { dealList: list, validResult: flag }
}
