import { isNotBlank, toPrecision } from '@/utils/data-type'
import { getDP } from '@/utils/data-type/number'
import { DP } from '@/settings/config'

// 处理金额变化
export default function usePriceSet(meteField = 'mete') {
  // 处理含税单价变化
  const handleUnitPriceChange = (val, row) => {
    const dp = getDP(val)
    if (dp > 10) {
      row.unitPrice = toPrecision(val, 10)
      val = row.unitPrice
    }
    row.priceType = 'unitPrice'
    row.amount = isNotBlank(val) ? toPrecision(val * row[meteField], DP.YUAN) : undefined
  }

  // 处理金额变化
  const handleAmountChange = (val, row) => {
    row.priceType = 'amount'
    row.unitPrice = isNotBlank(val) ? toPrecision(val / row[meteField], 10) : undefined
  }

  const handleMeteChangeCalcPrice = (row) => {
    // 像编辑时，数据都有，但是没有priceType，需要手动设置
    if (row.unitPrice && !row.amount && !row.priceType) row.priceType = 'unitPrice'
    if (!row.unitPrice && row.amount && !row.priceType) row.priceType = 'amount'
    if (!row.priceType || !row[meteField]) return
    if (row.priceType === 'unitPrice') {
      handleUnitPriceChange(row.unitPrice, row)
    }
    if (row.priceType === 'amount') {
      handleAmountChange(row.amount, row)
    }
  }

  return {
    handleUnitPriceChange,
    handleAmountChange,
    handleMeteChangeCalcPrice
  }
}
