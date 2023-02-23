import { ElMessageBox } from 'element-plus'

export default function useOverReceive({ quantityField = 'quantity', meteField = 'mete' }) {
  const handleOverQuantity = (row) => {
    if (row[quantityField] > row.purchaseQuantity && row[quantityField] !== row.originQuantity) {
      ElMessageBox.confirm('本次实收数大于采购数，请确认', '提示', {
        confirmButtonText: '是',
        cancelButtonText: '否',
        type: 'warning'
      }).then(() => {
        row.originQuantity = row[quantityField]
      }).catch(() => {
        row[quantityField] = row.originQuantity
      })
    } else {
      row.originQuantity = row[quantityField]
    }
  }

  const handleOverMete = (row) => {
    if (row[meteField] > row.purchaseMete && row[meteField] !== row.originMete) {
      ElMessageBox.confirm('本次实收量大于采购量，请确认', '提示', {
        confirmButtonText: '是',
        cancelButtonText: '否',
        type: 'warning'
      }).then(() => {
        row.originMete = row[meteField]
      }).catch(() => {
        row[meteField] = row.originMete
      })
    } else {
      row.originMete = row[meteField]
    }
  }

  return {
    handleOverQuantity,
    handleOverMete
  }
}
