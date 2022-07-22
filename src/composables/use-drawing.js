import { ref } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'

export default function useDrawing({ pidField = 'id', typeField = 'productType', productTypeField }) {
  const showDrawing = ref(false)
  const drawingRow = ref({})

  function drawingPreview(row) {
    drawingRow.value = {
      boolBim: row.boolBim,
      monomerId: row.monomerId,
      serialNumber: row.serialNumber,
      drawingSN: row.numbers, // 图纸编号
      attachmentId: row.attachmentId,
      productId: row[pidField],
      productType: productTypeField && componentTypeEnum[productTypeField]?.V || row[typeField]
    }
    showDrawing.value = true
  }

  return {
    showDrawing,
    drawingRow,
    drawingPreview
  }
}
