import { ref } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'

export default function useDrawing({ pidField = 'id', typeField = 'productType', productTypeField }) {
  const showDrawing = ref(false)
  const drawingRow = ref({})

  function drawingPreview(row) {
    drawingRow.value = {
      serialNumber: row.serialNumber,
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