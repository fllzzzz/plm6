import { ref, reactive, watch } from 'vue'
import { isNotBlank, deepClone } from '@data-type/index'
import useProductLines from '@compos/store/use-product-lines'

export default function useGetLines({ emit, dataHasFormatHook }) {
  const productionLineVisible = ref(false) // 生产线dlg
  const lineLoad = ref(false) // 生产线是否已经加载
  // 排产表单模板 k-v  k-v, k:productionLineId, v:{ id: 任务id， productionLineId: 生产线id, quantity , sourceQuantity }
  const schedulingMapTemplate = reactive({})

  const { loaded, productLines } = useProductLines()

  watch(
    productLines,
    (val) => {
      dataFormat()
    },
    { immediate: true, deep: true }
  )

  function dataFormat() {
    const data = []
    try {
      if (isNotBlank(productLines.value)) {
        const _content = deepClone(productLines.value)
        _content.forEach((factory) => {
          if (factory) {
            const workshops = factory.workshopList || []
            workshops.forEach((workshop) => {
              workshop.factoryName = factory.shortName
              const productionLineList = workshop.productionLineList || []
              productionLineList.forEach((line) => {
                line.selected = false // 默认未选中生产线
                // 生成排产表单模板
                schedulingMapTemplate[line.id] = {
                  productionLineId: line.id,
                  factoryId: line.factoryId,
                  workshopId: line.workshopId
                }
              })
              data.push(workshop)
            })
          }
        })
        lineLoad.value = true
        if (typeof dataHasFormatHook === 'function') dataHasFormatHook()
      }
    } catch (error) {
      console.log('获取生产线', error)
    } finally {
      emit('update:lines', data)
    }
  }

  return {
    productionLineVisible,
    loaded,
    lineLoad,
    schedulingMapTemplate
  }
}
