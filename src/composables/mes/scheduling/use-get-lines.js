import { getAllFactoryWorkshopLines as getAllLines } from '@/api/mes/common'
import { ref, reactive } from 'vue'

export default function useGetLines({ emit, dataHasFormatHook }) {
  const productionLineVisible = ref(false) // 生产线dlg
  const lineLoading = ref(false) // 生产线加载状态
  const lineLoad = ref(false) // 生产线是否已经加载
  // 排产表单模板 k-v  k-v, k:productionLineId, v:{ id: 任务id， productionLineId: 生产线id, quantity , sourceQuantity }
  const schedulingMapTemplate = reactive({})

  const fetchLines = async () => {
    const data = []
    try {
      lineLoading.value = true
      const { content } = await getAllLines()
      content.forEach((factory) => {
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
    } catch (error) {
      console.log('获取生产线', error)
    } finally {
      emit('update:lines', data)
      lineLoading.value = false
    }
  }

  fetchLines()

  return {
    productionLineVisible,
    lineLoading,
    lineLoad,
    schedulingMapTemplate
  }
}
