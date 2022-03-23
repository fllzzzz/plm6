import { ref, computed } from 'vue'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import useMaxHeight from '@compos/use-max-height'

export default function useIndexInfo({ CRUD, crud, defaultBasicClass }) {
  // 展开keys
  const expandRowKeys = ref([])

  // 表格高度
  const { maxHeight } = useMaxHeight({ paginate: true })
  // 当前处理行
  const currentRow = ref({})
  // 出库办理显示
  const outboundHandlingVisible = ref(false)
  // 调拨办理显示
  const transferHandlingVisible = ref(false)
  // header ref
  const headerRef = ref()

  // 基础类型
  const basicClass = computed(() => crud.query.basicClass || defaultBasicClass)

  // 处理刷新
  CRUD.HOOK.handleRefresh = async (crud, { data }) => {
    await setSpecInfoToList(data.content)
    data.content = await numFmtByBasicClass(data.content, {
      toSmallest: false,
      toNum: false
    })
    // TODO:后期考虑由服务端处理
    data.content.forEach(async (v) => {
      v.operableQuantity = v.quantity - (v.frozenQuantity || 0)
      v.operableMete = v.mete - (v.frozenMete || 0)
      if (v.outboundUnitType === measureTypeEnum.MEASURE.V) {
        // 实际在出库中使用的数量
        v.corQuantity = v.quantity // 数量
        v.corFrozenQuantity = v.frozenQuantity // 冻结数量
        v.corOperableQuantity = v.operableQuantity // 可操作数量
      } else {
        // 核算量
        v.corQuantity = v.mete
        v.corFrozenQuantity = v.frozenMete
        v.corOperableQuantity = v.operableMete
      }
      if (Array.isArray(v.projectFrozen)) {
        v.projectFrozenKV = {}
        v.projectFrozenForUnitKV = {}
        // 数据转换
        v.projectFrozen = await numFmtByBasicClass(v.projectFrozen, {
          measureUnit: v.measureUnit,
          accountingUnit: v.accountingUnit,
          accountingPrecision: v.accountingPrecision,
          measurePrecision: v.measurePrecision,
          toSmallest: false,
          toNum: true
        })
        v.projectFrozen.forEach((pf) => {
          // 用于普通出库
          v.projectFrozenForUnitKV[pf.projectId] = v.outboundUnitType === measureTypeEnum.MEASURE.V ? pf.quantity : pf.mete
          // 用于批量出库
          v.projectFrozenKV[pf.projectId] = pf
        })
      }
    })
  }

  // 进行出库办理
  function toOutHandle(row) {
    currentRow.value = row.sourceRow
    outboundHandlingVisible.value = true
  }

  // 进行调拨办理
  function toTransfer(row) {
    currentRow.value = row
    transferHandlingVisible.value = true
  }

  // 出库成功处理
  function handleOutboundSuccess() {
    headerRef.value && headerRef.value.updateListNumber()
    crud.refresh()
  }

  // 调拨成功
  function handleTransferSuccess() {
    crud.refresh()
  }
  // 刷新
  function handleRefresh() {
    headerRef.value && headerRef.value.updateListNumber()
    crud.refresh()
  }

  return {
    headerRef,
    expandRowKeys,
    maxHeight,
    basicClass,
    currentRow,
    outboundHandlingVisible,
    transferHandlingVisible,
    toTransfer,
    toOutHandle,
    handleOutboundSuccess,
    handleTransferSuccess,
    handleRefresh
  }
}
