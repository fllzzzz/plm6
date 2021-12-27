<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > CRUD.STATUS.NORMAL"
    :content-loading="crud.editDetailLoading"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-inbound-application-record-form"
  >
    <template #content>
      <component :is="comp" :detail="form" edit @success="crud.cancelCU" />
    </template>
  </common-drawer>
</template>

<script setup>
import { computed } from 'vue'
import { regForm } from '@compos/use-crud'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import SteelPlateApplication from '@/views/wms/return-application/steel-plate/index.vue'
import AuxMatApplication from '@/views/wms/inbound-application/auxiliary-material/index.vue'
import GasApplication from '@/views/wms/inbound-application/gas/index.vue'
import { calcTheoryWeight } from '@/utils/wms/measurement-calc'
import { toFixed } from '@/utils/data-type'

const { CRUD, crud, form } = regForm()

const comp = computed(() => {
  switch (form.basicClass) {
    case matClsEnum.STEEL_PLATE.V:
    case matClsEnum.SECTION_STEEL.V:
    case matClsEnum.STEEL_COIL.V:
      return SteelPlateApplication
    case matClsEnum.MATERIAL.V:
      return AuxMatApplication
    case matClsEnum.GAS.V:
      return GasApplication
    default:
      return undefined
  }
})

CRUD.HOOK.beforeEditDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.list)
  await numFmtByBasicClass(detail.list, { toNum: true })
  await calcTheoryWeight(detail.list)
  // source 原出库信息转换
  const sourceList = detail.list.map((row) => row.source)
  await setSpecInfoToList(sourceList)
  await numFmtByBasicClass(
    sourceList,
    { toNum: true },
    {
      mete: ['mete', 'returnableMete', 'singleMete', 'singleReturnableMete']
    }
  )
  // 计算source 理论重量
  await calcTheoryWeight(sourceList)
  sourceList.forEach((row) => {
    row.sourceReturnableMete = row.returnableMete
  })
  detail.list.forEach((row) => {
    row.warehouseId = row.warehouse ? row.warehouse.id : row.warehouseId
    row.factoryId = row.factory ? row.warehouse.id : row.factoryId
    // 计算单重
    row.singleMete = +toFixed((row.theoryWeight / row.source.theoryWeight) * row.source.singleMete)
  })
}
</script>

<style lang="scss" scoped>
.app-container {
  padding: 0px;
}
</style>
