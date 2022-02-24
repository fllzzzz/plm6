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
      <component :is="comp" :detail="form" edit @success="handleSuccess" />
    </template>
  </common-drawer>
</template>

<script setup>
import { computed } from 'vue'
import { regForm } from '@compos/use-crud'
import { matClsEnum, rawMatClsEnum } from '@/utils/enum/modules/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import SteelPlateApplication from '@/views/wms/material-return/raw-material/application/steel-plate/index.vue'
import SectionSteelApplication from '@/views/wms/material-return/raw-material/application/section-steel/index.vue'
import SteelCoilApplication from '@/views/wms/material-return/raw-material/application/steel-coil/index.vue'
import AuxMatApplication from '@/views/wms/material-return/raw-material/application/auxiliary-material/index.vue'
import GasApplication from '@/views/wms/material-return/raw-material/application/gas/index.vue'
import { calcTheoryWeight } from '@/utils/wms/measurement-calc'
import { toFixed } from '@/utils/data-type'

const { CRUD, crud, form } = regForm()

const comp = computed(() => {
  switch (form.basicClass) {
    case matClsEnum.STEEL_PLATE.V:
      return SteelPlateApplication
    case matClsEnum.SECTION_STEEL.V:
      return SectionSteelApplication
    case matClsEnum.STEEL_COIL.V:
      return SteelCoilApplication
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
    if (form.basicClass === rawMatClsEnum.SECTION_STEEL.V) {
      row.returnableLength = row.singleReturnableLength * row.quantity
      row.totalLength = row.length * row.quantity
      row.sourceReturnableLength = row.returnableLength
    }
  })
  detail.list.forEach((row) => {
    // 处理为表单可使用的数据
    row.warehouseId = row.warehouse ? row.warehouse.id : row.warehouseId
    row.factoryId = row.factory ? row.factory.id : row.factoryId
    // 计算单重
    row.singleMete = +toFixed((row.theoryWeight / row.source.theoryWeight) * row.source.singleMete)
  })
}

function handleSuccess() {
  crud.cancelCU()
  crud.refresh()
}
</script>

<style lang="scss" scoped>
.app-container {
  padding: 0px;
}
</style>
