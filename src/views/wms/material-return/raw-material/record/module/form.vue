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
import { isNotBlank } from '@data-type/index'

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
  // 真实退库状态
  crud.updateProp('boolRealReturn', detail.boolRealReturn || true)
  const allArr = []
  const allArr1 = []
  if (detail.basicClass === rawMatClsEnum.STEEL_PLATE.V) {
    detail.list.forEach(async (v) => {
      if (v.boolReturns && isNotBlank(v.list)) {
        await setSpecInfoToList(v.list)
        const ps = await numFmtByBasicClass(v.list, { toNum: true })
        await calcTheoryWeight(v.list)
        // source 原出库信息转换
        const childSourceList = v.list.map((row) => row.source)
        await setSpecInfoToList(childSourceList)
        const ps1 = await numFmtByBasicClass(
          childSourceList,
          { toNum: true },
          {
            mete: ['mete', 'returnableMete', 'singleMete', 'singleReturnableMete']
          }
        )
        // 计算source 理论重量
        await calcTheoryWeight(childSourceList)
        allArr.push(ps)
        allArr1.push(ps1)
      }
    })
  }
  await Promise.all(allArr1)
  await Promise.all(allArr)
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
  if (detail.basicClass === rawMatClsEnum.STEEL_PLATE.V) {
    detail.list.forEach(async (v) => {
      v.uid = v.id
      if (v.boolReturns && isNotBlank(v.list)) {
        v.list.forEach(k => {
          k.pid = v.id
          k.uid = k.id
          k.warehouseId = k.warehouse ? k.warehouse.id : k.warehouseId
          k.factoryId = k.factory ? k.factory.id : k.factoryId
          // 计算单重
          k.singleMete = +toFixed((k.theoryWeight / k.source.theoryWeight) * k.source.singleMete)
        })
      }
    })
  }
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
