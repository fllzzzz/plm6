<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > CRUD.STATUS.NORMAL"
    :content-loading="crud.editDetailLoading"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    size="90%"
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
import { STEEL_ENUM } from '@/settings/config'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import SteelApplication from '@/views/wms/inbound-application/steel/index.vue'
import AuxMatApplication from '@/views/wms/inbound-application/auxiliary-material/index.vue'
import GasApplication from '@/views/wms/inbound-application/gas/index.vue'

const { CRUD, crud, form } = regForm()

const comp = computed(() => {
  switch (form.basicClass) {
    case matClsEnum.STEEL_PLATE.V:
    case matClsEnum.SECTION_STEEL.V:
    case matClsEnum.STEEL_COIL.V:
      return SteelApplication
    case matClsEnum.MATERIAL.V:
      return AuxMatApplication
    case matClsEnum.GAS.V:
      return GasApplication
    default:
      if (form.basicClass & STEEL_ENUM) return SteelApplication
      return undefined
  }
})

CRUD.HOOK.beforeEditDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.list)
  detail.list = await numFmtByBasicClass(detail.list, {
    toSmallest: false,
    toNum: true
  })
}
</script>
