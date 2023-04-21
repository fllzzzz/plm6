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
      <component :is="comp" :detail="form" edit @success="handleSuccess" />
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

import SteelApplication from '@/views/wms/material-inbound/raw-material/application/steel/index.vue'
import AuxMatApplication from '@/views/wms/material-inbound/raw-material/application/auxiliary-material/index.vue'
import OtherApplication from '@/views/wms/material-inbound/raw-material/application/other/index.vue'
import GasApplication from '@/views/wms/material-inbound/raw-material/application/gas/index.vue'

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
    case matClsEnum.OTHER.V:
      return OtherApplication
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

function handleSuccess() {
  crud.cancelCU()
  crud.refresh()
}
</script>

<style lang="scss" scoped>
::v-deep(.inbound-application-container) {
  .header {
    padding: 0 0 10px 0;
  }
  .main-content {
    padding: 0;
  }
}
</style>
