<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > CRUD.STATUS.NORMAL"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    size="90%"
    custom-class="raw-mat-inbound-application-record-form"
  >
    <template #content>
      <component v-if="loaded" :is="comp" :detail="form" edit @success="crud.cancelCU" />
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import SteelApplication from '@/views/wms/inbound-application/steel/index.vue'
import AuxMatApplication from '@/views/wms/inbound-application/auxiliary-material/index.vue'
import GasApplication from '@/views/wms/inbound-application/gas/index.vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { STEEL_ENUM } from '@/settings/config'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

const { CRUD, crud, form } = regForm()
const loaded = ref(false)

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

CRUD.HOOK.beforeToEdit = async () => {
  loaded.value = false
  form.list = await numFmtByBasicClass(form.list, {
    toSmallest: false,
    toNum: true
  })
  await setSpecInfoToList(form.list)
}

CRUD.HOOK.afterToEdit = async () => {
  loaded.value = true
}
</script>
