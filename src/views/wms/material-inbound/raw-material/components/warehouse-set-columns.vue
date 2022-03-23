<template>
  <el-table-column prop="factoryId" align="center" min-width="120px" label="工厂">
    <template #default="{ row, $index }">
      <factory-select
        v-if="row.sourceRow"
        v-model="row.sourceRow.factoryId"
        placeholder="请选择工厂"
        only-one-default
        :show-extra="$index !== 0"
        @change="handleFactoryChange($event, $index, row.sourceRow)"
      />
    </template>
  </el-table-column>
  <el-table-column prop="warehouseId" label="存储位置" min-width="140px" align="center">
    <template #default="{ row, $index }">
      <warehouse-select
        v-if="row.sourceRow"
        v-model="row.sourceRow.warehouseId"
        :factory-id="getFactoryVal($index)"
        :basic-class="row.sourceRow.basicClass"
        :show-extra="!warehouseDittoableIndex.includes($index)"
        placeholder="存储位置"
      />
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed, watchEffect, ref } from 'vue'

import factorySelect from '@/components-system/base/factory-select.vue'
import warehouseSelect from '@/components-system/wms/warehouse-select.vue'
import useDittoRealVal from '@/composables/form/use-ditto-real-val'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  form: {
    type: Object,
    default: () => {
      return {}
    }
  }
})
const currentForm = ref({ list: [] })

watchEffect(() => { currentForm.value = props.form })

const {
  getNotDittoArr: getFactoryNotDittoArr,
  initScopeList: initFactoryScopeList,
  handleValueChange: handleFactoryChangeForValue,
  getRealVal: getFactoryVal
} = useDittoRealVal('factoryId')

const warehouseDittoableIndex = computed(() => {
  return getFactoryNotDittoArr()
})

watchEffect(() => initFactoryScopeList(currentForm.value.list || []))

function handleFactoryChange(val, index, row) {
  handleFactoryChangeForValue(val, index)
  if (val !== -1) {
    row.warehouseId = undefined
  } else {
    if (isBlank(row.warehouseId)) row.warehouseId = -1
  }
}
</script>
