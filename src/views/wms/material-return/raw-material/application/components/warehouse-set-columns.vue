<template>
  <el-table-column prop="factoryId" align="center" min-width="120px" label="工厂">
    <template #default="{ row: { sourceRow: row }, $index }">
      <factory-select
        v-model="row.factoryId"
        placeholder="选择工厂"
        only-one-default
        :show-extra="$index !== 0"
        @change="handleFactoryChange($event, $index, row)"
      />
    </template>
  </el-table-column>
  <el-table-column prop="warehouseId" label="存储位置" min-width="140px" align="center">
    <template #default="{ row: { sourceRow: row }, $index }">
      <warehouse-select
        v-model="row.warehouseId"
        :factory-id="getFactoryVal($index)"
        :basic-class="row.basicClass"
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
  list: {
    type: Array,
    default: () => []
  }
})
const currentList = ref([])

watchEffect(() => { currentList.value = props.list })

const {
  getNotDittoArr: getFactoryNotDittoArr,
  initScopeList: initFactoryScopeList,
  handleValueChange: handleFactoryChangeForValue,
  getRealVal: getFactoryVal
} = useDittoRealVal('factoryId')

const warehouseDittoableIndex = computed(() => {
  return getFactoryNotDittoArr()
})

watchEffect(() => initFactoryScopeList(currentList.value || []))

function handleFactoryChange(val, index, row) {
  handleFactoryChangeForValue(val, index)
  if (val !== -1) {
    row.warehouseId = undefined
  } else {
    if (isBlank(row.warehouseId)) row.warehouseId = -1
  }
}
</script>
