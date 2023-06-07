<template>
  <el-table-column prop="workshopId" align="center" min-width="120px" label="车间">
    <template #default="{ row: { sourceRow: row }, $index }">
      <workshop-select
        v-model="row.workshopId"
        placeholder="选择车间"
        only-one-default
        :show-extra="$index !== 0"
        @change="handleWorkshopChange($event, $index, row)"
      />
    </template>
  </el-table-column>
  <el-table-column prop="warehouseId" label="存储位置" min-width="140px" align="center">
    <template #default="{ row: { sourceRow: row }, $index }">
      <warehouse-select
        v-model="row.warehouseId"
        :workshop-id="getWorkshopVal($index)"
        :basic-class="row.basicClass"
        :show-extra="!warehouseDittoableIndex.includes($index)"
        placeholder="存储位置"
      />
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed, watchEffect, ref } from 'vue'

import workshopSelect from '@/components-system/base/workshop-select.vue'
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
  getNotDittoArr: getWorkshopNotDittoArr,
  initScopeList: initWorkshopScopeList,
  handleValueChange: handleWorkshopChangeForValue,
  getRealVal: getWorkshopVal
} = useDittoRealVal('workshopId')

const warehouseDittoableIndex = computed(() => {
  return getWorkshopNotDittoArr()
})

watchEffect(() => initWorkshopScopeList(currentList.value || []))

function handleWorkshopChange(val, index, row) {
  handleWorkshopChangeForValue(val, index)
  if (val !== -1) {
    row.warehouseId = undefined
  } else {
    if (isBlank(row.warehouseId)) row.warehouseId = -1
  }
}
</script>
